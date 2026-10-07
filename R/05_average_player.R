# ============================================================
# 05_average_player.R
# "The most average AFL career ever" - a WAR-style, z-score model.
#
# THE IDEA IN ONE PARAGRAPH
#   For every stat (plus games played) we ask: "how far is this player's
#   career from the average player's career, in standard deviations?"
#   That number is a z-score. 0 = exactly average, +1 = one SD above,
#   -1 = one SD below. Every stat gets the same weight. A player's
#   "average score" is the root-mean-square of their z-scores, so a
#   player who is +2 on one stat and -2 on another is NOT called average
#   (the misses add up instead of cancelling out).
#   Smallest score = the most average career.
#
# PIPELINE
#   1. Fetch per-game AFL Tables stats (cached per season).
#   2. Era-adjust each stat (divide by that season's league average).
#   3. Collapse to one row per player: whole-career per-game averages
#      (finals INCLUDED) plus career games played.
#   4. Z-score every stat across players (0 = average player).
#   5. Rank by RMS z-score. Check against Mahalanobis distance.
#
# Requires 00_config.R to already be sourced (CURRENT_SEASON, CACHE_DIR,
# PLAYER_STATS_CACHE).
# ============================================================

library(dplyr)
library(purrr)
library(tidyr)

# ---- Settings ---------------------------------------------------------
AVG_FIRST_CANDIDATE <- 1999   # earliest season we even try to fetch
AVG_LAST_SEASON     <- CURRENT_SEASON
AVG_MIN_GAMES       <- 0      # 0 = no minimum, i.e. the absolute average
AVG_EXCLUDE_ACTIVE  <- FALSE   # TRUE = only finished careers ("whole career")
AVG_RAW_DIR         <- file.path(CACHE_DIR, "avg_player_raw")
if (!dir.exists(AVG_RAW_DIR)) dir.create(AVG_RAW_DIR, recursive = TRUE)

# new_name = AFL Tables column name.
# `disposals` and `uncontested_possessions` are deliberately left out:
# disposals = kicks + handballs and uncontested = disposals - contested,
# so including them would count the same information twice and break the
# Mahalanobis check (a singular covariance matrix).
AVG_STATS <- c(
  kicks = "Kicks", handballs = "Handballs",
  contested_possessions = "Contested.Possessions",
  marks = "Marks", contested_marks = "Contested.Marks",
  marks_inside_50 = "Marks.Inside.50",
  tackles = "Tackles", clearances = "Clearances",
  inside_50s = "Inside.50s", rebounds = "Rebounds",
  one_percenters = "One.Percenters", hit_outs = "Hit.Outs",
  goals = "Goals", behinds = "Behinds", goal_assists = "Goal.Assists",
  clangers = "Clangers", frees_for = "Frees.For",
  frees_against = "Frees.Against", bounces = "Bounces"
)
AVG_STAT_NAMES  <- names(AVG_STATS)
AVG_COMPONENTS  <- c(AVG_STAT_NAMES, "games_played")  # 19 stats + games = 20

AVG_LABELS <- c(
  kicks = "Kicks", handballs = "Handballs",
  contested_possessions = "Contested possessions", marks = "Marks",
  contested_marks = "Contested marks", marks_inside_50 = "Marks inside 50",
  tackles = "Tackles", clearances = "Clearances", inside_50s = "Inside 50s",
  rebounds = "Rebound 50s", one_percenters = "One percenters",
  hit_outs = "Hit-outs", goals = "Goals", behinds = "Behinds",
  goal_assists = "Goal assists", clangers = "Clangers",
  frees_for = "Frees for", frees_against = "Frees against",
  bounces = "Bounces", games_played = "Games played"
)

# Stats where most players sit near zero and a few (rucks, key forwards)
# are huge. A raw z-score would be dominated by those few, so we
# compress with log1p() first. log1p(x) = log(1 + x), safe at x = 0.
AVG_SKEWED <- c("hit_outs", "goals", "behinds", "goal_assists",
                "marks_inside_50", "contested_marks", "bounces")

# Stats where MORE is worse. Only used to flip the sign of the signed
# "overall z" (so + always means "better"). The average score itself
# ignores direction: being far from average either way counts.
AVG_NEGATIVE <- c("clangers", "frees_against")

# ---- 1. Fetch (cached per season) --------------------------------------
fetch_avg_season <- function(yr) {
  f <- file.path(AVG_RAW_DIR, paste0("afltables_", yr, ".rds"))
  if (file.exists(f)) return(readRDS(f))

  # Reuse the Brownlow pipeline's raw cache where it already has this season.
  if (file.exists(PLAYER_STATS_CACHE)) {
    existing <- readRDS(PLAYER_STATS_CACHE)
    if ("Season" %in% names(existing) && yr %in% existing$Season) {
      out <- existing[existing$Season == yr, ]
      saveRDS(out, f)
      return(out)
    }
  }

  message("Fetching AFL Tables player stats for ", yr)
  out <- tryCatch(
    fitzRoy::fetch_player_stats_afltables(season = yr),
    error = function(e) {
      warning("Fetch failed for ", yr, ": ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(out) && nrow(out) > 0) saveRDS(out, f)
  out
}

# Rename to snake_case, build a stable player key, flag finals.
# Unlike the Brownlow pipeline, FINALS ARE KEPT here.
prep_avg_games <- function(raw) {
  needed <- c("Season", "Round", "Team", "Player", "ID",
              "First.name", "Surname", unname(AVG_STATS))
  missing_cols <- setdiff(needed, names(raw))
  if (length(missing_cols) > 0) {
    stop("AFL Tables pull is missing expected columns: ",
         paste(missing_cols, collapse = ", "))
  }
  raw %>%
    mutate(
      season   = as.integer(Season),
      # Finals have non-numeric round labels (QF, SF, PF, GF).
      is_final = is.na(suppressWarnings(as.integer(Round))),
      name     = coalesce(Player,
                          if_else(!is.na(First.name) & !is.na(Surname),
                                  paste(First.name, Surname), NA_character_)),
      # Numeric AFL Tables ID is the real identity; fall back to the name.
      player_key = coalesce(as.character(ID), name)
    ) %>%
    filter(!is.na(player_key)) %>%
    select(season, team = Team, is_final, name, player_key, all_of(AVG_STATS))
}

avg_games_all <- map(AVG_FIRST_CANDIDATE:AVG_LAST_SEASON, fetch_avg_season) %>%
  compact() %>%
  map(prep_avg_games) %>%
  bind_rows()

# ---- Which seasons record every stat we need? ---------------------------
# AFL Tables only started tracking some stats partway through its history.
# A season is "usable" if every stat has a non-zero total that year.
coverage <- avg_games_all %>%
  group_by(season) %>%
  summarise(across(all_of(AVG_STAT_NAMES), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(all_recorded = if_all(all_of(AVG_STAT_NAMES), ~ .x > 0))

ok_seasons <- sort(coverage$season[coverage$all_recorded])
# Keep the unbroken run of good seasons that ends at the latest season.
gaps <- which(diff(ok_seasons) > 1)
if (length(gaps) > 0) ok_seasons <- ok_seasons[(max(gaps) + 1):length(ok_seasons)]
if (length(ok_seasons) < 5) stop("Fewer than 5 usable seasons - check the data pull.")
AVG_START_SEASON <- min(ok_seasons)
AVG_END_SEASON   <- max(ok_seasons)

games_raw <- avg_games_all %>%
  filter(season %in% ok_seasons) %>%
  filter(if_all(all_of(AVG_STAT_NAMES), ~ !is.na(.x)))

# ---- 2. Era adjustment ---------------------------------------------------
# Divide each stat by that season's league-average value per player-game.
# After this, 1.00 = "average for the year it was played".
# ave(x, group, FUN = mean) returns the group mean repeated for every row.
games_idx <- games_raw
for (s in AVG_STAT_NAMES) {
  games_idx[[s]] <- games_idx[[s]] / ave(games_idx[[s]], games_idx$season, FUN = mean)
}

# ---- 3. One row per player (whole career) --------------------------------
display_names <- games_raw %>%
  count(player_key, name) %>%
  arrange(player_key, desc(n)) %>%
  distinct(player_key, .keep_all = TRUE) %>%     # most common spelling wins
  select(player_key, player = name)

career_idx <- games_idx %>%
  group_by(player_key) %>%
  summarise(
    games        = n(),                 # every game, finals included
    finals_games = sum(is_final),
    first_season = min(season),
    last_season  = max(season),
    across(all_of(AVG_STAT_NAMES), mean),   # mean era-adjusted index per game
    .groups = "drop"
  )

# Plain per-game averages (not era-adjusted), for display only.
career_raw <- games_raw %>%
  group_by(player_key) %>%
  summarise(across(all_of(AVG_STAT_NAMES), mean, .names = "{.col}_pg"),
            .groups = "drop")

# Who counts as a complete career?
#  - Anyone already playing in the first usable season may have debuted
#    earlier, so their career (and games played) would be cut off.
#  - Optionally drop players still active in the last season.
eligible <- career_idx %>%
  filter(first_season > AVG_START_SEASON, games >= AVG_MIN_GAMES)
if (AVG_EXCLUDE_ACTIVE) eligible <- eligible %>% filter(last_season < AVG_END_SEASON)

# ---- 4. Z-scores (0 = the average player) ---------------------------------
feat <- eligible %>%
  mutate(across(all_of(AVG_SKEWED), log1p),      # compress skewed stats
         games_played = log(games))              # careers are very skewed too

M <- as.matrix(feat[, AVG_COMPONENTS])
# z = (value - mean across players) / sd across players, column by column.
z_mat <- sweep(sweep(M, 2, colMeans(M)), 2, apply(M, 2, sd), "/")
colnames(z_mat) <- paste0("z_", AVG_COMPONENTS)

# ---- 5. Scores -----------------------------------------------------------------
# PRIMARY: root-mean-square z. Every stat has equal weight; direction is
# ignored. 0 = average on everything. Smaller = more average.
avg_score <- sqrt(rowMeans(z_mat^2))

# WAR-style signed rating: mean z with "bad" stats flipped. + = better than
# the average player overall, - = worse. Can be ~0 for someone who is
# great at some things and poor at others, which is why it is NOT the
# ranking metric.
direction <- ifelse(AVG_COMPONENTS %in% AVG_NEGATIVE, -1, 1)
overall_z <- rowMeans(sweep(z_mat, 2, direction, "*"))

# SENSITIVITY CHECK: Mahalanobis distance. Like RMS z but it discounts
# stats that move together (e.g. clearances and contested possessions).
# Computed through eigen-decomposition so a near-singular covariance
# can't crash it.
eig  <- eigen(cov(z_mat), symmetric = TRUE)
keep <- eig$values > 1e-8 * max(eig$values)
pcs  <- z_mat %*% eig$vectors[, keep, drop = FALSE]
d_mahal <- sqrt(rowSums(sweep(pcs^2, 2, eig$values[keep], "/")))

avg_career_table <- bind_cols(
  feat %>% select(player_key, first_season, last_season, games, finals_games),
  as_tibble(z_mat),
  tibble(avg_score = avg_score, overall_z = overall_z, d_mahal = d_mahal)
) %>%
  mutate(rank       = rank(avg_score, ties.method = "first"),
         rank_mahal = rank(d_mahal,   ties.method = "first")) %>%
  left_join(display_names, by = "player_key") %>%
  left_join(career_raw,    by = "player_key") %>%
  mutate(disposals_pg = kicks_pg + handballs_pg) %>%
  arrange(rank)

# Profile of the #1 career: one z-score per component.
avg_top_profile <- tibble(
  stat  = AVG_COMPONENTS,
  label = unname(AVG_LABELS[AVG_COMPONENTS]),
  z     = unlist(avg_career_table[1, paste0("z_", AVG_COMPONENTS)])
)

avg_meta <- list(
  start_season = AVG_START_SEASON, end_season = AVG_END_SEASON,
  n_seasons    = length(ok_seasons), n_players = nrow(avg_career_table),
  n_games      = nrow(games_raw),
  n_components = length(AVG_COMPONENTS),
  min_games    = AVG_MIN_GAMES, exclude_active = AVG_EXCLUDE_ACTIVE,
  # exp(mean(log(games))) is the games-played value that sits at z = 0.
  games_at_zero = exp(mean(log(feat$games))),
  one_game_players = sum(feat$games == 1),
  top10_overlap = length(intersect(
    avg_career_table$player_key[avg_career_table$rank       <= 10],
    avg_career_table$player_key[avg_career_table$rank_mahal <= 10]))
)
