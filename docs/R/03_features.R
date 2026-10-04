
# ============================================================
# 03_features.R
# Builds the master player-game table: standard features plus
# the signature/innovative features. Adjusted from the original
# design to work with AFL Tables' actual fields (no metres_gained
# or true disposal_efficiency/score_involvements -- those are
# Champion Data metrics AFL Tables doesn't carry; proxies are used
# instead, documented at each feature).
#
# Requires 00_config.R through 02_clean.R sourced first.
# ============================================================

library(dplyr)
library(tidyr)

# --- Start from player_stats, join coaches' votes ---------------------
# A team can play more than once under the same round number, so
# (season, round, team, player_key) is NOT a unique game identifier.
# Joining on it alone matches a player's coaches' votes from BOTH games
# to each of his rows, which duplicates rows (the cause of the
# "master has N rows ... but player_stats had M" error). When the
# coaches' table carries `opponent` we add it to the key, which pins
# the join to a single game.
cv_keys <- c("season", "round", "team", "player_key")
if ("opponent" %in% names(coaches_votes) && "opponent" %in% names(player_stats)) {
  cv_keys <- c(cv_keys, "opponent")
}

# Guarantee the right-hand table is unique on the join key no matter
# what (keeping the smaller value, consistent with 01/02), and say so
# if anything had to be collapsed.
coaches_join <- coaches_votes %>%
  dplyr::select(dplyr::all_of(cv_keys), coaches_votes) %>%
  group_by(across(all_of(cv_keys))) %>%
  summarise(coaches_votes = min(coaches_votes, na.rm = TRUE), .groups = "drop")
if (nrow(coaches_join) < nrow(coaches_votes)) {
  warning(nrow(coaches_votes) - nrow(coaches_join), " coaches_votes row(s) shared a join key ",
          "(", paste(cv_keys, collapse = ", "), ") and were collapsed to the smaller value.")
}

master <- player_stats %>%
  left_join(coaches_join, by = cv_keys) %>%
  mutate(coaches_votes = replace_na(coaches_votes, 0))

# Diagnostic, not a fix: if the coaches_votes join above matched more
# than one row for any (season, round, team, player) combination, that
# duplication compounds badly in the joins further down this script
# (each one multiplies it again). This check surfaces it immediately
# rather than as a much harder-to-diagnose memory error later.
join_dup_check <- player_stats %>%
  count(season, round, team, player_id) %>%
  filter(n > 1)
if (nrow(join_dup_check) > 0) {
  warning(nrow(join_dup_check), " (season, round, team, player_id) combination(s) already had more ",
          "than one row in player_stats before any join in this script ran. This is the real root ",
          "cause of any downstream row-explosion / memory error -- run `join_dup_check` to see which, ",
          "and cross-reference against player_stats to find the source.")
}
if (nrow(master) != nrow(player_stats)) {
  warning("The coaches_votes join changed row count: player_stats had ", nrow(player_stats),
          " rows, master now has ", nrow(master), " rows. The coaches_votes join key ",
          "(season, round, team, player_key) isn't unique for at least one row -- this is very ",
          "likely why the ladder-position joins later in this script blow up memory. Run ",
          "`coaches_votes %>% count(season, round, team, player_key) %>% filter(n > 1)` to find it.")
}

# --- SuperCoach / Fantasy scores -------------------------------------
# Unlike coaches' votes, a missing SuperCoach/Fantasy score genuinely
# means "unknown" (a fetch gap, a season outside that product's
# coverage), not a real zero -- replacing it with 0 would look like a
# terrible performance rather than missing data, so it's left as NA.
# XGBoost handles NA natively (learns an optimal default split
# direction per feature); the explanatory model only uses a curated
# feature list that excludes these two for exactly this reason -- see
# EXPLANATORY_FEATURES in 00_config.R.
rows_before_scores <- nrow(master)
unique_scores <- function(df, col) {
  df %>%
    dplyr::select(season, round, team, player_key, dplyr::all_of(col)) %>%
    group_by(season, round, team, player_key) %>%
    summarise(!!col := mean(.data[[col]], na.rm = TRUE), .groups = "drop") %>%
    mutate(across(all_of(col), ~ ifelse(is.nan(.x), NA_real_, .x)))
}
master <- master %>%
  left_join(unique_scores(supercoach_scores, "supercoach_score"),
            by = c("season", "round", "team", "player_key")) %>%
  left_join(unique_scores(fantasy_scores, "fantasy_score"),
            by = c("season", "round", "team", "player_key"))
if (nrow(master) != rows_before_scores) {
  warning("The supercoach/fantasy score join(s) changed row count: had ", rows_before_scores,
          " rows, now ", nrow(master), ". One of those sources has a non-unique ",
          "(season, round, team, player_key) combination -- run ",
          "`supercoach_scores %>% count(season, round, team, player_key) %>% filter(n > 1)` ",
          "and the same for fantasy_scores to find it.")
}
if (nrow(master) != nrow(player_stats)) {
  stop("master has ", nrow(master), " rows after the coaches/SuperCoach/Fantasy joins but player_stats ",
       "has ", nrow(player_stats), ". Two different players in the same team and round share a ",
       "normalised name (player_key). Run: player_stats %>% count(season, round, team, player_key) ",
       "%>% filter(n > 1)")
}
coverage_msg <- master %>%
  summarise(supercoach_pct = round(100 * mean(!is.na(supercoach_score)), 1),
            fantasy_pct = round(100 * mean(!is.na(fantasy_score)), 1))
message("SuperCoach score coverage: ", coverage_msg$supercoach_pct, "% of rows. ",
        "Fantasy score coverage: ", coverage_msg$fantasy_pct, "% of rows.")

# ---------------------------------------------------------------
# STANDARD FEATURES
# ---------------------------------------------------------------
master <- master %>%
  group_by(season, round, team) %>%
  mutate(
    disposal_share  = disposals  / pmax(1, sum(disposals, na.rm = TRUE)),
    clearance_share = clearances / pmax(1, sum(clearances, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate(
    team_won   = team_score > opponent_score,
    margin     = team_score - opponent_score,
    close_game = abs(margin) <= 12
  )

# ---------------------------------------------------------------
# INNOVATIVE / SIGNATURE FEATURES
# ---------------------------------------------------------------

# 1. UNDERDOG MULTIPLIER -- output scaled by opponent strength at the
# time (rolling win rate to date, no lookahead).
opponent_strength <- master %>%
  distinct(season, round, team, team_score, opponent_score) %>%
  mutate(win = team_score > opponent_score) %>%
  arrange(season, team, round) %>%
  group_by(season, team) %>%
  mutate(rolling_win_rate = lag(cummean(as.numeric(win)), default = 0.5)) %>%
  ungroup() %>%
  # Safeguard: guarantee exactly one row per (season, round, team) no
  # matter what, so the join below can never multiply rows. If this
  # actually drops anything, it means AFL Tables has more than one
  # score pair recorded for the same team in the same round number --
  # a real data anomaly worth knowing about, not a silent non-issue.
  distinct(season, round, team, .keep_all = TRUE) %>%
  dplyr::select(season, round, opponent = team, opponent_strength = rolling_win_rate)

opp_strength_dropped <- master %>% distinct(season, round, team, team_score, opponent_score) %>%
  count(season, round, team) %>% filter(n > 1)
if (nrow(opp_strength_dropped) > 0) {
  warning(nrow(opp_strength_dropped), " (season, round, team) combination(s) have more than one ",
          "distinct (team_score, opponent_score) pair recorded -- a real data anomaly, not just a ",
          "duplication artifact. Run `opp_strength_dropped` to see which games.")
}

master <- master %>%
  left_join(opponent_strength, by = c("season", "round", "opponent")) %>%
  mutate(
    opponent_strength = replace_na(opponent_strength, 0.5),
    underdog_multiplier = (disposals + 2 * clearances) * (0.5 + opponent_strength)
  )

# 2. NARRATIVE ATTENTION INDEX -- game-salience proxy: margin
# closeness, top-8 clash, late-season timing.
ladder_position <- master %>%
  distinct(season, round, team, team_score, opponent_score) %>%
  mutate(win = team_score > opponent_score) %>%
  group_by(season, team) %>%
  arrange(round) %>%
  mutate(cum_wins = lag(cumsum(as.numeric(win)), default = 0)) %>%
  ungroup() %>%
  group_by(season, round) %>%
  mutate(ladder_rank = rank(-cum_wins, ties.method = "min")) %>%
  ungroup() %>%
  # Same safeguard as opponent_strength above -- guarantees exactly one
  # row per (season, round, team) before the double join below, which
  # is where any leftover duplication would otherwise compound hardest
  # (joined twice in a row: once by team, once by opponent).
  distinct(season, round, team, .keep_all = TRUE) %>%
  dplyr::select(season, round, team, ladder_rank)

master <- master %>%
  left_join(ladder_position, by = c("season", "round", "team")) %>%
  left_join(ladder_position %>% rename(opponent = team, opponent_ladder_rank = ladder_rank),
            by = c("season", "round", "opponent")) %>%
  mutate(
    ladder_rank = replace_na(ladder_rank, 9),
    opponent_ladder_rank = replace_na(opponent_ladder_rank, 9),
    top8_clash = ladder_rank <= 8 & opponent_ladder_rank <= 8,
    late_season = round >= 18,
    narrative_attention_index =
      scales::rescale(1 / (1 + abs(margin)), to = c(0, 1)) * 0.5 +
      as.numeric(top8_clash) * 0.3 +
      as.numeric(late_season) * 0.2
  )

# Hard stop, not just a warning: the two joins just above are exactly
# where the earlier memory-crash happened, so this fails loudly and
# immediately (before anything expensive downstream runs) if row count
# has grown, rather than risking the same multi-gigabyte crash again.
if (nrow(master) != nrow(player_stats)) {
  stop("master has ", nrow(master), " rows after the ladder-position joins, but player_stats had ",
       nrow(player_stats), " -- one of the joins in this script is still multiplying rows despite ",
       "the distinct() safeguards. Check `join_dup_check` and `opp_strength_dropped` above for what ",
       "was flagged before this point.")
}

# 3. EFFICIENCY-WEIGHTED INFLUENCE SCORE -- rewards effective ball use.
# Redesigned after a VIF check found the original version (a near-
# linear combination of score_involvements, disposals and
# disposal_efficiency) was highly collinear with those same features
# already in the model (VIF ~33) -- it was mostly restating them, not
# adding independent signal. Now built as the INTERACTION between
# efficiency and productive volume (does efficiency matter more when
# volume is high?), then residualised against both main effects, so
# what's left specifically captures that interaction and nothing the
# main-effect features already explain on their own.
master <- master %>%
  mutate(efficiency_volume_interaction = disposal_efficiency * score_involvements)

efficiency_interaction_model <- lm(
  efficiency_volume_interaction ~ disposal_efficiency + score_involvements,
  data = master, na.action = na.exclude
)
master$efficiency_weighted_influence <- residuals(efficiency_interaction_model)
master$efficiency_volume_interaction <- NULL

# 4. DEFENSIVE SHADOW SCORE -- tackle/pressure work, normalised within
# position group and season (position_group is a rough heuristic --
# see 01_fetch_data.R).
master <- master %>%
  group_by(season, position_group) %>%
  mutate(
    defensive_shadow_score = scale(tackles)[, 1] * 0.6 +
      scale(one_percenters)[, 1] * 0.4
  ) %>%
  ungroup() %>%
  mutate(defensive_shadow_score = replace_na(defensive_shadow_score, 0))

# 5. CLUTCH CLEARANCE INDEX -- does clearance output matter more
# specifically in close games? Redesigned twice now: the original
# (clearances x a margin-based multiplier) was collinear with both
# clearances and margin via the VIF check. The first fix -- residuals
# of clearances regressed directly on margin -- created a WORSE
# problem: since clearances and margin both stay in the model as
# their own features too, that construction made clearances exactly
# reconstructable as clutch_clearance_index + margin's fitted effect,
# a perfect linear dependency that broke polr()'s fitting outright
# ("rank-deficient design"). Fixed the same way efficiency_weighted_influence
# was: residualise an INTERACTION (product) term instead of a raw
# feature -- a product can't be exactly reconstructed from its two
# linear main effects, so this doesn't recreate the same problem.
master <- master %>%
  mutate(clearance_closegame_interaction = clearances * as.numeric(close_game))

clutch_model <- lm(clearance_closegame_interaction ~ clearances + close_game,
                   data = master, na.action = na.exclude)
master$clutch_clearance_index <- residuals(clutch_model)
master$clearance_closegame_interaction <- NULL

# 6. REPUTATION GAP -- prior career votes/reputation, isolated as its
# own feature. Only meaningful within counted seasons, since a
# pending season's own votes aren't known yet (that's the point --
# reputation should be built from settled history, not the season
# being evaluated).
master <- master %>%
  arrange(player_id, season, round) %>%
  group_by(player_id) %>%
  mutate(
    votes_for_history    = if_else(season %in% counted_seasons, votes, NA_real_),
    votes_last5          = zoo::rollapply(lag(replace_na(votes_for_history, 0), default = 0),
                                          width = 5, FUN = sum, align = "right",
                                          partial = TRUE, fill = 0),
    career_votes_todate  = cumsum(replace_na(lag(votes_for_history, default = 0), 0)),
    reputation_gap        = scale(career_votes_todate)[, 1]
  ) %>%
  ungroup() %>%
  mutate(
    votes_last5 = replace_na(votes_last5, 0),
    reputation_gap = replace_na(reputation_gap, 0)
  )

saveRDS(master, MASTER_TABLE_CACHE)
