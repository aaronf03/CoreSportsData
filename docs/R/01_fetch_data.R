# ============================================================
# 01_fetch_data.R
# Pulls player stats (which already contain per-game Brownlow
# votes, scores, and teams -- no separate results/votes merge
# needed) plus AFLCA coaches' votes.
#
# Confirmed against a live fetch on 2026-09-22:
#   fetch_player_stats_afltables(season) returns Title.Case.Dot
#   column names (Disposals, Contested.Possessions, Team, Season,
#   Round, Brownlow.Votes, Home.score, Away.score, Home.Away, ...)
#   -- 81 columns total, includes per-game Brownlow.Votes, already
#   backfilled for counted seasons (confirmed non-zero for
#   2022-2025) and NA for a season not yet counted (confirmed for
#   2026, pre-count-night).
#
# Requires 00_config.R to already be sourced.
# ============================================================

library(dplyr)
library(purrr)
library(janitor)

# ---------------------------------------------------------------
# DEMO MODE: synthetic data generator
# ---------------------------------------------------------------
# Mirrors the real structure confirmed above: every season except
# the most recent has fully-populated per-game votes; the most
# recent season's votes are all NA (not yet counted), with a
# separate season-total table standing in for fetch_awards_brownlow.
generate_demo_data <- function(seasons) {
  teams <- c("Collingwood", "Geelong", "Fremantle", "Brisbane Lions",
             "Western Bulldogs", "Carlton", "Sydney", "Melbourne",
             "GWS", "Port Adelaide", "Hawthorn", "Adelaide",
             "St Kilda", "Gold Coast", "North Melbourne", "Richmond",
             "West Coast", "Essendon")

  n_players_per_team <- 30
  players <- expand.grid(team = teams, idx = 1:n_players_per_team) %>%
    mutate(player = paste0(team, "_Player_", idx),
           player_id = dplyr::row_number(),   # stand-in for AFL Tables' real numeric ID
           quality    = rbeta(n(), 2, 5),
           reputation = pmax(0, quality + rnorm(n(), 0, 0.1))) %>%
    dplyr::select(-idx)

  rounds <- 1:23
  pending_season <- max(seasons)   # matches the real "not yet counted" season

  player_stats <- players %>%
    tidyr::crossing(season = seasons, round = rounds) %>%
    mutate(played = rbinom(n(), 1, 0.85)) %>%
    filter(played == 1) %>%
    mutate(
      match_id   = paste(season, round, team, sep = "_"),
      opponent   = sample(teams, n(), replace = TRUE),
      team_score = round(rnorm(n(), 85, 20)),
      opponent_score = round(rnorm(n(), 85, 20)),
      disposals  = pmax(2, round(rnorm(n(), 18 + quality * 20, 6))),
      contested_possessions   = pmax(0, round(disposals * runif(n(), 0.3, 0.55))),
      uncontested_possessions = pmax(0, disposals - contested_possessions),
      handballs  = pmax(0, round(disposals * runif(n(), 0.35, 0.55))),
      kicks      = pmax(0, disposals - handballs),
      marks      = pmax(0, round(rnorm(n(), 3 + quality * 4, 2))),
      contested_marks  = pmax(0, round(marks * runif(n(), 0.1, 0.3))),
      marks_inside_50  = pmax(0, round(rnorm(n(), 0.5 + quality * 1.5, 1))),
      rebounds   = pmax(0, round(rnorm(n(), 1 + (1 - quality) * 2, 1.2))),
      bounces    = pmax(0, round(rpois(n(), quality * 1.2))),
      frees_for     = pmax(0, round(rpois(n(), 1 + quality * 0.8))),
      frees_against = pmax(0, round(rpois(n(), 1.2 - quality * 0.5))),
      clearances     = pmax(0, round(rnorm(n(), 2 + quality * 5, 2))),
      tackles        = pmax(0, round(rnorm(n(), 3 + quality * 3, 2))),
      inside_50s     = pmax(0, round(rnorm(n(), 2 + quality * 3, 1.5))),
      goals          = pmax(0, round(rpois(n(), quality * 1.5))),
      behinds        = pmax(0, round(rpois(n(), quality * 0.8))),
      goal_assists   = pmax(0, round(rpois(n(), quality * 0.5))),
      score_involvements = goals + behinds + goal_assists,
      one_percenters = pmax(0, round(rnorm(n(), 2 + (1 - quality) * 3, 1.5))),
      clangers       = pmax(0, round(rnorm(n(), 4 - quality * 1.5, 1.5))),
      disposal_efficiency = pmax(0.4, pmin(0.95, 1 - (clangers / pmax(1, disposals)))),
      time_on_ground_pct  = pmin(100, pmax(5, rnorm(n(), 82, 12))),
      hit_outs       = if_else(runif(n()) < 0.08, pmax(0, round(rnorm(n(), 20, 15))), 0),
      position_group = case_when(
        hit_outs >= 8 ~ "ruck",
        quality > 0.7 & inside_50s > 3 ~ "midfield",
        TRUE ~ sample(c("midfield", "forward", "defender"), n(), replace = TRUE,
                       prob = c(0.45, 0.3, 0.25))
      )
    ) %>%
    dplyr::select(-played)

  vote_pool <- player_stats %>%
    mutate(
      vote_propensity = 0.35 * scale(disposals)[,1] +
        0.25 * scale(contested_possessions)[,1] +
        0.15 * scale(score_involvements)[,1] +
        0.15 * scale(reputation)[,1] +
        0.10 * (team_score > opponent_score)
    )

  votes_full <- vote_pool %>%
    group_by(season, round, match_id) %>%
    slice_max(order_by = vote_propensity + rnorm(n(), 0, 0.5), n = 3, with_ties = FALSE) %>%
    mutate(votes = c(3, 2, 1)[row_number()]) %>%
    ungroup() %>%
    dplyr::select(season, round, match_id, team, player, votes)

  # Backfill per-game votes onto player_stats for counted seasons only;
  # NA (not zero) for the pending season, matching the real data exactly.
  player_stats <- player_stats %>%
    left_join(votes_full, by = c("season", "round", "match_id", "team", "player")) %>%
    mutate(votes = if_else(season == pending_season, NA_real_, coalesce(votes, 0)))

  season_total_votes <- votes_full %>%
    filter(season == pending_season) %>%
    group_by(season, player, team) %>%
    summarise(season_total_votes = sum(votes), .groups = "drop")

  coaches_pool <- vote_pool %>%
    mutate(
      coach_propensity = 0.30 * scale(disposals)[,1] +
        0.20 * scale(contested_possessions)[,1] +
        0.30 * scale(tackles)[,1] +
        0.20 * (team_score > opponent_score)
    )

  coaches_votes <- coaches_pool %>%
    group_by(season, round, match_id) %>%
    slice_max(order_by = coach_propensity + rnorm(n(), 0, 0.6), n = 3, with_ties = FALSE) %>%
    mutate(coaches_votes = c(3, 2, 1)[row_number()]) %>%
    ungroup() %>%
    dplyr::select(season, round, team, player, coaches_votes)

  # Built as separate tables, same as live mode's Footywire-sourced
  # fetches, rather than baked directly into player_stats -- keeps
  # demo and live mode structurally identical so a merge-logic bug
  # can't hide in demo mode only (same lesson learned earlier in this
  # project from a similar demo-vs-live mismatch).
  supercoach_scores <- player_stats %>%
    mutate(supercoach_score = pmax(0, round(rnorm(n(), 60 + quality * 60, 20)))) %>%
    dplyr::select(season, round, team, player, supercoach_score)

  fantasy_scores <- player_stats %>%
    mutate(fantasy_score = pmax(0, round(rnorm(n(), 55 + quality * 55, 18)))) %>%
    dplyr::select(season, round, team, player, fantasy_score)

  list(
    player_stats       = player_stats %>% dplyr::select(-quality, -reputation),
    coaches_votes      = coaches_votes,
    supercoach_scores  = supercoach_scores,
    fantasy_scores     = fantasy_scores,
    season_total_votes = season_total_votes,
    pending_season     = pending_season
  )
}

# ---------------------------------------------------------------
# LIVE MODE: standardise the real AFL Tables schema
# ---------------------------------------------------------------

# Rough position heuristic -- AFL Tables' player-stats scrape has
# no real position field. Directional only; don't treat as ground
# truth. Based on season-average hit-outs / inside-50s / disposals.
approximate_position_group <- function(df) {
  player_profile <- df %>%
    group_by(season, player_id) %>%
    summarise(
      avg_hitouts   = mean(hit_outs, na.rm = TRUE),
      avg_i50       = mean(inside_50s, na.rm = TRUE),
      avg_tackles   = mean(tackles, na.rm = TRUE),
      avg_disposals = mean(disposals, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(position_group = case_when(
      avg_hitouts >= 8                          ~ "ruck",
      avg_i50 >= 4 & avg_tackles < 4             ~ "forward",
      avg_disposals >= 20                        ~ "midfield",
      TRUE                                       ~ "defender"
    )) %>%
    dplyr::select(season, player_id, position_group)

  df %>% left_join(player_profile, by = c("season", "player_id"))
}

# Deterministic, stable synthetic ID for a recovered player name.
# Negative by construction, so it can never collide with a real AFL
# Tables numeric player ID (all positive) -- used only when fitzRoy's
# own ID lookup has failed but a name was still recoverable.
synthetic_id_from_name <- function(name) {
  if (is.na(name)) return(NA_integer_)
  -(as.integer(sum(utf8ToInt(name))) * 9973L + nchar(name))
}

# Best-effort column resolver, used for SuperCoach/Fantasy below since
# their exact output columns aren't documented (only "round-level
# performance metrics such as player rankings, salary, scores, and
# value" is confirmed from fitzRoy's own changelog) -- checks a list
# of plausible candidates (after janitor::clean_names() snake-cases
# everything) rather than hard-coding a guess.
resolve_column <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

# Shared standardiser for fetch_supercoach_scores()/fetch_fantasy_scores().
# Confirmed live (both functions, 2026): columns are year, round, rank,
# player, team, current_salary, round_salary, round_score, round_value,
# injured -- the actual per-round score is round_score for both, not a
# column literally named "score". team uses club nicknames ("Bulldogs",
# "Suns", "Dockers"), hence the replace_teams() call below. Candidate
# lists below still include a couple of fallback guesses in case this
# shape changes in a future fitzRoy version.
standardize_player_score_fetch <- function(raw, score_candidates, output_col_name) {
  df <- janitor::clean_names(raw)
  season_col <- resolve_column(df, c("season", "year"))
  round_col  <- resolve_column(df, c("round", "round_number"))
  team_col   <- resolve_column(df, c("team", "club"))
  player_col <- resolve_column(df, c("player", "player_name", "full_name", "name"))
  score_col  <- resolve_column(df, score_candidates)

  missing <- c(season = season_col, round = round_col, team = team_col,
               player = player_col, score = score_col)
  if (any(is.na(missing))) {
    warning(
      "A SuperCoach/Fantasy fetch returned column names this script didn't ",
      "recognise for '", output_col_name, "'. Missing: ",
      paste(names(missing)[is.na(missing)], collapse = ", "),
      ". Run `glimpse(janitor::clean_names(fitzRoy::fetch_supercoach_scores(year = ",
      CURRENT_SEASON, ", rounds = 1)))` (or fetch_fantasy_scores) and update the ",
      "candidate column names in standardize_player_score_fetch() in R/01_fetch_data.R."
    )
  }

  df %>%
    transmute(
      season = if (!is.na(season_col)) as.integer(.data[[season_col]]) else NA_integer_,
      round  = if (!is.na(round_col))  suppressWarnings(as.integer(.data[[round_col]])) else NA_integer_,
      team   = if (!is.na(team_col))   fitzRoy::replace_teams(as.character(.data[[team_col]])) else NA_character_,
      player = if (!is.na(player_col)) as.character(.data[[player_col]]) else NA_character_,
      !!output_col_name := if (!is.na(score_col)) as.numeric(.data[[score_col]]) else NA_real_
    )
}

standardize_live_player_stats <- function(raw) {
  df <- raw %>%
    mutate(
      season        = as.integer(Season),
      round_label   = Round,
      round         = suppressWarnings(as.integer(Round)),
      is_final      = is.na(round),   # non-numeric round label (QF/SF/PF/GF)
      team          = Team,
      opponent      = if_else(Home.Away == "Home", Away.team, Home.team),
      team_score    = if_else(Home.Away == "Home", Home.score, Away.score),
      opponent_score= if_else(Home.Away == "Home", Away.score, Home.score),
      match_id      = paste(season, round_label, Home.team, Away.team, sep = "_"),
      disposals     = Disposals,
      kicks         = Kicks,
      handballs     = Handballs,
      marks         = Marks,
      contested_marks  = Contested.Marks,
      marks_inside_50  = Marks.Inside.50,
      contested_possessions   = Contested.Possessions,
      uncontested_possessions = Uncontested.Possessions,
      rebounds      = Rebounds,
      bounces       = Bounces,
      frees_for     = Frees.For,
      frees_against = Frees.Against,
      clearances    = Clearances,
      tackles       = Tackles,
      inside_50s    = Inside.50s,
      goals         = Goals,
      behinds       = Behinds,
      goal_assists  = Goal.Assists,
      # AFL Tables has no direct "score involvements" field (that's a
      # Champion Data metric) -- this is a same-spirit proxy built from
      # what AFL Tables does provide.
      score_involvements = Goals + Behinds + Goal.Assists,
      one_percenters = One.Percenters,
      hit_outs      = Hit.Outs,
      clangers      = Clangers,
      # Approximate efficiency -- AFL Tables has no true disposal-efficiency
      # field either. This proxy (1 - turnovers/disposals) correlates with
      # the real thing but isn't identical to it.
      disposal_efficiency = pmax(0, pmin(1, 1 - (Clangers / pmax(1, Disposals)))),
      # Confirmed: this column is on a 0-100 scale in this source, not 0-1.
      time_on_ground_pct = Time.on.Ground,
      votes = Brownlow.Votes,
      career_games = Career.Games,
      # Recovery for rows where fitzRoy's own name/ID crosswalk failed to
      # resolve a match. Confirmed live: this happens for players who are
      # the 2nd/3rd+ person in AFL history with that exact name (e.g. Jack
      # Ross, Jack Williams, Jack Graham) -- fitzRoy's ID lookup can't
      # disambiguate and returns no Player/ID at all, even though the raw
      # scrape's First.name/Surname columns are still populated. Recovered
      # directly from those two columns instead, and given a synthetic
      # (negative, so it can never collide with a real numeric ID) but
      # stable player_id so they aren't silently dropped.
      recovered_name = if_else(is.na(Player) & !is.na(First.name) & !is.na(Surname),
                                 paste(First.name, Surname), NA_character_),
      player = coalesce(Player, recovered_name),
      player_id = if_else(
        !is.na(ID),
        ID,
        vapply(player, synthetic_id_from_name, integer(1))
      )
    ) %>%
    filter(!is_final) %>%   # Brownlow votes are never awarded in finals
    dplyr::select(season, round, team, player, player_id, opponent, match_id,
           team_score, opponent_score, disposals, kicks, handballs,
           marks, contested_marks, marks_inside_50,
           contested_possessions, uncontested_possessions,
           rebounds, bounces, frees_for, frees_against,
           clearances, tackles, inside_50s,
           goals, behinds, goal_assists, score_involvements,
           one_percenters, hit_outs, clangers, disposal_efficiency,
           time_on_ground_pct, votes, career_games)

  # After the recovery attempt above, any row still missing player/ID
  # has no First.name/Surname either -- there's no way to attribute
  # stats or votes to a player with literally no identity anywhere in
  # the row, so these (should be rare, now that named-but-unresolved
  # players are recovered above) are dropped.
  unidentified <- df %>% filter(is.na(player) | is.na(player_id))
  if (nrow(unidentified) > 0) {
    warning(nrow(unidentified), " player_stats row(s) had no player name at all (not even ",
            "First.name/Surname) and were dropped. Run this before re-sourcing to see which ",
            "team/rounds were affected: player_stats_raw %>% janitor::clean_names() %>% ",
            "filter(is.na(player), is.na(first_name) | is.na(surname)) %>% distinct(season, round, team)")
  }
  df <- df %>% filter(!is.na(player), !is.na(player_id))

  approximate_position_group(df)
}

# fetch_awards_brownlow()'s exact column meanings aren't documented.
# `Votes_3` is used here as the season vote total based on it matching
# publicly reported 2026 results and being internally consistent with
# `V_G` (votes-per-game) in the same row -- but this is an inference,
# not a confirmed spec. `season_total_check()` below prints a value
# you can eyeball against a known result before trusting it further.
standardize_season_totals <- function(raw) {
  df <- janitor::clean_names(raw)
  df %>%
    transmute(
      season = as.integer(season),
      player = as.character(player),
      team   = as.character(team),
      season_total_votes = as.numeric(votes_3)
    )
}

season_total_check <- function(season_totals, season, player_contains) {
  if (is.null(season_totals) || nrow(season_totals) == 0) {
    message("season_total_votes is NULL or empty -- the pending-season fetch either ",
            "hasn't run yet or failed. Re-run source(\"R/01_fetch_data.R\") and check ",
            "for a warning naming the failure, or confirm pending_seasons is non-empty.")
    return(invisible(NULL))
  }
  season_totals %>%
    filter(season == !!season, grepl(player_contains, player, ignore.case = TRUE))
}

fetch_live_data <- function(seasons) {
  library(fitzRoy)

  if (file.exists(PLAYER_STATS_CACHE)) {
    player_stats_raw <- readRDS(PLAYER_STATS_CACHE)
  } else {
    player_stats_raw <- purrr::map_dfr(seasons, function(yr) {
      message("Fetching player stats for ", yr)
      fitzRoy::fetch_player_stats_afltables(season = yr)
    })
    saveRDS(player_stats_raw, PLAYER_STATS_CACHE)
  }
  player_stats <- standardize_live_player_stats(player_stats_raw)

  # Which seasons are "counted" (real per-game votes present) vs
  # "pending" (not yet counted) is detected here, not hardcoded, so
  # this stays correct as seasons roll forward.
  vote_status <- player_stats %>%
    group_by(season) %>%
    summarise(total_votes = sum(votes, na.rm = TRUE), .groups = "drop")
  counted_seasons <- vote_status %>% filter(total_votes > 0) %>% pull(season)
  pending_seasons <- setdiff(seasons, counted_seasons)

  season_total_votes <- NULL
  if (length(pending_seasons) > 0) {
    season_total_votes <- tryCatch({
      raw <- purrr::map_dfr(pending_seasons, function(yr) {
        message("Fetching current-season Brownlow totals for ", yr,
                " (per-game votes not yet backfilled by AFL Tables)")
        fitzRoy::fetch_awards_brownlow(season = yr, type = "player")
      })
      out <- standardize_season_totals(raw)
      message("Season-total Brownlow votes fetched for ", paste(pending_seasons, collapse = ", "),
              ": ", nrow(out), " player rows.")
      out
    }, error = function(e) {
      warning("fetch_awards_brownlow() failed for pending season(s): ",
              conditionMessage(e))
      NULL
    })
  } else {
    message("No pending seasons detected -- season_total_votes left as NULL (nothing to fetch).")
  }

  if (file.exists(COACHES_VOTES_LIVE_CACHE)) {
    coaches_votes_raw <- readRDS(COACHES_VOTES_LIVE_CACHE)
  } else {
    coaches_votes_raw <- tryCatch({
      raw <- purrr::map_dfr(seasons, function(yr) {
        message("Fetching AFLCA coaches votes for ", yr)
        fitzRoy::fetch_coaches_votes(season = yr, comp = "AFLM")
      })
      out <- raw %>%
        janitor::clean_names() %>%
        transmute(
          season = as.integer(season),
          round  = suppressWarnings(as.integer(round)),
          # Confirmed live: AFLCA uses full/long team names ("Sydney Swans")
          # while AFL Tables (player_stats$team) uses its own short form
          # ("Sydney"). replace_teams() normalises to the AFL Tables
          # convention -- confirms e.g. "GWS Giants" -> "GWS".
          home_team = fitzRoy::replace_teams(as.character(home_team)),
          away_team = fitzRoy::replace_teams(as.character(away_team)),
          # Confirmed live: Player.Name carries a trailing club code,
          # e.g. "Justin McInerney (SYD)" -- strip it before name matching.
          player = stringr::str_remove(as.character(player_name), "\\s*\\([^)]*\\)\\s*$"),
          coaches_votes = as.numeric(coaches_votes)
        )

      # Confirmed live: a meaningful number of player-round combinations
      # (concentrated in rounds 19-23, the run-in to finals) come back
      # with TWO different coaches_votes values for the same game --
      # e.g. Isaac Smith round 20, 2022: one row shows 2.0, another
      # shows 15.0. The larger figure looks like a season-aggregate
      # number appearing alongside the real per-round score rather than
      # a plain scrape duplicate, but that's an inference, not something
      # confirmed against AFLCA's actual page or fitzRoy's source. Since
      # coaches_votes only feeds the secondary umpires-vs-coaches
      # comparison (it isn't part of the core vote-prediction model),
      # the safe choice here is to keep the SMALLER of the two values --
      # a single round's coaches votes should reasonably be smaller than
      # a multi-round aggregate -- rather than silently picking one via
      # first-row-wins or summing them (summing would double-count in
      # most cases). Flagged clearly rather than silently resolved.
      dup_summary <- out %>%
        count(season, round, home_team, away_team, player) %>%
        filter(n > 1)
      if (nrow(dup_summary) > 0) {
        warning(nrow(dup_summary), " player-game(s) in the AFLCA coaches-votes data had more than ",
                "one recorded value (concentrated in rounds 19-23 in testing) -- the smaller value is ",
                "kept for each, on the inference that the larger figure is a season aggregate rather ",
                "than that round's real score. This is unverified against AFLCA's source, so treat the ",
                "coaches-vs-umpires comparison in later rounds as approximate. Run `dup_summary` to see ",
                "which player-games were affected.")
      }
      out <- out %>%
        group_by(season, round, home_team, away_team, player) %>%
        summarise(coaches_votes = min(coaches_votes, na.rm = TRUE), .groups = "drop")

      saveRDS(out, COACHES_VOTES_LIVE_CACHE)
      out
    }, error = function(e) {
      warning("fetch_coaches_votes() failed: ", conditionMessage(e),
              " -- falling back to data/coaches_votes.csv if it exists.")
      NULL
    })
  }

  if (file.exists(SUPERCOACH_CACHE)) {
    supercoach_raw <- readRDS(SUPERCOACH_CACHE)
  } else {
    supercoach_raw <- tryCatch({
      raw <- purrr::map_dfr(seasons, function(yr) {
        message("Fetching SuperCoach scores for ", yr)
        fitzRoy::fetch_supercoach_scores(year = yr, rounds = 1:30)
      })
      out <- standardize_player_score_fetch(raw, c("round_score", "supercoach_score", "score", "points"), "supercoach_score")
      saveRDS(out, SUPERCOACH_CACHE)
      out
    }, error = function(e) {
      warning("fetch_supercoach_scores() failed: ", conditionMessage(e),
              " -- supercoach_score will be unavailable as a feature (NA for every row).")
      NULL
    })
  }

  if (file.exists(FANTASY_CACHE)) {
    fantasy_raw <- readRDS(FANTASY_CACHE)
  } else {
    fantasy_raw <- tryCatch({
      raw <- purrr::map_dfr(seasons, function(yr) {
        message("Fetching AFL Fantasy scores for ", yr)
        fitzRoy::fetch_fantasy_scores(year = yr, rounds = 1:30)
      })
      out <- standardize_player_score_fetch(raw, c("round_score", "fantasy_score", "score", "points"), "fantasy_score")
      saveRDS(out, FANTASY_CACHE)
      out
    }, error = function(e) {
      warning("fetch_fantasy_scores() failed: ", conditionMessage(e),
              " -- fantasy_score will be unavailable as a feature (NA for every row).")
      NULL
    })
  }

  list(player_stats = player_stats, coaches_votes_raw = coaches_votes_raw,
       supercoach_raw = supercoach_raw, fantasy_raw = fantasy_raw,
       season_total_votes = season_total_votes,
       counted_seasons = counted_seasons, pending_seasons = pending_seasons)
}

load_coaches_votes_fallback <- function() {
  if (file.exists(COACHES_VOTES_CSV)) {
    readr::read_csv(COACHES_VOTES_CSV, show_col_types = FALSE) %>%
      mutate(home_team = NA_character_, away_team = NA_character_)
  } else {
    warning("No live coaches votes and no data/coaches_votes.csv found -- ",
            "the umpires-vs-coaches section will be skipped.")
    tibble::tibble(season = integer(), round = integer(), home_team = character(),
                    away_team = character(), player = character(),
                    coaches_votes = double())
  }
}

# ---------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------
if (isTRUE(DEMO_MODE)) {
  message("DEMO_MODE is TRUE -- generating synthetic data. ",
          "Set DEMO_MODE <- FALSE in 00_config.R once fitzRoy is confirmed working.")
  demo <- generate_demo_data(SEASONS)
  player_stats       <- demo$player_stats
  coaches_votes      <- demo$coaches_votes
  supercoach_scores  <- demo$supercoach_scores
  fantasy_scores     <- demo$fantasy_scores
  season_total_votes <- demo$season_total_votes
  pending_seasons    <- demo$pending_season
  counted_seasons    <- setdiff(SEASONS, pending_seasons)
} else {
  live <- fetch_live_data(SEASONS)
  player_stats       <- live$player_stats
  season_total_votes <- live$season_total_votes
  counted_seasons    <- live$counted_seasons
  pending_seasons    <- live$pending_seasons

  coaches_votes <- if (!is.null(live$coaches_votes_raw) && nrow(live$coaches_votes_raw) > 0) {
    live$coaches_votes_raw
  } else {
    load_coaches_votes_fallback()
  }

  # Both left as 0-row (but correctly-shaped) tibbles if their fetch
  # failed, rather than NULL -- so 03_features.R's merge just produces
  # an all-NA feature column instead of erroring.
  supercoach_scores <- if (!is.null(live$supercoach_raw)) live$supercoach_raw else
    tibble::tibble(season = integer(), round = integer(), team = character(),
                     player = character(), supercoach_score = double())
  fantasy_scores <- if (!is.null(live$fantasy_raw)) live$fantasy_raw else
    tibble::tibble(season = integer(), round = integer(), team = character(),
                     player = character(), fantasy_score = double())
}
