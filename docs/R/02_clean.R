
# ============================================================
# 02_clean.R
# Validates vote totals (counted seasons only), standardises
# player names for joining, and removes token appearances.
#
# Requires 00_config.R and 01_fetch_data.R to already be sourced.
# ============================================================

library(dplyr)
library(stringr)

normalise_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z ]", "") %>%
    str_squish()
}

player_stats <- player_stats %>% mutate(player_key = normalise_name(player))

# --- Vote-total sanity check (counted seasons only) -----------------
# Every home-and-away game should have votes summing to exactly 6
# across its players, in seasons where Brownlow.Votes has been
# backfilled. Pending seasons are skipped here by design -- their
# votes are genuinely NA, not an error.
vote_check <- player_stats %>%
  filter(season %in% counted_seasons) %>%
  group_by(season, match_id) %>%
  summarise(total_votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  filter(total_votes != 6)

if (nrow(vote_check) > 0) {
  warning(nrow(vote_check), " game(s) in counted seasons have votes that don't ",
          "sum to 6 -- check for scrape/merge errors before trusting the model. ",
          "Run `vote_check` to see which games.")
}

# --- Remove token / concussion-sub appearances -----------------------
# Confirmed: time_on_ground_pct is on a 0-100 scale in this source.
player_stats <- player_stats %>%
  filter(is.na(time_on_ground_pct) | time_on_ground_pct > 10)

# --- Coaches' votes: resolve team from home/away when needed --------
# Live fetch_coaches_votes() returns Home.Team/Away.Team, not a direct
# player-team column, so the player's actual team is resolved against
# player_stats. Demo-mode and CSV-fallback data already arrive with a
# `team` column, so this step is skipped for those.
coaches_votes <- coaches_votes %>% mutate(player_key = normalise_name(player))

if (!"team" %in% names(coaches_votes) &&
    all(c("home_team", "away_team") %in% names(coaches_votes))) {
  
  team_lookup <- player_stats %>% distinct(season, round, player_key, team)
  
  coaches_votes <- coaches_votes %>%
    left_join(team_lookup, by = c("season", "round", "player_key"),
              relationship = "many-to-many") %>%
    # Constrain to the two teams actually playing in that match -- avoids
    # false cross-matches when a player elsewhere in the round shares a
    # normalised name with someone in this game (rare namesake collision).
    filter(is.na(team) | team == home_team | team == away_team) %>%
    distinct(season, round, player_key, home_team, away_team, coaches_votes, .keep_all = TRUE)
  
  # Conservative fallback for unresolved coach-vote names:
  # match by surname + season + round + one of the two match teams.
  unresolved_rows <- coaches_votes %>%
    filter(is.na(team))
  
  if (nrow(unresolved_rows) > 0) {
    
    surname_lookup <- player_stats %>%
      mutate(
        surname = stringr::word(player, -1)
      ) %>%
      dplyr::select(
        season,
        round,
        surname,
        player_stats_team = team,
        player_stats_player = player
      ) %>%
      distinct()
    
    surname_candidates <- unresolved_rows %>%
      mutate(
        surname = stringr::word(player, -1)
      ) %>%
      left_join(
        surname_lookup,
        by = c("season", "round", "surname"),
        relationship = "many-to-many"
      ) %>%
      filter(
        player_stats_team %in% c(home_team, away_team)
      ) %>%
      group_by(
        season, round, player,
        home_team, away_team, coaches_votes
      ) %>%
      summarise(
        n_matches = n_distinct(
          player_stats_player,
          player_stats_team
        ),
        matched_team = ifelse(
          n_matches == 1,
          first(player_stats_team),
          NA_character_
        ),
        .groups = "drop"
      )
    
    safe_surname <- surname_candidates %>%
      filter(n_matches == 1) %>%
      dplyr::select(
        season, round, player,
        home_team, away_team, coaches_votes,
        matched_team
      )
    
    coaches_votes <- coaches_votes %>%
      left_join(
        safe_surname,
        by = c(
          "season",
          "round",
          "player",
          "home_team",
          "away_team",
          "coaches_votes"
        )
      ) %>%
      mutate(
        team = dplyr::coalesce(team, matched_team)
      ) %>%
      dplyr::select(-matched_team)
  }
  
  unresolved <- sum(is.na(coaches_votes$team))
  if (unresolved > 0) {
    pct_unresolved <- round(100 * unresolved / nrow(coaches_votes), 1)
    warning(
      unresolved, " coaches-vote row(s) (", pct_unresolved, "%) couldn't be matched ",
      "to a team via player_stats -- these rows are dropped from the coaches-vs-umpires ",
      "comparison. A handful (name-spelling edge cases) is normal; a large share usually ",
      "means a systematic mismatch. Run this to compare the team-name sets directly: ",
      "setdiff(unique(c(coaches_votes$home_team, coaches_votes$away_team)), unique(player_stats$team))"
    )
  }
  coaches_votes <- coaches_votes %>% filter(!is.na(team))
  
  # Final dedup.
  # A team can play more than once in the same numbered round, so
  # round + team + player is NOT necessarily unique. Keep the opponent
  # as part of the match identity.
  
  coaches_votes <- coaches_votes %>%
    mutate(
      opponent = dplyr::if_else(
        team == home_team,
        away_team,
        home_team
      )
    )
  
  post_resolution_dups <- coaches_votes %>%
    count(season, round, team, opponent, player_key) %>%
    filter(n > 1)
  
  if (nrow(post_resolution_dups) > 0) {
    warning(
      nrow(post_resolution_dups),
      " (season, round, team, opponent, player) combination(s) still had ",
      "more than one coaches_votes row after team resolution."
    )
  }
  
  coaches_votes <- coaches_votes %>%
    group_by(season, round, team, opponent, player_key) %>%
    summarise(
      player = dplyr::first(player),
      coaches_votes = min(coaches_votes, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::select(
      season,
      round,
      team,
      opponent,
      player_key,
      player,
      coaches_votes
    )
}

# --- Season-total ground truth for pending season(s) -----------------
if (!is.null(season_total_votes) && nrow(season_total_votes) > 0) {
  season_total_votes <- season_total_votes %>% mutate(player_key = normalise_name(player))
}

# --- SuperCoach / Fantasy scores: add player_key, dedup defensively --
# These already have a direct `team` column (unlike coaches' votes),
# so no home/away resolution is needed -- just name normalisation and
# a safety dedup in case a round was fetched twice across cache runs.
if (nrow(supercoach_scores) > 0) {
  supercoach_scores <- supercoach_scores %>%
    mutate(player_key = normalise_name(player)) %>%
    group_by(season, round, team, player_key) %>%
    summarise(player = dplyr::first(player),
              supercoach_score = mean(supercoach_score, na.rm = TRUE), .groups = "drop")
}
if (nrow(fantasy_scores) > 0) {
  fantasy_scores <- fantasy_scores %>%
    mutate(player_key = normalise_name(player)) %>%
    group_by(season, round, team, player_key) %>%
    summarise(player = dplyr::first(player),
              fantasy_score = mean(fantasy_score, na.rm = TRUE), .groups = "drop")
}

# --- Stat-availability drift across the 10-year window ----------------
# AFL Tables has added stat categories over time -- a feature that's
# only partially present across this span would otherwise silently
# bias the model toward whichever seasons happen to have it. This
# reports presence per season for every raw stat used; doesn't fix
# anything automatically, since the right fix depends on how bad the
# gap is (impute, restrict the feature to recent seasons only, or drop
# it) -- a judgement call, not a default. Inspect `drift_report` if
# any feature's spread looks uneven across seasons.
stat_cols_to_check <- intersect(RAW_STAT_FEATURES, names(player_stats))
drift_report <- player_stats %>%
  group_by(season) %>%
  summarise(across(all_of(stat_cols_to_check), ~ round(100 * mean(!is.na(.x)), 1)), .groups = "drop")

stat_presence_report <- player_stats %>%
  group_by(season) %>%
  summarise(across(all_of(stat_cols_to_check), ~ as.integer(any(!is.na(.x)))), .groups = "drop")
incomplete_stats <- stat_presence_report %>%
  summarise(across(all_of(stat_cols_to_check), ~ sum(.x == 0))) %>%
  tidyr::pivot_longer(everything(), names_to = "stat", values_to = "seasons_missing") %>%
  filter(seasons_missing > 0)
if (nrow(incomplete_stats) > 0) {
  warning(nrow(incomplete_stats), " raw stat(s) aren't present in every season of the ",
          length(SEASONS), "-year window -- run `incomplete_stats` to see which, and ",
          "`drift_report` for the season-by-season presence percentage before trusting ",
          "those features' importance on the final page.")
}
