# ============================================================
# 00_config.R
# Shared constants for the Brownlow vote analysis pipeline.
# Sourced first by every other script and by the .Rmd itself.
# ============================================================

# --- Season window -------------------------------------------------
# Last 10 seasons, per request. Longer than the original 5-season
# window -- 02_clean.R's drift_report flags any stat column that
# isn't consistently present across this whole span, since AFL
# Tables has added a few stat categories over time.
CURRENT_SEASON <- 2026
SEASONS        <- (CURRENT_SEASON - 9):CURRENT_SEASON   # 2017:2026

# --- Demo mode -------------------------------------------------------
# If TRUE, 01_fetch_data.R generates a synthetic dataset instead of
# calling fitzRoy. Useful for testing the pipeline/layout before you
# have fitzRoy installed and working, or while offline. Set to FALSE
# once you've confirmed fitzRoy pulls real data on your machine.
DEMO_MODE <- FALSE

# --- File paths --------------------------------------------------
DATA_DIR   <- "data"
CACHE_DIR  <- file.path(DATA_DIR, "cache")
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)

PLAYER_STATS_CACHE       <- file.path(CACHE_DIR, "player_stats.rds")
COACHES_VOTES_LIVE_CACHE <- file.path(CACHE_DIR, "coaches_votes_live.rds")
SUPERCOACH_CACHE         <- file.path(CACHE_DIR, "supercoach_scores.rds")
FANTASY_CACHE             <- file.path(CACHE_DIR, "fantasy_scores.rds")
# Manual CSV fallback only -- used if fetch_coaches_votes() fails or returns
# nothing for a season (e.g. AFLCA site structure changes). Not the primary
# path: fetch_coaches_votes() is a real fitzRoy function and is tried first.
COACHES_VOTES_CSV        <- file.path(DATA_DIR, "coaches_votes.csv")
# Manual fallback for the market-odds benchmark -- no reliable
# programmatic source for historical Brownlow futures odds exists.
# Fill this in yourself (columns: season, player, market_odds) to
# activate that section; skipped with a clear note until then.
MARKET_ODDS_CSV           <- file.path(DATA_DIR, "brownlow_odds.csv")

HYPERPARAM_CACHE  <- file.path(CACHE_DIR, "best_hyperparams.rds")
MASTER_TABLE_CACHE <- file.path(CACHE_DIR, "master_table.rds")

# --- Raw AFL Tables stats (used directly, "every stat possible") ----
# Every per-game numeric column AFL Tables tracks, used as-is. These
# are safe for XGBoost (tree models aren't sensitive to collinearity
# the way a linear model is) but NOT all used for the explanatory
# ordinal model below -- see EXPLANATORY_FEATURES for why.
RAW_STAT_FEATURES <- c(
  "kicks", "handballs", "disposals", "marks", "contested_marks",
  "marks_inside_50", "contested_possessions", "uncontested_possessions",
  "clearances", "tackles", "inside_50s", "rebounds", "one_percenters",
  "bounces", "hit_outs", "frees_for", "frees_against",
  "goals", "behinds", "goal_assists", "clangers",
  "score_involvements", "disposal_efficiency"
)

# --- External context features ---------------------------------------
# Coaches' votes and SuperCoach/Fantasy scores are now real model
# INPUTS, not just side comparisons -- confirmed via two independent
# public Brownlow-modelling writeups (a Wheelo Ratings methodology
# note and a Betfair data-science writeup) that coaches' votes and
# fantasy-style composite scores are among the single strongest
# predictors of Brownlow votes available. supercoach_score and
# fantasy_score schemas aren't fully documented for fitzRoy's fetchers
# -- see the column-resolver and verification note in 01_fetch_data.R.
EXTERNAL_FEATURES <- c("coaches_votes", "supercoach_score", "fantasy_score")

# --- Signature / innovative features ---------------------------------
# These are the features that differentiate this analysis from a
# standard "disposals correlate with votes" writeup. See 03_features.R
# for the construction logic and rationale for each.
INNOVATIVE_FEATURES <- c(
  "underdog_multiplier",      # performance vs a stronger opponent, weighted up
  "narrative_attention_index", # proxy for how visible/high-profile the game was
  "efficiency_weighted_influence", # efficiency x volume interaction, residualised against both main effects
  "defensive_shadow_score",    # composite defensive/pressure work, position-normalised
  "clutch_clearance_index",    # clearances x close-game interaction, residualised against both main effects
  "reputation_gap"             # prior career votes/reputation, used to isolate its effect
)

CONTEXT_FEATURES <- c("team_won", "margin", "close_game")

# Full feature set -- used by the XGBoost probability model. "Every
# stat possible", as requested.
ALL_MODEL_FEATURES <- c(RAW_STAT_FEATURES, EXTERNAL_FEATURES,
                         INNOVATIVE_FEATURES, CONTEXT_FEATURES)

# Curated subset for the EXPLANATORY (ordinal regression) model only.
# This is NOT the same list used for XGBoost, deliberately: several raw
# stats are exact arithmetic identities of each other (disposals =
# kicks + handballs; score_involvements is built from goals/behinds/
# goal_assists), and feeding an exact linear identity into a linear
# model causes "rank-deficient design" -- not a soft collinearity
# warning but a hard crash, confirmed the hard way earlier in this
# project. XGBoost's tree splits aren't vulnerable to that the same
# way, so it keeps the full raw set; this curated list drops the
# redundant raw components and keeps one representative of each
# concept, chosen by whichever had the clearer, more direct
# relationship to votes in earlier VIF/Brant testing.
EXPLANATORY_FEATURES <- c(
  "disposal_share", "contested_possessions", "uncontested_possessions",
  "clearances", "tackles", "inside_50s", "marks_inside_50",
  "one_percenters", "hit_outs", "frees_for", "frees_against",
  "goals", "disposal_efficiency", "score_involvements",
  "coaches_votes", "supercoach_score", "fantasy_score",
  INNOVATIVE_FEATURES, CONTEXT_FEATURES
)
# Deliberately excluded from EXPLANATORY_FEATURES vs the full set:
# kicks, handballs (both exact components of disposals, which stays),
# marks (correlates heavily with contested_marks + marks_inside_50),
# bounces, clangers, behinds, goal_assists (all exact components of
# derived features that stay instead), rebounds (thin signal, kept
# for XGBoost only). Re-run the VIF check after any change here.

# --- Data source notes (confirmed against fitzRoy 1.8.0 / AFL Tables) -----
# 1. Brownlow votes are only awarded for home-and-away rounds, never
#    finals -- finals rows are dropped during standardisation.
# 2. AFL Tables backfills a season's `Brownlow.Votes` column only after
#    that season's count night. A season with no votes recorded yet
#    ("pending") can still be used for every other feature, but can't
#    be used to TRAIN the vote model -- only to SCORE it. Which seasons
#    are "counted" vs "pending" is detected at runtime in 03_features.R,
#    not hardcoded, so this keeps working correctly in future years.
# 3. AFL Tables' player-stats scrape has no `position` or `metres_gained`
#    field (those are Champion Data metrics). position_group is
#    therefore a rough stat-based heuristic, not an authoritative
#    position -- treat it as directional only. disposal_efficiency is
#    approximated from Clangers/Disposals, also for the same reason.
# 4. fetch_supercoach_scores()/fetch_fantasy_scores() take a `year`
#    argument, not `season` like every other fetcher in this pipeline
#    -- confirmed from fitzRoy's own docs, easy to get wrong by
#    copy-pasting the pattern from the other fetch calls.

# --- Hyperparameter tuning ------------------------------------------
TUNE_MODELS <- if (exists("TUNE_MODELS")) TUNE_MODELS else TRUE

# --- Position-specific models -----------------------------------
# Tested and found not to help overall under the probability-scale
# model (see 04_model.R) -- production predictions use a single
# pooled model. Position-specific fitting still runs inside the CV
# loop purely to report that comparison honestly on the page.

# --- Monte Carlo simulation ------------------------------------------
# Full Brownlow-count simulation, inspired directly by Wheelo Ratings'
# published methodology (they report simulating the count 20,000
# times; headline numbers are averages across those simulations).
# Implemented via the Gumbel-max trick, which is mathematically
# equivalent to Plackett-Luce top-k sampling but fully vectorisable --
# generates every simulation's outcome via matrix operations instead
# of a sampling loop, which is the only way 10,000-20,000 sims per
# game across a whole season finishes in reasonable time in R. See
# simulate_season() in 04_model.R for the implementation.
N_SIMULATIONS <- 20000
# A much smaller number used only for the historical backtest (did the
# simulated most-likely winner match the real winner, for every
# counted season) -- that runs once per season in the backtest, so a
# lighter count keeps total runtime sane while still being a
# meaningful check.
N_SIMULATIONS_BACKTEST <- 2000

set.seed(2026)
