# ============================================================
# 04_model.R
# Trains on seasons with real per-game votes ("counted seasons"),
# scores every season including a season not yet counted
# ("pending"), and builds the Vote Equity Index -- using actual
# per-game votes as ground truth for counted seasons, and the
# season-total figure from fetch_awards_brownlow for the pending
# one.
#
# Requires 00_config.R through 03_features.R sourced first.
# ============================================================

library(dplyr)
library(MASS)
library(xgboost)

# MASS::select() masks dplyr::select() the moment MASS loads -- this
# script (and the .Rmd after it) both call select() many times below,
# so pin it back to dplyr's version explicitly rather than leaving a
# silent trap for the rest of the R session.
select <- dplyr::select

model_features <- intersect(ALL_MODEL_FEATURES, names(master))
missing_features <- setdiff(ALL_MODEL_FEATURES, names(master))
if (length(missing_features) > 0) {
  warning("These configured features aren't in `master` and were skipped: ",
          paste(missing_features, collapse = ", "))
}
explanatory_features <- intersect(EXPLANATORY_FEATURES, names(master))

# scorable_data: every row that might get scored, full feature set,
# NA left as NA rather than requiring every one of 20+ columns to be
# complete. This matters now in a way it didn't with a smaller
# feature set: supercoach_score/fantasy_score come from an external
# Footywire-based source with real coverage gaps, and discarding every
# row missing ANY one of 20+ features would throw out a lot of
# otherwise-good data for no good reason. XGBoost handles missing
# features natively (learns an optimal default split direction per
# feature from the training data itself) -- it doesn't need complete
# cases the way a linear model does.
scorable_data <- master

# Training data: counted seasons only, real votes required. Still no
# completeness filter on the full feature set, for the same reason.
train_eligible <- scorable_data %>%
  filter(season %in% counted_seasons, !is.na(votes))

# A separate, COMPLETE-CASE subset used only for the explanatory
# ordinal model, the Brant test, and the VIF check below -- those are
# classical statistical techniques that can't use XGBoost's native
# missing-value handling, so they need genuinely complete rows. Using
# the smaller, curated EXPLANATORY_FEATURES list (not the full one)
# keeps this as large as reasonably possible.
explanatory_data <- train_eligible %>%
  filter(if_all(all_of(explanatory_features), ~ !is.na(.x)))

# ---------------------------------------------------------------
# 1. EXPLANATORY MODEL: ordinal logistic regression
# ---------------------------------------------------------------
explanatory_data <- explanatory_data %>%
  mutate(votes_f = factor(pmin(votes, 3), ordered = TRUE, levels = 0:3))

baseline_formula <- as.formula(paste("votes_f ~", paste(explanatory_features, collapse = " + ")))
baseline_model <- polr(baseline_formula, data = explanatory_data, Hess = TRUE)

baseline_coefs <- broom::tidy(baseline_model, conf.int = TRUE) %>%
  filter(coef.type == "coefficient") %>%
  arrange(desc(abs(estimate)))

brant_test <- tryCatch(brant::brant(baseline_model), error = function(e) NULL)

# ---------------------------------------------------------------
# 2 & 3. PREDICTIVE MODEL: a genuine 4-class probability model.
#
#    Every player-game gets an actual probability distribution
#    across P(0 votes), P(1), P(2), P(3) -- not an arbitrary score.
#    This replaces an earlier two-stage hurdle+ranker design that
#    combined scores via within-game percentile rank: that worked,
#    but had a real side effect -- rescoring one position's players
#    with their own model shifted everyone ELSE's percentile in that
#    same game too, purely as a side effect of sharing a ranking
#    context, even when their own score hadn't changed. Real
#    probabilities don't have that problem: a probability is
#    comparable on its own absolute scale regardless of which
#    model produced it, so combining position-specific and global
#    models no longer has any coupling side effect.
#
#    Built as XGBoost's multi:softprob objective (softmax cross-
#    entropy over 4 classes) -- this is precisely the objective that
#    optimises for well-calibrated class probabilities, which is
#    exactly what's needed here.
#
#    Within a game, players are ranked by EXPECTED votes
#    (0*P0 + 1*P1 + 2*P2 + 3*P3) -- itself a genuine expectation over
#    a real probability distribution, not a heuristic score. The top
#    3 by expected votes are assigned 3/2/1 predicted votes, same as
#    before, landing predictions on the same discrete scale as real
#    votes.
# ---------------------------------------------------------------

fit_multiclass <- function(train_df, params, nrounds = 200) {
  dtrain <- xgb.DMatrix(as.matrix(train_df[model_features]), label = pmin(train_df$votes, 3))
  xgb.train(params = c(params, list(objective = "multi:softprob", num_class = 4)),
            data = dtrain, nrounds = nrounds, verbose = 0)
}

# xgboost's predict() for a multiclass model has TWO possible return
# shapes depending on package version: older versions return a flat
# vector (one probability per row*class pair, row-major) that needs
# manual reshaping; newer versions (confirmed live, xgboost as
# installed here) already return a proper matrix directly. Reshaping
# an already-shaped matrix with matrix(raw, ...) silently scrambles
# it -- flattens column-major, rebuilds row-major -- which is exactly
# what corrupted results earlier (probabilities of 1.36, 2.94, 3.00
# showed up, when a real probability can never exceed 1). Checking
# is.matrix() first, rather than assuming either shape, handles both
# package versions correctly.
predict_vote_probs <- function(model, newdata) {
  dnew <- xgb.DMatrix(as.matrix(newdata[model_features]))
  raw <- predict(model, dnew)
  probs <- if (is.matrix(raw)) raw else matrix(raw, ncol = 4, byrow = TRUE)
  colnames(probs) <- c("p0", "p1", "p2", "p3")

  # Hard runtime check, not just a comment: catches this exact class of
  # bug immediately and loudly if it ever recurs (e.g. after a future
  # xgboost package update changes the return shape again), instead of
  # silently producing garbage predictions that only show up much later
  # as an accuracy collapse.
  row_sums <- rowSums(probs)
  if (any(abs(row_sums - 1) > 0.01)) {
    stop("predict_vote_probs(): probabilities don't sum to 1 (range: ",
         round(min(row_sums), 3), " to ", round(max(row_sums), 3), ") -- ",
         "the reshape logic doesn't match this xgboost version's actual ",
         "predict() output shape. Run `predict(model, xgb.DMatrix(as.matrix(newdata[model_features])))` ",
         "directly and inspect its class/dim before touching this function again.")
  }
  probs
}

within_game_ranking_eval <- function(df, pred_col) {
  df %>%
    group_by(season, round, match_id) %>%
    mutate(pred_rank = rank(-.data[[pred_col]], ties.method = "first")) %>%
    ungroup() %>%
    filter(votes > 0) %>%
    mutate(
      top3_hit = pred_rank <= 3,
      expected_rank = 4 - votes,
      exact_order_match = pred_rank == expected_rank
    ) %>%
    summarise(top3_hit_rate = mean(top3_hit), exact_order_rate = mean(exact_order_match),
              .groups = "drop")
}

discretize_predicted_votes <- function(df, score_col) {
  df %>%
    group_by(season, round, match_id) %>%
    mutate(
      pred_rank = rank(-.data[[score_col]], ties.method = "first"),
      predicted_votes = case_when(pred_rank == 1 ~ 3, pred_rank == 2 ~ 2,
                                    pred_rank == 3 ~ 1, TRUE ~ 0)
    ) %>%
    ungroup() %>%
    select(-pred_rank)
}

# ---------------------------------------------------------------
# PER-POSITION MODELS, BLENDED VIA REAL PROBABILITIES
#
# Fits a separate multiclass model per position_group (reusing the
# same tuned hyperparameters as the global model), falling back to
# the global model for any position with too little data.
# position_group is still the stat-based heuristic from
# 01_fetch_data.R, not real position data -- see the note there.
#
# Unlike the earlier percentile-blend design, ALL FOUR positions are
# specialised here where data allows -- the probability-scale
# approach doesn't have the cross-position coupling problem that made
# specialising forwards/midfielders backfire before, so there's no
# reason to hold them back the way there was previously.
# ---------------------------------------------------------------
MIN_ROWS_FOR_POSITION_MODEL <- 1500

fit_position_models <- function(train_df, params) {
  eligible <- train_df %>% count(position_group) %>% filter(n >= MIN_ROWS_FOR_POSITION_MODEL) %>% pull(position_group)
  purrr::map(eligible, function(pg) {
    pg_train <- train_df %>% filter(position_group == pg)
    fit_multiclass(pg_train, params)
  }) %>% purrr::set_names(eligible)
}

# Scores every row with its own position's model where one exists,
# falling back to the supplied global model otherwise. Returns
# newdata with p0-p3 and expected_votes columns attached, in the
# original row order.
score_probabilities <- function(newdata, position_models, global_model) {
  newdata$row_order_tmp <- seq_len(nrow(newdata))
  scored <- purrr::map_dfr(unique(newdata$position_group), function(pg) {
    subset_df <- newdata %>% filter(position_group == pg)
    model_to_use <- if (pg %in% names(position_models)) position_models[[pg]] else global_model
    probs <- predict_vote_probs(model_to_use, subset_df)
    subset_df$p0 <- probs[, "p0"]; subset_df$p1 <- probs[, "p1"]
    subset_df$p2 <- probs[, "p2"]; subset_df$p3 <- probs[, "p3"]
    subset_df$expected_votes <- subset_df$p1 * 1 + subset_df$p2 * 2 + subset_df$p3 * 3
    subset_df
  })
  scored %>% arrange(row_order_tmp) %>% select(-row_order_tmp)
}

# Small grid over max_depth/eta (kept modest since it's evaluated
# across every held-out season), PLUS fixed regularisation defaults
# applied to every candidate -- reg_alpha/reg_lambda/min_child_weight,
# and tighter subsample/colsample than before. Not tuned via the grid
# itself (that would multiply the search size several-fold for
# marginal extra gain); set as sensible non-zero shrinkage specifically
# because the feature set is now much larger (~30 features, "every
# stat possible") and the overfitting risk that comes with that is
# exactly what regularisation is for. Delete data/cache/best_hyperparams.rds
# to force a fresh search; edit the values below directly to widen the
# grid or change the fixed regularisation defaults.
param_grid <- expand.grid(max_depth = c(3, 4, 6), eta = c(0.03, 0.07, 0.12))
REGULARIZATION_DEFAULTS <- list(subsample = 0.7, colsample_bytree = 0.7,
                                  min_child_weight = 5, reg_alpha = 1, reg_lambda = 2)

score_param_combo <- function(params) {
  fold_metrics <- purrr::map_dfr(counted_seasons, function(held_out) {
    train_df <- train_eligible %>% filter(season != held_out)
    test_df  <- train_eligible %>% filter(season == held_out)
    if (nrow(train_df) == 0 || nrow(test_df) == 0) return(NULL)

    global_fit <- fit_multiclass(train_df, params)
    probs <- predict_vote_probs(global_fit, test_df)
    test_df$expected_votes <- probs[, "p1"] * 1 + probs[, "p2"] * 2 + probs[, "p3"] * 3

    within_game_ranking_eval(test_df, "expected_votes")
  })
  if (nrow(fold_metrics) == 0) return(tibble::tibble(top3_hit_rate = NA_real_, exact_order_rate = NA_real_))
  fold_metrics %>% summarise(top3_hit_rate = mean(top3_hit_rate), exact_order_rate = mean(exact_order_rate))
}

if (file.exists(HYPERPARAM_CACHE)) {
  cached_tuning <- readRDS(HYPERPARAM_CACHE)
  best_params     <- cached_tuning$best_params
  tuning_results  <- cached_tuning$tuning_results
  message("Loaded cached hyperparameters from a previous tuning run. ",
          "Delete ", HYPERPARAM_CACHE, " to force a fresh search.")
} else if (isTRUE(TUNE_MODELS)) {
  message("Tuning hyperparameters across ", nrow(param_grid), " combinations x ",
          length(counted_seasons), " held-out seasons -- this will take a while.")
  tuning_results <- purrr::map_dfr(seq_len(nrow(param_grid)), function(i) {
    params <- c(list(max_depth = param_grid$max_depth[i], eta = param_grid$eta[i]),
                 REGULARIZATION_DEFAULTS)
    metrics <- score_param_combo(params)
    tibble::tibble(max_depth = params$max_depth, eta = params$eta,
                    top3_hit_rate = metrics$top3_hit_rate, exact_order_rate = metrics$exact_order_rate)
  })
  best_row <- tuning_results %>% arrange(desc(top3_hit_rate), desc(exact_order_rate)) %>% slice(1)
  best_params <- c(list(max_depth = best_row$max_depth, eta = best_row$eta), REGULARIZATION_DEFAULTS)
  saveRDS(list(best_params = best_params, tuning_results = tuning_results), HYPERPARAM_CACHE)
} else {
  best_params <- c(list(max_depth = 4, eta = 0.05), REGULARIZATION_DEFAULTS)
  tuning_results <- NULL
}

# Leave-one-season-out CV, evaluating THREE things per fold: the
# blended (position-specific) model actually being shipped, a
# global-only model as a comparison baseline (fit fresh per fold, not
# reusing the final full-data model, so this stays honest), and the
# naive "most disposals wins" benchmark. One shared pass feeds
# cv_results, position_accuracy_summary and calibration_data below,
# rather than three separate (and 3x as expensive) CV loops.
cv_full <- purrr::map_dfr(counted_seasons, function(held_out) {
  train_df <- train_eligible %>% filter(season != held_out)
  test_df  <- train_eligible %>% filter(season == held_out)
  if (nrow(train_df) == 0 || nrow(test_df) == 0) return(NULL)

  global_fit <- fit_multiclass(train_df, best_params)
  fold_position_models <- fit_position_models(train_df, best_params)

  scored <- score_probabilities(test_df, fold_position_models, global_fit)
  global_probs <- predict_vote_probs(global_fit, test_df)
  scored$global_expected_votes <- global_probs[, "p1"] * 1 + global_probs[, "p2"] * 2 + global_probs[, "p3"] * 3
  scored$naive_pred <- scored$disposals
  scored$held_out_season <- held_out
  scored
})
# Inspect `cv_full`: with only `length(counted_seasons)` seasons
# available, treat these as indicative, not precise.

# Rank computed here, once, across the FULL game field for both
# scoring approaches -- needed before position_accuracy_summary can
# filter down to just the vote-getters within each position group.
cv_full <- cv_full %>%
  group_by(season, round, match_id) %>%
  mutate(pred_rank_blended = rank(-expected_votes, ties.method = "first"),
         pred_rank_global  = rank(-global_expected_votes, ties.method = "first")) %>%
  ungroup()

# OUT-OF-SAMPLE PREDICTIONS FOR HISTORICAL ANALYSIS
# The global model is the final production model, so historical
# predictions use the held-out global model predictions.

cv_predictions <- cv_full %>%
  mutate(
    predicted_votes_oos = case_when(
      pred_rank_global == 1 ~ 3,
      pred_rank_global == 2 ~ 2,
      pred_rank_global == 3 ~ 1,
      TRUE ~ 0
    )
  )

cv_results <- bind_rows(
  cv_full %>% group_by(held_out_season) %>%
    group_modify(~ within_game_ranking_eval(.x, "expected_votes")) %>% ungroup() %>% mutate(model = "xgboost"),
  cv_full %>% group_by(held_out_season) %>%
    group_modify(~ within_game_ranking_eval(.x, "global_expected_votes")) %>% ungroup() %>% mutate(model = "xgboost_global_only"),
  cv_full %>% group_by(held_out_season) %>%
    group_modify(~ within_game_ranking_eval(.x, "naive_pred")) %>% ungroup() %>% mutate(model = "naive_disposals")
)
# "xgboost" here is the position-blended model -- kept under that
# label for compatibility with the existing .Rmd. "xgboost_global_only"
# is the single-model baseline. Checked, not assumed: under this
# probability-scale architecture, position-specialisation doesn't earn
# its keep -- see the decision right below the final model fit.

position_accuracy_summary <- bind_rows(
  cv_full %>% filter(votes > 0) %>%
    mutate(top3_hit = pred_rank_blended <= 3, expected_rank = 4 - votes,
           exact_order_match = pred_rank_blended == expected_rank) %>%
    group_by(position_group) %>%
    summarise(top3_hit_rate = mean(top3_hit), exact_order_rate = mean(exact_order_match),
              n_vote_getters = n(), .groups = "drop") %>%
    mutate(model = "Position-specific (blended)"),
  cv_full %>% filter(votes > 0) %>%
    mutate(top3_hit = pred_rank_global <= 3, expected_rank = 4 - votes,
           exact_order_match = pred_rank_global == expected_rank) %>%
    group_by(position_group) %>%
    summarise(top3_hit_rate = mean(top3_hit), exact_order_rate = mean(exact_order_match),
              n_vote_getters = n(), .groups = "drop") %>%
    mutate(model = "Global (single model)")
) %>% arrange(position_group, model)

calibration_data <- cv_full %>%
  mutate(prob_of_polling = p1 + p2 + p3,
         actual_polled = as.numeric(votes > 0), prob_bin = ntile(prob_of_polling, 10)) %>%
  group_by(prob_bin) %>%
  summarise(mean_predicted_prob = mean(prob_of_polling), actual_poll_rate = mean(actual_polled),
            n = n(), .groups = "drop")

# Final model: the GLOBAL model alone, not position-blended.
#
# Checked, not assumed: cv_results/position_accuracy_summary above
# show position-specific blending no longer earns its keep under this
# probability-scale architecture. Estimating a full 4-class
# distribution is a harder, more data-hungry task than the old
# binary-hurdle-plus-ranking-pairs design was -- ruck, for example,
# has only ~162 vote-getting games total across training, split
# across 3 non-zero classes, leaving too few examples per class to
# calibrate reliably on its own. Only midfield (by far the largest
# group) showed a real gain from specialising; every other position
# came out flat or worse. Rather than ship complexity that isn't
# validated to help, the model that actually generates predictions is
# the plain global one. position_accuracy_summary above still reports
# the full comparison honestly, as a tested-and-rejected approach
# rather than a silently-dropped one.
global_model_full <- fit_multiclass(train_eligible, best_params)

scored_final <- predict_vote_probs(global_model_full, scorable_data)
scorable_data$p0 <- scored_final[, "p0"]
scorable_data$p1 <- scored_final[, "p1"]
scorable_data$p2 <- scored_final[, "p2"]
scorable_data$p3 <- scored_final[, "p3"]
scorable_data$combined_score <- scorable_data$p1 * 1 + scorable_data$p2 * 2 + scorable_data$p3 * 3
scorable_data <- discretize_predicted_votes(scorable_data, "combined_score")

# SHAP reflects the GLOBAL model's view of feature importance, not a
# blend of all position models' views -- building and reconciling
# separate SHAP summaries per position is a much larger undertaking
# for a secondary diagnostic; the accuracy numbers above (not this
# chart) are what actually reflect the position-specific approach.
# xgb.importance() works the same regardless of objective. shap.prep()
# was built mainly for binary/regression objectives and may not cleanly
# handle a 4-class model's multi-dimensional output -- if it fails for
# that reason, the tryCatch below falls back to xgb.importance()
# automatically rather than breaking the render.
shap_summary <- tryCatch({
  library(SHAPforxgboost)
  shap.prep(xgb_model = global_model_full, X_train = as.matrix(train_eligible[model_features]))
}, error = function(e) {
  warning("SHAPforxgboost not available -- falling back to xgb's built-in importance.")
  xgb.importance(model = global_model_full)
})

# ---------------------------------------------------------------
# 4. VOTE EQUITY INDEX
# ---------------------------------------------------------------
# Counted seasons: predicted votes vs real summed per-game votes.
# Grouped by player_id (AFL Tables' real numeric ID), not the name
# string -- a namesake collision (two real players sharing a name)
# would otherwise silently merge into one row here.
vote_equity_counted <- scorable_data %>%
  filter(season %in% counted_seasons) %>%
  group_by(season, player_id, team, position_group) %>%
  summarise(player = dplyr::first(player),   # name kept for display only, not as part of the key
            actual_votes = sum(votes), predicted_votes = sum(predicted_votes),
            games = n(), .groups = "drop")

# Pending season(s): predicted votes vs the season-total figure from
# fetch_awards_brownlow (Votes_3 -- see the caveat in 01_fetch_data.R).
vote_equity_pending <- tibble::tibble()
if (length(pending_seasons) > 0 && !is.null(season_total_votes) && nrow(season_total_votes) > 0) {
  predicted_by_player <- scorable_data %>%
    filter(season %in% pending_seasons) %>%
    mutate(player_key = normalise_name(player)) %>%
    group_by(season, player_id, team, position_group) %>%
    summarise(player = dplyr::first(player), player_key = dplyr::first(player_key),
              predicted_votes = sum(predicted_votes), games = n(), .groups = "drop")

  vote_equity_pending <- predicted_by_player %>%
    left_join(season_total_votes %>% select(season, player_key, actual_votes = season_total_votes),
               by = c("season", "player_key")) %>%
    mutate(actual_votes = replace_na(actual_votes, 0)) %>%
    select(-player_key)
}

vote_equity <- bind_rows(vote_equity_counted, vote_equity_pending) %>%
  mutate(vote_equity_index = predicted_votes - actual_votes,
         vote_equity_per_game = vote_equity_index / games) %>%
  arrange(desc(vote_equity_index))

# ---------------------------------------------------------------
# 5. REPUTATION-ADJUSTED RESIDUAL
# ---------------------------------------------------------------
no_reputation_features <- setdiff(explanatory_features, "reputation_gap")
no_rep_formula <- as.formula(paste("votes_f ~", paste(no_reputation_features, collapse = " + ")))
no_rep_model <- polr(no_rep_formula, data = explanatory_data, Hess = TRUE)

explanatory_data$reputation_adjusted_residual <-
  as.numeric(predict(baseline_model, type = "class")) -
  as.numeric(predict(no_rep_model, type = "class"))

# ---------------------------------------------------------------
# 6. "ONES TO WATCH" -- validated against history before presenting
# ---------------------------------------------------------------
# Collapse to one row per player per season first. vote_equity_counted
# is deliberately grouped by team as well (so a genuine same-season
# team change shows up as two rows on the leaderboard), but that same
# grouping causes a many-to-many blowup in the self-joins below if
# left as-is -- so it's summed across team here, just for this
# validation step. Grouped by player_id, not the name string, so a
# namesake collision can't silently merge two different real players.
player_season_totals <- vote_equity_counted %>%
  group_by(season, player_id) %>%
  summarise(player = dplyr::first(player),   # name kept for display only, not as part of the key
            actual_votes = sum(actual_votes), predicted_votes = sum(predicted_votes),
            .groups = "drop") %>%
  mutate(vote_equity_index = predicted_votes - actual_votes)

next_season_change <- player_season_totals %>%
  select(season, player_id, player, vote_equity_index) %>%
  mutate(next_season = season + 1) %>%
  inner_join(player_season_totals %>% select(season, player_id, actual_votes),
             by = c("next_season" = "season", "player_id")) %>%
  inner_join(player_season_totals %>% select(season, player_id, actual_votes) %>%
               rename(prior_votes = actual_votes),
             by = c("season", "player_id")) %>%
  mutate(vote_change = actual_votes - prior_votes)

equity_predicts_growth <- tryCatch(
  cor.test(next_season_change$vote_equity_index, next_season_change$vote_change,
           method = "spearman", exact = FALSE),
  error = function(e) NULL
)

current_season_for_watch <- if (length(pending_seasons) > 0) max(pending_seasons) else max(counted_seasons)
ones_to_watch <- vote_equity %>%
  filter(season == current_season_for_watch, vote_equity_index > 0) %>%
  arrange(desc(vote_equity_index)) %>%
  slice_head(n = 15)

# ---------------------------------------------------------------
# 7. COACHES VS UMPIRES DIFFERENTIAL MODEL (counted seasons only --
#    a pending season has no umpire-vote ground truth to diff against)
# ---------------------------------------------------------------
diff_data <- scorable_data %>%
  filter(season %in% counted_seasons, !is.na(coaches_votes), !is.na(votes)) %>%
  mutate(vote_diff = coaches_votes - votes)

coaches_diff_model <- if (nrow(diff_data) > 20) {
  lm(vote_diff ~ tackles + defensive_shadow_score + disposals +
       score_involvements + efficiency_weighted_influence, data = diff_data)
} else {
  NULL
}

coaches_case_studies <- diff_data %>%
  filter(abs(vote_diff) >= 2) %>%
  select(player, season, round, team, votes, coaches_votes, vote_diff) %>%
  arrange(desc(abs(vote_diff)))

# ---------------------------------------------------------------
# 9. BIGGEST MISSES + BEST CALLS
# ---------------------------------------------------------------
biggest_surprises <- cv_predictions %>%
  mutate(
    surprise_gap = abs(predicted_votes_oos - votes)
  ) %>%
  filter(surprise_gap >= 2) %>%
  arrange(desc(surprise_gap), desc(votes)) %>%
  select(
    season,
    round,
    team,
    player,
    disposals,
    votes,
    predicted_votes = predicted_votes_oos,
    surprise_gap
  )

# The model's best correct calls, not just its misses -- games where
# it named the exact right vote count for a real vote-getter.
best_calls <- cv_predictions %>%
  filter(
    votes > 0,
    predicted_votes_oos == votes
  ) %>%
  select(
    season,
    round,
    team,
    player,
    disposals,
    votes,
    predicted_votes = predicted_votes_oos
  ) %>%
  arrange(desc(votes), season, round)

# ---------------------------------------------------------------
# 10. HOW VOTE-WINNING HAS CHANGED OVER TIME
# ---------------------------------------------------------------
key_stats_over_time <- c("disposals", "contested_possessions", "tackles",
                          "clearances", "score_involvements")

votes_over_time <- train_eligible %>%
  group_by(season) %>%
  summarise(across(all_of(key_stats_over_time),
                    ~ cor(.x, votes, method = "spearman", use = "complete.obs")),
            .groups = "drop") %>%
  tidyr::pivot_longer(-season, names_to = "stat", values_to = "correlation")

# ---------------------------------------------------------------
# 11. MULTICOLLINEARITY CHECK (VIF)
# ---------------------------------------------------------------
# VIF doesn't depend on the actual outcome model type -- it's purely
# a function of the predictor matrix -- so a plain lm() against votes
# is sufficient here even though the real models are polr/xgboost.
# A VIF above ~5 for a feature means it's substantially explainable
# by the others, and its individual coefficient/importance should be
# read with that in mind.
vif_check <- tryCatch({
  library(car)
  vif_formula <- as.formula(paste("as.numeric(votes) ~", paste(explanatory_features, collapse = " + ")))
  vif_lm <- lm(vif_formula, data = explanatory_data)
  vif_values <- car::vif(vif_lm)
  tibble::tibble(feature = names(vif_values), vif = as.numeric(vif_values)) %>% arrange(desc(vif))
}, error = function(e) {
  warning("car package not available or VIF computation failed (", conditionMessage(e),
          ") -- install.packages('car') to enable the multicollinearity check.")
  NULL
})

# ---------------------------------------------------------------
# 12. MARKET ODDS BENCHMARK (optional -- needs manually-supplied data)
# ---------------------------------------------------------------
# No reliable programmatic source for historical Brownlow medal market
# odds exists in fitzRoy or elsewhere readily available here --
# fetch_betting_odds_footywire() covers match betting (head-to-head/
# line) only, not player award futures. Same fallback pattern as
# data/coaches_votes.csv: fill in data/brownlow_odds.csv yourself
# (columns: season, player, market_odds -- decimal odds, lower is
# more favoured) and this activates automatically; until then it's
# skipped with a clear note rather than silently absent.
market_odds_comparison <- NULL
if (file.exists(MARKET_ODDS_CSV) && nrow(readr::read_csv(MARKET_ODDS_CSV, show_col_types = FALSE)) > 0) {
  market_odds <- readr::read_csv(MARKET_ODDS_CSV, show_col_types = FALSE) %>%
    mutate(player_key = normalise_name(player))

  model_pick_rank <- vote_equity_counted %>%
    mutate(player_key = normalise_name(player)) %>%
    group_by(season, player_key) %>%
    summarise(player = dplyr::first(player), predicted_votes = sum(predicted_votes), .groups = "drop") %>%
    group_by(season) %>%
    mutate(model_rank = rank(-predicted_votes, ties.method = "first")) %>%
    ungroup()

  market_odds_comparison <- market_odds %>%
    left_join(model_pick_rank %>% select(season, player_key, model_rank), by = c("season", "player_key")) %>%
    group_by(season) %>%
    mutate(market_rank = rank(market_odds)) %>%
    ungroup() %>%
    arrange(season, market_odds) %>%
    select(-player_key)
} else {
  message("No data/brownlow_odds.csv found -- market-odds benchmark section will be skipped ",
          "until that file is filled in. See the comment above this line for the expected format.")
}

# ---------------------------------------------------------------
# 13. MONTE CARLO SEASON SIMULATION
# ---------------------------------------------------------------
# Directly inspired by Wheelo Ratings' published methodology: instead
# of one deterministic top-3-per-game prediction, every game's outcome
# is simulated many times, respecting the real constraint that exactly
# one player gets 3 votes, one gets 2, one gets 1 -- aggregated across
# a whole season across many simulations, this gives genuine
# probabilities (who wins the medal, the shape of the vote-count
# distribution), not just a point estimate.
#
# Implemented via the Gumbel-max trick: adding i.i.d. Gumbel(0,1)
# noise to log(strength) and taking the top 3 by the perturbed value
# is mathematically equivalent to Plackett-Luce sequential sampling
# (draw 1st proportional to strength, remove, draw 2nd from the
# remainder, remove, draw 3rd), but fully vectorisable as matrix
# operations per game -- the only way thousands of simulations across
# a whole season's games finishes in reasonable time in R, rather than
# a sampling loop. "Strength" (the Plackett-Luce worth parameter) is
# expected_votes -- already a genuine, non-negative expectation over
# the model's real probability distribution.
simulate_season_votes <- function(season_data, n_sims, top_n_candidates = 30) {
  season_data <- season_data %>%
    mutate(strength = pmax(expected_votes, 1e-6), log_strength = log(strength))

  # Exactly one row per player_id, even if a player genuinely has two
  # rows in season_data with different team values -- collapsing with
  # first() rather than distinct(player_id, player, team) directly
  # avoids the same many-to-many join duplication this project has
  # already hit (and fixed) more than once elsewhere in this pipeline.
  player_lookup <- season_data %>%
    group_by(player_id) %>%
    summarise(player = dplyr::first(player), team = dplyr::first(team), .groups = "drop")

  games <- season_data %>% distinct(round, match_id) %>% arrange(round, match_id)

  game_results_list <- vector("list", nrow(games))
  three_vote_list <- vector("list", nrow(games))

  for (i in seq_len(nrow(games))) {
    g <- games[i, ]
    game_players <- season_data %>% filter(round == g$round, match_id == g$match_id)
    n_players <- nrow(game_players)
    if (n_players < 3) next

    noise <- matrix(-log(-log(matrix(runif(n_players * n_sims), nrow = n_players))),
                     nrow = n_players, ncol = n_sims)
    perturbed <- game_players$log_strength + noise   # log_strength recycled across columns

    # Column = one simulation; row indices of its top 3 perturbed
    # values, in order, become votes 3, 2, 1 for this game in that sim.
    top3_idx <- apply(perturbed, 2, function(col) order(col, decreasing = TRUE)[1:3])

    game_result <- tibble::tibble(
      sim = rep(seq_len(n_sims), each = 3),
      player_id = game_players$player_id[as.vector(top3_idx)],
      votes_sim = rep(c(3, 2, 1), times = n_sims)
    )
    game_results_list[[i]] <- game_result

    three_vote_list[[i]] <- game_result %>%
      filter(votes_sim == 3) %>%
      count(player_id, name = "n_3vote") %>%
      mutate(round = g$round, match_id = g$match_id, prob_3votes = n_3vote / n_sims)
  }

  all_results <- bind_rows(game_results_list)

  totals_by_sim <- all_results %>%
    group_by(sim, player_id) %>%
    summarise(total_votes = sum(votes_sim), .groups = "drop")

  three_vote_games <- bind_rows(three_vote_list) %>%
    left_join(player_lookup, by = "player_id") %>%
    arrange(desc(prob_3votes))

  # Per simulation, whoever has the single highest total is that
  # simulation's medallist -- a player who never appears in
  # totals_by_sim for a given sim genuinely has 0 for it, and 0 can
  # never beat whoever does appear (every game awards 6 votes total,
  # so someone always has a positive total in every sim), so this is
  # correct without needing to materialise explicit zero-rows for
  # every player in every simulation.
  win_sims <- totals_by_sim %>% group_by(sim) %>% slice_max(total_votes, n = 1, with_ties = FALSE) %>% ungroup()
  win_probabilities <- win_sims %>% count(player_id, name = "wins") %>%
    mutate(win_probability = wins / n_sims) %>%
    left_join(player_lookup, by = "player_id") %>%
    arrange(desc(win_probability))

  # Vote-count distributions ARE explicitly zero-filled, but only for
  # a shortlist of candidates (not every player who ever appears) --
  # needed here because a player's average should reflect every
  # simulation they didn't poll in, not just the ones where they did.
  candidate_ids <- totals_by_sim %>% group_by(player_id) %>%
    summarise(mean_total = mean(total_votes), .groups = "drop") %>%
    arrange(desc(mean_total)) %>% slice_head(n = top_n_candidates) %>% pull(player_id)

  vote_distributions <- tidyr::expand_grid(sim = seq_len(n_sims), player_id = candidate_ids) %>%
    left_join(totals_by_sim, by = c("sim", "player_id")) %>%
    mutate(total_votes = replace_na(total_votes, 0)) %>%
    group_by(player_id) %>%
    summarise(mean_votes = mean(total_votes), median_votes = median(total_votes),
              p10 = quantile(total_votes, 0.1), p90 = quantile(total_votes, 0.9), .groups = "drop") %>%
    left_join(player_lookup, by = "player_id") %>%
    left_join(win_probabilities %>% select(player_id, win_probability), by = "player_id") %>%
    mutate(win_probability = replace_na(win_probability, 0)) %>%
    arrange(desc(mean_votes))

  list(win_probabilities = win_probabilities, vote_distributions = vote_distributions,
       three_vote_games = three_vote_games)
}

simulation_season <- max(counted_seasons)

simulation_input <- scorable_data %>%
  filter(season == simulation_season) %>%
  mutate(expected_votes = combined_score)

message("Running ", N_SIMULATIONS, " simulations of the ", simulation_season,
        " Brownlow count -- this is the slowest step in the whole pipeline, expect it to take a while.")
simulation_results <- simulate_season_votes(simulation_input, N_SIMULATIONS)

win_probabilities  <- simulation_results$win_probabilities
vote_distributions <- simulation_results$vote_distributions
three_vote_games   <- simulation_results$three_vote_games

# ---------------------------------------------------------------
# 14. ROUND-BY-ROUND EXPECTED LEADERBOARD
# ---------------------------------------------------------------
# Not simulation-based -- re-simulating at every round checkpoint
# would need storing/aggregating full simulated distributions at each
# cut, a lot more compute for secondary insight. Uses cumulative
# EXPECTED votes instead, itself already a genuine expectation over
# the model's real probability distribution, just evaluated once per
# player-game rather than re-simulated at every round.
round_by_round_leaderboard <- scorable_data %>%
  filter(season == simulation_season) %>%
  group_by(round, match_id) %>%
  mutate(
    predicted_votes = case_when(
      rank(-combined_score, ties.method = "first") == 1 ~ 3,
      rank(-combined_score, ties.method = "first") == 2 ~ 2,
      rank(-combined_score, ties.method = "first") == 3 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  ungroup() %>%
  arrange(player_id, round) %>%
  group_by(player_id) %>%
  mutate(
    cumulative_predicted_votes = cumsum(predicted_votes)
  ) %>%
  ungroup() %>%
  select(
    round,
    player_id,
    player,
    team,
    cumulative_predicted_votes
  )

# ---------------------------------------------------------------
# 15. SIMULATION BACKTEST -- a genuinely different, arguably more
#     meaningful check than the per-game top-3 accuracy metrics
#     above: would this whole pipeline have correctly named the real
#     medallist, for every season where that's actually known? Uses a
#     lighter simulation count (N_SIMULATIONS_BACKTEST) since this
#     runs once per counted season, not just once.
# ---------------------------------------------------------------
simulation_backtest <- purrr::map_dfr(counted_seasons, function(yr) {
  
  season_input <- cv_predictions %>%
    filter(held_out_season == yr) %>%
    mutate(expected_votes = global_expected_votes)
  
  if (nrow(season_input) == 0) return(NULL)
  
  sim_result <- simulate_season_votes(
    season_input,
    N_SIMULATIONS_BACKTEST,
    top_n_candidates = 5
  )
  
  winner_prediction <- sim_result$win_probabilities %>%
    slice_max(win_probability, n = 1, with_ties = FALSE)
  
  actual_winner <- season_input %>%
    group_by(player_id) %>%
    summarise(
      real_total = sum(votes, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    slice_max(real_total, n = 1, with_ties = FALSE) %>%
    left_join(
      season_input %>% distinct(player_id, player),
      by = "player_id"
    )
  
  actual_winner_probability <- sim_result$win_probabilities %>%
    filter(player_id == actual_winner$player_id[1]) %>%
    summarise(
      actual_winner_win_prob = first(win_probability),
      .groups = "drop"
    )
  
  tibble::tibble(
    season = yr,
    predicted_winner = winner_prediction$player[1],
    predicted_win_prob = winner_prediction$win_probability[1],
    real_winner = actual_winner$player[1],
    actual_winner_win_prob = actual_winner_probability$actual_winner_win_prob[1],
    real_winner_total_votes = actual_winner$real_total[1],
    correct = identical(
      winner_prediction$player_id[1],
      actual_winner$player_id[1]
    )
  )
})
