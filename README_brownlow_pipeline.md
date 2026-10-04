# Brownlow Vote Analysis — setup notes

## Files added

```
source/
├── R/
│   ├── 00_config.R      # seasons, feature lists, DEMO_MODE switch
│   ├── 01_fetch_data.R  # fitzRoy pulls + synthetic demo-mode fallback
│   ├── 02_clean.R       # validation, name matching, drift checks
│   ├── 03_features.R    # standard + 6 signature innovative features
│   └── 04_model.R       # ordinal model, XGBoost, CV, Vote Equity Index
├── data/
│   └── coaches_votes.csv   # empty template — fill this in by hand
└── brownlow_analysis.Rmd   # the site page itself
```

## What changed in this version (confirmed against a live fetch)

- **No separate Brownlow-votes fetch anymore.** `fetch_player_stats_afltables()`
  already returns a per-game `Brownlow.Votes` column — the pipeline uses that
  directly instead of merging in a second source.
- **No separate results/match fetch anymore either** — scores, teams, and venue
  are all already in the player-stats pull, so `fetch_results()` was dropped.
- **Column names are AFL Tables' real `Title.Case.Dot` convention**
  (`Disposals`, `Contested.Possessions`, `Home.score`, etc.), not the
  `snake_case` guesses from earlier versions. `standardize_live_player_stats()`
  in `01_fetch_data.R` maps them over.
- **Finals rows are dropped** — Brownlow votes are only awarded in
  home-and-away rounds.
- **"Counted" vs "pending" seasons are detected at runtime**, not hardcoded.
  AFL Tables only backfills a season's per-game votes after that season's
  count night — confirmed live: 2022–2025 fully populated, 2026 entirely
  `NA` (pre-count-night when tested). The model trains only on counted
  seasons; a pending season is still scored, just validated against a
  season-total figure instead of per-game votes.
- **`position_group` is a rough stat-based heuristic**, not real position
  data — AFL Tables' scrape doesn't carry that field. Treat it as
  directional, not authoritative.
- **`metres_gained` and true `disposal_efficiency`/`score_involvements`
  don't exist in this data source** (they're Champion Data metrics) —
  the features that used them now use AFL-Tables-native proxies instead,
  documented inline in `03_features.R`.

### One unverified assumption worth checking yourself

`fetch_awards_brownlow()`'s column meanings aren't documented. The pipeline
assumes `Votes_3` is the real season vote total (it matched the real
publicly reported 2026 result and was internally consistent with `V_G`
in the same row), and that the separate `Votes` column is something else
(possibly career or team total) — not season points. Worth a quick sanity
check before trusting the "pending season" section of the page:

```r
season_total_check(season_total_votes, season = 2026, player_contains = "Daicos")
```

Compare the `season_total_votes` value against a result you already know
to be correct. If it doesn't match, the column mapping in
`standardize_season_totals()` in `01_fetch_data.R` needs adjusting.

### The prediction model: a genuine 4-class probability model

The predictive model is a single 4-class probability model
(`multi:softprob`), not the earlier two-stage hurdle+ranker design.
Every player-game gets a real `P(0 votes), P(1), P(2), P(3)` estimate;
players are ranked within a game by expected votes
(`0*P0 + 1*P1 + 2*P2 + 3*P3`), and the top 3 get 3/2/1 predicted votes.

Position-specific models were built and tested (`fit_position_models()`,
used inside the CV loop in `04_model.R`) on the theory that a real
probability scale would avoid an earlier percentile-blending design's
coupling problem between positions. It did fix that specific problem --
but position-specialised models turned out not to help overall anyway,
for a different reason: estimating a full 4-class distribution needs
more data per class than a simpler binary-hurdle-plus-ranking approach
did, and smaller position groups (ruck especially, ~160 vote-getting
games total) don't have enough examples per class to calibrate
reliably on their own. Only midfield showed a real gain. **The model
that actually generates predictions (`global_model_full`) is the plain
pooled model** -- position-specific fitting still runs inside the CV
loop purely to report that comparison honestly on the page, not to
feed production output.

This still meaningfully increases render time versus a single-model
pipeline: the leave-one-season-out CV pass fits a global model AND up
to four position-specific models per held-out season, on top of the
hyperparameter search itself, purely for that diagnostic. Expect a
first render after any `04_model.R` change to take noticeably longer
than a plain single-model pipeline would.

### Market-odds benchmark

The market-odds comparison section needs `data/brownlow_odds.csv` filled
in manually (columns: `season, player, market_odds`) -- no reliable
programmatic source for historical Brownlow futures odds was available.
It's skipped with a clear note on the page until that file has real rows.

## This version: a full redesign, inspired by Wheelo Ratings

This rebuild changed almost everything except the overall pipeline
shape (`00` through `04`, then the `.Rmd`). Summary of what's new:

- **10 seasons of data**, not 5 (`SEASONS` in `00_config.R`).
- **"Every stat possible"**: every raw AFL Tables column is now a
  feature (kicks, handballs, marks, contested marks, marks inside 50,
  rebounds, bounces, frees for/against, hit-outs, one-percenters, on
  top of what was already there) -- see `RAW_STAT_FEATURES`.
- **Coaches' votes, SuperCoach and AFL Fantasy scores are now real
  model inputs**, not just a side comparison. Two independent public
  Brownlow-modelling writeups (Wheelo Ratings' own methodology note,
  and a Betfair data-science writeup) both found coaches' votes and
  fantasy-style composite scores among the single strongest available
  predictors -- confirmed via the main search context, not assumed.
- **Two separate feature lists, not one**: `ALL_MODEL_FEATURES` (used
  by XGBoost, which doesn't need complete cases and isn't vulnerable
  to collinearity the way a linear model is) and the smaller, curated
  `EXPLANATORY_FEATURES` (used only by the ordinal regression, VIF
  check, and Brant test). This is a direct lesson from this project's
  own history: several raw stats are exact arithmetic identities of
  each other (disposals = kicks + handballs), and including an exact
  identity in a linear model causes a hard crash ("rank-deficient
  design"), not just a soft collinearity warning -- confirmed the hard
  way earlier in this build. `EXPLANATORY_FEATURES` deliberately
  excludes the redundant raw components; `ALL_MODEL_FEATURES` keeps
  everything, since XGBoost's tree splits don't have this problem.
- **Missing values are no longer discarded wholesale.** The old
  pipeline required every one of a smaller feature set to be
  non-missing before a row could be used anywhere. With ~30 features
  now, several sourced externally (SuperCoach/Fantasy) with real
  coverage gaps, that would throw out a lot of otherwise-good data.
  `scorable_data`/`train_eligible` now keep NA as NA for XGBoost
  (which handles missing features natively); only the smaller,
  curated `explanatory_data` subset is filtered to complete cases,
  since the classical statistical models actually need that.
- **Regularisation, not just tree-depth tuning.** With a much larger
  feature set, overfitting risk is real -- `REGULARIZATION_DEFAULTS`
  in `04_model.R` fixes non-zero `reg_alpha`/`reg_lambda`/
  `min_child_weight` and tighter subsampling on every model fit,
  rather than leaving them at XGBoost's defaults.
- **A real Monte Carlo simulation layer**, directly modelled on
  Wheelo's published approach of simulating the full count 20,000
  times. Implemented via the Gumbel-max trick (mathematically
  equivalent to Plackett-Luce sampling, but vectorisable, which is
  the only way this finishes in reasonable time in R) -- see
  `simulate_season_votes()` in `04_model.R`. Produces genuine win
  probabilities, vote-count distributions, and "most likely 3-vote
  game" probabilities, not just one deterministic prediction.
- **A simulation backtest**: for every counted season, does the
  simulation's most-likely winner actually match the real historical
  medallist? A different, arguably more meaningful check than the
  per-game top-3 accuracy metrics already in place.
- **A round-by-round expected leaderboard**, using cumulative expected
  votes (not a full re-simulation at every round, which would cost a
  lot more compute for secondary insight).

### Before you run this for real

- **SuperCoach/Fantasy schema is unverified.** `fetch_supercoach_scores()`/
  `fetch_fantasy_scores()`'s exact output columns aren't documented.
  The pipeline resolves them defensively (same pattern as the
  Brownlow season-totals fetch) and warns clearly if it can't find a
  match -- watch for that warning on first run, and check the message
  it prints for the exact `glimpse()` command to run.
- **Runtime is now substantial.** Ten years of data, a much larger
  feature set, regularised hyperparameter tuning, the full CV +
  position-comparison pass, a 20,000-simulation season forecast, AND
  a simulation backtest across every counted season -- all in one
  render. Expect this to take considerably longer than any earlier
  version of this pipeline. If a first run proves impractically slow,
  the two easiest things to turn down are `N_SIMULATIONS` and
  `N_SIMULATIONS_BACKTEST` in `00_config.R`.
- **Check `drift_report` and `incomplete_stats`** (printed by
  `02_clean.R`) before trusting any specific raw stat's importance --
  AFL Tables has added stat categories over time, and a feature only
  partially present across the 10-year window could look more or less
  important than it really is for reasons that have nothing to do
  with the Brownlow.

## Before you render for real

1. **Install packages** (once):
   ```r
   install.packages(c(
     "fitzRoy", "tidyverse", "MASS", "xgboost", "SHAPforxgboost",
     "reactable", "plotly", "broom", "brant", "zoo", "scales", "glue",
     "car", "janitor"
   ))
   ```
   `car` is needed for the multicollinearity (VIF) check; everything else runs fine without it if that install fails, with the VIF section skipped and a clear note printed instead. `janitor` is used throughout `01_fetch_data.R` for column-name cleanup and is NOT optional -- the pipeline will error without it.

2. **Confirm fitzRoy's function names for your installed version** —
   they've shifted across releases:
   ```r
   library(fitzRoy)
   ls("package:fitzRoy")
   ```
   Update the calls inside `01_fetch_data.R`'s `fetch_live_data()` if the
   names in your version differ from `fetch_player_stats()`,
   `fetch_brownlow_votes()`, `fetch_results()`.

3. **Flip demo mode off** in `R/00_config.R`:
   ```r
   DEMO_MODE <- FALSE
   ```
   Until you do this, the page renders on synthetic data (clearly labeled
   on the page itself) so you can check the layout and code path work
   before your real data pull is confirmed.

4. **Coaches' votes now pull live** via `fitzRoy::fetch_coaches_votes()` —
   an earlier version of this README said there was no package source
   for this; that was wrong, the function exists and is used by default.
   `data/coaches_votes.csv` is now only a fallback, used automatically
   if the live fetch fails for some reason (e.g. the AFLCA site structure
   changes). You shouldn't need to touch it unless you see a warning
   telling you to.

5. **Add the page to your navbar** in `source/_site.yml`:
   ```yaml
   navbar:
     left:
       - text: "Brownlow"
         href: brownlow_analysis.html
   ```

6. **Render**: `rmarkdown::render_site(encoding = "UTF-8")` or
   Build → Build Website in RStudio.

## What to sanity-check on first real render

- **Run this once before anything else**, to confirm `fetch_awards_brownlow()`'s
  column names match what `standardize_brownlow()` in `01_fetch_data.R`
  expects (its exact output columns aren't documented, so the script
  guesses defensively and warns if it can't find a match):
  ```r
  library(dplyr)
  glimpse(fitzRoy::fetch_awards_brownlow(season = 2026, type = "player"))
  ```
  If you see the warning about unrecognised columns when you render,
  come back to this output and update the candidate lists in
  `standardize_brownlow()`.
- `vote_check` in `02_clean.R` should have 0 rows (every game's votes
  sum to 6). If not, there's a scrape/merge problem to fix before
  trusting anything downstream.
- The "coaches-vote row(s) couldn't be matched to a team" warning, if
  it appears, tells you how many rows were dropped due to name
  mismatches between AFLCA and AFL Tables spellings -- a handful is
  normal, a large number means the name-matching in `02_clean.R` needs
  attention for specific players.
- `cv_results` in `04_model.R`: the `xgboost` rows should clearly beat
  the `naive_disposals` rows in every held-out season.
- `equity_predicts_growth` (in the rendered page, under "Does
  undervaluation predict future breakouts?"): confirm the correlation
  is meaningfully positive before leaning on the "Ones to Watch" list
  in front of anyone — if it isn't, that's worth reporting honestly
  rather than presenting the list as validated.
