# World Cup 2026 Analysis — Claude Context

## Project Purpose

A multi-topic analysis of the 2026 FIFA World Cup group stage. Current analyses:
- **Tiebreaker rules**: impact of switching from Goal Difference (GD) to Head-to-Head (H2H) as the primary tiebreaker
- **Hydration breaks**: whether FIFA-mandated cooling breaks measurably shift match momentum

The 2026 tournament expanded to 48 teams with 12 groups of 4, where the **top 3** teams in each group advance.

## Folder Structure

```
World Cup/
├── .here                              ← project root anchor for the `here` package
├── config.R                           ← ALL constants: API IDs, Elo model params, break minutes
├── data/                              ← all CSV inputs and outputs
│   ├── fixtures/                      ← raw match schedules
│   │   ├── clean_fixtures.csv         ← 2026 fixtures (worldcup26.ir)
│   │   ├── clean_fixtures_thestatsapi.csv ← 2026 fixtures (TheStatsAPI)
│   │   └── clean_fixtures_thestatsapi_2022.csv ← 2022 fixtures (TheStatsAPI)
│   ├── shotmaps/                      ← shot-level data
│   │   ├── shotmap_thestatsapi.csv    ← 2026 shots with xG
│   │   └── shotmap_statsbomb_2022.csv ← 2022 shots (StatsBomb open data)
│   ├── events/                        ← in-match event data
│   │   ├── goal_events.csv
│   │   └── hydration_breaks.csv       ← break schedule; edit manually for actual break minutes
│   ├── standings/                     ← computed standings and analysis outputs
│   │   ├── group_standings_R.csv
│   │   ├── elimination_scenarios.csv
│   │   └── api_predictions_cache.csv
│   └── ratings/                       ← external ratings cache
│       └── elo_ratings_cache.csv
├── fetch/                             ← data fetching scripts (run before analyses)
│   ├── fetch_fixtures.R               → clean_fixtures.csv
│   ├── fetch_fixtures_thestatsapi.R   → clean_fixtures_thestatsapi.csv
│   ├── fetch_fixtures_thestatsapi_2022.R → clean_fixtures_thestatsapi_2022.csv
│   ├── fetch_shotmap_thestatsapi.R    → shotmap_thestatsapi.csv
│   ├── fetch_shotmap_thestatsapi_2022.R (superseded — TheStatsAPI has no historical shotmaps)
│   ├── fetch_shotmap_statsbomb_2022.R → shotmap_statsbomb_2022.csv
│   └── calculate_groups.R            → group_standings_R.csv
├── analyses/
│   ├── tiebreaker_rules/              ← H2H vs GD tiebreaker analysis (run in order)
│   │   ├── 01_elimination_simulator.R
│   │   ├── 02_fetch_predictions.R
│   │   ├── 03_elimination_likelihood.R
│   │   └── 04_analyze_gd_miracles.R
│   └── hydration_breaks/              ← cooling break momentum analysis
│       ├── 01_parse_goal_events.R
│       ├── 02_build_break_schedule.R
│       ├── 03_compare_2022_2026.R     ← 2022 control group comparison
│       ├── 05_matchday1_graphic.R
│       ├── 06_matchday1_metrics_graphic.R
│       ├── 07_quality_effect.R
│       └── hydration_momentum.R       ← main 2026 analysis
├── standardization/                   ← shared functions sourced by multiple scripts
│   ├── team_names.R                   ← norm_name(), elo_name()
│   ├── elo.R                          ← get_elo_database(), get_elo()
│   ├── shot_metrics.R                 ← compute_shot_metrics(), compute_deltas(), half_counts()
│   ├── api_helpers.R                  ← get_api_key(), tsa_get() for TheStatsAPI
│   └── validation.R                   ← require_file(), require_cols(), require_rows()
├── plots/                             ← PNG outputs from graphic scripts
├── utils/
│   └── manual_score_updater.R         ← interactive score entry tool
├── update_data.R                      ← runs all fetch scripts in order
├── run_tiebreaker_analysis.R          ← runs tiebreaker pipeline end-to-end
├── run_hydration_analysis.R           ← runs hydration break pipeline end-to-end
└── *.bat                              ← thin launchers for double-click; call the .R files above
```

## Script Boilerplate

**Every new script must start with:**

```r
library(here)
source(here("config.R"))                        # always — constants and IDs
source(here("standardization", "validation.R")) # if reading any CSV
# + whichever standardization files you need:
source(here("standardization", "team_names.R"))
source(here("standardization", "shot_metrics.R"))
source(here("standardization", "elo.R"))
source(here("standardization", "api_helpers.R"))  # fetch scripts only
```

All file paths use `here()` with the data subfolder:
```r
df <- read.csv(here("data", "fixtures", "clean_fixtures.csv"), stringsAsFactors = FALSE)
write.csv(df, here("data", "standings", "output.csv"), row.names = FALSE)
```

## config.R — Key Constants

All hardcoded values live in `config.R`. Edit this file when starting a new tournament analysis.

| Object | Key fields |
|--------|-----------|
| `TOURNAMENT` | `season`, `thestatsapi_comp_id`, `thestatsapi_season_id`, `host_nations` |
| `HISTORICAL$qatar_2022` | `thestatsapi_season_id`, `statsbomb_comp_id`, `statsbomb_season_id` |
| `ELO_MODEL` | `host_bonus` (100), `xg_multiplier` (1.25), `elo_divisor` (1000), `default_rating` (1500), `epsilon` (0.1) |
| `HYDRATION` | `break_1h_default` (30), `break_2h_default` (75) |
| `API` | `timeout_secs` (15), `sleep_between` (6), `page_size` (100) |

## Pipelines — Run in Order

Orchestrator scripts at the project root run each pipeline end-to-end:

| Script | What it runs |
|--------|-------------|
| `update_data.R` | All 8 fetch scripts — run this first before any analysis |
| `run_tiebreaker_analysis.R` | Steps 01–04 of the tiebreaker pipeline |
| `run_hydration_analysis.R` | Steps 01–02 + momentum analysis of the hydration pipeline |

Each has a matching `.bat` launcher for double-click on Windows.

**Step 1 — Fetch/update data (`update_data.R`):**
```
fetch/fetch_fixtures.R                  → data/fixtures/clean_fixtures.csv
fetch/fetch_fixtures_thestatsapi.R      → data/fixtures/clean_fixtures_thestatsapi.csv
fetch/calculate_groups.R               → data/standings/group_standings_R.csv
fetch/fetch_elo_ratings.R              → data/ratings/elo_ratings_cache.csv
fetch/fetch_shotmap_thestatsapi.R      → data/shotmaps/shotmap_thestatsapi.csv
fetch/fetch_fixtures_thestatsapi_2022.R → data/fixtures/clean_fixtures_thestatsapi_2022.csv
fetch/fetch_shotmap_statsbomb_2022.R   → data/shotmaps/shotmap_statsbomb_2022.csv
```

**Step 2 — Tiebreaker analysis (`run_tiebreaker_analysis.R`):**
```
01_elimination_simulator.R   → data/standings/elimination_scenarios.csv
02_fetch_predictions.R       → data/standings/api_predictions_cache.csv
03_elimination_likelihood.R  → (console)
04_analyze_gd_miracles.R     → (console)
```

**Step 2 — Hydration break analysis (`run_hydration_analysis.R`):**
```
analyses/hydration_breaks/01_parse_goal_events.R    → data/events/goal_events.csv
analyses/hydration_breaks/02_build_break_schedule.R → data/events/hydration_breaks.csv
analyses/hydration_breaks/hydration_momentum.R      → (console)
```

**Step 2 — Hydration 2022 comparison (run manually):**
```
analyses/hydration_breaks/03_compare_2022_2026.R → (console)
```

## Key Concepts — Tiebreaker

**H2H Rules (2026 actual)**: When teams are tied on points, head-to-head results among the tied teams break the tie first.

**GD Rules (classic baseline)**: When teams are tied on points, overall goal difference breaks the tie first.

**"Controversy" scenario**: A team eliminated under H2H rules that would survive under classic GD rules, or vice versa.

**"Miracle" analysis** (`04_analyze_gd_miracles.R`): For teams doomed under H2H, calculates the Poisson probability of the exact GD swing needed to survive under classic rules.

## Tiebreaker Logic Implementation

- `check_survives_h2h()` and `check_survives_gd()` in `01_elimination_simulator.R`: compute points, isolate tied teams, add `ELO_MODEL$epsilon` (+0.1) to the target team's score as a best-case tiebreaker advantage.

## Elo / Poisson Model

All constants sourced from `config.R → ELO_MODEL`:
- Ratings: live from `eloratings.net`
- Host nations (`TOURNAMENT$host_nations`) get `ELO_MODEL$host_bonus` (+100) Elo
- Base xG = `ELO_MODEL$xg_multiplier * 10^(elo_diff / ELO_MODEL$elo_divisor)`
- Probability matrix: `(ELO_MODEL$max_goals + 1)²` Poisson joint distribution

## Team Name Normalization — `standardization/team_names.R`

Two functions serve different purposes:

| Function | Output | Use when |
|----------|--------|---------|
| `norm_name(x)` | compact token e.g. `"southkorea"` | joining across APIs (cross-source matching) |
| `elo_name(x)` | eloratings.net convention e.g. `"south korea"` | looking up Elo ratings |

When adding new teams, update both functions in `standardization/team_names.R`.

## Data Files

| File | Source | Description |
|---|---|---|
| `data/fixtures/clean_fixtures.csv` | worldcup26.ir API | 2026 group-stage fixtures with scores |
| `data/fixtures/clean_fixtures_thestatsapi.csv` | TheStatsAPI | 2026 fixtures (shot data join key) |
| `data/fixtures/clean_fixtures_thestatsapi_2022.csv` | TheStatsAPI | 2022 Qatar fixtures |
| `data/standings/group_standings_R.csv` | `calculate_groups.R` | Standings sorted by Pts/GD/GF |
| `data/standings/elimination_scenarios.csv` | `01_elimination_simulator.R` | Scenarios under H2H and GD rules |
| `data/standings/api_predictions_cache.csv` | `02_fetch_predictions.R` | Poisson match-outcome probabilities |
| `data/events/goal_events.csv` | `01_parse_goal_events.R` | Minute-level goal events |
| `data/events/hydration_breaks.csv` | `02_build_break_schedule.R` | Break schedule (both API IDs + break minutes) |
| `data/shotmaps/shotmap_thestatsapi.csv` | `fetch_shotmap_thestatsapi.R` | 2026 shot-level data with xG |
| `data/shotmaps/shotmap_statsbomb_2022.csv` | `fetch_shotmap_statsbomb_2022.R` | 2022 shot-level data (StatsBomb) |
| `data/ratings/elo_ratings_cache.csv` | `fetch/fetch_elo_ratings.R` | National team Elo ratings snapshot with `fetched_at` timestamp |

## Dependencies

```r
install.packages(c("dplyr", "jsonlite", "here", "httr", "ggplot2", "tidyr", "scales"))
```
