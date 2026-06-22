# World Cup 2026 Analysis — Claude Context

## Project Purpose

A multi-topic analysis of the 2026 FIFA World Cup group stage. Current analyses:
- **Tiebreaker rules**: impact of switching from Goal Difference (GD) to Head-to-Head (H2H) as the primary tiebreaker
- **Hydration breaks**: whether FIFA-mandated cooling breaks measurably shift match momentum

The 2026 tournament expanded to 48 teams with 12 groups of 4, where the **top 3** teams in each group advance.

## Folder Structure

```
World Cup/
├── .here                          ← project root anchor for the `here` package
├── data/                          ← all CSV inputs and outputs
│   ├── clean_fixtures.csv
│   ├── group_standings_R.csv
│   ├── elimination_scenarios.csv
│   ├── api_predictions_cache.csv
│   └── hydration_breaks.csv       ← populate manually as matches are played
├── analyses/
│   ├── tiebreaker_rules/          ← H2H vs GD elimination analysis (run in order)
│   │   ├── 01_fetch_fixtures.R
│   │   ├── 02_calculate_groups.R
│   │   ├── 03_elimination_simulator.R
│   │   ├── 04_fetch_predictions.R
│   │   ├── 05_elimination_likelihood.R
│   │   └── 06_analyze_gd_miracles.R
│   └── hydration_breaks/          ← cooling break momentum analysis
│       └── hydration_momentum.R
└── utils/
    └── manual_score_updater.R     ← interactive score entry tool
```

## Path Conventions

**IMPORTANT — ALL new R scripts MUST follow this pattern. Never use bare relative paths like `"../../data/file.csv"` or `"file.csv"`.**

Use the `here` package. It resolves all paths from the project root, anchored by the `.here` file, regardless of where the script lives in the folder tree.

Every new script must include `library(here)` and reference files like this:

```r
library(here)

# Reading data
df <- read.csv(here("data", "clean_fixtures.csv"), stringsAsFactors = FALSE)

# Writing data
write.csv(df, here("data", "output.csv"), row.names = FALSE)
```

## Tiebreaker Pipeline — Run in Order

```
01_fetch_fixtures.R        → pulls live match data      → data/clean_fixtures.csv
02_calculate_groups.R      → computes standings         → data/group_standings_R.csv
03_elimination_simulator.R → dual-ruleset sim           → data/elimination_scenarios.csv
04_fetch_predictions.R     → Poisson/Elo model          → data/api_predictions_cache.csv
05_elimination_likelihood.R → weighted risk output      → (console)
06_analyze_gd_miracles.R   → GD miracle analysis       → (console)
```

## Hydration Breaks Pipeline

```
1. Populate data/hydration_breaks.csv with per-match shot/goal counts split before/after each break
2. Run analyses/hydration_breaks/hydration_momentum.R
```

See the column guide at the top of `hydration_momentum.R` for the expected CSV schema.

## Key Concepts — Tiebreaker

**H2H Rules (2026 actual)**: When teams are tied on points, head-to-head results among the tied teams break the tie first.

**GD Rules (classic baseline)**: When teams are tied on points, overall goal difference breaks the tie first.

**"Controversy" scenario**: A team eliminated under H2H rules that would survive under classic GD rules, or vice versa.

**"Miracle" analysis** (`06_analyze_gd_miracles.R`): For teams doomed under H2H, calculates the Poisson probability of the exact GD swing needed to survive under classic rules.

## Tiebreaker Logic Implementation

- `check_survives_h2h()` in `03_elimination_simulator.R`: computes points, isolates tied teams, re-runs H2H sub-table, uses the +0.1 "infinite GD" trick to resolve any remaining tie in the target team's favor (best-case scenario).
- `check_survives_gd()` in `03_elimination_simulator.R`: same structure but +0.1 on overall points.

## Elo / Poisson Model

- Source: live ratings from `eloratings.net`
- Host nations (USA, Mexico, Canada) get +100 Elo bonus for home advantage
- Base xG = `1.25 * 10^(elo_diff / 1000)` per team
- Probability matrix: 11×11 Poisson joint distribution (0–10 goals per side)
- `data/api_predictions_cache.csv` caches results to avoid re-fetching Elo on every run

## Data Files

| File | Source | Description |
|---|---|---|
| `data/clean_fixtures.csv` | worldcup26.ir API | All 2026 group-stage fixtures with scores |
| `data/group_standings_R.csv` | `02_calculate_groups.R` | Current standings sorted by Pts/GD/GF |
| `data/elimination_scenarios.csv` | `03_elimination_simulator.R` | Per-team elimination scenarios under H2H and GD rules |
| `data/api_predictions_cache.csv` | `04_fetch_predictions.R` | Poisson match-outcome probabilities |
| `data/hydration_breaks.csv` | Manual entry | Per-match shot/goal splits around each cooling break |

## Team Name Normalization

The `clean_team_name()` function handles API name mismatches:
- `"USA"` → `"united states"`
- `"IR Iran"` → `"iran"`
- `"Korea Republic"` → `"south korea"`
- `"Côte d'Ivoire"` → `"ivory coast"`

When adding new teams or fixing match-up bugs, check this function first.

## Dependencies

```r
install.packages(c("dplyr", "jsonlite", "here"))
```
