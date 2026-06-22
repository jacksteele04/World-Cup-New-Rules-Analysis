# World Cup 2026 Analysis

An R-based analysis project covering multiple aspects of the 2026 FIFA World Cup group stage. The tournament expanded to 48 teams across 12 groups of 4, with the top 3 in each group advancing.

---

## Analyses

### Tiebreaker Rules Change
Compares how FIFA's 2026 rule change affects group-stage elimination outcomes. The 2026 World Cup is the first to use **Head-to-Head results** as the primary tiebreaker instead of **Goal Difference** — this analysis quantifies exactly when and how that changes who goes home.

| Priority | Classic Rules (pre-2026) | 2026 Rules (new) |
|---|---|---|
| 1 | Points | Points |
| 2 | **Goal Difference** | **Head-to-Head results** |
| 3 | Goals Scored | Head-to-Head GD |
| 4 | H2H Results | Overall GD |

### Hydration Breaks Momentum
Tests whether FIFA-mandated cooling breaks (triggered when WBGT > 28°C) measurably shift match momentum — shots, goal rates, and pressing intensity before vs. after each break.

---

## Quick Start

### Prerequisites

```r
install.packages(c("dplyr", "jsonlite", "here"))
```

### Path Convention

All scripts use the [`here`](https://here.r-lib.org/) package for file paths. The `.here` file at the project root anchors all paths so scripts work from any working directory:

```r
library(here)
read.csv(here("data", "clean_fixtures.csv"))
```

Never use bare relative paths like `"../../data/file.csv"`.

### Tiebreaker Pipeline — Run in Order

```r
source("analyses/tiebreaker_rules/01_fetch_fixtures.R")        # Pull live match data
source("analyses/tiebreaker_rules/02_calculate_groups.R")      # Compute group standings
source("analyses/tiebreaker_rules/03_elimination_simulator.R") # Dual-ruleset elimination sim
source("analyses/tiebreaker_rules/04_fetch_predictions.R")     # Build Poisson probability cache
source("analyses/tiebreaker_rules/05_elimination_likelihood.R")# Weighted elimination risk
source("analyses/tiebreaker_rules/06_analyze_gd_miracles.R")   # GD miracle analysis
```

### Hydration Breaks Pipeline

1. Populate `data/hydration_breaks.csv` with per-match shot/goal counts split before and after each break (see column guide at the top of `hydration_momentum.R`)
2. Run `analyses/hydration_breaks/hydration_momentum.R`

---

## Folder Structure

```
World Cup/
├── .here                              ← project root anchor for `here` package
├── README.md
├── CLAUDE.md                          ← AI assistant context
│
├── data/                              ← all CSV inputs and outputs
│   ├── clean_fixtures.csv             # [generated] All 2026 group-stage fixtures
│   ├── group_standings_R.csv          # [generated] Current group standings
│   ├── elimination_scenarios.csv      # [generated] Elimination scenarios by ruleset
│   ├── api_predictions_cache.csv      # [generated] Cached Poisson match predictions
│   └── hydration_breaks.csv           # [manual] Per-match shot/goal splits around breaks
│
├── analyses/
│   ├── tiebreaker_rules/
│   │   ├── 01_fetch_fixtures.R        # Fetches live fixture data from worldcup26.ir
│   │   ├── 02_calculate_groups.R      # Computes current group standings
│   │   ├── 03_elimination_simulator.R # Core dual-ruleset elimination engine
│   │   ├── 04_fetch_predictions.R     # Poisson/Elo match probability model
│   │   ├── 05_elimination_likelihood.R# Weighted elimination risk output
│   │   └── 06_analyze_gd_miracles.R   # GD miracle probability analysis
│   └── hydration_breaks/
│       └── hydration_momentum.R       # Cooling break momentum analysis
│
└── utils/
    └── manual_score_updater.R         # Interactive score entry fallback
```

---

## Tiebreaker Analysis — Script Details

### `01_fetch_fixtures.R`
Pulls the full fixture list from the `worldcup26.ir` API and writes/updates `data/clean_fixtures.csv`. Uses an upsert pattern — only writes rows that have changed, so re-running after a match finishes is safe.

### `02_calculate_groups.R`
Reads `data/clean_fixtures.csv` and computes group standings (Pts, GD, GF, GA) for all finished matches, sorted in the standard FIFA tiebreaker order.

### `03_elimination_simulator.R`
The analytical core. For each team in each group, exhaustively simulates all possible remaining-match outcomes (3^n combinations) and checks survival under both rulesets:
- **H2H check**: isolates tied teams, computes sub-table, applies +0.1 "infinite H2H advantage" trick for best-case analysis
- **GD check**: applies +0.1 to points for best-case GD scenario
- Outputs `data/elimination_scenarios.csv` with per-team, per-ruleset scenarios

### `04_fetch_predictions.R`
Builds `data/api_predictions_cache.csv` by running a Poisson model against live Elo ratings from `eloratings.net`. Host nations (USA, Mexico, Canada) receive a +100 Elo home-advantage bonus.

### `05_elimination_likelihood.R`
Reads scenarios and cache, weights each elimination scenario by its Poisson probability, and prints a dashboard: teams already eliminated, teams at risk, and total "controversy count" (cases where rulesets disagree).

### `06_analyze_gd_miracles.R`
For teams eliminated under H2H but not GD, runs a full 11×11 Poisson score matrix (14,641 combinations) over final-matchday games to calculate the probability of hitting the goal-difference swing needed to survive under classic rules.

---

## Tiebreaker Model Details

**Elo source**: Live ratings from [eloratings.net](https://www.eloratings.net)

**xG formula**: `1.25 * 10^(elo_diff / 1000)` per team

**Score distribution**: Poisson, 0–10 goals per side (11×11 joint probability matrix)

**Home advantage**: +100 Elo for USA, Mexico, Canada when hosting

---

## Key Output Concepts

**"At Risk"**: A team that would be mathematically eliminated if specific Round 2 results occur.

**"Already Eliminated"**: A team that is mathematically out regardless of all remaining results.

**"Controversy"**: A scenario where H2H and GD rules produce different outcomes for the same team.

**Miracle probability**: The Poisson-weighted chance that the exact goal swing needed to survive under GD rules actually occurs in the final round of matches.
