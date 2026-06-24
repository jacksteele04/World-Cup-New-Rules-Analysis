library(dplyr)
library(here)

source(here("config.R"))
source(here("standardization", "validation.R"))
source(here("standardization", "shot_metrics.R"))

# =============================================================================
# 03_compare_2022_2026.R
# Compares momentum shifts around the same time windows (30' / 75') between:
#   2026 World Cup — mandated hydration breaks at those minutes
#   2022 World Cup — no breaks; same windows used as a natural control
#
# 2026 shots: TheStatsAPI (xG model from TheStatsAPI)
# 2022 shots: StatsBomb open data (xG model from StatsBomb)
# Note: shot rate / SOT rate comparisons are model-independent and fully valid.
#       xG delta comparison should be interpreted with caution (different models).
#
# Statistical test: Mann-Whitney U, one-sided
#   H1: 2026 delta > 2022 delta  ->  break effect is real, not just tempo
# =============================================================================

compare_metric <- function(d26, d22, metric, label) {
  x26 <- d26[[metric]]; x26 <- x26[!is.na(x26)]
  x22 <- d22[[metric]]; x22 <- x22[!is.na(x22)]

  if (length(x26) < 3 || length(x22) < 3) {
    cat(sprintf("   %-18s insufficient data\n", label)); return(invisible(NULL))
  }

  mw  <- wilcox.test(x26, x22, alternative = "greater", exact = FALSE)
  sig <- ifelse(mw$p.value < 0.05, " *", "")

  cat(sprintf("   %-18s 2022: %+.4f (n=%d)   2026: %+.4f (n=%d)   p=%.4f%s\n",
              label,
              median(x22), length(x22),
              median(x26), length(x26),
              mw$p.value, sig))
}

# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------

cat("LOADING DATA\n", strrep("=", 60), "\n", sep = "")

require_file(here("data", "events", "hydration_breaks.csv"),          "run 02_build_break_schedule.R first")
require_file(here("data", "shotmaps", "shotmap_thestatsapi.csv"),        "run fetch/fetch_shotmap_thestatsapi.R first")
require_file(here("data", "fixtures", "clean_fixtures_thestatsapi.csv"), "run fetch/fetch_fixtures_thestatsapi.R first")
require_file(here("data", "shotmaps", "shotmap_statsbomb_2022.csv"),     "run fetch/fetch_shotmap_statsbomb_2022.R first")

# 2026
breaks_26   <- read.csv(here("data", "events", "hydration_breaks.csv"),          stringsAsFactors = FALSE)
shots_26    <- read.csv(here("data", "shotmaps", "shotmap_thestatsapi.csv"),        stringsAsFactors = FALSE)
fixtures_26 <- read.csv(here("data", "fixtures", "clean_fixtures_thestatsapi.csv"), stringsAsFactors = FALSE)

require_cols(shots_26, c("match_id", "team_id", "minute", "is_on_target", "expected_goals"), "shotmap_thestatsapi.csv")

# 2022 (StatsBomb open data)
shots_22 <- read.csv(here("data", "shotmaps", "shotmap_statsbomb_2022.csv"), stringsAsFactors = FALSE)
require_cols(shots_22, c("match_id", "team_id", "minute", "is_on_target", "expected_goals"), "shotmap_statsbomb_2022.csv")

# Build 2022 pseudo-break schedule from StatsBomb shotmap (consistent StatsBomb IDs)
schedule_22 <- shots_22 %>%
  select(match_id, home_team, away_team, home_team_id, away_team_id) %>%
  distinct() %>%
  mutate(break_minute_1h = DEFAULT_BREAK_1H, break_minute_2h = DEFAULT_BREAK_2H)

fixtures_22 <- schedule_22 %>% select(match_id, home_team_id, away_team_id)

# 2026: use thestatsapi_id as match_id (drop the worldcup26.ir integer match_id)
breaks_26_shots <- breaks_26 %>%
  select(-match_id) %>%
  rename(match_id = thestatsapi_id) %>%
  filter(!is.na(match_id))

cat(sprintf("2026: %d matches in break schedule | %d shot records\n", nrow(breaks_26_shots), nrow(shots_26)))
cat(sprintf("2022: %d matches in pseudo schedule | %d shot records\n\n", nrow(schedule_22), nrow(shots_22)))

# -----------------------------------------------------------------------------
# Compute shot metrics and deltas
# -----------------------------------------------------------------------------

cat("Computing metrics...\n")
df_26 <- compute_shot_metrics(breaks_26_shots, shots_26, fixtures_26)
df_22 <- compute_shot_metrics(schedule_22,     shots_22, fixtures_22)

deltas_26_1h <- compute_deltas(df_26, "1")
deltas_26_2h <- compute_deltas(df_26, "2")
deltas_22_1h <- compute_deltas(df_22, "1")
deltas_22_2h <- compute_deltas(df_22, "2")

# -----------------------------------------------------------------------------
# Output
# -----------------------------------------------------------------------------

cat("\n")
cat(strrep("=", 70), "\n")
cat("MOMENTUM SHIFT COMPARISON: 2022 (no break) vs 2026 (with break)\n")
cat("Time windows: 1st half = 0-30' vs 30-45'  |  2nd half = 46-75' vs 75-90'\n")
cat("Test: Mann-Whitney U, one-sided (H1: 2026 delta > 2022 delta)\n")
cat("Note: xG uses different models (StatsBomb '22 vs TheStatsAPI '26);\n")
cat("      shot rate and SOT rate are model-independent.\n")
cat(strrep("=", 70), "\n")

cat("\nFIRST-HALF WINDOW (break/pseudo-break at ~30')\n", strrep("-", 70), "\n", sep = "")
compare_metric(deltas_26_1h, deltas_22_1h, "delta_shot", "Shot rate/min")
compare_metric(deltas_26_1h, deltas_22_1h, "delta_sot",  "SOT rate/min")
compare_metric(deltas_26_1h, deltas_22_1h, "delta_xg",   "xG rate/min")

cat("\nSECOND-HALF WINDOW (break/pseudo-break at ~75')\n", strrep("-", 70), "\n", sep = "")
compare_metric(deltas_26_2h, deltas_22_2h, "delta_shot", "Shot rate/min")
compare_metric(deltas_26_2h, deltas_22_2h, "delta_sot",  "SOT rate/min")
compare_metric(deltas_26_2h, deltas_22_2h, "delta_xg",   "xG rate/min")

cat("\n* p < 0.05: 2026 shift significantly larger than 2022 baseline\n")
cat("  (suggests the break, not just natural tempo, drives the effect)\n")
cat(strrep("=", 70), "\n")
