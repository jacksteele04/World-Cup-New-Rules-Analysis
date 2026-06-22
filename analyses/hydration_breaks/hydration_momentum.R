library(dplyr)
library(here)

# =============================================================================
# HYDRATION BREAK MOMENTUM ANALYSIS — 2026 FIFA WORLD CUP
# =============================================================================
# FIFA mandated cooling/hydration breaks in extreme heat (WBGT > 28°C).
# This script tests whether those breaks measurably shift match momentum:
# shots, attacks, pressing intensity, and scoring rates before vs. after.
# =============================================================================

# -----------------------------------------------------------------------------
# DATA LOADING
# Expects a CSV with one row per match that had a hydration break, columns:
#   match_id, group, home_team, away_team, venue, kickoff_temp_c, wbgt,
#   break_minute_1h, break_minute_2h,   <- minute the break occurred (NA if none)
#   plus per-period shot/goal/attack counts (see COLUMN GUIDE below)
# -----------------------------------------------------------------------------

# COLUMN GUIDE (fill in as data becomes available):
#   shots_home_pre1,  shots_away_pre1   <- shots before 1st-half break
#   shots_home_post1, shots_away_post1  <- shots after 1st-half break
#   shots_home_pre2,  shots_away_pre2   <- shots before 2nd-half break
#   shots_home_post2, shots_away_post2  <- shots after 2nd-half break
#   goals_home_pre1,  goals_away_pre1   <- goals before 1st-half break
#   goals_home_post1, goals_away_post1  <- goals after 1st-half break
#   goals_home_pre2,  goals_away_pre2
#   goals_home_post2, goals_away_post2
#   pressing_home_pre1, pressing_home_post1  <- PPDA or similar pressing metric
#   pressing_away_pre1, pressing_away_post1

load_hydration_data <- function() {
  path <- here("data", "hydration_breaks.csv")
  if (!file.exists(path)) {
    stop("❌ data/hydration_breaks.csv not found. Populate match data first.")
  }
  read.csv(path, stringsAsFactors = FALSE)
}

# -----------------------------------------------------------------------------
# MOMENTUM METRICS
# -----------------------------------------------------------------------------

# Shot rate per minute (normalised for window length)
shot_rate <- function(shots, minutes) {
  ifelse(minutes > 0, shots / minutes, NA_real_)
}

# Momentum Shift Index: positive = home team gained momentum after break
momentum_shift <- function(pre_home, pre_away, post_home, post_away, window_pre, window_post) {
  pre_balance  <- shot_rate(pre_home,  window_pre)  - shot_rate(pre_away,  window_pre)
  post_balance <- shot_rate(post_home, window_post) - shot_rate(post_away, window_post)
  post_balance - pre_balance
}

# -----------------------------------------------------------------------------
# FIRST-HALF BREAK ANALYSIS
# -----------------------------------------------------------------------------

analyze_first_half_breaks <- function(df) {
  cat("\n📊 FIRST-HALF HYDRATION BREAK — MOMENTUM ANALYSIS\n")
  cat(paste0(rep("=", 50), collapse = ""), "\n")

  has_break <- df %>% filter(!is.na(break_minute_1h))

  if (nrow(has_break) == 0) {
    cat("⚠️  No first-half hydration breaks recorded yet.\n")
    return(invisible(NULL))
  }

  has_break <- has_break %>%
    mutate(
      window_pre  = break_minute_1h,
      window_post = 45 - break_minute_1h,
      msi = momentum_shift(
        shots_home_pre1, shots_away_pre1,
        shots_home_post1, shots_away_post1,
        window_pre, window_post
      ),
      goals_pre_total  = goals_home_pre1  + goals_away_pre1,
      goals_post_total = goals_home_post1 + goals_away_post1,
      goal_rate_pre    = shot_rate(goals_pre_total,  window_pre),
      goal_rate_post   = shot_rate(goals_post_total, window_post)
    )

  cat(sprintf("Matches with 1st-half break: %d\n", nrow(has_break)))
  cat(sprintf("Avg break minute:            %.1f'\n", mean(has_break$break_minute_1h, na.rm = TRUE)))
  cat(sprintf("Avg WBGT at kickoff:         %.1f°C\n\n", mean(has_break$wbgt, na.rm = TRUE)))

  cat("Momentum Shift Index (positive = home team gained momentum):\n")
  print(summary(has_break$msi))

  cat(sprintf("\nGoal rate per minute BEFORE break: %.4f\n", mean(has_break$goal_rate_pre,  na.rm = TRUE)))
  cat(sprintf("Goal rate per minute AFTER  break: %.4f\n", mean(has_break$goal_rate_post, na.rm = TRUE)))

  invisible(has_break)
}

# -----------------------------------------------------------------------------
# SECOND-HALF BREAK ANALYSIS
# -----------------------------------------------------------------------------

analyze_second_half_breaks <- function(df) {
  cat("\n📊 SECOND-HALF HYDRATION BREAK — MOMENTUM ANALYSIS\n")
  cat(paste0(rep("=", 50), collapse = ""), "\n")

  has_break <- df %>% filter(!is.na(break_minute_2h))

  if (nrow(has_break) == 0) {
    cat("⚠️  No second-half hydration breaks recorded yet.\n")
    return(invisible(NULL))
  }

  has_break <- has_break %>%
    mutate(
      window_pre  = break_minute_2h - 45,
      window_post = 90 - break_minute_2h,
      msi = momentum_shift(
        shots_home_pre2, shots_away_pre2,
        shots_home_post2, shots_away_post2,
        window_pre, window_post
      ),
      goals_pre_total  = goals_home_pre2  + goals_away_pre2,
      goals_post_total = goals_home_post2 + goals_away_post2,
      goal_rate_pre    = shot_rate(goals_pre_total,  window_pre),
      goal_rate_post   = shot_rate(goals_post_total, window_post)
    )

  cat(sprintf("Matches with 2nd-half break: %d\n", nrow(has_break)))
  cat(sprintf("Avg break minute:            %.1f'\n", mean(has_break$break_minute_2h, na.rm = TRUE)))

  cat("Momentum Shift Index:\n")
  print(summary(has_break$msi))

  cat(sprintf("\nGoal rate per minute BEFORE break: %.4f\n", mean(has_break$goal_rate_pre,  na.rm = TRUE)))
  cat(sprintf("Goal rate per minute AFTER  break: %.4f\n", mean(has_break$goal_rate_post, na.rm = TRUE)))

  invisible(has_break)
}

# -----------------------------------------------------------------------------
# HEAT CORRELATION
# Does momentum shift correlate with how hot it was?
# -----------------------------------------------------------------------------

analyze_heat_correlation <- function(df) {
  cat("\n🌡️  HEAT vs MOMENTUM SHIFT CORRELATION\n")
  cat(paste0(rep("=", 50), collapse = ""), "\n")

  has_break <- df %>%
    filter(!is.na(break_minute_1h) | !is.na(break_minute_2h)) %>%
    mutate(
      msi_1h = momentum_shift(
        shots_home_pre1, shots_away_pre1,
        shots_home_post1, shots_away_post1,
        break_minute_1h, 45 - break_minute_1h
      ),
      msi_2h = momentum_shift(
        shots_home_pre2, shots_away_pre2,
        shots_home_post2, shots_away_post2,
        break_minute_2h - 45, 90 - break_minute_2h
      ),
      avg_msi = rowMeans(cbind(msi_1h, msi_2h), na.rm = TRUE)
    )

  if (nrow(has_break) < 3) {
    cat("⚠️  Not enough data yet for correlation analysis (need ≥ 3 matches).\n")
    return(invisible(NULL))
  }

  r <- cor(has_break$wbgt, has_break$avg_msi, use = "complete.obs")
  cat(sprintf("Pearson r (WBGT vs avg MSI): %.3f\n", r))
  cat("(Positive r = hotter conditions → more momentum swing after break)\n")

  invisible(has_break)
}

# -----------------------------------------------------------------------------
# MAIN PIPELINE
# -----------------------------------------------------------------------------

run_hydration_analysis <- function() {
  cat("💧 HYDRATION BREAK MOMENTUM ANALYSIS — 2026 FIFA WORLD CUP\n")
  cat(paste0(rep("=", 50), collapse = ""), "\n")

  df <- load_hydration_data()
  cat(sprintf("✅ Loaded %d matches.\n", nrow(df)))

  analyze_first_half_breaks(df)
  analyze_second_half_breaks(df)
  analyze_heat_correlation(df)

  cat("\n✅ Analysis complete.\n")
}

run_hydration_analysis()
