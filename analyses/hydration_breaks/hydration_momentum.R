library(dplyr)
library(here)

source(here("config.R"))
source(here("standardization", "validation.R"))
source(here("standardization", "shot_metrics.R"))

# =============================================================================
# HYDRATION BREAK MOMENTUM ANALYSIS — 2026 FIFA WORLD CUP
# =============================================================================
# Tests whether FIFA-mandated hydration breaks measurably shift match momentum.
#
# PIPELINE (run in order):
#   fetch/fetch_fixtures.R                         -> data/clean_fixtures.csv
#   fetch/fetch_fixtures_thestatsapi.R             -> data/clean_fixtures_thestatsapi.csv
#   fetch/fetch_shotmap_thestatsapi.R              -> data/shotmap_thestatsapi.csv
#   analyses/hydration_breaks/01_parse_goal_events.R -> data/goal_events.csv
#   analyses/hydration_breaks/02_build_break_schedule.R -> data/hydration_breaks.csv
#   This script                                    -> console output
# =============================================================================

# -----------------------------------------------------------------------------
# DATA LOADING
# -----------------------------------------------------------------------------

load_breaks <- function() {
  path <- here("data", "events", "hydration_breaks.csv")
  require_file(path, "run analyses/hydration_breaks/02_build_break_schedule.R first")
  df <- read.csv(path, stringsAsFactors = FALSE)
  require_cols(df, c("match_id", "thestatsapi_id", "break_minute_1h", "break_minute_2h"), "hydration_breaks.csv")
  cat(sprintf("✅ Break schedule: %d matches loaded.\n", nrow(df)))
  df
}

load_goals <- function() {
  path <- here("data", "events", "goal_events.csv")
  if (!file.exists(path)) {
    cat("⚠️  goal_events.csv not found — run 01_parse_goal_events.R. Goal metrics skipped.\n")
    return(NULL)
  }
  df <- read.csv(path, stringsAsFactors = FALSE)
  cat(sprintf("✅ Goal events: %d records loaded.\n", nrow(df)))
  df
}

load_shots <- function() {
  path <- here("data", "shotmaps", "shotmap_thestatsapi.csv")
  if (!file.exists(path)) {
    cat("⚠️  shotmap_thestatsapi.csv not found — run fetch/fetch_shotmap_thestatsapi.R. Shot metrics skipped.\n")
    return(NULL)
  }
  df <- read.csv(path, stringsAsFactors = FALSE)
  require_cols(df, c("match_id", "team_id", "minute", "is_on_target", "expected_goals"), "shotmap_thestatsapi.csv")
  cat(sprintf("✅ Shot data: %d shots across %d matches loaded.\n",
              nrow(df), length(unique(df$match_id))))
  df
}

load_fixtures <- function() {
  path <- here("data", "fixtures", "clean_fixtures_thestatsapi.csv")
  if (!file.exists(path)) return(NULL)
  read.csv(path, stringsAsFactors = FALSE)
}

# -----------------------------------------------------------------------------
# TIER 1 — GOAL METRICS (from goal_events.csv, keyed by worldcup26 match_id)
# -----------------------------------------------------------------------------

compute_game_state <- function(df_breaks, df_goals) {
  if (is.null(df_goals)) return(df_breaks)

  df_breaks$score_diff_at_break_1h <- NA_integer_
  df_breaks$game_state_1h          <- NA_character_
  df_breaks$score_diff_at_break_2h <- NA_integer_
  df_breaks$game_state_2h          <- NA_character_

  for (i in seq_len(nrow(df_breaks))) {
    mg <- df_goals %>% filter(match_id == df_breaks$match_id[i])

    brk1 <- df_breaks$break_minute_1h[i]
    if (!is.na(brk1)) {
      diff <- sum(mg$scoring_team == "home" & mg$minute_base < brk1) -
              sum(mg$scoring_team == "away" & mg$minute_base < brk1)
      df_breaks$score_diff_at_break_1h[i] <- diff
      df_breaks$game_state_1h[i] <- ifelse(diff > 0, "leading", ifelse(diff < 0, "trailing", "level"))
    }

    brk2 <- df_breaks$break_minute_2h[i]
    if (!is.na(brk2)) {
      diff <- sum(mg$scoring_team == "home" & mg$minute_base < brk2) -
              sum(mg$scoring_team == "away" & mg$minute_base < brk2)
      df_breaks$score_diff_at_break_2h[i] <- diff
      df_breaks$game_state_2h[i] <- ifelse(diff > 0, "leading", ifelse(diff < 0, "trailing", "level"))
    }
  }
  df_breaks
}

compute_goal_metrics <- function(df_breaks, df_goals) {
  if (is.null(df_goals)) return(df_breaks)

  goal_cols <- c("goals_home_pre1", "goals_away_pre1", "goals_home_post1", "goals_away_post1",
                 "goals_home_pre2", "goals_away_pre2", "goals_home_post2", "goals_away_post2")
  for (col in goal_cols) df_breaks[[col]] <- NA_integer_

  for (i in seq_len(nrow(df_breaks))) {
    mg <- df_goals %>% filter(match_id == df_breaks$match_id[i])

    brk1 <- df_breaks$break_minute_1h[i]
    if (!is.na(brk1)) {
      g1 <- mg %>% filter(minute_base <= 45)
      df_breaks$goals_home_pre1[i]  <- sum(g1$scoring_team == "home" & g1$minute_base <  brk1)
      df_breaks$goals_away_pre1[i]  <- sum(g1$scoring_team == "away" & g1$minute_base <  brk1)
      df_breaks$goals_home_post1[i] <- sum(g1$scoring_team == "home" & g1$minute_base >= brk1)
      df_breaks$goals_away_post1[i] <- sum(g1$scoring_team == "away" & g1$minute_base >= brk1)
    }

    brk2 <- df_breaks$break_minute_2h[i]
    if (!is.na(brk2)) {
      g2 <- mg %>% filter(minute_base > 45, minute_base <= 90)
      df_breaks$goals_home_pre2[i]  <- sum(g2$scoring_team == "home" & g2$minute_base <  brk2)
      df_breaks$goals_away_pre2[i]  <- sum(g2$scoring_team == "away" & g2$minute_base <  brk2)
      df_breaks$goals_home_post2[i] <- sum(g2$scoring_team == "home" & g2$minute_base >= brk2)
      df_breaks$goals_away_post2[i] <- sum(g2$scoring_team == "away" & g2$minute_base >= brk2)
    }
  }
  df_breaks
}

# -----------------------------------------------------------------------------
# STATISTICAL TEST
# -----------------------------------------------------------------------------

analyze_wilcoxon <- function(pre_vec, post_vec, metric_name) {
  diffs <- post_vec - pre_vec
  diffs <- diffs[!is.na(diffs)]

  if (length(diffs) < 3) {
    cat(sprintf("   [%s] n=%d — need >=3 for test\n", metric_name, length(diffs)))
    return(invisible(NULL))
  }

  res <- wilcox.test(diffs, mu = 0, alternative = "two.sided", exact = FALSE)
  cat(sprintf("   [%s] median shift %+.4f | p=%.4f%s\n",
              metric_name,
              median(diffs),
              res$p.value,
              ifelse(res$p.value < 0.05, " *", "")))
}

# -----------------------------------------------------------------------------
# HALF-BREAK ANALYSIS (unified for 1H and 2H)
# -----------------------------------------------------------------------------

analyze_half <- function(df, half) {
  brk_col    <- if (half == 1) "break_minute_1h"    else "break_minute_2h"
  state_col  <- if (half == 1) "game_state_1h"      else "game_state_2h"
  label      <- if (half == 1) "FIRST-HALF"         else "SECOND-HALF"
  win_pre_fn <- if (half == 1) function(b) b        else function(b) b - 45
  win_post_fn<- if (half == 1) function(b) 45 - b   else function(b) 90 - b
  suf        <- if (half == 1) "1"                  else "2"

  cat(sprintf("\n%s HYDRATION BREAK\n%s\n", label, strrep("=", 50)))

  d <- df[!is.na(df[[brk_col]]), ]
  if (nrow(d) == 0) { cat("No break data yet.\n"); return(invisible(NULL)) }

  d$win_pre  <- win_pre_fn(d[[brk_col]])
  d$win_post <- win_post_fn(d[[brk_col]])

  cat(sprintf("Matches: %d | Avg break: %.1f'\n\n", nrow(d), mean(d[[brk_col]])))

  # Game state
  if (state_col %in% names(d)) {
    cat("Game state at break:\n")
    gs <- table(d[[state_col]])
    for (s in names(gs)) cat(sprintf("   %-10s %d\n", s, gs[[s]]))
    cat("\n")
  }

  # --- Tier 1: Goal rates ---
  gph_pre  <- paste0("goals_home_pre",  suf); gpa_pre  <- paste0("goals_away_pre",  suf)
  gph_post <- paste0("goals_home_post", suf); gpa_post <- paste0("goals_away_post", suf)

  if (all(c(gph_pre, gpa_pre, gph_post, gpa_post) %in% names(d))) {
    d$goals_total_pre  <- d[[gph_pre]]  + d[[gpa_pre]]
    d$goals_total_post <- d[[gph_post]] + d[[gpa_post]]
    d$goal_rate_pre    <- rate(d$goals_total_pre,  d$win_pre)
    d$goal_rate_post   <- rate(d$goals_total_post, d$win_post)

    cat(sprintf("Goal rate/min — before: %.4f | after: %.4f\n",
                mean(d$goal_rate_pre, na.rm = TRUE),
                mean(d$goal_rate_post, na.rm = TRUE)))
    analyze_wilcoxon(d$goal_rate_pre, d$goal_rate_post, "Goal rate")
  }

  # --- Tier 2: Shot / SOT / xG rates ---
  sh_pre  <- paste0("shots_home_pre",  suf); sa_pre  <- paste0("shots_away_pre",  suf)
  sh_post <- paste0("shots_home_post", suf); sa_post <- paste0("shots_away_post", suf)

  has_shots <- all(c(sh_pre, sa_pre, sh_post, sa_post) %in% names(d)) &&
               !all(is.na(d[[sh_pre]]))

  if (has_shots) {
    cat("\n")

    # Shot rate
    d$shot_rate_pre  <- rate(d[[sh_pre]]  + d[[sa_pre]],  d$win_pre)
    d$shot_rate_post <- rate(d[[sh_post]] + d[[sa_post]], d$win_post)
    cat(sprintf("Shot rate/min  — before: %.4f | after: %.4f\n",
                mean(d$shot_rate_pre, na.rm = TRUE),
                mean(d$shot_rate_post, na.rm = TRUE)))
    analyze_wilcoxon(d$shot_rate_pre, d$shot_rate_post, "Shot rate")

    # SOT rate
    soths_pre  <- paste0("sot_home_pre",  suf); sotas_pre  <- paste0("sot_away_pre",  suf)
    soths_post <- paste0("sot_home_post", suf); sotas_post <- paste0("sot_away_post", suf)
    if (all(c(soths_pre, sotas_pre, soths_post, sotas_post) %in% names(d))) {
      d$sot_rate_pre  <- rate(d[[soths_pre]]  + d[[sotas_pre]],  d$win_pre)
      d$sot_rate_post <- rate(d[[soths_post]] + d[[sotas_post]], d$win_post)
      cat(sprintf("SOT rate/min   — before: %.4f | after: %.4f\n",
                  mean(d$sot_rate_pre, na.rm = TRUE),
                  mean(d$sot_rate_post, na.rm = TRUE)))
      analyze_wilcoxon(d$sot_rate_pre, d$sot_rate_post, "SOT rate")
    }

    # xG rate
    xgh_pre  <- paste0("xg_home_pre",  suf); xga_pre  <- paste0("xg_away_pre",  suf)
    xgh_post <- paste0("xg_home_post", suf); xga_post <- paste0("xg_away_post", suf)
    if (all(c(xgh_pre, xga_pre, xgh_post, xga_post) %in% names(d)) &&
        !all(is.na(d[[xgh_pre]]))) {
      d$xg_rate_pre  <- rate(d[[xgh_pre]]  + d[[xga_pre]],  d$win_pre)
      d$xg_rate_post <- rate(d[[xgh_post]] + d[[xga_post]], d$win_post)
      cat(sprintf("xG rate/min    — before: %.4f | after: %.4f\n",
                  mean(d$xg_rate_pre, na.rm = TRUE),
                  mean(d$xg_rate_post, na.rm = TRUE)))
      analyze_wilcoxon(d$xg_rate_pre, d$xg_rate_post, "xG rate")

      # xG per shot (shot quality)
      shots_pre_total  <- d[[sh_pre]]  + d[[sa_pre]]
      shots_post_total <- d[[sh_post]] + d[[sa_post]]
      xg_pre_total     <- d[[xgh_pre]] + d[[xga_pre]]
      xg_post_total    <- d[[xgh_post]]+ d[[xga_post]]
      d$xg_per_shot_pre  <- ifelse(shots_pre_total  > 0, xg_pre_total  / shots_pre_total,  NA)
      d$xg_per_shot_post <- ifelse(shots_post_total > 0, xg_post_total / shots_post_total, NA)
      cat(sprintf("xG per shot    — before: %.4f | after: %.4f\n",
                  mean(d$xg_per_shot_pre, na.rm = TRUE),
                  mean(d$xg_per_shot_post, na.rm = TRUE)))
      analyze_wilcoxon(d$xg_per_shot_pre, d$xg_per_shot_post, "xG/shot")

      # Momentum Shift Index
      pre_balance  <- rate(d[[sh_pre]],  d$win_pre)  - rate(d[[sa_pre]],  d$win_pre)
      post_balance <- rate(d[[sh_post]], d$win_post) - rate(d[[sa_post]], d$win_post)
      msi <- post_balance - pre_balance
      cat(sprintf("\nMomentum Shift Index (shots, +ve = home gained):\n"))
      cat(sprintf("   Median: %+.4f | Mean: %+.4f\n",
                  median(msi, na.rm = TRUE), mean(msi, na.rm = TRUE)))
      analyze_wilcoxon(pre_balance, post_balance, "Shot MSI")
    }
  }

  invisible(d)
}

# -----------------------------------------------------------------------------
# MAIN PIPELINE
# -----------------------------------------------------------------------------

run_hydration_analysis <- function() {
  cat("HYDRATION BREAK MOMENTUM ANALYSIS — 2026 FIFA WORLD CUP\n")
  cat(strrep("=", 55), "\n\n")

  df_breaks  <- load_breaks()
  df_goals   <- load_goals()
  df_shots   <- load_shots()
  df_fixtures <- load_fixtures()
  cat("\n")

  df_breaks <- compute_game_state(df_breaks, df_goals)
  df_breaks <- compute_goal_metrics(df_breaks, df_goals)
  df_breaks <- compute_shot_metrics(df_breaks, df_shots, df_fixtures, id_col = "thestatsapi_id")

  analyze_half(df_breaks, half = 1)
  analyze_half(df_breaks, half = 2)

  cat(sprintf("\n%s\nAnalysis complete — %d matches analysed.\n",
              strrep("=", 55), nrow(df_breaks)))
}

run_hydration_analysis()
