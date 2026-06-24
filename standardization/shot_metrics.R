# =============================================================================
# standardization/shot_metrics.R
#
# Shared computation functions for shot-based momentum analysis.
# Requires config.R (for HYDRATION$break_*h_default).
# Used by: hydration_momentum.R, 03_compare_2022_2026.R,
#          05_matchday1_graphic.R, 06_matchday1_metrics_graphic.R,
#          07_quality_effect.R
# =============================================================================

if (!exists("HYDRATION")) source(here("config.R"))

DEFAULT_BREAK_1H <- HYDRATION$break_1h_default
DEFAULT_BREAK_2H <- HYDRATION$break_2h_default

# Per-minute rate; returns NA when window is zero
rate <- function(count, minutes) ifelse(minutes > 0, count / minutes, NA_real_)

# -----------------------------------------------------------------------------
# compute_shot_metrics()
#
# Adds 24 pre/post shot-count columns to df_schedule (shots, SOT, xG for home
# and away in each half). Keyed by id_col in df_schedule, which must match
# match_id in both df_shots and df_fixtures.
#
# df_schedule : data frame with id_col, break_minute_1h, break_minute_2h
# df_shots    : data frame with match_id, team_id, minute, is_on_target, expected_goals
# df_fixtures : data frame with match_id, home_team_id, away_team_id
# id_col      : column in df_schedule that joins to match_id in shots/fixtures
#               (default "match_id"; use "thestatsapi_id" for hydration_momentum.R)
# -----------------------------------------------------------------------------
compute_shot_metrics <- function(df_schedule, df_shots, df_fixtures,
                                  id_col = "match_id") {
  if (is.null(df_shots) || is.null(df_fixtures)) return(df_schedule)

  shot_cols <- c(
    "shots_home_pre1", "shots_away_pre1", "shots_home_post1", "shots_away_post1",
    "shots_home_pre2", "shots_away_pre2", "shots_home_post2", "shots_away_post2",
    "sot_home_pre1",   "sot_away_pre1",   "sot_home_post1",   "sot_away_post1",
    "sot_home_pre2",   "sot_away_pre2",   "sot_home_post2",   "sot_away_post2",
    "xg_home_pre1",    "xg_away_pre1",    "xg_home_post1",    "xg_away_post1",
    "xg_home_pre2",    "xg_away_pre2",    "xg_home_post2",    "xg_away_post2"
  )
  for (col in shot_cols) df_schedule[[col]] <- NA_real_

  for (i in seq_len(nrow(df_schedule))) {
    mid <- df_schedule[[id_col]][i]
    if (is.na(mid)) next

    fix <- df_fixtures[df_fixtures$match_id == mid, ]
    if (nrow(fix) == 0) next
    home_id <- fix$home_team_id[1]
    away_id <- fix$away_team_id[1]

    ms <- df_shots[df_shots$match_id == mid, ]
    if (nrow(ms) == 0) next

    ms$side <- ifelse(as.character(ms$team_id) == as.character(home_id), "home",
               ifelse(as.character(ms$team_id) == as.character(away_id), "away", NA))
    ms <- ms[!is.na(ms$side), ]

    n_shots <- function(sub) nrow(sub)
    n_sot   <- function(sub) sum(sub$is_on_target == TRUE, na.rm = TRUE)
    n_xg    <- function(sub) sum(as.numeric(sub$expected_goals), na.rm = TRUE)

    brk1 <- df_schedule$break_minute_1h[i]
    if (!is.na(brk1)) {
      ms1 <- ms[ms$minute <= 45, ]
      ph <- ms1[ms1$side == "home" & ms1$minute <  brk1, ]; pa <- ms1[ms1$side == "away" & ms1$minute <  brk1, ]
      qh <- ms1[ms1$side == "home" & ms1$minute >= brk1, ]; qa <- ms1[ms1$side == "away" & ms1$minute >= brk1, ]
      df_schedule$shots_home_pre1[i]  <- n_shots(ph); df_schedule$shots_away_pre1[i]  <- n_shots(pa)
      df_schedule$shots_home_post1[i] <- n_shots(qh); df_schedule$shots_away_post1[i] <- n_shots(qa)
      df_schedule$sot_home_pre1[i]    <- n_sot(ph);   df_schedule$sot_away_pre1[i]    <- n_sot(pa)
      df_schedule$sot_home_post1[i]   <- n_sot(qh);   df_schedule$sot_away_post1[i]   <- n_sot(qa)
      df_schedule$xg_home_pre1[i]     <- n_xg(ph);    df_schedule$xg_away_pre1[i]     <- n_xg(pa)
      df_schedule$xg_home_post1[i]    <- n_xg(qh);    df_schedule$xg_away_post1[i]    <- n_xg(qa)
    }

    brk2 <- df_schedule$break_minute_2h[i]
    if (!is.na(brk2)) {
      ms2 <- ms[ms$minute > 45 & ms$minute <= 90, ]
      ph <- ms2[ms2$side == "home" & ms2$minute <  brk2, ]; pa <- ms2[ms2$side == "away" & ms2$minute <  brk2, ]
      qh <- ms2[ms2$side == "home" & ms2$minute >= brk2, ]; qa <- ms2[ms2$side == "away" & ms2$minute >= brk2, ]
      df_schedule$shots_home_pre2[i]  <- n_shots(ph); df_schedule$shots_away_pre2[i]  <- n_shots(pa)
      df_schedule$shots_home_post2[i] <- n_shots(qh); df_schedule$shots_away_post2[i] <- n_shots(qa)
      df_schedule$sot_home_pre2[i]    <- n_sot(ph);   df_schedule$sot_away_pre2[i]    <- n_sot(pa)
      df_schedule$sot_home_post2[i]   <- n_sot(qh);   df_schedule$sot_away_post2[i]   <- n_sot(qa)
      df_schedule$xg_home_pre2[i]     <- n_xg(ph);    df_schedule$xg_away_pre2[i]     <- n_xg(pa)
      df_schedule$xg_home_post2[i]    <- n_xg(qh);    df_schedule$xg_away_post2[i]    <- n_xg(qa)
    }
  }
  df_schedule
}

# -----------------------------------------------------------------------------
# compute_deltas()
#
# Derives combined (home+away) per-minute rate deltas for one half from the
# columns added by compute_shot_metrics(). Returns a data frame with one row
# per match: delta_shot, delta_sot, delta_xg.
#
# df  : output of compute_shot_metrics()
# suf : "1" for first half, "2" for second half
# -----------------------------------------------------------------------------
compute_deltas <- function(df, suf) {
  brk      <- df[[paste0("break_minute_", if (suf == "1") "1h" else "2h")]]
  win_pre  <- if (suf == "1") brk       else brk - 45
  win_post <- if (suf == "1") 45 - brk  else 90 - brk

  data.frame(
    delta_shot = rate(df[[paste0("shots_home_post", suf)]] + df[[paste0("shots_away_post", suf)]], win_post) -
                 rate(df[[paste0("shots_home_pre",  suf)]] + df[[paste0("shots_away_pre",  suf)]], win_pre),
    delta_sot  = rate(df[[paste0("sot_home_post",   suf)]] + df[[paste0("sot_away_post",   suf)]], win_post) -
                 rate(df[[paste0("sot_home_pre",    suf)]] + df[[paste0("sot_away_pre",    suf)]], win_pre),
    delta_xg   = rate(df[[paste0("xg_home_post",    suf)]] + df[[paste0("xg_away_post",    suf)]], win_post) -
                 rate(df[[paste0("xg_home_pre",     suf)]] + df[[paste0("xg_away_pre",     suf)]], win_pre)
  )
}

# -----------------------------------------------------------------------------
# half_counts()
#
# Returns per-match pre/post rates for shots, SOT, and xG for one half.
# Used by graphic scripts that need one row per match rather than the wide
# format produced by compute_shot_metrics().
#
# df_schedule : data frame with match_id, home_team, away_team,
#               break_minute_1h / break_minute_2h
# df_shots    : data frame with match_id, team_id, minute, is_on_target,
#               expected_goals
# df_fixtures : data frame with match_id, home_team_id, away_team_id
# suf         : "1" or "2"
# -----------------------------------------------------------------------------
half_counts <- function(df_schedule, df_shots, df_fixtures, suf) {
  brk_col <- if (suf == "1") "break_minute_1h" else "break_minute_2h"

  rows <- lapply(seq_len(nrow(df_schedule)), function(i) {
    mid <- df_schedule$match_id[i]
    brk <- df_schedule[[brk_col]][i]
    if (is.na(brk)) return(NULL)

    fix <- df_fixtures[df_fixtures$match_id == mid, ]
    if (nrow(fix) == 0) return(NULL)
    ms  <- df_shots[df_shots$match_id == mid, ]
    if (nrow(ms) == 0) return(NULL)

    home_id <- fix$home_team_id[1]
    away_id <- fix$away_team_id[1]

    ms$side <- ifelse(as.character(ms$team_id) == as.character(home_id), "home",
               ifelse(as.character(ms$team_id) == as.character(away_id), "away", NA))
    ms <- ms[!is.na(ms$side), ]

    if (suf == "1") ms <- ms[ms$minute <= 45, ]
    else            ms <- ms[ms$minute > 45 & ms$minute <= 90, ]

    pre  <- ms[ms$minute <  brk, ]
    post <- ms[ms$minute >= brk, ]

    win_pre  <- if (suf == "1") brk else brk - 45
    win_post <- if (suf == "1") 45 - brk else 90 - brk

    data.frame(
      match_id   = as.character(mid),
      home_team  = df_schedule$home_team[i],
      away_team  = df_schedule$away_team[i],
      half       = if (suf == "1") "1st Half" else "2nd Half",
      shots_pre  = nrow(pre)  / win_pre,
      shots_post = nrow(post) / win_post,
      sot_pre    = sum(pre$is_on_target  == TRUE, na.rm = TRUE) / win_pre,
      sot_post   = sum(post$is_on_target == TRUE, na.rm = TRUE) / win_post,
      xg_pre     = sum(as.numeric(pre$expected_goals),  na.rm = TRUE) / win_pre,
      xg_post    = sum(as.numeric(post$expected_goals), na.rm = TRUE) / win_post,
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(Filter(Negate(is.null), rows))
}
