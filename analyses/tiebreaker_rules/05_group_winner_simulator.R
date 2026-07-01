library(dplyr)
library(here)

source(here("config.R"))

# =============================================================================
# 05_group_winner_simulator.R
#
# Identifies teams that have mathematically clinched 1st place in their group
# before matchday 3 kicks off — under both H2H (2026 actual) and GD (classic)
# tiebreaker rulesets.
#
# A team is "Clinched" if they finish 1st in EVERY possible combination of
# matchday 3 outcomes. A team is "Not Clinched" if at least one matchday 3
# scenario results in them NOT being 1st — those blocking scenarios are
# recorded so their probability can be computed in 06_clinching_likelihood.R.
#
# Epsilon convention (conservative for clinching):
#   In the elimination simulator, epsilon is added to the TARGET team to give
#   it a best-case tiebreaker advantage (testing if it CAN survive).
#   Here, epsilon is added to CHALLENGERS to give them worst-case advantage
#   against the target (testing if ANY opponent could BEAT the target in a
#   tiebreaker). A team is only "Clinched" if it leads even in the worst case.
#
# Requires: data/fixtures/clean_fixtures.csv
# Output:   data/standings/clinching_scenarios.csv
# =============================================================================

get_outcome_desc <- function(match_row, outcome) {
  h <- match_row$home_team_name_en
  a <- match_row$away_team_name_en
  if (outcome == 0) return(sprintf("'%s' beats '%s'", h, a))
  if (outcome == 1) return(sprintf("'%s' draws '%s'", h, a))
  if (outcome == 2) return(sprintf("'%s' loses to '%s'", h, a))
}

apply_outcome <- function(matches, match_indices, outcomes) {
  for (k in seq_along(match_indices)) {
    idx <- match_indices[k]
    o   <- outcomes[k]
    if (o == 0) { matches$home_score[idx] <- 1; matches$away_score[idx] <- 0 }
    if (o == 1) { matches$home_score[idx] <- 0; matches$away_score[idx] <- 0 }
    if (o == 2) { matches$home_score[idx] <- 0; matches$away_score[idx] <- 1 }
  }
  matches
}

# Returns TRUE if team_target is 1st under H2H rules (conservative: challengers get epsilon).
rank_first_h2h <- function(team_target, matches) {
  teams <- unique(c(matches$home_team_name_en, matches$away_team_name_en))
  pts   <- setNames(rep(0L, length(teams)), teams)

  for (i in seq_len(nrow(matches))) {
    h  <- matches$home_team_name_en[i]
    a  <- matches$away_team_name_en[i]
    hg <- suppressWarnings(as.numeric(matches$home_score[i]))
    ag <- suppressWarnings(as.numeric(matches$away_score[i]))
    if (is.na(hg) || is.na(ag)) next
    if (hg > ag) { pts[h] <- pts[h] + 3L }
    else if (hg < ag) { pts[a] <- pts[a] + 3L }
    else { pts[h] <- pts[h] + 1L; pts[a] <- pts[a] + 1L }
  }

  t_pts   <- pts[team_target]
  others  <- pts[names(pts) != team_target]
  if (any(others > t_pts)) return(FALSE)   # challenger ahead on points
  if (all(others < t_pts)) return(TRUE)    # clear leader on points

  # Tied on points — H2H sub-table
  tied <- names(pts)[pts == t_pts]
  h2h  <- setNames(rep(0L, length(tied)), tied)

  for (i in seq_len(nrow(matches))) {
    h  <- matches$home_team_name_en[i]
    a  <- matches$away_team_name_en[i]
    hg <- suppressWarnings(as.numeric(matches$home_score[i]))
    ag <- suppressWarnings(as.numeric(matches$away_score[i]))
    if (is.na(hg) || is.na(ag)) next
    if (h %in% tied && a %in% tied) {
      if (hg > ag) { h2h[h] <- h2h[h] + 3L }
      else if (hg < ag) { h2h[a] <- h2h[a] + 3L }
      else { h2h[h] <- h2h[h] + 1L; h2h[a] <- h2h[a] + 1L }
    }
  }

  t_h2h        <- h2h[team_target]
  challenger_h2h <- h2h[names(h2h) != team_target] + ELO_MODEL$epsilon
  if (any(challenger_h2h >= t_h2h)) return(FALSE)   # challenger wins tiebreaker
  return(TRUE)
}

# Returns TRUE if team_target is 1st under GD rules (conservative: challengers get epsilon).
rank_first_gd <- function(team_target, matches) {
  teams <- unique(c(matches$home_team_name_en, matches$away_team_name_en))
  pts   <- setNames(rep(0L, length(teams)), teams)
  gd    <- setNames(rep(0L, length(teams)), teams)

  for (i in seq_len(nrow(matches))) {
    h  <- matches$home_team_name_en[i]
    a  <- matches$away_team_name_en[i]
    hg <- suppressWarnings(as.numeric(matches$home_score[i]))
    ag <- suppressWarnings(as.numeric(matches$away_score[i]))
    if (is.na(hg) || is.na(ag)) next
    if (hg > ag) { pts[h] <- pts[h] + 3L }
    else if (hg < ag) { pts[a] <- pts[a] + 3L }
    else { pts[h] <- pts[h] + 1L; pts[a] <- pts[a] + 1L }
    gd[h] <- gd[h] + (hg - ag)
    gd[a] <- gd[a] + (ag - hg)
  }

  t_pts <- pts[team_target]
  t_gd  <- gd[team_target]

  for (o in names(pts)[names(pts) != team_target]) {
    if (pts[o] > t_pts) return(FALSE)
    if (pts[o] == t_pts && gd[o] + ELO_MODEL$epsilon >= t_gd) return(FALSE)
  }
  return(TRUE)
}

# =============================================================================
# Main simulation
# =============================================================================

run_clinching_sim <- function() {
  cat("Scanning for clinched group winners...\n\n")

  df           <- read.csv(here("data", "fixtures", "clean_fixtures.csv"), stringsAsFactors = FALSE)
  group_matches <- df %>% filter(type == "group")

  export_rows <- list()

  for (g in sort(unique(group_matches$group))) {
    g_m          <- group_matches %>% filter(group == g) %>% arrange(matchday)
    unplayed_idx <- which(g_m$finished == "FALSE" | g_m$finished == FALSE)
    teams        <- unique(c(g_m$home_team_name_en, g_m$away_team_name_en))

    cat(sprintf("Group %s — %d unplayed match(es)\n", g, length(unplayed_idx)))

    # -------------------------------------------------------------------------
    # All matches complete: determine the actual group winner
    # -------------------------------------------------------------------------
    if (length(unplayed_idx) == 0) {
      for (t in teams) {
        is_1st_h2h <- rank_first_h2h(t, g_m)
        is_1st_gd  <- rank_first_gd(t, g_m)
        for (rs in c("H2H", "GD")) {
          is_1st <- if (rs == "H2H") is_1st_h2h else is_1st_gd
          if (is_1st) {
            cat(sprintf("  🏆 [%s] Won group (%s rules)\n", t, rs))
            export_rows[[length(export_rows) + 1]] <- data.frame(
              Team=t, Group=g, RuleSet=rs, Status="Group Winner",
              R3_M1_Home=NA_character_, R3_M1_Away=NA_character_,
              R3_M1_Code=NA_real_,     R3_M1_Desc=NA_character_,
              R3_M2_Home=NA_character_, R3_M2_Away=NA_character_,
              R3_M2_Code=NA_real_,     R3_M2_Desc=NA_character_,
              stringsAsFactors=FALSE
            )
          }
        }
      }
      next
    }

    # -------------------------------------------------------------------------
    # Matchday 2 still in progress (> 2 unplayed): too early to clinch
    # -------------------------------------------------------------------------
    if (length(unplayed_idx) > 2) {
      cat("  (Matchday 2 in progress — skipping)\n")
      next
    }

    # -------------------------------------------------------------------------
    # Exactly 1 or 2 unplayed = matchday 3 pending: enumerate all combos
    # -------------------------------------------------------------------------
    n_rem     <- length(unplayed_idx)
    sim_combos <- if (n_rem == 1) matrix(c(0L, 1L, 2L), ncol = 1) else
                  as.matrix(expand.grid(m1 = 0:2, m2 = 0:2))

    m1_row <- g_m[unplayed_idx[1], ]
    m2_row <- if (n_rem == 2) g_m[unplayed_idx[2], ] else NULL

    for (t in teams) {
      for (rs in c("H2H", "GD")) {
        rank_fn <- if (rs == "H2H") rank_first_h2h else rank_first_gd
        blocking <- list()

        for (i in seq_len(nrow(sim_combos))) {
          combo <- as.integer(sim_combos[i, ])
          sim   <- apply_outcome(g_m, unplayed_idx, combo)
          if (!rank_fn(t, sim)) blocking[[length(blocking) + 1]] <- combo
        }

        if (length(blocking) == 0) {
          cat(sprintf("  ✅ [%s] CLINCHED 1st (%s rules)\n", t, rs))
          export_rows[[length(export_rows) + 1]] <- data.frame(
            Team=t, Group=g, RuleSet=rs, Status="Clinched",
            R3_M1_Home=NA_character_, R3_M1_Away=NA_character_,
            R3_M1_Code=NA_real_,     R3_M1_Desc=NA_character_,
            R3_M2_Home=NA_character_, R3_M2_Away=NA_character_,
            R3_M2_Code=NA_real_,     R3_M2_Desc=NA_character_,
            stringsAsFactors=FALSE
          )
        } else {
          for (scen in blocking) {
            desc1 <- get_outcome_desc(m1_row, scen[1])
            if (!is.null(m2_row)) {
              desc2 <- get_outcome_desc(m2_row, scen[2])
              export_rows[[length(export_rows) + 1]] <- data.frame(
                Team=t, Group=g, RuleSet=rs, Status="Not Clinched",
                R3_M1_Home=m1_row$home_team_name_en, R3_M1_Away=m1_row$away_team_name_en,
                R3_M1_Code=scen[1], R3_M1_Desc=desc1,
                R3_M2_Home=m2_row$home_team_name_en, R3_M2_Away=m2_row$away_team_name_en,
                R3_M2_Code=scen[2], R3_M2_Desc=desc2,
                stringsAsFactors=FALSE
              )
            } else {
              export_rows[[length(export_rows) + 1]] <- data.frame(
                Team=t, Group=g, RuleSet=rs, Status="Not Clinched",
                R3_M1_Home=m1_row$home_team_name_en, R3_M1_Away=m1_row$away_team_name_en,
                R3_M1_Code=scen[1], R3_M1_Desc=desc1,
                R3_M2_Home=NA_character_, R3_M2_Away=NA_character_,
                R3_M2_Code=NA_real_,     R3_M2_Desc=NA_character_,
                stringsAsFactors=FALSE
              )
            }
          }
        }
      }
    }
    cat("\n")
  }

  export_df <- bind_rows(export_rows)
  write.csv(export_df, here("data", "standings", "clinching_scenarios.csv"), row.names = FALSE)

  # Summary
  clinched_h2h <- export_df %>% filter(Status %in% c("Clinched","Group Winner") & RuleSet == "H2H") %>%
                  pull(Team) %>% unique()
  clinched_gd  <- export_df %>% filter(Status %in% c("Clinched","Group Winner") & RuleSet == "GD")  %>%
                  pull(Team) %>% unique()

  cat("\n=======================================================\n")
  cat("GROUP WINNER CLINCHING SUMMARY\n")
  cat("=======================================================\n")
  cat(sprintf("Clinched/Won (H2H rules): %d — %s\n",
              length(clinched_h2h), paste(sort(clinched_h2h), collapse=", ")))
  cat(sprintf("Clinched/Won (GD rules):  %d — %s\n",
              length(clinched_gd),  paste(sort(clinched_gd),  collapse=", ")))

  h2h_only <- setdiff(clinched_h2h, clinched_gd)
  gd_only  <- setdiff(clinched_gd,  clinched_h2h)

  if (length(h2h_only) > 0 || length(gd_only) > 0) {
    cat("\nCONTROVERSY — ruleset divergence:\n")
    if (length(h2h_only) > 0) cat(sprintf("  Clinched H2H only (not GD): %s\n", paste(h2h_only, collapse=", ")))
    if (length(gd_only)  > 0) cat(sprintf("  Clinched GD only (not H2H): %s\n", paste(gd_only,  collapse=", ")))
  } else {
    cat("\nNo controversy — both rulesets agree on all clinched/won teams.\n")
  }

  cat(sprintf("\nSaved: data/standings/clinching_scenarios.csv (%d rows)\n", nrow(export_df)))
}

run_clinching_sim()
