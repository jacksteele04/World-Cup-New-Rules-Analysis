library(dplyr)
library(here)

source(here("config.R"))
source(here("standardization", "elo.R"))
source(here("standardization", "team_names.R"))

# =============================================================================
# 06_clinching_likelihood.R
#
# Computes, for each group, the probability that each team clinches 1st place
# under H2H (2026 actual) and GD (classic) rules, using the blocking scenarios
# produced by 05_group_winner_simulator.R.
#
# For already-confirmed group winners (all matchday 3 complete): 100%.
# For clinched teams (all matchday 3 outcomes → 1st): 100%.
# For not-yet-clinched teams: P(clinch) = 1 - sum(P(blocking scenarios)).
#
# Probabilities are computed via the same Elo/Poisson model as
# 02_fetch_predictions.R, looking up matchday 3 pairings from
# clean_fixtures.csv and Elo ratings from the ratings cache.
#
# Requires:
#   data/standings/clinching_scenarios.csv  (05_group_winner_simulator.R)
#   data/fixtures/clean_fixtures.csv
#   data/ratings/elo_ratings_cache.csv
# =============================================================================

# Poisson win/draw/loss probabilities from Elo ratings
calc_match_probs <- function(h_name, a_name, elo_db) {
  h_elo <- get_elo(h_name, elo_db)
  a_elo <- get_elo(a_name, elo_db)
  if (is.na(h_elo)) h_elo <- ELO_MODEL$default_rating
  if (is.na(a_elo)) a_elo <- ELO_MODEL$default_rating

  host_nations <- TOURNAMENT$host_nations
  h_norm <- elo_name(h_name)
  a_norm <- elo_name(a_name)
  if (h_norm %in% host_nations && !a_norm %in% host_nations) h_elo <- h_elo + ELO_MODEL$host_bonus
  if (a_norm %in% host_nations && !h_norm %in% host_nations) a_elo <- a_elo + ELO_MODEL$host_bonus

  dr   <- h_elo - a_elo
  h_xg <- ELO_MODEL$xg_multiplier * 10 ^ ( dr / ELO_MODEL$elo_divisor)
  a_xg <- ELO_MODEL$xg_multiplier * 10 ^ (-dr / ELO_MODEL$elo_divisor)

  g    <- 0:ELO_MODEL$max_goals
  mat  <- outer(dpois(g, h_xg), dpois(g, a_xg))
  p_h  <- sum(mat[lower.tri(mat)])
  p_d  <- sum(diag(mat))
  p_a  <- sum(mat[upper.tri(mat)])
  tot  <- p_h + p_d + p_a

  list(home = round(p_h / tot, 3),
       draw = round(p_d / tot, 3),
       away = round(p_a / tot, 3))
}

get_prob <- function(h_team, a_team, outcome_code, prob_cache) {
  key <- paste(norm_name(h_team), norm_name(a_team))
  if (!key %in% names(prob_cache)) {
    key_rev <- paste(norm_name(a_team), norm_name(h_team))
    if (key_rev %in% names(prob_cache)) {
      p <- prob_cache[[key_rev]]
      return(if (outcome_code == 0) p$away else if (outcome_code == 1) p$draw else p$home)
    }
    return(1/3)
  }
  p <- prob_cache[[key]]
  if (outcome_code == 0) return(p$home)
  if (outcome_code == 1) return(p$draw)
  p$away
}

# =============================================================================
# Main
# =============================================================================

analyze_clinching_likelihood <- function() {
  scen_file <- here("data", "standings", "clinching_scenarios.csv")
  fix_file  <- here("data", "fixtures", "clean_fixtures.csv")

  if (!file.exists(scen_file)) stop("Run 05_group_winner_simulator.R first.")
  if (!file.exists(fix_file))  stop("Run fetch/fetch_fixtures.R first.")

  scen <- read.csv(scen_file, stringsAsFactors = FALSE)
  fx   <- read.csv(fix_file,  stringsAsFactors = FALSE)

  # Build Elo probability cache for matchday 3 pairs
  md3_pairs <- fx %>%
    filter(type == "group", matchday == 3) %>%
    select(home_team_name_en, away_team_name_en) %>%
    distinct()

  elo_db     <- get_elo_database()
  prob_cache <- list()
  for (i in seq_len(nrow(md3_pairs))) {
    h   <- md3_pairs$home_team_name_en[i]
    a   <- md3_pairs$away_team_name_en[i]
    key <- paste(norm_name(h), norm_name(a))
    if (!key %in% names(prob_cache)) {
      prob_cache[[key]] <- calc_match_probs(h, a, elo_db)
    }
  }

  cat("GROUP WINNER CLINCHING LIKELIHOOD\n")
  cat(strrep("=", 55), "\n\n")

  controversy_teams <- character(0)

  for (g in sort(unique(scen$Group))) {
    g_scen <- scen %>% filter(Group == g)
    cat(sprintf("Group %s\n%s\n", g, strrep("-", 40)))

    teams <- unique(g_scen$Team)
    for (t in teams) {
      t_scen <- g_scen %>% filter(Team == t)

      results <- list()
      for (rs in c("H2H", "GD")) {
        rs_scen <- t_scen %>% filter(RuleSet == rs)

        if (nrow(rs_scen) == 0) {
          results[[rs]] <- list(pct = NA_real_, status = "no data")
          next
        }

        # Already confirmed winner or clinched
        if (all(rs_scen$Status %in% c("Group Winner", "Clinched"))) {
          results[[rs]] <- list(pct = 100.0, status = rs_scen$Status[1])
          next
        }

        # Not clinched — sum blocking scenario probabilities
        blocking <- rs_scen %>% filter(Status == "Not Clinched")
        p_blocked <- 0
        for (i in seq_len(nrow(blocking))) {
          row  <- blocking[i, ]
          p1   <- get_prob(row$R3_M1_Home, row$R3_M1_Away, row$R3_M1_Code, prob_cache)
          p2   <- if (!is.na(row$R3_M2_Home))
                    get_prob(row$R3_M2_Home, row$R3_M2_Away, row$R3_M2_Code, prob_cache)
                  else 1.0
          p_blocked <- p_blocked + p1 * p2
        }
        p_clinch <- max(0, 1 - p_blocked)
        results[[rs]] <- list(pct = round(p_clinch * 100, 1), status = "Not Clinched")
      }

      # Print
      h2h_pct  <- results[["H2H"]]$pct
      gd_pct   <- results[["GD"]]$pct
      h2h_stat <- results[["H2H"]]$status
      gd_stat  <- results[["GD"]]$status

      controversy_flag <- ""
      if (!is.na(h2h_pct) && !is.na(gd_pct) && abs(h2h_pct - gd_pct) >= 5) {
        controversy_flag <- "  ⚖️  CONTROVERSY"
        if (!t %in% controversy_teams) controversy_teams <- c(controversy_teams, t)
      }

      cat(sprintf("  %-28s  H2H: %5.1f%%   GD: %5.1f%%%s\n",
                  t, h2h_pct, gd_pct, controversy_flag))
    }
    cat("\n")
  }

  # Summary
  clinched_h2h <- scen %>%
    filter(Status %in% c("Clinched","Group Winner"), RuleSet == "H2H") %>%
    pull(Team) %>% unique()
  clinched_gd <- scen %>%
    filter(Status %in% c("Clinched","Group Winner"), RuleSet == "GD") %>%
    pull(Team) %>% unique()

  cat(strrep("=", 55), "\n")
  cat("SUMMARY\n")
  cat(strrep("=", 55), "\n")
  cat(sprintf("  Confirmed/Clinched group winners (H2H): %d\n", length(clinched_h2h)))
  cat(sprintf("  Confirmed/Clinched group winners (GD):  %d\n", length(clinched_gd)))

  h2h_only <- setdiff(clinched_h2h, clinched_gd)
  gd_only  <- setdiff(clinched_gd,  clinched_h2h)

  if (length(h2h_only) > 0 || length(gd_only) > 0) {
    cat(sprintf("\n  CONTROVERSY — %d team(s) where rulesets diverge:\n",
                length(h2h_only) + length(gd_only)))
    if (length(h2h_only) > 0)
      cat(sprintf("    H2H only (not GD): %s\n", paste(h2h_only, collapse=", ")))
    if (length(gd_only) > 0)
      cat(sprintf("    GD only (not H2H): %s\n", paste(gd_only,  collapse=", ")))
  } else if (length(controversy_teams) > 0) {
    cat(sprintf("\n  Probability divergence (>=5pp) for: %s\n",
                paste(controversy_teams, collapse=", ")))
  } else {
    cat("\n  No controversy — rulesets agree on all clinching teams.\n")
  }
  cat(strrep("=", 55), "\n")
}

analyze_clinching_likelihood()
