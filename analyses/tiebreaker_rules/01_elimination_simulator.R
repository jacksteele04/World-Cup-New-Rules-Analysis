library(dplyr)
library(here)

source(here("config.R"))

get_outcome_desc <- function(match_row, outcome) {
  h <- match_row$home_team_name_en
  a <- match_row$away_team_name_en
  if(outcome == 0) return(sprintf("'%s' beats '%s'", h, a))
  if(outcome == 1) return(sprintf("'%s' draws '%s'", h, a))
  if(outcome == 2) return(sprintf("'%s' loses to '%s'", h, a))
}

apply_outcome <- function(matches, match_indices, outcomes) {
  for(k in 1:length(match_indices)) {
    idx <- match_indices[k]
    o <- outcomes[k]
    if(o == 0) { matches$home_score[idx] <- 1; matches$away_score[idx] <- 0 }
    if(o == 1) { matches$home_score[idx] <- 0; matches$away_score[idx] <- 0 }
    if(o == 2) { matches$home_score[idx] <- 0; matches$away_score[idx] <- 1 }
  }
  return(matches)
}

# Classic Rules: Goal Difference Primary (Infinite GD Limit)
check_survives_gd <- function(team_target, matches) {
  teams <- unique(c(matches$home_team_name_en, matches$away_team_name_en))
  pts <- setNames(rep(0, length(teams)), teams)
  
  for(i in 1:nrow(matches)) {
    h <- matches$home_team_name_en[i]
    a <- matches$away_team_name_en[i]
    hg <- as.numeric(matches$home_score[i])
    ag <- as.numeric(matches$away_score[i])
    if(hg > ag) { pts[h] <- pts[h] + 3 }
    else if(hg < ag) { pts[a] <- pts[a] + 3 }
    else { pts[h] <- pts[h] + 1; pts[a] <- pts[a] + 1 }
  }
  
  # The "Infinite GD" Trick: Automatically win any tiebreaker by having slightly more points
  pts[team_target] <- pts[team_target] + ELO_MODEL$epsilon
  
  sorted_pts <- sort(pts, decreasing = TRUE)
  target_rank <- which(names(sorted_pts) == team_target)[1]
  
  if(target_rank <= 3) return(TRUE)
  return(FALSE)
}

# New 2026 Rules: Head-to-Head Primary (Infinite GD Limit)
check_survives_h2h <- function(team_target, matches) {
  teams <- unique(c(matches$home_team_name_en, matches$away_team_name_en))
  pts <- setNames(rep(0, length(teams)), teams)
  for(i in 1:nrow(matches)) {
    h <- matches$home_team_name_en[i]
    a <- matches$away_team_name_en[i]
    hg <- as.numeric(matches$home_score[i])
    ag <- as.numeric(matches$away_score[i])
    if(hg > ag) { pts[h] <- pts[h] + 3 }
    else if(hg < ag) { pts[a] <- pts[a] + 3 }
    else { pts[h] <- pts[h] + 1; pts[a] <- pts[a] + 1 }
  }
  
  t_pts <- pts[team_target]
  sorted_pts <- sort(pts, decreasing = TRUE)
  if(t_pts > sorted_pts[4]) return(TRUE)
  if(t_pts < sorted_pts[3]) return(FALSE)
  
  tied_teams <- names(pts)[pts == t_pts]
  h2h_pts <- setNames(rep(0, length(tied_teams)), tied_teams)
  for(i in 1:nrow(matches)) {
    h <- matches$home_team_name_en[i]
    a <- matches$away_team_name_en[i]
    hg <- as.numeric(matches$home_score[i])
    ag <- as.numeric(matches$away_score[i])
    if(h %in% tied_teams && a %in% tied_teams) {
      if(hg > ag) { h2h_pts[h] <- h2h_pts[h] + 3 }
      else if(hg < ag) { h2h_pts[a] <- h2h_pts[a] + 3 }
      else { h2h_pts[h] <- h2h_pts[h] + 1; h2h_pts[a] <- h2h_pts[a] + 1 }
    }
  }
  
  # Infinite GD secondary tiebreaker trick
  h2h_pts[team_target] <- h2h_pts[team_target] + ELO_MODEL$epsilon
  
  sorted_h2h <- sort(h2h_pts, decreasing = TRUE)
  target_rank_h2h <- which(names(sorted_h2h) == team_target)[1]
  
  # Calculate how many survival spots are available to the tied teams
  spots_taken_by_better_teams <- sum(pts > t_pts)
  spots_available <- 3 - spots_taken_by_better_teams
  
  if(target_rank_h2h <= spots_available) return(TRUE)
  return(FALSE)
}

run_elimination_sim <- function() {
  cat("🔮 Initializing Dual-Simulation Mathematical Elimination Engine...\n")
  df <- read.csv(here("data", "fixtures", "clean_fixtures.csv"), stringsAsFactors = FALSE)
  names(df) <- gsub("^games\\.", "", names(df))
  group_matches <- df %>% filter(type == "group")
  outcomes_matrix <- as.matrix(expand.grid(m1 = 0:2, m2 = 0:2))
  
  export_df <- data.frame(
    Team = character(), Group = character(), Status = character(), RuleSet = character(),
    R2_M1_Home = character(), R2_M1_Away = character(), R2_M1_Code = numeric(), R2_M1_Desc = character(),
    R2_M2_Home = character(), R2_M2_Away = character(), R2_M2_Code = numeric(), R2_M2_Desc = character(),
    stringsAsFactors = FALSE
  )
  
  total_teams_analyzed <- 0
  
  for (g in unique(group_matches$group)) {
    g_m <- group_matches %>% filter(group == g) %>% arrange(matchday)
    unplayed_idx <- which(g_m$finished == "FALSE" | g_m$finished == FALSE)
    teams <- unique(c(g_m$home_team_name_en, g_m$away_team_name_en))
    
    # If Round 2 is completely finished, check for Already Eliminated teams
    if (length(unplayed_idx) == 2) {
      for (t in teams) {
        total_teams_analyzed <- total_teams_analyzed + 1
        survives_h2h <- FALSE
        survives_gd <- FALSE
        
        for (i in 1:nrow(outcomes_matrix)) {
          r3_out <- outcomes_matrix[i, ]
          sim_r3 <- apply_outcome(g_m, unplayed_idx, r3_out)
          if (check_survives_h2h(t, sim_r3)) survives_h2h <- TRUE
          if (check_survives_gd(t, sim_r3)) survives_gd <- TRUE
        }
        
        if (!survives_h2h && !survives_gd) {
          export_df <- bind_rows(export_df, data.frame(
            Team=t, Group=g, Status="Already Eliminated", RuleSet="H2H",
            R2_M1_Home=NA, R2_M1_Away=NA, R2_M1_Code=NA, R2_M1_Desc="N/A",
            R2_M2_Home=NA, R2_M2_Away=NA, R2_M2_Code=NA, R2_M2_Desc="N/A"
          ))
          export_df <- bind_rows(export_df, data.frame(
            Team=t, Group=g, Status="Already Eliminated", RuleSet="GD",
            R2_M1_Home=NA, R2_M1_Away=NA, R2_M1_Code=NA, R2_M1_Desc="N/A",
            R2_M2_Home=NA, R2_M2_Away=NA, R2_M2_Code=NA, R2_M2_Desc="N/A"
          ))
          cat(sprintf("\n💀 [%s] (Group %s) IS ALREADY MATHEMATICALLY ELIMINATED UNDER BOTH RULES!\n", t, g))
        } else if (!survives_h2h && survives_gd) {
          export_df <- bind_rows(export_df, data.frame(
            Team=t, Group=g, Status="Already Eliminated", RuleSet="H2H",
            R2_M1_Home=NA, R2_M1_Away=NA, R2_M1_Code=NA, R2_M1_Desc="N/A",
            R2_M2_Home=NA, R2_M2_Away=NA, R2_M2_Code=NA, R2_M2_Desc="N/A"
          ))
          cat(sprintf("\n💀 [%s] (Group %s) IS ALREADY MATHEMATICALLY ELIMINATED (2026 H2H Rules ONLY)!\n", t, g))
        } else if (survives_h2h && !survives_gd) {
          export_df <- bind_rows(export_df, data.frame(
            Team=t, Group=g, Status="Already Eliminated", RuleSet="GD",
            R2_M1_Home=NA, R2_M1_Away=NA, R2_M1_Code=NA, R2_M1_Desc="N/A",
            R2_M2_Home=NA, R2_M2_Away=NA, R2_M2_Code=NA, R2_M2_Desc="N/A"
          ))
          cat(sprintf("\n💀 [%s] (Group %s) IS ALREADY MATHEMATICALLY ELIMINATED (Classic GD Rules ONLY)!\n", t, g))
        }
      }
      next # Proceed to next group, skip the R2 simulation logic
    }
    
    # Identify if we need 1 or 2 matches to simulate for Round 2
    simulate_matches <- NULL
    if (length(unplayed_idx) == 3) {
      simulate_matches <- unplayed_idx[1]
      sim_outcomes <- matrix(c(0, 1, 2), ncol=1)
    } else if (length(unplayed_idx) == 4) {
      simulate_matches <- unplayed_idx[1:2]
      sim_outcomes <- outcomes_matrix
    } else {
      next # Only analyze mid-R2 or pre-R2
    }
    
    for (t in teams) {
      total_teams_analyzed <- total_teams_analyzed + 1
      
      eliminated_both <- list()
      eliminated_h2h_only <- list()
      eliminated_gd_only <- list()
      
      for (i in 1:nrow(sim_outcomes)) {
        r2_out <- sim_outcomes[i, ]
        sim_r2 <- apply_outcome(g_m, simulate_matches, r2_out)
        
        survives_h2h_r3 <- FALSE
        survives_gd_r3 <- FALSE
        
        r3_idx <- unplayed_idx[(length(simulate_matches)+1):length(unplayed_idx)]
        
        for (j in 1:nrow(outcomes_matrix)) {
          r3_out <- outcomes_matrix[j, ]
          sim_r3 <- apply_outcome(sim_r2, r3_idx, r3_out)
          
          if (check_survives_h2h(t, sim_r3)) survives_h2h_r3 <- TRUE
          if (check_survives_gd(t, sim_r3)) survives_gd_r3 <- TRUE
          
          if(survives_h2h_r3 && survives_gd_r3) break
        }
        
        if (!survives_h2h_r3 && !survives_gd_r3) eliminated_both[[length(eliminated_both) + 1]] <- r2_out
        else if (!survives_h2h_r3 && survives_gd_r3) eliminated_h2h_only[[length(eliminated_h2h_only) + 1]] <- r2_out
        else if (survives_h2h_r3 && !survives_gd_r3) eliminated_gd_only[[length(eliminated_gd_only) + 1]] <- r2_out
      }
      
      total_elim <- length(eliminated_both) + length(eliminated_h2h_only) + length(eliminated_gd_only)
      
      if(total_elim > 0) {
        cat(sprintf("\n🚨 [%s] (Group %s) could be MATHEMATICALLY ELIMINATED after Round 2!\n", t, g))
        
        # Helper to export
        export_scen <- function(scen, ruleset) {
          match1 <- g_m[simulate_matches[1], ]
          desc1 <- get_outcome_desc(match1, scen[1])
          
          if(length(simulate_matches) == 2) {
            match2 <- g_m[simulate_matches[2], ]
            desc2 <- get_outcome_desc(match2, scen[2])
            export_df <<- bind_rows(export_df, data.frame(
              Team=t, Group=g, Status="At Risk", RuleSet=ruleset,
              R2_M1_Home=match1$home_team_name_en, R2_M1_Away=match1$away_team_name_en, R2_M1_Code=scen[1], R2_M1_Desc=desc1,
              R2_M2_Home=match2$home_team_name_en, R2_M2_Away=match2$away_team_name_en, R2_M2_Code=scen[2], R2_M2_Desc=desc2
            ))
            return(sprintf("   ► %s   -AND-   %s", desc1, desc2))
          } else {
            export_df <<- bind_rows(export_df, data.frame(
              Team=t, Group=g, Status="At Risk", RuleSet=ruleset,
              R2_M1_Home=match1$home_team_name_en, R2_M1_Away=match1$away_team_name_en, R2_M1_Code=scen[1], R2_M1_Desc=desc1,
              R2_M2_Home=NA, R2_M2_Away=NA, R2_M2_Code=NA, R2_M2_Desc=NA
            ))
            return(sprintf("   ► %s", desc1))
          }
        }
        
        if(length(eliminated_both) > 0) {
          cat("   ❌ Eliminated under BOTH sets of rules if:\n")
          for(scen in eliminated_both) {
            str <- export_scen(scen, "H2H")
            export_scen(scen, "GD") # Export for both
            cat(sprintf("%s\n", str))
          }
        }
        
        if(length(eliminated_h2h_only) > 0) {
          cat("   ⚖️  CONTROVERSY: Eliminated ONLY by 2026 Head-to-Head rules if:\n")
          for(scen in eliminated_h2h_only) {
            str <- export_scen(scen, "H2H")
            cat(sprintf("%s\n", str))
          }
        }
        
        if(length(eliminated_gd_only) > 0) {
          cat("   ⚖️  CONTROVERSY: Eliminated ONLY by Classic Goal Difference rules if:\n")
          for(scen in eliminated_gd_only) {
            str <- export_scen(scen, "GD")
            cat(sprintf("%s\n", str))
          }
        }
      }
    }
  }
  
  write.csv(export_df, here("data", "standings", "elimination_scenarios.csv"), row.names = FALSE)
  
  cat("\n=======================================================\n")
  cat("🏆 TOURNAMENT ELIMINATION SUMMARY 🏆\n")
  cat("=======================================================\n")
  cat(sprintf("Analyzed %d national teams.\n", total_teams_analyzed))
  cat(sprintf("Saved updated scenarios to data/elimination_scenarios.csv\n"))
}

run_elimination_sim()
