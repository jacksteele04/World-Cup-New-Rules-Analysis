library(dplyr)
library(here)

get_probability <- function(h_team, a_team, outcome_code, cache_df) {
  row <- cache_df %>% filter(Home == h_team & Away == a_team)
  if (nrow(row) > 0) {
    if (outcome_code == 0 && !is.na(row$Prob_0[1])) return(row$Prob_0[1])
    if (outcome_code == 1 && !is.na(row$Prob_1[1])) return(row$Prob_1[1])
    if (outcome_code == 2 && !is.na(row$Prob_2[1])) return(row$Prob_2[1])
  }
  
  row_rev <- cache_df %>% filter(Home == a_team & Away == h_team)
  if (nrow(row_rev) > 0) {
    if (outcome_code == 0 && !is.na(row_rev$Prob_2[1])) return(row_rev$Prob_2[1])
    if (outcome_code == 1 && !is.na(row_rev$Prob_1[1])) return(row_rev$Prob_1[1])
    if (outcome_code == 2 && !is.na(row_rev$Prob_0[1])) return(row_rev$Prob_0[1])
  }
  
  return(0.3333)
}

analyze_elimination_likelihood <- function() {
  scen_file <- here("data", "standings", "elimination_scenarios.csv")
  cache_file <- here("data", "standings", "api_predictions_cache.csv")

  if (!file.exists(scen_file)) stop("❌ Error: data/elimination_scenarios.csv not found.")
  if (!file.exists(cache_file)) stop("❌ Error: data/api_predictions_cache.csv not found.")
  
  df <- read.csv(scen_file, stringsAsFactors = FALSE)
  cache_df <- read.csv(cache_file, stringsAsFactors = FALSE)
  
  if(nrow(df) == 0) {
    cat("✨ No teams are currently at risk of elimination.\n")
    return()
  }
  
  cat("📊 DUAL-RULESET WEIGHTED ELIMINATION LIKELIHOOD\n")
  cat("=======================================================\n")
  
  already_elim <- df %>% filter(Status == "Already Eliminated")
  if(nrow(already_elim) > 0) {
    cat("💀 ALREADY ELIMINATED:\n")
    for(t in unique(already_elim$Team)) {
      rs_arr <- already_elim %>% filter(Team == t) %>% pull(RuleSet) %>% unique()
      rs_str <- paste(rs_arr, collapse=" & ")
      if(rs_str == "") rs_str <- "All"
      cat(sprintf("   ► %s: 100.0%% (Mathematically out via %s rules)\n", t, rs_str))
    }
    cat("\n")
  }
  
  h2h_risk_count <- 0
  gd_risk_count <- 0
  
  at_risk_df <- df %>% filter(Status == "At Risk")
  
  if(nrow(at_risk_df) > 0) {
    cat("⚠️  AT RISK OF ELIMINATION:\n")
    
    teams <- unique(at_risk_df$Team)
    for(t in teams) {
      cat(sprintf("   ► %s (Group %s):\n", t, at_risk_df %>% filter(Team == t) %>% pull(Group) %>% .[1]))
      
      for(rs in c("H2H", "GD")) {
        team_data <- at_risk_df %>% filter(Team == t & RuleSet == rs)
        
        if(nrow(team_data) == 0) {
          cat(sprintf("      [%s Rules]: 0%% Risk (Mathematically Safe!)\n", rs))
          next
        }
        
        total_elim_probability <- 0.0
        scenario_strings <- list()
        
        for(i in 1:nrow(team_data)) {
          row <- team_data[i, ]
          prob1 <- get_probability(row$R2_M1_Home, row$R2_M1_Away, row$R2_M1_Code, cache_df)
          
          if (!is.na(row$R2_M2_Home)) {
            prob2 <- get_probability(row$R2_M2_Home, row$R2_M2_Away, row$R2_M2_Code, cache_df)
            scenario_prob <- prob1 * prob2
            desc_str <- sprintf("        * %s (%.1f%%) AND %s (%.1f%%)", row$R2_M1_Desc, prob1 * 100, row$R2_M2_Desc, prob2 * 100)
          } else {
            prob2 <- 1.0
            scenario_prob <- prob1
            desc_str <- sprintf("        * %s (%.1f%%)", row$R2_M1_Desc, prob1 * 100)
          }
          
          total_elim_probability <- total_elim_probability + scenario_prob
          scenario_strings[[i]] <- data.frame(desc=desc_str, prob=scenario_prob, stringsAsFactors=FALSE)
        }
        
        final_pct <- round(total_elim_probability * 100, 2)
        scen_df <- bind_rows(scenario_strings) %>% arrange(desc(prob))
        
        cat(sprintf("      [%s Rules]: %s%% Risk\n", rs, final_pct))
        
        # Track stats
        if(rs == "H2H" && final_pct > 0) h2h_risk_count <- h2h_risk_count + 1
        if(rs == "GD" && final_pct > 0) gd_risk_count <- gd_risk_count + 1
        
        # Print top 3 scenarios max to keep output clean
        max_print <- min(3, nrow(scen_df))
        for(j in 1:max_print) {
          cat(sprintf("        * %s  -->  Scenario Likelihood: %.1f%%\n", scen_df$desc[j], scen_df$prob[j] * 100))
        }
        if(nrow(scen_df) > 3) {
          cat(sprintf("        * ... plus %d other less likely scenarios\n", nrow(scen_df) - 3))
        }
      }
      cat("\n")
    }
  }
  
  teams_elim_h2h <- length(unique(df %>% filter(RuleSet == "H2H" & Status == "Already Eliminated") %>% pull(Team)))
  teams_elim_gd <- length(unique(df %>% filter(RuleSet == "GD" & Status == "Already Eliminated") %>% pull(Team)))

  cat("=======================================================\n")
  cat("📈 OVERALL TOURNAMENT ELIMINATION STATS\n")
  cat("=======================================================\n")
  cat(sprintf("   ► Already Eliminated (2026 Head-to-Head Rules): %d\n", teams_elim_h2h))
  cat(sprintf("   ► Already Eliminated (Classic GD Rules): %d\n", teams_elim_gd))
  cat(sprintf("   ► Teams At Risk (2026 Head-to-Head Rules): %d\n", h2h_risk_count))
  cat(sprintf("   ► Teams At Risk (Classic Goal Difference Rules): %d\n", gd_risk_count))
  
  total_h2h_casualties <- teams_elim_h2h + h2h_risk_count
  total_gd_casualties <- teams_elim_gd + gd_risk_count
  
  cat(sprintf("   ► Difference (Controversy Count): %d\n\n", abs(total_h2h_casualties - total_gd_casualties)))
}

analyze_elimination_likelihood()
