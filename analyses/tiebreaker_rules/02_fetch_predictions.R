library(dplyr)
library(here)

source(here("config.R"))
source(here("standardization", "validation.R"))
source(here("standardization", "team_names.R"))
source(here("standardization", "elo.R"))

# Apply the authentic Poisson Distribution mathematical model
calc_poisson_probs <- function(h_name, a_name, elo_db) {
  h_elo <- get_elo(h_name, elo_db); if (is.na(h_elo)) h_elo <- ELO_MODEL$default_rating
  a_elo <- get_elo(a_name, elo_db); if (is.na(a_elo)) a_elo <- ELO_MODEL$default_rating

  h_clean <- elo_name(h_name)
  a_clean <- elo_name(a_name)

  if (h_clean %in% TOURNAMENT$host_nations && !(a_clean %in% TOURNAMENT$host_nations)) {
    h_elo <- h_elo + ELO_MODEL$host_bonus
  } else if (a_clean %in% TOURNAMENT$host_nations && !(h_clean %in% TOURNAMENT$host_nations)) {
    a_elo <- a_elo + ELO_MODEL$host_bonus
  }

  dr <- h_elo - a_elo

  home_xG <- ELO_MODEL$xg_multiplier * (10 ^ (dr  / ELO_MODEL$elo_divisor))
  away_xG <- ELO_MODEL$xg_multiplier * (10 ^ (-dr / ELO_MODEL$elo_divisor))

  home_dist <- dpois(0:ELO_MODEL$max_goals, lambda = home_xG)
  away_dist <- dpois(0:ELO_MODEL$max_goals, lambda = away_xG)
  prob_matrix <- outer(home_dist, away_dist)
  
  # Step 3: Sum the outcomes
  p_home <- sum(prob_matrix[lower.tri(prob_matrix)]) # Home goals > Away goals
  p_draw <- sum(diag(prob_matrix))                   # Home goals == Away goals
  p_away <- sum(prob_matrix[upper.tri(prob_matrix)]) # Home goals < Away goals
  
  # Normalize to 1.0 (to account for the microscopic chance of >10 goals in a game)
  total <- p_home + p_draw + p_away
  p_home <- p_home / total
  p_draw <- p_draw / total
  p_away <- p_away / total
  
  return(list(
    home=round(p_home, 3), draw=round(p_draw, 3), away=round(p_away, 3), 
    h_elo=h_elo, a_elo=a_elo,
    h_xg=round(home_xG, 2), a_xg=round(away_xG, 2)
  ))
}

build_cache <- function() {
  cat("=======================================================\n")
  cat("🏆 INITIALIZING POISSON XG PREDICTION ENGINE 🏆\n")
  cat("=======================================================\n")
  
  require_file(here("data", "standings", "elimination_scenarios.csv"), "run analyses/tiebreaker_rules/01_elimination_simulator.R first")

  scenarios <- read.csv(here("data", "standings", "elimination_scenarios.csv"), stringsAsFactors = FALSE)
  require_cols(scenarios, c("Status", "Team", "Group", "R2_M1_Home", "R2_M1_Away", "R2_M2_Home", "R2_M2_Away"), "elimination_scenarios.csv")
  scenarios <- scenarios %>% filter(Status == "At Risk")
  if (nrow(scenarios) == 0) {
    cat("✨ No teams at risk. Predictions not needed.\n")
    return()
  }
  
  matches <- bind_rows(
    scenarios %>% select(Home = R2_M1_Home, Away = R2_M1_Away),
    scenarios %>% select(Home = R2_M2_Home, Away = R2_M2_Away)
  ) %>% distinct() %>% filter(!is.na(Home) & !is.na(Away))
  
  elo_db <- get_elo_database()
  
  cache_file <- here("data", "standings", "api_predictions_cache.csv")
  cache_df <- data.frame(
    Home = character(), Away = character(), 
    Prob_0 = numeric(), Prob_1 = numeric(), Prob_2 = numeric(), 
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(matches)) {
    h <- matches$Home[i]
    a <- matches$Away[i]
    
    cat(sprintf("🔍 Calculating match: %s vs %s...\n", h, a))
    
    res <- calc_poisson_probs(h, a, elo_db)
    
    cat(sprintf("   ► Elo: %s (%d) vs %s (%d)\n", h, res$h_elo, a, res$a_elo))
    cat(sprintf("   ► Expected Goals (xG): %s (%.2f) vs %s (%.2f)\n", h, res$h_xg, a, res$a_xg))
    cat(sprintf("   ► Poisson Odds  | Win: %s%% | Draw: %s%% | Loss: %s%%\n\n", res$home*100, res$draw*100, res$away*100))
    
    cache_df <- bind_rows(cache_df, data.frame(
      Home=h, Away=a, 
      Prob_0=res$home, Prob_1=res$draw, Prob_2=res$away
    ))
  }
  
  write.csv(cache_df, cache_file, row.names = FALSE)
  cat(sprintf("✅ Poisson calculations complete! Saved %d predictions to %s.\n", nrow(cache_df), cache_file))
}

build_cache()
