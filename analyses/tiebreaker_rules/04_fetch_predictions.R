library(dplyr)
library(here)

# Download and parse live Elo ratings from eloratings.net
get_elo_database <- function() {
  cat("📡 Downloading Live World Football Elo Ratings...\n")
  
  # Suppress warnings for incomplete final lines in TSV
  teams_raw <- suppressWarnings(readLines("https://www.eloratings.net/en.teams.tsv"))
  teams_df <- do.call(rbind, lapply(strsplit(teams_raw, "\t"), function(x) {
    if(length(x) >= 2) data.frame(Code=x[1], Name=x[2], stringsAsFactors=FALSE) else NULL
  }))
  
  world_raw <- suppressWarnings(readLines("https://www.eloratings.net/World.tsv"))
  world_df <- do.call(rbind, lapply(strsplit(world_raw, "\t"), function(x) {
    if(length(x) >= 4) data.frame(Code=x[3], Elo=as.numeric(x[4]), stringsAsFactors=FALSE) else NULL
  }))
  
  elo_db <- merge(teams_df, world_df, by="Code")
  cat(sprintf("✅ Successfully loaded %d national team ratings.\n\n", nrow(elo_db)))
  return(elo_db)
}

clean_team_name <- function(n) {
  n <- tolower(trimws(n))
  if(n == "usa") return("united states")
  if(n == "ir iran") return("iran")
  if(n == "korea republic") return("south korea")
  if(n == "côte d'ivoire") return("ivory coast")
  return(n)
}

get_elo <- function(team_name, elo_db) {
  name_clean <- clean_team_name(team_name)
  db_clean <- tolower(elo_db$Name)
  
  idx <- which(db_clean == name_clean)
  if(length(idx) > 0) return(elo_db$Elo[idx[1]])
  
  idx_fuzzy <- grep(name_clean, db_clean)
  if(length(idx_fuzzy) > 0) return(elo_db$Elo[idx_fuzzy[1]])
  
  return(1500) # Global average fallback
}

# Apply the authentic Poisson Distribution mathematical model
calc_poisson_probs <- function(h_name, a_name, elo_db) {
  h_elo <- get_elo(h_name, elo_db)
  a_elo <- get_elo(a_name, elo_db)
  
  hosts <- c("united states", "usa", "mexico", "canada")
  h_clean <- clean_team_name(h_name)
  a_clean <- clean_team_name(a_name)
  
  # 2026 Home Field Advantage (+100 Elo) for the 3 host nations
  if(h_clean %in% hosts && !(a_clean %in% hosts)) {
    h_elo <- h_elo + 100
  } else if(a_clean %in% hosts && !(h_clean %in% hosts)) {
    a_elo <- a_elo + 100
  }
  
  dr <- h_elo - a_elo
  
  # Step 1: Calculate Expected Goals (xG) based on Elo difference
  # We use a base 1.25 goals and smoothly scale it. A 200 pt diff results in roughly 2.0xG vs 0.8xG.
  home_xG <- 1.25 * (10 ^ (dr / 1000))
  away_xG <- 1.25 * (10 ^ (-dr / 1000))
  
  # Step 2: Poisson Distribution Matrix
  max_goals <- 10
  home_dist <- dpois(0:max_goals, lambda = home_xG)
  away_dist <- dpois(0:max_goals, lambda = away_xG)
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
  
  if (!file.exists(here("data", "elimination_scenarios.csv"))) {
    stop("❌ Error: data/elimination_scenarios.csv not found.")
  }

  scenarios <- read.csv(here("data", "elimination_scenarios.csv"), stringsAsFactors = FALSE) %>% filter(Status == "At Risk")
  if (nrow(scenarios) == 0) {
    cat("✨ No teams at risk. Predictions not needed.\n")
    return()
  }
  
  matches <- bind_rows(
    scenarios %>% select(Home = R2_M1_Home, Away = R2_M1_Away),
    scenarios %>% select(Home = R2_M2_Home, Away = R2_M2_Away)
  ) %>% distinct() %>% filter(!is.na(Home) & !is.na(Away))
  
  elo_db <- get_elo_database()
  
  cache_file <- here("data", "api_predictions_cache.csv")
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
