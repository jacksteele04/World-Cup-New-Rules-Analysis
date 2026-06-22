library(dplyr)
library(here)

# Re-use Elo database fetcher
get_elo_database <- function() {
  cat("📡 Downloading Live World Football Elo Ratings...\n")
  teams_raw <- suppressWarnings(readLines("https://www.eloratings.net/en.teams.tsv"))
  teams_df <- do.call(rbind, lapply(strsplit(teams_raw, "\t"), function(x) {
    if(length(x) >= 2) data.frame(Code=x[1], Name=x[2], stringsAsFactors=FALSE) else NULL
  }))
  world_raw <- suppressWarnings(readLines("https://www.eloratings.net/World.tsv"))
  world_df <- do.call(rbind, lapply(strsplit(world_raw, "\t"), function(x) {
    if(length(x) >= 4) data.frame(Code=x[3], Elo=as.numeric(x[4]), stringsAsFactors=FALSE) else NULL
  }))
  return(merge(teams_df, world_df, by="Code"))
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
  return(1500)
}

get_poisson_lambdas <- function(h_name, a_name, elo_db) {
  h_elo <- get_elo(h_name, elo_db)
  a_elo <- get_elo(a_name, elo_db)
  
  hosts <- c("united states", "usa", "mexico", "canada")
  h_clean <- clean_team_name(h_name)
  a_clean <- clean_team_name(a_name)
  if(h_clean %in% hosts && !(a_clean %in% hosts)) h_elo <- h_elo + 100
  else if(a_clean %in% hosts && !(h_clean %in% hosts)) a_elo <- a_elo + 100
  
  dr <- h_elo - a_elo
  h_xg <- 1.25 * (10 ^ (dr / 1000))
  a_xg <- 1.25 * (10 ^ (-dr / 1000))
  return(list(h=h_xg, a=a_xg))
}

analyze_gd_miracles <- function() {
  cat("=======================================================\n")
  cat("✨ INITIALIZING GOAL DIFFERENCE MIRACLE ENGINE ✨\n")
  cat("=======================================================\n")
  
  elo_db <- get_elo_database()
  
  df <- read.csv(here("data", "elimination_scenarios.csv"), stringsAsFactors = FALSE)
  fixtures <- read.csv(here("data", "clean_fixtures.csv"), stringsAsFactors = FALSE)
  names(fixtures) <- gsub("^games\\.", "", names(fixtures))
  
  # Find controversies (Eliminated by H2H, but NOT eliminated by GD)
  # This implies they survive under "Infinite GD Limit"
  h2h_scen <- df %>% filter(RuleSet == "H2H" & Status == "At Risk")
  gd_scen <- df %>% filter(RuleSet == "GD" & Status == "At Risk")
  
  # If a scenario is in H2H but not GD, it's a controversy
  if(nrow(h2h_scen) > 0 && nrow(gd_scen) > 0) {
    controversies <- anti_join(h2h_scen, gd_scen, by=c("Team", "Group", "R2_M1_Code", "R2_M2_Code"))
  } else if (nrow(h2h_scen) > 0 && nrow(gd_scen) == 0) {
    controversies <- h2h_scen
  } else {
    cat("✅ No GD controversy scenarios found! All eliminations are mathematically certain.\n")
    return()
  }
  
  if(nrow(controversies) == 0) {
    cat("✅ No GD controversy scenarios found! All eliminations are mathematically certain.\n")
    return()
  }
  
  cat(sprintf("🔍 Found %d controversial scenarios where a team could theoretically survive on GD!\n\n", nrow(controversies)))
  
  for(i in 1:nrow(controversies)) {
    row <- controversies[i, ]
    target_team <- row$Team
    g <- row$Group
    
    cat(sprintf("► [%s] Miracle Analysis\n", target_team))
    
    # Apply R2 scenario string
    r2_str <- sprintf("If %s", row$R2_M1_Desc)
    if(!is.na(row$R2_M2_Desc)) r2_str <- paste(r2_str, "AND", row$R2_M2_Desc)
    cat(sprintf("  %s\n", r2_str))
    
    # Build R2 Standings state
    g_m <- fixtures %>% filter(group == g, type == "group")
    
    unplayed_idx <- which(g_m$finished == "FALSE" | g_m$finished == FALSE)
    played_r2 <- c()
    
    # Inject R2 outcomes
    if(!is.na(row$R2_M1_Code)) {
      idx1 <- which(g_m$home_team_name_en == row$R2_M1_Home & g_m$away_team_name_en == row$R2_M1_Away)
      played_r2 <- c(played_r2, idx1)
      if(row$R2_M1_Code == 0) { g_m$home_score[idx1] = 1; g_m$away_score[idx1] = 0 }
      if(row$R2_M1_Code == 1) { g_m$home_score[idx1] = 0; g_m$away_score[idx1] = 0 }
      if(row$R2_M1_Code == 2) { g_m$home_score[idx1] = 0; g_m$away_score[idx1] = 1 }
    }
    if(!is.na(row$R2_M2_Code)) {
      idx2 <- which(g_m$home_team_name_en == row$R2_M2_Home & g_m$away_team_name_en == row$R2_M2_Away)
      played_r2 <- c(played_r2, idx2)
      if(row$R2_M2_Code == 0) { g_m$home_score[idx2] = 1; g_m$away_score[idx2] = 0 }
      if(row$R2_M2_Code == 1) { g_m$home_score[idx2] = 0; g_m$away_score[idx2] = 0 }
      if(row$R2_M2_Code == 2) { g_m$home_score[idx2] = 0; g_m$away_score[idx2] = 1 }
    }
    
    # Round 3 matches are exactly the unplayed matches minus the ones we just simulated
    r3_idx <- setdiff(unplayed_idx, played_r2)
    r3_matches <- g_m[r3_idx, ]
    
    if(nrow(r3_matches) != 2) {
      cat(sprintf("  ❌ Error: Expected 2 unplayed Round 3 matches, but found %d.\n\n", nrow(r3_matches)))
      next
    }
    
    # Get lambdas for both matches
    m1_h <- r3_matches$home_team_name_en[1]; m1_a <- r3_matches$away_team_name_en[1]
    m2_h <- r3_matches$home_team_name_en[2]; m2_a <- r3_matches$away_team_name_en[2]
    
    lam1 <- get_poisson_lambdas(m1_h, m1_a, elo_db)
    lam2 <- get_poisson_lambdas(m2_h, m2_a, elo_db)
    
    cat(sprintf("  ⚽ Round 3 Match 1: %s (xG: %.2f) vs %s (xG: %.2f)\n", m1_h, lam1$h, m1_a, lam1$a))
    cat(sprintf("  ⚽ Round 3 Match 2: %s (xG: %.2f) vs %s (xG: %.2f)\n", m2_h, lam2$h, m2_a, lam2$a))
    
    # Baseline Points/GD/GF from R1 and simulated R2
    teams <- unique(c(g_m$home_team_name_en, g_m$away_team_name_en))
    base_pts <- setNames(rep(0,4), teams)
    base_gd <- setNames(rep(0,4), teams)
    base_gf <- setNames(rep(0,4), teams)
    
    # Aggregate only R1 matches and the simulated R2 matches (exclude R3 entirely)
    played <- g_m[-r3_idx, ]
    for(k in 1:nrow(played)) {
      h <- played$home_team_name_en[k]; a <- played$away_team_name_en[k]
      hg <- as.numeric(played$home_score[k]); ag <- as.numeric(played$away_score[k])
      base_gd[h] <- base_gd[h] + (hg - ag)
      base_gd[a] <- base_gd[a] + (ag - hg)
      base_gf[h] <- base_gf[h] + hg
      base_gf[a] <- base_gf[a] + ag
      if(hg > ag) { base_pts[h] <- base_pts[h] + 3 }
      else if(hg < ag) { base_pts[a] <- base_pts[a] + 3 }
      else { base_pts[h] <- base_pts[h] + 1; base_pts[a] <- base_pts[a] + 1 }
    }
    
    cat("  📊 Standings going into final match:\n")
    base_standings <- data.frame(team=teams, pts=base_pts[teams], gd=base_gd[teams], gf=base_gf[teams], stringsAsFactors=FALSE)
    base_standings <- base_standings[order(-base_standings$pts, -base_standings$gd, -base_standings$gf), ]
    for(k in 1:nrow(base_standings)) {
      marker <- if(base_standings$team[k] == target_team) "👉" else "  "
      cat(sprintf("   %s %d. %-15s | %d pts | GD: %+d\n", marker, k, base_standings$team[k], base_standings$pts[k], base_standings$gd[k]))
    }
    
    # Convolute 11x11 matrices (14,641 combinations)
    dist1_h <- dpois(0:10, lambda = lam1$h); dist1_a <- dpois(0:10, lambda = lam1$a)
    dist2_h <- dpois(0:10, lambda = lam2$h); dist2_a <- dpois(0:10, lambda = lam2$a)
    
    prob_m1 <- outer(dist1_h, dist1_a)
    prob_m2 <- outer(dist2_h, dist2_a)
    
    miracle_prob <- 0.0
    
    for(x1 in 0:10) {
      for(y1 in 0:10) {
        p1 <- prob_m1[x1+1, y1+1]
        if(p1 < 1e-5) next
        
        for(x2 in 0:10) {
          for(y2 in 0:10) {
            p2 <- prob_m2[x2+1, y2+1]
            p_combo <- p1 * p2
            if(p_combo < 1e-5) next
            
            # Evaluate Survival
            pts <- base_pts; gd <- base_gd; gf <- base_gf
            
            # Match 1
            gd[m1_h] <- gd[m1_h] + (x1 - y1); gd[m1_a] <- gd[m1_a] + (y1 - x1)
            gf[m1_h] <- gf[m1_h] + x1; gf[m1_a] <- gf[m1_a] + y1
            if(x1 > y1) { pts[m1_h] <- pts[m1_h] + 3 } else if(x1 < y1) { pts[m1_a] <- pts[m1_a] + 3 } else { pts[m1_h] <- pts[m1_h] + 1; pts[m1_a] <- pts[m1_a] + 1 }
            
            # Match 2
            gd[m2_h] <- gd[m2_h] + (x2 - y2); gd[m2_a] <- gd[m2_a] + (y2 - x2)
            gf[m2_h] <- gf[m2_h] + x2; gf[m2_a] <- gf[m2_a] + y2
            if(x2 > y2) { pts[m2_h] <- pts[m2_h] + 3 } else if(x2 < y2) { pts[m2_a] <- pts[m2_a] + 3 } else { pts[m2_h] <- pts[m2_h] + 1; pts[m2_a] <- pts[m2_a] + 1 }
            
            # GD Tiebreaker Rank
            standings <- data.frame(team=teams, pts=pts[teams], gd=gd[teams], gf=gf[teams], stringsAsFactors=FALSE)
            standings <- standings[order(-standings$pts, -standings$gd, -standings$gf), ]
            
            if(which(standings$team == target_team)[1] <= 3) {
              miracle_prob <- miracle_prob + p_combo
            }
          }
        }
      }
    }
    
    cat(sprintf("  ✨ Likelihood of hitting required Goal Difference swing: %.2f%%\n\n", miracle_prob * 100))
  }
}

analyze_gd_miracles()
