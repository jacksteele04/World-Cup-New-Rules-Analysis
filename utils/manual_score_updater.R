library(dplyr)
library(here)

update_scores_interactively <- function() {
  csv_file <- here("data", "clean_fixtures.csv")
  
  if (!file.exists(csv_file)) {
    stop(sprintf("❌ Error: Could not find %s. Please make sure you have it in your folder.", csv_file))
  }
  
  # Read the clean CSV
  df <- read.csv(csv_file, stringsAsFactors = FALSE)
  # Ensure column names don't have the 'games.' prefix
  names(df) <- gsub("^games\\.", "", names(df))
  
  # Find all matches that are NOT finished, and sort them chronologically
  unplayed_matches <- df %>%
    filter(finished == "FALSE" | finished == FALSE) %>%
    arrange(as.numeric(matchday), id)
  
  if (nrow(unplayed_matches) == 0) {
    cat("✨ All fixtures in the CSV are already marked as finished!\n")
    return()
  }
  
  cat(sprintf("⚽ Found %d unplayed fixtures. Let's update some scores!\n", nrow(unplayed_matches)))
  cat("Type the score and press Enter. If you want to stop updating and save, just leave it blank and press Enter.\n\n")
  
  updated_count <- 0
  
  for (i in 1:nrow(unplayed_matches)) {
    match_row <- unplayed_matches[i, ]
    match_id <- match_row$id
    h_team <- match_row$home_team_name_en
    a_team <- match_row$away_team_name_en
    
    cat(sprintf("------------ MATCH %d of %d ------------\n", i, nrow(unplayed_matches)))
    cat(sprintf("🌍 Group %s | Matchday %s\n", match_row$group, match_row$matchday))
    cat(sprintf("📅 %s\n", match_row$local_date))
    cat(sprintf("🏟️  %s vs %s\n", h_team, a_team))
    
    # Prompt for Home Score
    cat(sprintf("Enter goals for %s: ", h_team))
    h_input <- readLines(con = "stdin", n = 1)
    
    # If the user just presses Enter without typing anything, break and save
    if (trimws(h_input) == "") {
      cat("🛑 Stopping updates and saving your progress...\n")
      break
    }
    
    # Prompt for Away Score
    cat(sprintf("Enter goals for %s: ", a_team))
    a_input <- readLines(con = "stdin", n = 1)
    
    if (trimws(a_input) == "") {
      cat("🛑 Stopping updates and saving your progress...\n")
      break
    }
    
    # Safely convert to numeric
    h_score <- suppressWarnings(as.numeric(trimws(h_input)))
    a_score <- suppressWarnings(as.numeric(trimws(a_input)))
    
    if (is.na(h_score) || is.na(a_score)) {
      cat("⚠️ Invalid number entered. Skipping this match.\n\n")
      next
    }
    
    # Update the master dataframe
    df_idx <- which(df$id == match_id)
    df$home_score[df_idx] <- h_score
    df$away_score[df_idx] <- a_score
    df$finished[df_idx] <- "TRUE"
    
    updated_count <- updated_count + 1
    cat(sprintf("✅ Score saved: %s %d - %d %s\n\n", h_team, h_score, a_score, a_team))
  }
  
  if (updated_count > 0) {
    # Save the updated dataset back to CSV
    write.csv(df, csv_file, row.names = FALSE)
    cat(sprintf("\n💾 Successfully updated and saved %d new scores to %s!\n", updated_count, csv_file))
  } else {
    cat("\nNo scores were updated.\n")
  }
}

update_scores_interactively()
