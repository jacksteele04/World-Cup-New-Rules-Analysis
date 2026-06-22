# Load necessary libraries (uncomment install.packages if you don't have them)
# install.packages(c("dplyr", "jsonlite"))

library(dplyr)
library(jsonlite)
library(here)

fetch_world_cup_fixtures <- function() {
  endpoint <- "https://worldcup26.ir/get/games"
  cat("📡 Contacting worldcup26.ir for 2026 fixtures...\n")
  
  # Suppress warnings for self-signed certificates if any
  tryCatch({
    # jsonlite::fromJSON automatically parses the JSON into a clean R dataframe
    data <- fromJSON(endpoint, flatten = TRUE)
    
    # Extract the dataframe from the response
    if ("data" %in% names(data)) {
      fixtures <- as.data.frame(data$data)
    } else {
      fixtures <- as.data.frame(data)
    }
    
    cat("✅ Successfully pulled data from API!\n")
    return(fixtures)
  }, error = function(e) {
    cat(sprintf("❌ Network/HTTP Error occurred: %s\n", e$message))
    return(NULL)
  })
}

update_fixtures_csv <- function(new_fixtures, file_path = here("data", "clean_fixtures.csv")) {
  if (is.null(new_fixtures) || nrow(new_fixtures) == 0) {
    cat("⚠️ No new data to update.\n")
    return()
  }
  
  if (file.exists(file_path)) {
    cat(sprintf("🔄 Found existing %s, checking for out-of-date matches...\n", file_path))
    
    existing_fixtures <- read.csv(file_path, stringsAsFactors = FALSE)
    
    # Dynamically detect if the ID column is named 'id' or 'games.id'
    id_col <- if("games.id" %in% names(new_fixtures)) "games.id" else "id"
    
    # Ensure ALL columns are characters so the strict dplyr join doesn't fail on type mismatches 
    # (e.g. API returns string "2", but CSV reads integer 2)
    new_fixtures[] <- lapply(new_fixtures, as.character)
    existing_fixtures[] <- lapply(existing_fixtures, as.character)
    
    # Identify which records in the new data are actually different or entirely new
    updated_or_new <- suppressMessages(anti_join(new_fixtures, existing_fixtures))
    
    if (nrow(updated_or_new) > 0) {
      cat(sprintf("📝 Found %d updated or new matches. Applying changes...\n", nrow(updated_or_new)))
      
      # Upsert logic: Remove old versions of the updated matches based on their dynamic ID
      existing_fixtures <- existing_fixtures[!(existing_fixtures[[id_col]] %in% updated_or_new[[id_col]]), ]
      
      # Bind the new/updated matches
      final_fixtures <- bind_rows(existing_fixtures, updated_or_new)
      
      # Sort by match ID or Date
      final_fixtures <- final_fixtures %>% arrange(as.numeric(id))
      
      # Save the updated dataset
      write.csv(final_fixtures, file_path, row.names = FALSE)
      cat(sprintf("✅ Success! %s has been updated.\n", file_path))
    } else {
      cat("✨ Everything is already up to date! No changes written to CSV.\n")
    }
    
  } else {
    cat(sprintf("🆕 Creating new file: %s...\n", file_path))
    write.csv(new_fixtures, file_path, row.names = FALSE)
    cat(sprintf("✅ Success! Clean data exported to: %s\n", file_path))
  }
}

# Execute the pipeline
raw_fixtures <- fetch_world_cup_fixtures()
update_fixtures_csv(raw_fixtures, here("data", "clean_fixtures.csv"))
