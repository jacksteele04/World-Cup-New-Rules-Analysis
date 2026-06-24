# =============================================================================
# standardization/elo.R
#
# Elo rating utilities sourced from eloratings.net.
# Requires: standardization/team_names.R (for elo_name())
# =============================================================================

# Load Elo ratings from cache (data/elo_ratings_cache.csv).
# If the cache doesn't exist, downloads fresh from eloratings.net and saves it.
# To refresh ratings, run fetch/fetch_elo_ratings.R (or update_data.bat).
# Returns a data frame with columns: Code, Name, Elo
get_elo_database <- function() {
  cache_path <- here("data", "ratings", "elo_ratings_cache.csv")

  if (file.exists(cache_path)) {
    db <- read.csv(cache_path, stringsAsFactors = FALSE)
    cat(sprintf("  Elo ratings loaded from cache (%s, fetched %s).\n",
                basename(cache_path),
                if ("fetched_at" %in% names(db)) db$fetched_at[1] else "unknown"))
    return(db[, c("Code", "Name", "Elo")])
  }

  cat("  No Elo cache found — downloading from eloratings.net...\n")
  teams_raw <- suppressWarnings(readLines("https://www.eloratings.net/en.teams.tsv"))
  teams_df  <- do.call(rbind, lapply(strsplit(teams_raw, "\t"), function(x) {
    if (length(x) >= 2) data.frame(Code = x[1], Name = x[2], stringsAsFactors = FALSE)
  }))
  world_raw <- suppressWarnings(readLines("https://www.eloratings.net/World.tsv"))
  world_df  <- do.call(rbind, lapply(strsplit(world_raw, "\t"), function(x) {
    if (length(x) >= 4) data.frame(Code = x[3], Elo = as.numeric(x[4]), stringsAsFactors = FALSE)
  }))
  db <- merge(teams_df, world_df, by = "Code")
  db$fetched_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  write.csv(db, cache_path, row.names = FALSE)
  cat(sprintf("  %d ratings downloaded and saved to cache.\n", nrow(db)))
  db[, c("Code", "Name", "Elo")]
}

# Look up a single team's Elo rating.
# Uses elo_name() for normalisation, then exact match, then partial grep fallback.
# Returns NA if no match found (caller should check).
get_elo <- function(team_name, elo_db) {
  key    <- elo_name(team_name)
  db_low <- tolower(elo_db$Name)

  # 1. Exact match after normalisation
  idx <- which(db_low == key)
  if (length(idx) > 0) return(elo_db$Elo[idx[1]])

  # 2. Partial match (normalised key appears anywhere in db name)
  idx <- grep(key, db_low, fixed = TRUE)
  if (length(idx) > 0) return(elo_db$Elo[idx[1]])

  # 3. First-word match (catches e.g. "Congo DR" when key is "congo dr")
  first_word <- strsplit(key, " ")[[1]][1]
  if (nchar(first_word) > 3) {
    idx <- grep(first_word, db_low, fixed = TRUE)
    if (length(idx) > 0) return(elo_db$Elo[idx[1]])
  }

  NA_real_
}
