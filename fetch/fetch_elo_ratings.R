library(here)

source(here("standardization", "validation.R"))

# =============================================================================
# fetch/fetch_elo_ratings.R
# Downloads national team Elo ratings from eloratings.net and caches to
# data/elo_ratings_cache.csv with a fetched_at timestamp.
#
# Run this as part of update_data.bat whenever you want fresh ratings.
# Analysis scripts (02_fetch_predictions.R, 04_analyze_gd_miracles.R,
# 07_quality_effect.R) read from the cache automatically via get_elo_database().
# =============================================================================

OUT_FILE <- here("data", "ratings", "elo_ratings_cache.csv")

cat("Downloading Elo ratings from eloratings.net...\n")

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

require_rows(db, 100, "Elo database")

write.csv(db, OUT_FILE, row.names = FALSE)
cat(sprintf("✅ %d national team ratings saved to %s\n", nrow(db), OUT_FILE))
