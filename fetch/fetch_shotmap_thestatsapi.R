library(httr)
library(jsonlite)
library(dplyr)
library(here)

source(here("config.R"))
source(here("standardization", "api_helpers.R"))

# =============================================================================
# fetch_shotmap_thestatsapi.R
# Fetches per-shot xG data for every finished match from TheStatsAPI.
# Caches results in data/shotmap_thestatsapi.csv â€” skips already-fetched matches.
# =============================================================================

OUT_FILE <- here("data", "shotmaps", "shotmap_thestatsapi.csv")

fetch_shotmap <- function(match_id, home_team, away_team, api_key) {
  resp <- tsa_get(sprintf("/football/matches/%s/shotmap", match_id), api_key)
  if (is.null(resp$data) || length(resp$data) == 0) return(NULL)

  shots <- as.data.frame(resp$data)
  shots$match_id  <- match_id
  shots$home_team <- home_team
  shots$away_team <- away_team
  shots
}

# -----------------------------------------------------------------------------
# Run
# -----------------------------------------------------------------------------

api_key  <- get_api_key()
fixtures <- read.csv(here("data", "fixtures", "clean_fixtures_thestatsapi.csv"), stringsAsFactors = FALSE)
finished <- fixtures[fixtures$status == "finished", ]

# Load existing cache
if (file.exists(OUT_FILE)) {
  cache    <- read.csv(OUT_FILE, stringsAsFactors = FALSE)
  done_ids <- unique(as.character(cache$match_id))
} else {
  cache    <- NULL
  done_ids <- character(0)
}

pending <- finished[!as.character(finished$match_id) %in% done_ids, ]

cat(sprintf("ðŸ“Š %d finished matches â€” %d cached, %d to fetch\n",
            nrow(finished), length(done_ids), nrow(pending)))

if (nrow(pending) == 0) {
  cat("âœ¨ Already up to date â€” no new shots to fetch.\n")
  }

new_shots <- list()

for (i in seq_len(nrow(pending))) {
  row  <- pending[i, ]
  cat(sprintf("  [%d/%d] %s vs %s (%s)...",
              i, nrow(pending), row$home_team, row$away_team, row$match_id))

  shots <- tryCatch(
    fetch_shotmap(row$match_id, row$home_team, row$away_team, api_key),
    error = function(e) { cat(sprintf(" âŒ %s\n", e$message)); NULL }
  )

  if (!is.null(shots)) {
    new_shots[[length(new_shots) + 1]] <- shots
    cat(sprintf(" âœ… %d shots\n", nrow(shots)))
  }

  if (i < nrow(pending)) Sys.sleep(API$sleep_between)
}

if (length(new_shots) > 0) {
  combined <- bind_rows(cache, bind_rows(new_shots))
  write.csv(combined, OUT_FILE, row.names = FALSE)
  cat(sprintf("\nâœ… %s updated â€” %d new shot records added.\n",
              basename(OUT_FILE), nrow(bind_rows(new_shots))))
} else {
  cat("âš ï¸  No new shot data retrieved.\n")
}
