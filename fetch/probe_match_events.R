library(httr)
library(jsonlite)
library(dplyr)
library(here)

source(here("config.R"))
source(here("standardization", "api_helpers.R"))

# =============================================================================
# probe_match_events.R
# One-shot probe: tries known TheStatsAPI event/timeline endpoints on a single
# finished match and prints everything useful — structure, field names, and any
# event types that look like breaks/stoppages.
# =============================================================================

# Pick the first finished match from the cached fixtures
fixtures <- read.csv(here("data", "fixtures", "clean_fixtures_thestatsapi.csv"),
                     stringsAsFactors = FALSE)
finished <- fixtures[fixtures$status == "finished", ]
if (nrow(finished) == 0) stop("No finished matches found in clean_fixtures_thestatsapi.csv.")

probe_id   <- finished$match_id[1]
probe_home <- finished$home_team[1]
probe_away <- finished$away_team[1]
api_key    <- get_api_key()

cat(sprintf("Probing match: %s vs %s  (id: %s)\n\n", probe_home, probe_away, probe_id))

# Endpoints to try — add more here if you find others in the API docs
endpoints <- c(
  "events",
  "timeline",
  "incidents",
  "commentary"
)

for (ep in endpoints) {
  path <- sprintf("/football/matches/%s/%s", probe_id, ep)
  cat(sprintf("--- %s ---\n", path))

  result <- tryCatch(
    tsa_get(path, api_key),
    error = function(e) { cat(sprintf("  ERROR: %s\n\n", e$message)); NULL }
  )

  if (is.null(result)) next

  # Print top-level keys
  cat(sprintf("  Top-level keys: %s\n", paste(names(result), collapse = ", ")))

  data <- result$data
  if (is.null(data) || length(data) == 0) {
    cat("  data: empty or null\n\n")
    next
  }

  # Coerce to data frame if possible
  df <- tryCatch(as.data.frame(data), error = function(e) NULL)

  if (!is.null(df) && nrow(df) > 0) {
    cat(sprintf("  Rows: %d  |  Columns: %s\n", nrow(df), paste(names(df), collapse = ", ")))

    # Print unique values of any column that looks like a type/category field
    type_cols <- grep("type|kind|category|event|incident|name", names(df),
                      value = TRUE, ignore.case = TRUE)
    for (col in type_cols) {
      vals <- sort(unique(as.character(df[[col]])))
      cat(sprintf("  %s unique values (%d): %s\n", col, length(vals),
                  paste(head(vals, 20), collapse = ", ")))
    }

    # Look for anything break-related in all character columns
    char_cols <- names(df)[sapply(df, is.character)]
    for (col in char_cols) {
      break_rows <- df[grepl("break|cool|hydrat|water|stop", df[[col]],
                             ignore.case = TRUE), ]
      if (nrow(break_rows) > 0) {
        cat(sprintf("  ** Break-related rows in column '%s':\n", col))
        print(break_rows)
      }
    }

    # Show first 3 rows for a general sense of structure
    cat("  First 3 rows:\n")
    print(head(df, 3))
  } else {
    cat("  data is not a rectangular data frame — raw structure:\n")
    str(data, max.level = 2)
  }

  cat("\n")
  Sys.sleep(2)
}

cat("Probe complete.\n")
