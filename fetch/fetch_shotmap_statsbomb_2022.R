library(httr)
library(jsonlite)
library(dplyr)
library(here)

source(here("config.R"))

# =============================================================================
# fetch_shotmap_statsbomb_2022.R
# Downloads 2022 World Cup shot data from StatsBomb open data (GitHub).
# No API key required — data is publicly available under open licence.
#
# Source: https://github.com/statsbomb/open-data
# Output: data/shotmap_statsbomb_2022.csv
# =============================================================================

.sb <- HISTORICAL$qatar_2022
MATCHES_URL <- sprintf(
  "https://raw.githubusercontent.com/statsbomb/open-data/master/data/matches/%d/%d.json",
  .sb$statsbomb_comp_id, .sb$statsbomb_season_id
)
EVENTS_URL <- "https://raw.githubusercontent.com/statsbomb/open-data/master/data/events/%s.json"
OUT_FILE   <- here("data", "shotmaps", "shotmap_statsbomb_2022.csv")

sb_get <- function(url) {
  resp <- GET(url, timeout(30))
  if (http_error(resp)) stop(sprintf("HTTP %d fetching %s", status_code(resp), url))
  fromJSON(content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
}

# -----------------------------------------------------------------------------
# Step 1 — Get group-stage match list
# -----------------------------------------------------------------------------

cat("📡 Fetching 2022 World Cup match list from StatsBomb...\n")
matches_raw <- sb_get(MATCHES_URL)
matches     <- as.data.frame(matches_raw)

gs <- matches[matches$competition_stage.name == "Group Stage", ]
cat(sprintf("✅ %d group-stage matches found.\n\n", nrow(gs)))

# Load existing cache to skip already-fetched matches
if (file.exists(OUT_FILE)) {
  cache    <- read.csv(OUT_FILE, stringsAsFactors = FALSE)
  done_ids <- unique(as.character(cache$match_id))
} else {
  cache    <- NULL
  done_ids <- character(0)
}

pending <- gs[!as.character(gs$match_id) %in% done_ids, ]
cat(sprintf("📊 %d group matches — %d cached, %d to fetch\n",
            nrow(gs), length(done_ids), nrow(pending)))

if (nrow(pending) == 0) {
  cat("✨ Already up to date.\n")
  quit(save = "no", status = 0)
}

# -----------------------------------------------------------------------------
# Step 2 — Download events and extract shots for each match
# -----------------------------------------------------------------------------

new_shots <- list()

for (i in seq_len(nrow(pending))) {
  row <- pending[i, ]
  mid <- row$match_id

  home <- if ("home_team.home_team_name" %in% names(row)) row[["home_team.home_team_name"]] else
          if ("home_team"                %in% names(row)) row[["home_team"]]                else "?"
  away <- if ("away_team.away_team_name" %in% names(row)) row[["away_team.away_team_name"]] else
          if ("away_team"                %in% names(row)) row[["away_team"]]                else "?"

  home_id <- if ("home_team.home_team_id" %in% names(row)) row[["home_team.home_team_id"]] else NA
  away_id <- if ("away_team.away_team_id" %in% names(row)) row[["away_team.away_team_id"]] else NA

  cat(sprintf("  [%d/%d] %s vs %s...", i, nrow(pending), home, away))

  events <- tryCatch(sb_get(sprintf(EVENTS_URL, mid)),
                     error = function(e) { cat(sprintf(" ❌ %s\n", e$message)); NULL })
  if (is.null(events)) next

  # Filter to shot events
  shots_raw <- events[events$type.name == "Shot", ]

  if (nrow(shots_raw) == 0) {
    cat(" (0 shots)\n"); next
  }

  shots <- data.frame(
    match_id       = mid,
    home_team      = home,
    away_team      = away,
    home_team_id   = home_id,
    away_team_id   = away_id,
    minute         = shots_raw$minute,
    team_id        = shots_raw$team.id,
    team_name      = shots_raw$team.name,
    player_name    = if ("player.name" %in% names(shots_raw)) shots_raw$player.name else NA,
    expected_goals = if ("shot.statsbomb_xg" %in% names(shots_raw)) shots_raw$shot.statsbomb_xg else NA,
    result         = if ("shot.outcome.name" %in% names(shots_raw)) shots_raw$shot.outcome.name else NA,
    stringsAsFactors = FALSE
  )

  shots$is_on_target <- shots$result %in% c("Goal", "Saved")

  new_shots[[length(new_shots) + 1]] <- shots
  cat(sprintf(" ✅ %d shots\n", nrow(shots)))

  if (i < nrow(pending)) Sys.sleep(0.5)
}

# -----------------------------------------------------------------------------
# Step 3 — Write cache
# -----------------------------------------------------------------------------

if (length(new_shots) > 0) {
  combined <- bind_rows(cache, bind_rows(new_shots))
  write.csv(combined, OUT_FILE, row.names = FALSE)
  cat(sprintf("\n✅ %s written — %d shot records across %d matches.\n",
              basename(OUT_FILE), nrow(combined), length(unique(combined$match_id))))
} else {
  cat("⚠️  No shot data retrieved.\n")
}
