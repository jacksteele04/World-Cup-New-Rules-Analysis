library(dplyr)
library(here)

# =============================================================================
# 02_build_break_schedule.R
# Builds data/events/hydration_breaks.csv from clean_fixtures.csv.
#
# Both match_id and thestatsapi_id point to the same TheStatsAPI ID since
# clean_fixtures.csv now uses TheStatsAPI as its source.
#
# - match_id        : TheStatsAPI match ID (joins with shotmap_thestatsapi.csv)
# - thestatsapi_id  : same as match_id (kept for backward compatibility)
# - break_minute_1h : default 30 — edit manually per match if actual minute known
# - break_minute_2h : default 75 — edit manually per match if actual minute known
#
# Running this again is safe — existing rows are preserved so manual edits
# to break minutes are not overwritten.
# =============================================================================

source(here("config.R"))
source(here("standardization", "shot_metrics.R"))
source(here("standardization", "validation.R"))

OUT_FILE <- here("data", "events", "hydration_breaks.csv")
fx_file  <- here("data", "fixtures", "clean_fixtures.csv")

require_file(fx_file, "run fetch/fetch_fixtures.R first")
fx <- read.csv(fx_file, stringsAsFactors = FALSE)
require_cols(fx, c("match_id", "matchday", "group", "home_team_name_en", "away_team_name_en",
                   "type", "finished"), basename(fx_file))

# Filter to group stage
gs <- fx %>%
  filter(type == "group" | grepl("^[A-L]$", group)) %>%
  filter(matchday %in% 1:3) %>%
  mutate(
    match_id       = as.character(match_id),
    thestatsapi_id = match_id,
    home_team      = home_team_name_en,
    away_team      = away_team_name_en
  ) %>%
  select(match_id, thestatsapi_id, group, matchday, home_team, away_team)

cat(sprintf("📋 %d group-stage matches found in clean_fixtures.csv.\n", nrow(gs)))

# Upsert — preserve existing rows so manual break-minute edits are not lost
if (file.exists(OUT_FILE)) {
  existing <- read.csv(OUT_FILE, stringsAsFactors = FALSE, colClasses = "character")
  new_ids  <- gs$match_id[!gs$match_id %in% existing$match_id]
  to_add   <- gs[gs$match_id %in% new_ids, ]
} else {
  existing <- NULL
  to_add   <- gs
}

if (nrow(to_add) == 0) {
  cat(sprintf("✨ hydration_breaks.csv already has all %d group-stage matches.\n", nrow(gs)))
} else {
  to_add <- to_add %>%
    mutate(
      break_minute_1h = as.character(DEFAULT_BREAK_1H),
      break_minute_2h = as.character(DEFAULT_BREAK_2H)
    )

  to_add[]   <- lapply(to_add,   as.character)
  if (!is.null(existing)) existing[] <- lapply(existing, as.character)

  combined <- bind_rows(existing, to_add) %>% arrange(as.integer(matchday))
  write.csv(combined, OUT_FILE, row.names = FALSE)
  cat(sprintf("✅ hydration_breaks.csv updated — %d new match(es) added (%d total).\n",
              nrow(to_add), nrow(combined)))
}

cat(sprintf("\nBreak minute defaults: 1st half = %d', 2nd half = %d'\n",
            DEFAULT_BREAK_1H, DEFAULT_BREAK_2H))
cat("Edit these per match in data/events/hydration_breaks.csv if the actual break minute is known.\n")
