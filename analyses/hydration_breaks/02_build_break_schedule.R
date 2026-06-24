library(dplyr)
library(here)

# =============================================================================
# 02_build_break_schedule.R
# Builds data/hydration_breaks.csv by joining both fixture sources to create
# a unified match table with default break minutes (30' / 75').
#
# - match_id        : worldcup26.ir integer ID (joins with goal_events.csv)
# - thestatsapi_id  : TheStatsAPI match ID    (joins with shotmap_thestatsapi.csv)
# - break_minute_1h : default 30 — edit manually per match if actual minute known
# - break_minute_2h : default 75 — edit manually per match if actual minute known
#
# Running this again is safe — existing rows are preserved so manual edits
# to break minutes are not overwritten.
# =============================================================================

source(here("standardization", "team_names.R"))
source(here("standardization", "shot_metrics.R"))

OUT_FILE <- here("data", "events", "hydration_breaks.csv")

# -----------------------------------------------------------------------------
# Load both fixture sources
# -----------------------------------------------------------------------------

wc_file  <- here("data", "fixtures", "clean_fixtures.csv")
tsa_file <- here("data", "fixtures", "clean_fixtures_thestatsapi.csv")

if (!file.exists(wc_file))  stop("clean_fixtures.csv not found — run fetch/fetch_fixtures.R first.")
if (!file.exists(tsa_file)) stop("clean_fixtures_thestatsapi.csv not found — run fetch/fetch_fixtures_thestatsapi.R first.")

wc <- read.csv(wc_file,  stringsAsFactors = FALSE)
tsa <- read.csv(tsa_file, stringsAsFactors = FALSE)

# Standardise column names from worldcup26.ir source
wc <- wc %>%
  rename(
    match_id   = games.id,
    home_team  = games.home_team_name_en,
    away_team  = games.away_team_name_en,
    group      = games.group,
    matchday   = games.matchday,
    finished   = games.finished
  ) %>%
  mutate(match_id = as.integer(match_id)) %>%
  filter(games.type == "group" | grepl("^[A-L]$", group))  # group stage only

# Filter TheStatsAPI source to group stage (matchday 1–3)
tsa_gs <- tsa %>%
  filter(matchday %in% 1:3) %>%
  select(thestatsapi_id = match_id, home_team, away_team, group, matchday)

# Add normalised keys for joining
wc     <- wc     %>% mutate(hkey = norm_name(home_team), akey = norm_name(away_team))
tsa_gs <- tsa_gs %>% mutate(hkey = norm_name(home_team), akey = norm_name(away_team))

# Join on normalised team names
joined <- wc %>%
  select(match_id, group, matchday, home_team, away_team, hkey, akey) %>%
  left_join(tsa_gs %>% select(thestatsapi_id, hkey, akey),
            by = c("hkey", "akey")) %>%
  select(match_id, thestatsapi_id, group, matchday, home_team, away_team)

unmatched <- sum(is.na(joined$thestatsapi_id))
if (unmatched > 0) {
  cat(sprintf("⚠️  %d match(es) could not be linked to TheStatsAPI — shotmap data will be skipped for those.\n", unmatched))
}

# -----------------------------------------------------------------------------
# Upsert into hydration_breaks.csv
# -----------------------------------------------------------------------------

if (file.exists(OUT_FILE)) {
  existing <- read.csv(OUT_FILE, stringsAsFactors = FALSE, colClasses = "character")
  new_ids  <- joined$match_id[!as.character(joined$match_id) %in% existing$match_id]
  to_add   <- joined[joined$match_id %in% new_ids, ]
} else {
  existing <- NULL
  to_add   <- joined
}

# Coerce all ID columns to character to avoid bind_rows type conflicts
joined$match_id         <- as.character(joined$match_id)
joined$thestatsapi_id   <- as.character(joined$thestatsapi_id)
to_add$match_id         <- as.character(to_add$match_id)
to_add$thestatsapi_id   <- as.character(to_add$thestatsapi_id)
if (!is.null(existing)) {
  existing$match_id       <- as.character(existing$match_id)
  existing$thestatsapi_id <- as.character(existing$thestatsapi_id)
}

if (nrow(to_add) == 0) {
  cat(sprintf("✨ hydration_breaks.csv already has all %d group-stage matches.\n", nrow(joined)))
} else {
  to_add <- to_add %>%
    mutate(
      break_minute_1h = DEFAULT_BREAK_1H,
      break_minute_2h = DEFAULT_BREAK_2H
    )

  # Coerce all columns to character before binding to avoid type conflicts
  to_add[]   <- lapply(to_add,   as.character)
  if (!is.null(existing)) existing[] <- lapply(existing, as.character)

  combined <- bind_rows(existing, to_add) %>%
    arrange(as.integer(match_id))

  write.csv(combined, OUT_FILE, row.names = FALSE)
  cat(sprintf("✅ hydration_breaks.csv updated — %d new match(es) added (%d total).\n",
              nrow(to_add), nrow(combined)))
}

cat(sprintf("\nBreak minute defaults: 1st half = %d', 2nd half = %d'\n",
            DEFAULT_BREAK_1H, DEFAULT_BREAK_2H))
cat("Edit these per match in data/hydration_breaks.csv if the actual break minute is known.\n")
