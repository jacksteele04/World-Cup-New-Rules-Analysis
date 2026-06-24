library(httr)
library(jsonlite)
library(dplyr)
library(here)

source(here("config.R"))
source(here("standardization", "api_helpers.R"))

# =============================================================================
# fetch_fixtures_thestatsapi.R
# Pulls 2026 World Cup fixtures from TheStatsAPI and upserts into
# data/clean_fixtures_thestatsapi.csv
#
# Requires: THESTATSAPI_KEY set in .Renviron
# =============================================================================

OUT_FILE <- here("data", "fixtures", "clean_fixtures_thestatsapi.csv")

# -----------------------------------------------------------------------------
# Step 1 — Resolve World Cup competition + season IDs
# -----------------------------------------------------------------------------

find_worldcup_ids <- function(api_key) {
  cat("🔍 Looking up World Cup competition...\n")
  resp  <- tsa_get("/football/competitions", api_key, search = "World Cup", per_page = API$page_size)
  comps <- as.data.frame(resp$data)

  wc <- comps[grepl("FIFA World Cup", comps$name, ignore.case = TRUE), ]
  if (nrow(wc) == 0) wc <- comps[grepl("World Cup", comps$name, ignore.case = TRUE), ]
  if (nrow(wc) == 0) {
    cat("Available competitions:\n"); print(comps[, c("id", "name")]); stop("World Cup not found.")
  }

  comp_id   <- wc$id[1]
  comp_name <- wc$name[1]
  cat(sprintf("✅ Found: %s (id = %s)\n", comp_name, comp_id))

  # Resolve current/latest season for this competition
  seasons_resp <- tsa_get(sprintf("/football/competitions/%s/seasons", comp_id), api_key, per_page = 5)
  seasons <- as.data.frame(seasons_resp$data)

  # Pick 2026 season, fall back to first result
  sn <- seasons[grepl("2026", seasons$name, ignore.case = TRUE), ]
  if (nrow(sn) == 0) sn <- seasons[1, ]
  season_id <- sn$id[1]
  cat(sprintf("✅ Season: %s (id = %s)\n", sn$name[1], season_id))

  list(comp_id = comp_id, season_id = season_id)
}

# -----------------------------------------------------------------------------
# Step 2 — Fetch all matches (paginated)
# -----------------------------------------------------------------------------

fetch_all_fixtures <- function(comp_id, season_id, api_key) {
  cat("📡 Fetching all World Cup fixtures from TheStatsAPI...\n")
  all_pages <- list()
  page      <- 1

  repeat {
    resp <- tsa_get("/football/matches", api_key,
      competition_id = comp_id,
      season_id      = season_id,
      per_page       = API$page_size,
      page           = page
    )

    batch <- resp$data
    if (is.null(batch) || length(batch) == 0) break

    all_pages[[page]] <- as.data.frame(batch)
    total_pages <- resp$meta$total_pages
    cat(sprintf("  Page %d / %d — %d matches\n", page, total_pages, nrow(all_pages[[page]])))

    if (page >= total_pages) break
    page <- page + 1
    Sys.sleep(0.5)
  }

  if (length(all_pages) == 0) stop("No fixture data returned from API.")
  bind_rows(all_pages)
}

# -----------------------------------------------------------------------------
# Step 3 — Normalise to a clean, consistent schema
# -----------------------------------------------------------------------------

normalize_fixtures <- function(df) {
  # Build a select list using only columns that actually exist in the response
  pick <- function(preferred, fallback = NULL) {
    cols <- c(preferred, fallback)
    match <- cols[cols %in% names(df)]
    if (length(match) == 0) return(NA_character_)
    df[[match[1]]]
  }

  data.frame(
    match_id       = pick("id"),
    competition_id = pick("competition_id"),
    season_id      = pick("season_id"),
    matchday       = pick("matchday"),
    date           = pick("utc_date", "date"),
    status         = pick("status"),
    group          = pick("group", "stage.name"),
    home_team_id   = pick("home_team.id"),
    away_team_id   = pick("away_team.id"),
    home_team      = pick("home_team.name"),
    away_team      = pick("away_team.name"),
    home_score     = pick("score.home"),
    away_score     = pick("score.away"),
    xg_available   = pick("xg_available"),
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# Step 4 — Upsert into CSV (same logic as fetch_fixtures.R)
# -----------------------------------------------------------------------------

update_fixtures_csv <- function(new_fixtures, file_path = OUT_FILE) {
  if (is.null(new_fixtures) || nrow(new_fixtures) == 0) {
    cat("⚠️  No data to write.\n"); return()
  }

  if (file.exists(file_path)) {
    cat(sprintf("🔄 Existing %s found — checking for changes...\n", basename(file_path)))
    existing <- read.csv(file_path, stringsAsFactors = FALSE)

    # Coerce all columns to character for safe comparison
    new_fixtures[] <- lapply(new_fixtures, as.character)
    existing[]     <- lapply(existing, as.character)

    updated_or_new <- suppressMessages(anti_join(new_fixtures, existing))

    if (nrow(updated_or_new) > 0) {
      cat(sprintf("📝 %d updated or new matches found. Applying...\n", nrow(updated_or_new)))
      existing  <- existing[!(existing$match_id %in% updated_or_new$match_id), ]
      combined  <- bind_rows(existing, updated_or_new)
      combined  <- combined %>% arrange(as.numeric(match_id))
      write.csv(combined, file_path, row.names = FALSE)
      cat(sprintf("✅ %s updated.\n", file_path))
    } else {
      cat("✨ Already up to date — no changes written.\n")
    }

  } else {
    cat(sprintf("🆕 Creating %s...\n", file_path))
    new_fixtures <- new_fixtures %>% arrange(as.numeric(match_id))
    write.csv(new_fixtures, file_path, row.names = FALSE)
    cat(sprintf("✅ Exported to: %s\n", file_path))
  }
}

# -----------------------------------------------------------------------------
# Run
# -----------------------------------------------------------------------------

api_key <- get_api_key()
ids      <- find_worldcup_ids(api_key)
raw      <- fetch_all_fixtures(ids$comp_id, ids$season_id, api_key)
clean    <- normalize_fixtures(raw)

cat(sprintf("\n📊 %d total fixtures fetched (%d finished).\n",
            nrow(clean),
            sum(clean$status == "finished", na.rm = TRUE)))

update_fixtures_csv(clean, OUT_FILE)
