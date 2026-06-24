library(httr)
library(jsonlite)
library(dplyr)
library(here)

source(here("config.R"))
source(here("standardization", "validation.R"))
source(here("standardization", "api_helpers.R"))

# =============================================================================
# fetch_fixtures.R
# Pulls 2026 World Cup fixtures from TheStatsAPI and upserts into
# data/fixtures/clean_fixtures.csv
#
# Output schema:
#   match_id, matchday, date, group, type, home_team_id, away_team_id,
#   home_team_name_en, away_team_name_en, home_score, away_score, status, finished
#
# Requires: THESTATSAPI_KEY set in .Renviron
# IDs sourced from config.R: TOURNAMENT$thestatsapi_*
# =============================================================================

OUT_FILE <- here("data", "fixtures", "clean_fixtures.csv")

fetch_all_fixtures <- function(api_key) {
  cat("📡 Fetching 2026 World Cup fixtures from TheStatsAPI...\n")
  all_pages <- list()
  page      <- 1

  repeat {
    resp <- tsa_get("/football/matches", api_key,
      competition_id = TOURNAMENT$thestatsapi_comp_id,
      season_id      = TOURNAMENT$thestatsapi_season_id,
      per_page       = API$page_size,
      page           = page
    )

    batch <- resp$data
    if (is.null(batch) || length(batch) == 0) break

    all_pages[[page]] <- as.data.frame(batch)
    total_pages       <- resp$meta$total_pages
    cat(sprintf("  Page %d / %d — %d matches\n", page, total_pages, nrow(all_pages[[page]])))

    if (page >= total_pages) break
    page <- page + 1
    Sys.sleep(0.5)
  }

  if (length(all_pages) == 0) stop("No fixture data returned from API.")
  bind_rows(all_pages)
}

normalize_fixtures <- function(df) {
  pick <- function(preferred, fallback = NULL) {
    cols  <- c(preferred, fallback)
    hit   <- cols[cols %in% names(df)]
    if (length(hit) == 0) return(NA_character_)
    df[[hit[1]]]
  }

  out <- data.frame(
    match_id          = pick("id"),
    matchday          = pick("matchday"),
    date              = pick("utc_date", "date"),
    group             = pick(c("group_label", "group", "stage_name")),
    type              = "group",
    home_team_id      = pick("home_team.id"),
    away_team_id      = pick("away_team.id"),
    home_team_name_en = pick("home_team.name"),
    away_team_name_en = pick("away_team.name"),
    home_score        = pick("score.home"),
    away_score        = pick("score.away"),
    status            = pick("status"),
    stringsAsFactors  = FALSE
  )
  out$finished <- out$status == "finished"
  out
}

update_fixtures_csv <- function(new_fixtures) {
  if (is.null(new_fixtures) || nrow(new_fixtures) == 0) {
    cat("⚠️  No data to write.\n"); return()
  }

  if (file.exists(OUT_FILE)) {
    cat(sprintf("🔄 Existing %s found — checking for changes...\n", basename(OUT_FILE)))
    existing <- read.csv(OUT_FILE, stringsAsFactors = FALSE)

    new_fixtures[] <- lapply(new_fixtures, as.character)
    existing[]     <- lapply(existing, as.character)

    updated_or_new <- suppressMessages(anti_join(new_fixtures, existing))

    if (nrow(updated_or_new) > 0) {
      cat(sprintf("📝 %d updated or new matches found. Applying...\n", nrow(updated_or_new)))
      existing <- existing[!(existing$match_id %in% updated_or_new$match_id), ]
      combined <- bind_rows(existing, updated_or_new) %>% arrange(matchday, date)
      write.csv(combined, OUT_FILE, row.names = FALSE)
      cat(sprintf("✅ %s updated.\n", basename(OUT_FILE)))
    } else {
      cat("✨ Already up to date — no changes written.\n")
    }
  } else {
    cat(sprintf("🆕 Creating %s...\n", basename(OUT_FILE)))
    new_fixtures %>% arrange(matchday, date) %>%
      write.csv(OUT_FILE, row.names = FALSE)
    cat(sprintf("✅ Exported to: %s\n", OUT_FILE))
  }
}

api_key <- get_api_key()
raw     <- fetch_all_fixtures(api_key)
clean   <- normalize_fixtures(raw)

cat(sprintf("\n📊 %d total fixtures (%d finished, %d group stage matchday 1–3).\n",
            nrow(clean),
            sum(clean$finished, na.rm = TRUE),
            sum(clean$matchday %in% 1:3, na.rm = TRUE)))

update_fixtures_csv(clean)
