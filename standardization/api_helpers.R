# =============================================================================
# standardization/api_helpers.R
#
# Shared HTTP helpers for TheStatsAPI. Source config.R before this file.
# Used by: all fetch/fetch_*_thestatsapi*.R scripts
# =============================================================================

if (!exists("API")) source(here("config.R"))

TSA_BASE_URL <- "https://api.thestatsapi.com/api"

get_api_key <- function() {
  key <- Sys.getenv("THESTATSAPI_KEY")
  if (nchar(key) == 0) stop("Set THESTATSAPI_KEY in .Renviron before running.")
  key
}

# Generic GET wrapper for TheStatsAPI. Pass query parameters via ...
# e.g. tsa_get("/football/matches", api_key, competition_id="comp_6107", per_page=100)
tsa_get <- function(path, api_key = get_api_key(), ...) {
  url  <- paste0(TSA_BASE_URL, path)
  resp <- GET(
    url,
    add_headers(Authorization = paste("Bearer", api_key)),
    query   = list(...),
    timeout(API$timeout_secs)
  )
  if (status_code(resp) == 401) stop("API key rejected (401) — check your THESTATSAPI_KEY.")
  if (status_code(resp) == 403) stop("Forbidden (403) — this endpoint may require a higher plan tier.")
  if (http_error(resp)) stop(sprintf("HTTP %d from %s", status_code(resp), url))
  fromJSON(content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
}
