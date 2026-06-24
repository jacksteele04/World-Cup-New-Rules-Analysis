library(here)

rscript <- file.path(R.home("bin"), "Rscript")

run_step <- function(n, total, label, script) {
  cat(sprintf("\n[%d/%d] %s\n%s\n", n, total, label, strrep("-", 50)))
  ret <- system2(rscript, args = here(script))
  if (ret != 0) stop(sprintf("Script failed (exit %d): %s", ret, script))
  invisible(ret)
}

cat("============================================\n")
cat(" World Cup 2026 -- Data Update\n")
cat("============================================\n")

run_step(1, 8, "Fetching latest fixtures (worldcup26.ir)",       "fetch/fetch_fixtures.R")
run_step(2, 8, "Calculating group standings",                     "fetch/calculate_groups.R")
run_step(3, 8, "Fetching latest fixtures (TheStatsAPI)",          "fetch/fetch_fixtures_thestatsapi.R")
run_step(4, 8, "Fetching shot data (TheStatsAPI)",                "fetch/fetch_shotmap_thestatsapi.R")
run_step(5, 8, "Parsing goal events",                             "analyses/hydration_breaks/01_parse_goal_events.R")
run_step(6, 8, "Fetching Elo ratings",                            "fetch/fetch_elo_ratings.R")
run_step(7, 8, "Fetching 2022 WC fixtures (TheStatsAPI)",         "fetch/fetch_fixtures_thestatsapi_2022.R")
run_step(8, 8, "Fetching 2022 shot data (StatsBomb open data)",   "fetch/fetch_shotmap_statsbomb_2022.R")

cat("\n============================================\n")
cat(" Done! Data files updated:\n")
cat("   data/fixtures/clean_fixtures.csv\n")
cat("   data/standings/group_standings_R.csv\n")
cat("   data/fixtures/clean_fixtures_thestatsapi.csv\n")
cat("   data/shotmaps/shotmap_thestatsapi.csv\n")
cat("   data/events/goal_events.csv\n")
cat("   data/ratings/elo_ratings_cache.csv\n")
cat("   data/fixtures/clean_fixtures_thestatsapi_2022.csv\n")
cat("   data/shotmaps/shotmap_statsbomb_2022.csv\n")
cat("============================================\n")
