library(here)

rscript <- file.path(R.home("bin"), "Rscript")

run_step <- function(n, total, label, script) {
  cat(sprintf("\n[%d/%d] %s\n%s\n", n, total, label, strrep("-", 50)))
  ret <- system2(rscript, args = here(script))
  if (ret != 0) stop(sprintf("Script failed (exit %d): %s", ret, script))
  invisible(ret)
}

cat("============================================\n")
cat(" World Cup 2026 -- Tiebreaker Analysis\n")
cat("============================================\n")

run_step(1, 4, "Running elimination simulator",    "analyses/tiebreaker_rules/01_elimination_simulator.R")
run_step(2, 4, "Fetching Poisson predictions",     "analyses/tiebreaker_rules/02_fetch_predictions.R")
run_step(3, 4, "Analyzing elimination likelihood", "analyses/tiebreaker_rules/03_elimination_likelihood.R")
run_step(4, 4, "Analyzing GD miracles",            "analyses/tiebreaker_rules/04_analyze_gd_miracles.R")

cat("\n============================================\n")
cat(" Done! Results in:\n")
cat("   data/standings/elimination_scenarios.csv\n")
cat("   data/standings/api_predictions_cache.csv\n")
cat("============================================\n")
