library(here)

run_step <- function(n, total, label, script) {
  cat(sprintf("\n[%d/%d] %s\n%s\n", n, total, label, strrep("-", 50)))
  tryCatch(
    source(here(script)),
    error = function(e) stop(sprintf("Script failed: %s\n  %s", script, conditionMessage(e)))
  )
  invisible(NULL)
}

cat("============================================\n")
cat(" World Cup 2026 -- Tiebreaker Analysis\n")
cat("============================================\n")

run_step(1, 6, "Running elimination simulator",    "analyses/tiebreaker_rules/01_elimination_simulator.R")
run_step(2, 6, "Fetching Poisson predictions",     "analyses/tiebreaker_rules/02_fetch_predictions.R")
run_step(3, 6, "Analyzing elimination likelihood", "analyses/tiebreaker_rules/03_elimination_likelihood.R")
run_step(4, 6, "Analyzing GD miracles",            "analyses/tiebreaker_rules/04_analyze_gd_miracles.R")
run_step(5, 6, "Group winner clinching simulator", "analyses/tiebreaker_rules/05_group_winner_simulator.R")
run_step(6, 6, "Clinching likelihood report",      "analyses/tiebreaker_rules/06_clinching_likelihood.R")

cat("\n============================================\n")
cat(" Done! Results in:\n")
cat("   data/standings/elimination_scenarios.csv\n")
cat("   data/standings/api_predictions_cache.csv\n")
cat("   data/standings/clinching_scenarios.csv\n")
cat("============================================\n")
