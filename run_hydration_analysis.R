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
cat(" World Cup 2026 -- Hydration Break Analysis\n")
cat("============================================\n")

run_step(1, 7, "Building break schedule",        "analyses/hydration_breaks/02_build_break_schedule.R")
run_step(2, 7, "Running momentum analysis",      "analyses/hydration_breaks/hydration_momentum.R")
run_step(3, 7, "2022 comparison",                "analyses/hydration_breaks/03_compare_2022_2026.R")
run_step(4, 7, "Matchday 1 shot rate graphic",   "analyses/hydration_breaks/05_matchday1_graphic.R")
run_step(5, 7, "Matchday 1 all metrics graphic", "analyses/hydration_breaks/06_matchday1_metrics_graphic.R")
run_step(6, 7, "Quality effect graphic",         "analyses/hydration_breaks/07_quality_effect.R")
run_step(7, 7, "±5-min break window graphic",    "analyses/hydration_breaks/08_break_window_graphic.R")

cat("\n============================================\n")
cat(" Done! Plots saved to plots/\n")
cat("============================================\n")
