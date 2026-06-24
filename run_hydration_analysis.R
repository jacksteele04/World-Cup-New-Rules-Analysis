library(here)

rscript <- file.path(R.home("bin"), "Rscript")

run_step <- function(n, total, label, script) {
  cat(sprintf("\n[%d/%d] %s\n%s\n", n, total, label, strrep("-", 50)))
  ret <- system2(rscript, args = here(script))
  if (ret != 0) stop(sprintf("Script failed (exit %d): %s", ret, script))
  invisible(ret)
}

cat("============================================\n")
cat(" World Cup 2026 -- Hydration Break Analysis\n")
cat("============================================\n")

run_step(1, 3, "Parsing goal events",      "analyses/hydration_breaks/01_parse_goal_events.R")
run_step(2, 3, "Building break schedule",  "analyses/hydration_breaks/02_build_break_schedule.R")
run_step(3, 3, "Running momentum analysis","analyses/hydration_breaks/hydration_momentum.R")

cat("\n============================================\n")
cat(" Done!\n")
cat("============================================\n")
