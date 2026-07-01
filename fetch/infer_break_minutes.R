library(dplyr)
library(here)

source(here("config.R"))
source(here("standardization", "validation.R"))
source(here("standardization", "shot_metrics.R"))  # for DEFAULT_BREAK_*

# =============================================================================
# fetch/infer_break_minutes.R
#
# Infers the hydration break minute per match using a narrow target window.
#
# FIFA mandates the 1H break around minutes 24-27 and the 2H break around
# minutes 68-71. For each match:
#   1. Look for a clear shot gap (>= MIN_GAP) within the target window.
#      If found, the break minute is the start of that gap.
#   2. If shots fall inside the window but no clear gap exists, use the
#      window midpoint (26 / 70) so those shots are correctly assigned to
#      either side of the break.
#   3. If no shots exist in the window at all, also use the midpoint — it
#      is a better estimate than the old hardcoded 30 / 75.
#   4. If the match has NO shot data at all (not yet played / fetched),
#      the current value is left unchanged.
#
# Re-running this script overwrites all previously inferred values for
# matches that have shot data. To preserve a manual edit, run
# 02_build_break_schedule.R first to reset to defaults, make your manual
# edit, then skip re-running this script for that match.
#
# Requires:
#   data/shotmaps/shotmap_thestatsapi.csv   (fetch/fetch_shotmap_thestatsapi.R)
#   data/events/hydration_breaks.csv        (analyses/hydration_breaks/02_build_break_schedule.R)
# =============================================================================

WINDOW_1H   <- c(24L, 27L)
WINDOW_2H   <- c(68L, 71L)
MIDPOINT_1H <- 26L   # ceiling((24+27)/2)
MIDPOINT_2H <- 70L   # ceiling((68+71)/2)
MIN_GAP     <- 2L    # minimum diff between consecutive shot minutes to count as a gap

shots_file  <- here("data", "shotmaps", "shotmap_thestatsapi.csv")
breaks_file <- here("data", "events", "hydration_breaks.csv")

require_file(shots_file,  "run fetch/fetch_shotmap_thestatsapi.R first")
require_file(breaks_file, "run analyses/hydration_breaks/02_build_break_schedule.R first")

shots  <- read.csv(shots_file,  stringsAsFactors = FALSE)
breaks <- read.csv(breaks_file, stringsAsFactors = FALSE)

require_cols(shots,  c("match_id", "minute"), "shotmap_thestatsapi.csv")
require_cols(breaks, c("match_id", "break_minute_1h", "break_minute_2h"), "hydration_breaks.csv")

shots$minute   <- as.integer(shots$minute)
shots$match_id <- as.character(shots$match_id)
breaks$match_id <- as.character(breaks$match_id)

# Set of match IDs that have any shot data
matches_with_shots <- unique(shots$match_id)

# -----------------------------------------------------------------------------
# find_break()
#
# Returns the best break minute estimate for one half of one match.
#   - If a gap >= MIN_GAP exists in the window: start of that gap + 1
#   - Otherwise (shots scattered or window empty): midpoint
# Also returns method and gap size for reporting.
# -----------------------------------------------------------------------------
find_break <- function(shot_minutes, lo, hi, midpoint) {
  mins <- sort(unique(shot_minutes[shot_minutes >= lo & shot_minutes <= hi]))

  if (length(mins) < 2) {
    return(list(minute = midpoint, gap = NA_integer_,
                n_shots = length(mins), method = "midpoint"))
  }

  gaps <- diff(mins)
  best_gap <- max(gaps)
  best_idx <- which.max(gaps)

  if (best_gap >= MIN_GAP) {
    return(list(
      minute  = mins[best_idx] + 1L,
      gap     = as.integer(best_gap),
      n_shots = length(mins),
      method  = "gap"
    ))
  }

  # Shots scattered through window — midpoint splits them correctly
  return(list(minute = midpoint, gap = as.integer(best_gap),
              n_shots = length(mins), method = "midpoint (shots in window)"))
}

# -----------------------------------------------------------------------------
# Main loop
# -----------------------------------------------------------------------------

cat(sprintf(
  "Inferring break minutes\n  1H window: %d-%d  midpoint: %d'  |  2H window: %d-%d  midpoint: %d'  |  MIN_GAP: %d'\n\n",
  WINDOW_1H[1], WINDOW_1H[2], MIDPOINT_1H,
  WINDOW_2H[1], WINDOW_2H[2], MIDPOINT_2H, MIN_GAP
))

result_rows <- vector("list", nrow(breaks) * 2L)
result_idx  <- 0L

update_half <- function(i, half) {
  brk_col  <- if (half == 1) "break_minute_1h" else "break_minute_2h"
  window   <- if (half == 1) WINDOW_1H         else WINDOW_2H
  midpoint <- if (half == 1) MIDPOINT_1H       else MIDPOINT_2H
  label    <- if (half == 1) "1H"              else "2H"

  cur <- suppressWarnings(as.integer(breaks[[brk_col]][i]))
  mid <- breaks$match_id[i]
  match_label <- sprintf("%s vs %s", breaks$home_team[i], breaks$away_team[i])

  result_idx <<- result_idx + 1L

  if (!mid %in% matches_with_shots) {
    result_rows[[result_idx]] <<- list(
      match = match_label, half = label,
      old = cur, new = cur, gap = NA_integer_, n_shots = 0L,
      method = "—", action = "no shot data — unchanged"
    )
    return(invisible(NULL))
  }

  ms <- shots$minute[shots$match_id == mid]
  r  <- find_break(ms, window[1], window[2], midpoint)

  breaks[[brk_col]][i] <<- r$minute
  result_rows[[result_idx]] <<- list(
    match = match_label, half = label,
    old = cur, new = r$minute, gap = r$gap, n_shots = r$n_shots,
    method = r$method,
    action = if (r$minute == cur) "unchanged" else "updated"
  )
}

for (i in seq_len(nrow(breaks))) {
  update_half(i, 1)
  update_half(i, 2)
}

results <- bind_rows(result_rows[seq_len(result_idx)])

# -----------------------------------------------------------------------------
# Print results table
# -----------------------------------------------------------------------------

cat(sprintf("%-35s  %2s  %3s -> %3s  %4s  %-30s  %s\n",
            "Match", "Hf", "Old", "New", "Gap", "Method", "Action"))
cat(strrep("-", 95), "\n")
for (j in seq_len(nrow(results))) {
  r <- results[j, ]
  cat(sprintf("%-35s  %2s  %3s -> %3s  %4s  %-30s  %s\n",
              substr(r$match, 1, 35), r$half,
              ifelse(is.na(r$old), " NA", r$old),
              ifelse(is.na(r$new), " NA", r$new),
              ifelse(is.na(r$gap), "  NA", sprintf("%2d'", r$gap)),
              r$method,
              r$action))
}

updated  <- results[results$action == "updated",       ]
same     <- results[results$action == "unchanged",     ]
no_data  <- results[grepl("no shot data", results$action), ]

cat(sprintf("\n%s\n", strrep("=", 95)))
cat(sprintf("  Updated  : %d half-matches (value changed)\n",      nrow(updated)))
cat(sprintf("  Unchanged: %d half-matches (new estimate = old)\n", nrow(same)))
cat(sprintf("  No data  : %d half-matches (not played/fetched)\n", nrow(no_data)))
cat(sprintf("  Methods  — gap: %d  |  midpoint: %d  |  midpoint (shots in window): %d\n",
            sum(results$method == "gap", na.rm = TRUE),
            sum(results$method == "midpoint", na.rm = TRUE),
            sum(results$method == "midpoint (shots in window)", na.rm = TRUE)))
cat(sprintf("%s\n", strrep("=", 95)))

# -----------------------------------------------------------------------------
# Write back
# -----------------------------------------------------------------------------

breaks[] <- lapply(breaks, as.character)
write.csv(breaks, breaks_file, row.names = FALSE)
cat(sprintf("\n✅ %s updated.\n", breaks_file))
