# =============================================================================
# standardization/validation.R
#
# Lightweight data validators for use at script entry points.
# Fail early with a clear message rather than propagating NAs silently.
# =============================================================================

# Stop if a file doesn't exist. hint = run-this-script message shown in error.
require_file <- function(path, hint = "") {
  if (!file.exists(path)) {
    msg <- sprintf("Missing required file: %s", path)
    if (nchar(hint) > 0) msg <- paste0(msg, "\n  → ", hint)
    stop(msg, call. = FALSE)
  }
  invisible(path)
}

# Stop if any expected columns are absent from a data frame.
require_cols <- function(df, cols, label = deparse(substitute(df))) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0)
    stop(sprintf("%s is missing columns: %s", label, paste(missing, collapse = ", ")),
         call. = FALSE)
  invisible(df)
}

# Warn (not stop) if a data frame has fewer rows than expected.
require_rows <- function(df, min_n, label = deparse(substitute(df))) {
  if (nrow(df) < min_n)
    warning(sprintf("%s has only %d rows (expected >= %d)", label, nrow(df), min_n),
            call. = FALSE)
  invisible(df)
}
