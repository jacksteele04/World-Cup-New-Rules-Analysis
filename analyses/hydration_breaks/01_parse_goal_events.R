library(dplyr)
library(here)

# Parses the scorer string formats produced by the worldcup26.ir API into
# individual scorer tokens. Handles four formats:
#   {"J. Quiñones 9'","R. Jiménez 67'"}   <- single quotes around values
#   {""I.B. Hwang 67'"",""H.G. Oh 80'""}  <- doubled internal quotes
#   null                                   <- no goals
#   "F. Balogun 45'+5'(p)"                <- stoppage time / penalty suffix
parse_scorer_string <- function(s) {
  if (is.na(s) || trimws(s) == "null") return(character(0))
  s <- gsub("^\\{|\\}$", "", s)
  s <- gsub('""', '"', s)
  tokens <- strsplit(s, '","')[[1]]
  tokens <- gsub('^"|"$', "", tokens)
  tokens <- trimws(tokens)
  tokens[nchar(tokens) > 0]
}

# Extracts the base minute from a scorer token.
# "45'+5'" -> 45  (stoppage time stripped so goal bins to correct half)
extract_minute <- function(token) {
  m <- regmatches(token, regexpr("\\d+(?=(?:\\+\\d+)?')", token, perl = TRUE))
  if (length(m) == 0) return(NA_integer_)
  as.integer(m)
}

parse_goal_events <- function() {
  cat("⚽ Parsing goal events from clean_fixtures.csv...\n")

  df <- read.csv(here("data", "fixtures", "clean_fixtures.csv"), stringsAsFactors = FALSE)
  names(df) <- gsub("^games\\.", "", names(df))

  finished <- df %>% filter((finished == "TRUE" | finished == TRUE), type == "group")

  if (nrow(finished) == 0) {
    cat("⚠️  No finished group-stage matches found.\n")
    return(invisible(NULL))
  }

  events <- list()

  for (i in seq_len(nrow(finished))) {
    row <- finished[i, ]

    for (side in c("home", "away")) {
      raw    <- if (side == "home") row$home_scorers else row$away_scorers
      tokens <- parse_scorer_string(raw)

      for (tok in tokens) {
        minute <- extract_minute(tok)
        is_og  <- grepl("\\(OG\\)", tok, ignore.case = TRUE)
        is_pen <- grepl("\\(p\\)",  tok, ignore.case = TRUE)

        # Own goals flip which team is credited
        scoring_team <- if (is_og) {
          if (side == "home") "away" else "home"
        } else {
          side
        }

        events[[length(events) + 1]] <- data.frame(
          match_id     = as.integer(row$id),
          group        = row$group,
          matchday     = as.integer(row$matchday),
          home_team    = row$home_team_name_en,
          away_team    = row$away_team_name_en,
          scoring_team = scoring_team,
          minute_base  = minute,
          is_og        = is_og,
          is_penalty   = is_pen,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(events) == 0) {
    cat("⚠️  No goals parsed. Check scorer string format in clean_fixtures.csv.\n")
    return(invisible(NULL))
  }

  goal_events <- bind_rows(events) %>%
    filter(!is.na(minute_base)) %>%
    arrange(match_id, minute_base)

  write.csv(goal_events, here("data", "events", "goal_events.csv"), row.names = FALSE)
  cat(sprintf("✅ Parsed %d goal events across %d matches → data/goal_events.csv\n",
              nrow(goal_events), nrow(finished)))

  invisible(goal_events)
}

parse_goal_events()
