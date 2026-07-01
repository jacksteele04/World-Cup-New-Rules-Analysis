library(dplyr)
library(here)

source(here("standardization", "validation.R"))

calculate_group_standings <- function(csv_file) {
  cat(sprintf("📊 Reading match data from %s...\n", csv_file))

  require_file(csv_file, "run fetch/fetch_fixtures.R first")
  parsed_df <- read.csv(csv_file, stringsAsFactors = FALSE)

  require_cols(parsed_df, c("type", "finished", "group", "home_team_name_en", "away_team_name_en",
                             "home_score", "away_score"), basename(csv_file))

  # Filter for only group stage matches that are finished
  # Note: API may return strings like "TRUE" or booleans — handle both
  parsed_df <- parsed_df %>%
    filter(type == "group" & (finished == "TRUE" | finished == TRUE))

  if (nrow(parsed_df) == 0) {
    stop("⚠️ No finished group stage matches found to calculate.")
  }
  
  # Calculate stats for Home teams
  home_stats <- parsed_df %>%
    select(Group = group, Team = home_team_name_en, GF = home_score, GA = away_score) %>%
    mutate(
      GF = as.numeric(GF),
      GA = as.numeric(GA),
      Points = case_when(GF > GA ~ 3, GF == GA ~ 1, TRUE ~ 0)
    )
  
  # Calculate stats for Away teams
  away_stats <- parsed_df %>%
    select(Group = group, Team = away_team_name_en, GF = away_score, GA = home_score) %>%
    mutate(
      GF = as.numeric(GF),
      GA = as.numeric(GA),
      Points = case_when(GF > GA ~ 3, GF == GA ~ 1, TRUE ~ 0)
    )
  
  # Combine both home and away into a single table
  all_stats <- bind_rows(home_stats, away_stats)
  
  # Group by Team and aggregate to build the final standings table
  standings <- all_stats %>%
    group_by(Group, Team) %>%
    summarise(
      Played = n(),
      Points = sum(Points),
      GF = sum(GF),
      GA = sum(GA),
      GD = sum(GF) - sum(GA),
      .groups = 'drop'
    ) %>%
    # Sort by Group (A-Z), then Points (High-Low), GD (High-Low), GF (High-Low)
    arrange(Group, desc(Points), desc(GD), desc(GF))
  
  # Print the standings nicely formatted to the console
  cat("\n🏆 CURRENT GROUP STANDINGS 🏆\n")
  cat(paste0(rep("=", 40), collapse = ""), "\n")
  
  # Split by group and print
  groups <- unique(standings$Group)
  for (g in groups) {
    cat(sprintf("\n🌍 GROUP %s\n", g))
    group_table <- standings %>% filter(Group == g) %>% select(-Group)
    print(as.data.frame(group_table), row.names = TRUE)
  }
  
  # Export to CSV
  output_file <- here("data", "standings", "group_standings_R.csv")
  write.csv(standings, output_file, row.names = FALSE)
  cat(sprintf("\n✅ Standings successfully calculated in R and exported to: %s\n", output_file))
}

# Rank the 12 third-place teams and identify the best 8 that advance.
# Tiebreaker: Points → GD → GF  (standard GD rules, no H2H sub-table).
# Groups with matchday 3 still unplayed are flagged so their 3rd-place
# position is treated as provisional.
calculate_third_place_table <- function(csv_file) {
  parsed_df <- read.csv(csv_file, stringsAsFactors = FALSE)

  # Total matches played per group (a group is complete once all 6 are finished)
  finished_df <- parsed_df %>%
    filter(type == "group" & (finished == "TRUE" | finished == TRUE))

  matches_per_group <- finished_df %>%
    count(group, name = "matches_played")

  # Reuse the standings already computed by calculate_group_standings
  standings_file <- here("data", "standings", "group_standings_R.csv")
  if (!file.exists(standings_file)) stop("Run calculate_group_standings() first.")
  standings <- read.csv(standings_file, stringsAsFactors = FALSE)

  # Pull the 3rd-place team from each group (standings already sorted by Pts/GD/GF)
  third_place <- standings %>%
    group_by(Group) %>%
    slice(3) %>%
    ungroup() %>%
    left_join(matches_per_group, by = c("Group" = "group")) %>%
    mutate(
      Complete = !is.na(matches_played) & matches_played == 6L,
      Status   = ifelse(Complete, "Final", "Provisional")
    ) %>%
    arrange(desc(Points), desc(GD), desc(GF)) %>%
    mutate(Rank = row_number(), Advances = Rank <= 8)

  cat("\n🏅 THIRD-PLACE TABLE (Top 8 advance to R32)\n")
  cat(paste0(rep("=", 62), collapse = ""), "\n")
  cat(sprintf("%-3s %-22s %-5s %3s %4s %4s  %-11s  %-6s\n",
              "Pos", "Team", "Grp", "Pts", " GD", " GF", "Status", "R32?"))
  cat(paste0(rep("-", 62), collapse = ""), "\n")

  for (i in seq_len(nrow(third_place))) {
    r   <- third_place[i, ]
    adv <- if (r$Advances) "✅ ADVANCE" else "❌ OUT"
    cat(sprintf("%-3d %-22s %-5s  %2d  %+4d  %3d  %-11s  %s\n",
                r$Rank, r$Team, r$Group, r$Points, r$GD, r$GF, r$Status, adv))
    if (i == 8) cat(paste0(rep("-", 62), collapse = ""), " ← cut-off\n")
  }

  # Scotland-specific callout
  scot_row <- third_place[third_place$Team == "Scotland", ]
  if (nrow(scot_row) > 0) {
    cat(sprintf("\n🏴󠁧󠁢󠁳󠁣󠁴󠁿  Scotland: %dth place — %s (%d pts, GD %+d)\n",
                scot_row$Rank, if (scot_row$Advances) "CURRENTLY ADVANCING" else "CURRENTLY ELIMINATED",
                scot_row$Points, scot_row$GD))

    # Show how far behind the 8th-place team Scotland is (if outside top 8)
    if (!scot_row$Advances) {
      cutoff <- third_place[8, ]
      cat(sprintf("   Behind %s (8th) by: %d pts, GD %+d\n",
                  cutoff$Team,
                  scot_row$Points - cutoff$Points,
                  scot_row$GD - cutoff$GD))
    }

    # Flag provisional groups that could displace Scotland
    provisional_ahead <- third_place %>%
      filter(Status == "Provisional" & Rank < scot_row$Rank)
    provisional_below <- third_place %>%
      filter(Status == "Provisional" & Rank >= scot_row$Rank)
    if (nrow(provisional_below) > 0) {
      cat(sprintf("   ⚠️  %d provisional group(s) still playing could push Scotland out: %s\n",
                  nrow(provisional_below),
                  paste(provisional_below$Group, collapse = ", ")))
    }
  }

  # Export
  output_file <- here("data", "standings", "third_place_table.csv")
  write.csv(third_place, output_file, row.names = FALSE)
  cat(sprintf("\n✅ Third-place table saved to: %s\n", output_file))

  invisible(third_place)
}

# Run the function using the new clean CSV
calculate_group_standings(here("data", "fixtures", "clean_fixtures.csv"))
calculate_third_place_table(here("data", "fixtures", "clean_fixtures.csv"))
