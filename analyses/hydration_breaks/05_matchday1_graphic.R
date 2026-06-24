library(httr)
library(jsonlite)
library(dplyr)
library(here)

if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2: install.packages('ggplot2')")
library(ggplot2)

source(here("standardization", "shot_metrics.R"))

# =============================================================================
# 05_matchday1_graphic.R
# Grouped bar chart comparing shot rates before/after the ~30'/~75' windows
# for matchday-1 group-stage matches only: 2022 Qatar vs 2026 USA/Canada/Mexico
# =============================================================================

OUT_DIR <- here("plots")
dir.create(OUT_DIR, showWarnings = FALSE)

# =============================================================================
# 2026 — matchday 1
# =============================================================================

breaks_26   <- read.csv(here("data", "events", "hydration_breaks.csv"),          stringsAsFactors = FALSE)
shots_26    <- read.csv(here("data", "shotmaps", "shotmap_thestatsapi.csv"),        stringsAsFactors = FALSE)
fixtures_26 <- read.csv(here("data", "fixtures", "clean_fixtures_thestatsapi.csv"), stringsAsFactors = FALSE)

sched_26_md1 <- breaks_26 %>%
  filter(matchday == 1) %>%
  select(-match_id) %>%
  rename(match_id = thestatsapi_id) %>%
  filter(!is.na(match_id)) %>%
  left_join(fixtures_26 %>% select(match_id, home_team_id, away_team_id), by = "match_id")

r26_1h <- half_counts(sched_26_md1, shots_26, fixtures_26, "1")
r26_2h <- half_counts(sched_26_md1, shots_26, fixtures_26, "2")
r26 <- bind_rows(r26_1h, r26_2h) %>% mutate(year = "2026")

# =============================================================================
# 2022 — matchday 1 (StatsBomb open data)
# =============================================================================

# Get matchday from StatsBomb matches JSON (match_week field)
cat("Fetching 2022 match list from StatsBomb...\n")
resp <- GET(
  "https://raw.githubusercontent.com/statsbomb/open-data/master/data/matches/43/106.json",
  timeout(30)
)
sb_matches <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
sb_df <- as.data.frame(sb_matches)

# match_week 1 = matchday 1
gs_md1_22 <- sb_df[sb_df$competition_stage.name == "Group Stage" & sb_df$match_week == 1, ]
cat(sprintf("2022 matchday-1 matches: %d\n", nrow(gs_md1_22)))

shots_22 <- read.csv(here("data", "shotmaps", "shotmap_statsbomb_2022.csv"), stringsAsFactors = FALSE)

sched_22_md1 <- shots_22 %>%
  filter(match_id %in% gs_md1_22$match_id) %>%
  select(match_id, home_team, away_team, home_team_id, away_team_id) %>%
  distinct() %>%
  mutate(break_minute_1h = DEFAULT_BREAK_1H, break_minute_2h = DEFAULT_BREAK_2H)

fixtures_22_ids <- sched_22_md1 %>% select(match_id, home_team_id, away_team_id)

r22_1h <- half_counts(sched_22_md1, shots_22, fixtures_22_ids, "1")
r22_2h <- half_counts(sched_22_md1, shots_22, fixtures_22_ids, "2")
r22 <- bind_rows(r22_1h, r22_2h) %>% mutate(year = "2022")

# =============================================================================
# Combine and reshape to long format
# =============================================================================

r26$match_id <- as.character(r26$match_id)
r22$match_id <- as.character(r22$match_id)

if (!requireNamespace("tidyr", quietly = TRUE)) {
  stop("Install tidyr: install.packages('tidyr')")
}

all_data <- bind_rows(r26, r22) %>%
  tidyr::pivot_longer(
    cols      = c(shots_pre, shots_post),
    names_to  = "timing",
    values_to = "shots_per_min"
  ) %>%
  mutate(
    timing = recode(timing, shots_pre = "Before Break", shots_post = "After Break"),
    timing = factor(timing, levels = c("Before Break", "After Break")),
    half   = factor(half,   levels = c("1st Half", "2nd Half")),
    year   = factor(year,   levels = c("2022", "2026"))
  )

# Summary stats for bars
summary_data <- all_data %>%
  group_by(year, half, timing) %>%
  summarise(
    mean_rate = mean(shots_per_min, na.rm = TRUE),
    se        = sd(shots_per_min, na.rm = TRUE) / sqrt(sum(!is.na(shots_per_min))),
    n         = sum(!is.na(shots_per_min)),
    .groups   = "drop"
  )

# =============================================================================
# Plot
# =============================================================================

pal_22   <- "#5B8DB8"
pal_26   <- "#E07B2A"
bg_color <- "#F7F7F5"
text_col <- "#2C2C2C"

p <- ggplot() +
  # Individual match dots (jittered)
  geom_point(
    data  = all_data,
    aes(x = timing, y = shots_per_min, colour = year),
    size = 1.8, alpha = 0.45,
    position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.12)
  ) +
  # Mean bars
  geom_col(
    data     = summary_data,
    aes(x = timing, y = mean_rate, fill = year),
    position = position_dodge(width = 0.7),
    width    = 0.55, alpha = 0.85
  ) +
  # Error bars (±1 SE)
  geom_errorbar(
    data  = summary_data,
    aes(x = timing, ymin = mean_rate - se, ymax = mean_rate + se, group = year),
    position = position_dodge(width = 0.7),
    width    = 0.18, linewidth = 0.7, colour = text_col
  ) +
  # n labels inside bars
  geom_text(
    data  = summary_data,
    aes(x = timing, y = 0.01, label = paste0("n=", n), group = year),
    position  = position_dodge(width = 0.7),
    vjust     = 0, size = 2.8, colour = "white", fontface = "bold"
  ) +
  facet_wrap(~ half, ncol = 2) +
  scale_fill_manual(
    values = c("2022" = pal_22, "2026" = pal_26),
    name   = "Tournament"
  ) +
  scale_colour_manual(
    values = c("2022" = pal_22, "2026" = pal_26),
    name   = "Tournament"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12)),
    labels = function(x) sprintf("%.2f", x)
  ) +
  labs(
    title    = "Shot Rate Before vs After Cooling Break — Matchday 1 Only",
    subtitle = "2022 Qatar (no break) vs 2026 (mandated hydration break)\nBars = mean shots/min  ·  dots = individual matches  ·  error bars = ±1 SE",
    x        = NULL,
    y        = "Shots per minute",
    caption  = "2022 data: StatsBomb open data  ·  2026 data: TheStatsAPI"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = bg_color, colour = NA),
    panel.background = element_rect(fill = bg_color, colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "#DEDEDE"),
    strip.text       = element_text(face = "bold", size = 13, colour = text_col),
    plot.title       = element_text(face = "bold", size = 15, colour = text_col),
    plot.subtitle    = element_text(size = 10, colour = "#555555", margin = margin(b = 12)),
    plot.caption     = element_text(size = 8,  colour = "#888888"),
    legend.position  = "top",
    legend.title     = element_text(face = "bold"),
    axis.text        = element_text(colour = text_col),
    axis.title.y     = element_text(margin = margin(r = 8))
  )

out_path <- file.path(OUT_DIR, "matchday1_shot_rate_comparison.png")
ggsave(out_path, plot = p, width = 10, height = 6, dpi = 180, bg = bg_color)
cat(sprintf("\nSaved: %s\n", out_path))
