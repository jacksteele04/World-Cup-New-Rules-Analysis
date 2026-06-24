library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

source(here("standardization", "shot_metrics.R"))

# =============================================================================
# 06_matchday1_metrics_graphic.R
# 3x2 faceted bar chart: shot rate / SOT rate / xG rate  x  1st half / 2nd half
# Matchday-1 group-stage only: 2022 Qatar (no break) vs 2026 (hydration break)
# Note: xG uses different models (StatsBomb '22 vs TheStatsAPI '26); shot and
#       SOT rates are model-independent.
# =============================================================================

OUT_DIR <- here("plots")
dir.create(OUT_DIR, showWarnings = FALSE)

# =============================================================================
# Load 2026 matchday-1 data
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

r26 <- bind_rows(
  half_counts(sched_26_md1, shots_26, fixtures_26, "1"),
  half_counts(sched_26_md1, shots_26, fixtures_26, "2")
) %>% mutate(year = "2026")

# =============================================================================
# Load 2022 matchday-1 data (StatsBomb)
# =============================================================================

cat("Fetching 2022 match list from StatsBomb...\n")
resp      <- GET("https://raw.githubusercontent.com/statsbomb/open-data/master/data/matches/43/106.json", timeout(30))
sb_df     <- as.data.frame(fromJSON(content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE))
md1_ids   <- sb_df$match_id[sb_df$competition_stage.name == "Group Stage" & sb_df$match_week == 1]
cat(sprintf("2022 matchday-1 matches: %d\n", length(md1_ids)))

shots_22  <- read.csv(here("data", "shotmaps", "shotmap_statsbomb_2022.csv"), stringsAsFactors = FALSE)

sched_22_md1 <- shots_22 %>%
  filter(match_id %in% md1_ids) %>%
  select(match_id, home_team, away_team, home_team_id, away_team_id) %>%
  distinct() %>%
  mutate(break_minute_1h = DEFAULT_BREAK_1H, break_minute_2h = DEFAULT_BREAK_2H,
         match_id = as.character(match_id))

fixtures_22_ids <- sched_22_md1 %>% select(match_id, home_team_id, away_team_id)
shots_22_md1    <- shots_22 %>% mutate(match_id = as.character(match_id)) %>%
                   filter(match_id %in% sched_22_md1$match_id)

r22 <- bind_rows(
  half_counts(sched_22_md1, shots_22_md1, fixtures_22_ids, "1"),
  half_counts(sched_22_md1, shots_22_md1, fixtures_22_ids, "2")
) %>% mutate(year = "2022")

# =============================================================================
# Reshape to long format
# =============================================================================

metric_labels <- c(
  shots_pre  = "Shot Rate (shots/min)",
  shots_post = "Shot Rate (shots/min)",
  sot_pre    = "SOT Rate (on-target/min)",
  sot_post   = "SOT Rate (on-target/min)",
  xg_pre     = "xG Rate (xG/min) †",
  xg_post    = "xG Rate (xG/min) †"
)

all_wide <- bind_rows(r26, r22)

all_long <- all_wide %>%
  pivot_longer(
    cols      = c(shots_pre, shots_post, sot_pre, sot_post, xg_pre, xg_post),
    names_to  = "col",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(col,
      shots_pre = "Shot Rate\n(shots/min)",  shots_post = "Shot Rate\n(shots/min)",
      sot_pre   = "SOT Rate\n(on-target/min)", sot_post = "SOT Rate\n(on-target/min)",
      xg_pre    = "xG Rate\n(xG/min) †", xg_post  = "xG Rate\n(xG/min) †"
    ),
    timing = ifelse(grepl("_pre$", col), "Before Break", "After Break"),
    timing = factor(timing, levels = c("Before Break", "After Break")),
    half   = factor(half,   levels = c("1st Half", "2nd Half")),
    metric = factor(metric, levels = c(
      "Shot Rate\n(shots/min)",
      "SOT Rate\n(on-target/min)",
      "xG Rate\n(xG/min) †"
    )),
    year   = factor(year, levels = c("2022", "2026"))
  )

summary_data <- all_long %>%
  group_by(year, half, metric, timing) %>%
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    se       = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
    n        = sum(!is.na(value)),
    .groups  = "drop"
  )

# =============================================================================
# Plot
# =============================================================================

pal_22   <- "#5B8DB8"
pal_26   <- "#E07B2A"
bg_color <- "#F7F7F5"
text_col <- "#2C2C2C"

p <- ggplot() +
  geom_point(
    data     = all_long,
    aes(x = timing, y = value, colour = year),
    size     = 1.6, alpha = 0.4,
    position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.10)
  ) +
  geom_col(
    data     = summary_data,
    aes(x = timing, y = mean_val, fill = year),
    position = position_dodge(width = 0.7),
    width    = 0.55, alpha = 0.85
  ) +
  geom_errorbar(
    data     = summary_data,
    aes(x = timing, ymin = mean_val - se, ymax = mean_val + se, group = year),
    position = position_dodge(width = 0.7),
    width    = 0.16, linewidth = 0.65, colour = text_col
  ) +
  geom_text(
    data     = summary_data,
    aes(x = timing, y = 0, label = paste0("n=", n), group = year),
    position = position_dodge(width = 0.7),
    vjust    = -0.3, size = 2.4, colour = "white", fontface = "bold"
  ) +
  facet_grid(metric ~ half, scales = "free_y", switch = "y") +
  scale_fill_manual(values   = c("2022" = pal_22, "2026" = pal_26), name = "Tournament") +
  scale_colour_manual(values = c("2022" = pal_22, "2026" = pal_26), name = "Tournament") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = function(x) sprintf("%.2f", x)) +
  labs(
    title    = "Momentum Metrics Before vs After Break Window — Matchday 1",
    subtitle = "2022 Qatar (no mandated break) vs 2026 USA/Canada/Mexico (hydration break)\nBars = mean  ·  dots = individual matches  ·  error bars = ±1 SE",
    x        = NULL,
    y        = NULL,
    caption  = "† xG uses different models: StatsBomb (2022) vs TheStatsAPI (2026) — interpret xG comparisons across years with caution\n2022 data: StatsBomb open data  ·  2026 data: TheStatsAPI"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background    = element_rect(fill = bg_color, colour = NA),
    panel.background   = element_rect(fill = bg_color, colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "#DEDEDE"),
    strip.text.x       = element_text(face = "bold", size = 12, colour = text_col),
    strip.text.y.left  = element_text(face = "bold", size = 9,  colour = text_col, angle = 90),
    strip.placement    = "outside",
    plot.title         = element_text(face = "bold", size = 14, colour = text_col),
    plot.subtitle      = element_text(size = 9.5, colour = "#555555", margin = margin(b = 10)),
    plot.caption       = element_text(size = 7.5, colour = "#888888", hjust = 0),
    legend.position    = "top",
    legend.title       = element_text(face = "bold"),
    axis.text          = element_text(colour = text_col, size = 9),
    panel.spacing.y    = unit(1.2, "lines"),
    panel.spacing.x    = unit(1.0, "lines")
  )

out_path <- file.path(OUT_DIR, "matchday1_all_metrics_comparison.png")
ggsave(out_path, plot = p, width = 10, height = 11, dpi = 180, bg = bg_color)
cat(sprintf("\nSaved: %s\n", out_path))
