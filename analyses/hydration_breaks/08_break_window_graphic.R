library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

source(here("config.R"))
source(here("standardization", "validation.R"))
source(here("standardization", "shot_metrics.R"))

# =============================================================================
# 08_break_window_graphic.R
# Faceted bar chart: shot rate, SOT rate, xG rate in the 5 minutes immediately
# before and after each hydration break — 2026 group stage, all matchdays.
# =============================================================================

WIN <- 5L  # minutes on each side of the break

OUT_DIR  <- here("plots")
OUT_FILE <- file.path(OUT_DIR, "break_window_5min.png")
dir.create(OUT_DIR, showWarnings = FALSE)

# --- Load data ----------------------------------------------------------------

breaks_file <- here("data", "events", "hydration_breaks.csv")
shots_file  <- here("data", "shotmaps", "shotmap_thestatsapi.csv")
fix_file    <- here("data", "fixtures", "clean_fixtures_thestatsapi.csv")

require_file(breaks_file, "run analyses/hydration_breaks/02_build_break_schedule.R first")
require_file(shots_file,  "run fetch/fetch_shotmap_thestatsapi.R first")
require_file(fix_file,    "run fetch/fetch_fixtures_thestatsapi.R first")

breaks   <- read.csv(breaks_file, stringsAsFactors = FALSE)
shots    <- read.csv(shots_file,  stringsAsFactors = FALSE)
fixtures <- read.csv(fix_file,    stringsAsFactors = FALSE)

require_cols(breaks, c("thestatsapi_id", "break_minute_1h", "break_minute_2h"), "hydration_breaks.csv")
require_cols(shots,  c("match_id", "team_id", "minute", "is_on_target", "expected_goals"), "shotmap_thestatsapi.csv")
require_cols(fixtures, c("match_id", "home_team_id", "away_team_id"), "clean_fixtures_thestatsapi.csv")

# Align IDs: thestatsapi_id in breaks matches match_id in shots/fixtures
# Restrict to finished matches so unplayed games don't contribute zero-shot rows
finished_ids <- fixtures$match_id[fixtures$status == "finished"]

sched <- breaks %>%
  mutate(match_id = thestatsapi_id) %>%
  filter(!is.na(match_id), match_id %in% finished_ids) %>%
  distinct(match_id, .keep_all = TRUE) %>%
  left_join(fixtures %>% select(match_id, home_team_id, away_team_id), by = "match_id")

cat(sprintf("Matches with break schedule: %d (1H: %d, 2H: %d)\n",
            nrow(sched),
            sum(!is.na(sched$break_minute_1h)),
            sum(!is.na(sched$break_minute_2h))))

# --- 5-minute window rate function --------------------------------------------

window_rates <- function(sched, shots, half) {
  brk_col  <- if (half == 1) "break_minute_1h" else "break_minute_2h"
  min_lo   <- if (half == 1) 0  else 46
  min_hi   <- if (half == 1) 45 else 90

  rows <- lapply(seq_len(nrow(sched)), function(i) {
    mid  <- sched$match_id[i]
    brk  <- as.numeric(sched[[brk_col]][i])
    if (is.na(brk) || is.na(mid)) return(NULL)

    # Skip if 5-minute window would fall outside the half
    if (brk - WIN < min_lo || brk + WIN > min_hi) return(NULL)

    fix <- fixtures[fixtures$match_id == mid, ]
    if (nrow(fix) == 0) return(NULL)

    ms <- shots[shots$match_id == mid &
                shots$minute >= (brk - WIN) &
                shots$minute <= (brk + WIN), ]

    pre  <- ms[ms$minute <  brk, ]
    post <- ms[ms$minute >= brk, ]

    data.frame(
      match_id  = as.character(mid),
      half      = if (half == 1) "1st Half" else "2nd Half",
      break_min = brk,
      shots_pre  = nrow(pre)  / WIN,
      shots_post = nrow(post) / WIN,
      sot_pre    = sum(pre$is_on_target  == TRUE, na.rm = TRUE) / WIN,
      sot_post   = sum(post$is_on_target == TRUE, na.rm = TRUE) / WIN,
      xg_pre     = sum(as.numeric(pre$expected_goals),  na.rm = TRUE) / WIN,
      xg_post    = sum(as.numeric(post$expected_goals), na.rm = TRUE) / WIN,
      stringsAsFactors = FALSE
    )
  })
  bind_rows(Filter(Negate(is.null), rows))
}

r1 <- window_rates(sched, shots, 1)
r2 <- window_rates(sched, shots, 2)
all_wide <- bind_rows(r1, r2)

cat(sprintf("Matches with shot data in window: %d (1H: %d, 2H: %d)\n",
            nrow(all_wide), sum(all_wide$half == "1st Half"), sum(all_wide$half == "2nd Half")))

# --- Reshape to long ----------------------------------------------------------

all_long <- all_wide %>%
  pivot_longer(
    cols      = c(shots_pre, shots_post, sot_pre, sot_post, xg_pre, xg_post),
    names_to  = "col",
    values_to = "value"
  ) %>%
  mutate(
    metric = case_when(
      grepl("^shots", col) ~ "Shot Rate\n(shots / min)",
      grepl("^sot",   col) ~ "SOT Rate\n(on-target / min)",
      grepl("^xg",    col) ~ "xG Rate\n(xG / min)"
    ),
    timing = ifelse(grepl("_pre$", col), "Before Break", "After Break"),
    timing = factor(timing, levels = c("Before Break", "After Break")),
    half   = factor(half,   levels = c("1st Half", "2nd Half")),
    metric = factor(metric, levels = c(
      "Shot Rate\n(shots / min)",
      "SOT Rate\n(on-target / min)",
      "xG Rate\n(xG / min)"
    ))
  )

summary_data <- all_long %>%
  group_by(half, metric, timing) %>%
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    se       = sd(value,   na.rm = TRUE) / sqrt(sum(!is.na(value))),
    n        = sum(!is.na(value)),
    .groups  = "drop"
  )

# --- Plot ---------------------------------------------------------------------

accent  <- "#E07B2A"
bg_color <- "#F7F7F5"
text_col <- "#2C2C2C"
bar_col  <- c("Before Break" = "#5B8DB8", "After Break" = accent)

p <- ggplot() +
  geom_point(
    data     = all_long,
    aes(x = timing, y = value, colour = timing),
    size     = 1.8, alpha = 0.40,
    position = position_jitter(width = 0.12, height = 0, seed = 42)
  ) +
  geom_col(
    data     = summary_data,
    aes(x = timing, y = mean_val, fill = timing),
    width    = 0.55, alpha = 0.88
  ) +
  geom_errorbar(
    data     = summary_data,
    aes(x = timing, ymin = mean_val - se, ymax = mean_val + se),
    width    = 0.18, linewidth = 0.7, colour = text_col
  ) +
  geom_text(
    data     = summary_data,
    aes(x = timing, y = 0, label = paste0("n=", n)),
    vjust    = -0.3, size = 2.6, colour = "white", fontface = "bold"
  ) +
  facet_grid(metric ~ half, scales = "free_y", switch = "y") +
  scale_fill_manual(  values = bar_col, guide = "none") +
  scale_colour_manual(values = bar_col, guide = "none") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    labels = function(x) sprintf("%.2f", x)
  ) +
  labs(
    title    = sprintf("±%d-Minute Break Window: Three Momentum Metrics", WIN),
    subtitle = sprintf(
      "2026 FIFA World Cup group stage — all matchdays\nShots, shots on target, and xG in the %d minutes immediately before vs after each hydration break",
      WIN
    ),
    x        = NULL,
    y        = NULL,
    caption  = "Bars = mean  ·  dots = individual matches  ·  error bars = ±1 SE  ·  Data: TheStatsAPI"
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
    axis.text          = element_text(colour = text_col, size = 9),
    panel.spacing.y    = unit(1.2, "lines"),
    panel.spacing.x    = unit(1.0, "lines")
  )

ggsave(OUT_FILE, plot = p, width = 9, height = 10, dpi = 180, bg = bg_color)
cat(sprintf("\nSaved: %s\n", OUT_FILE))
