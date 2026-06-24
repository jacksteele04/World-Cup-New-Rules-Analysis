library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

source(here("standardization", "team_names.R"))
source(here("standardization", "elo.R"))
source(here("standardization", "shot_metrics.R"))

# =============================================================================
# 07_quality_effect.R
# Does team quality (Elo rating) predict a team's post-break momentum boost?
#
# For each 2026 group-stage match, compute each team's own shot rate delta
# (their shots post-break minus pre-break, per minute). Aggregate to one
# median delta per team across all their group matches, then scatter against
# their Elo rating. If quality matters, better teams should show a different
# (larger or smaller) delta pattern.
# =============================================================================

OUT_DIR <- here("plots")
dir.create(OUT_DIR, showWarnings = FALSE)

# =============================================================================
# Load 2026 data
# =============================================================================

breaks_26   <- read.csv(here("data", "events", "hydration_breaks.csv"),          stringsAsFactors = FALSE)
shots_26    <- read.csv(here("data", "shotmaps", "shotmap_thestatsapi.csv"),        stringsAsFactors = FALSE)
fixtures_26 <- read.csv(here("data", "fixtures", "clean_fixtures_thestatsapi.csv"), stringsAsFactors = FALSE)

# Prepare break schedule: use thestatsapi_id as the join key to shotmap
sched <- breaks_26 %>%
  select(-match_id) %>%
  rename(match_id = thestatsapi_id) %>%
  filter(!is.na(match_id)) %>%
  left_join(fixtures_26 %>% select(match_id, home_team_id, away_team_id), by = "match_id")

elo_db <- get_elo_database()

# =============================================================================
# Compute per-team, per-match, per-half shot rate delta
# =============================================================================

compute_team_deltas <- function(sched, shots, suf) {
  brk_col  <- if (suf == "1") "break_minute_1h" else "break_minute_2h"
  win_pre  <- if (suf == "1") DEFAULT_BREAK_1H       else DEFAULT_BREAK_2H - 45
  win_post <- if (suf == "1") 45 - DEFAULT_BREAK_1H  else 90 - DEFAULT_BREAK_2H
  half_lbl <- if (suf == "1") "1st Half (break ~30')" else "2nd Half (break ~75')"

  rows <- lapply(seq_len(nrow(sched)), function(i) {
    mid  <- sched$match_id[i]
    brk  <- sched[[brk_col]][i]
    if (is.na(brk)) return(NULL)

    ms <- shots[shots$match_id == mid, ]
    if (nrow(ms) == 0) return(NULL)

    home_id <- sched$home_team_id[i]
    away_id <- sched$away_team_id[i]

    ms$side <- ifelse(as.character(ms$team_id) == as.character(home_id), "home",
               ifelse(as.character(ms$team_id) == as.character(away_id), "away", NA))
    ms <- ms[!is.na(ms$side), ]

    if (suf == "1") ms <- ms[ms$minute <= 45, ]
    else            ms <- ms[ms$minute > 45 & ms$minute <= 90, ]

    team_delta <- function(side_label, team_name) {
      sub <- ms[ms$side == side_label, ]
      pre  <- sub[sub$minute <  brk, ]
      post <- sub[sub$minute >= brk, ]
      data.frame(
        match_id    = mid,
        team        = team_name,
        side        = side_label,
        half        = half_lbl,
        delta_shot  = nrow(post) / win_post - nrow(pre) / win_pre,
        delta_sot   = (sum(post$is_on_target == TRUE, na.rm = TRUE) / win_post) -
                      (sum(pre$is_on_target  == TRUE, na.rm = TRUE) / win_pre),
        delta_xg    = (sum(as.numeric(post$expected_goals), na.rm = TRUE) / win_post) -
                      (sum(as.numeric(pre$expected_goals),  na.rm = TRUE) / win_pre),
        stringsAsFactors = FALSE
      )
    }

    bind_rows(
      team_delta("home", sched$home_team[i]),
      team_delta("away", sched$away_team[i])
    )
  })
  bind_rows(Filter(Negate(is.null), rows))
}

cat("Computing per-team deltas...\n")
deltas <- bind_rows(
  compute_team_deltas(sched, shots_26, "1"),
  compute_team_deltas(sched, shots_26, "2")
)

# =============================================================================
# Attach Elo ratings
# =============================================================================

teams_unique <- unique(deltas$team)
elo_lookup   <- data.frame(
  team = teams_unique,
  elo  = sapply(teams_unique, get_elo, elo_db = elo_db),
  stringsAsFactors = FALSE
)

missing <- elo_lookup$team[is.na(elo_lookup$elo)]
if (length(missing) > 0) cat("⚠️  No Elo found for:", paste(missing, collapse = ", "), "\n")

deltas <- deltas %>% left_join(elo_lookup, by = "team")

# =============================================================================
# Aggregate: one median delta per team per half
# =============================================================================

team_summary <- deltas %>%
  filter(!is.na(elo)) %>%
  group_by(team, elo, half) %>%
  summarise(
    med_shot = median(delta_shot, na.rm = TRUE),
    med_sot  = median(delta_sot,  na.rm = TRUE),
    med_xg   = median(delta_xg,   na.rm = TRUE),
    n_matches = n_distinct(match_id),
    .groups  = "drop"
  ) %>%
  mutate(
    tier = cut(elo,
      breaks = quantile(elo, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      labels = c("Lower third", "Middle third", "Top third"),
      include.lowest = TRUE
    ),
    half = factor(half, levels = c("1st Half (break ~30')", "2nd Half (break ~75')"))
  )

cat(sprintf("Teams with Elo data: %d | Elo range: %d – %d\n",
            n_distinct(team_summary$team),
            min(team_summary$elo), max(team_summary$elo)))

# =============================================================================
# Plot 1: Scatter — Elo vs shot delta, with smooth + team labels
# =============================================================================

bg_color <- "#F7F7F5"
text_col <- "#2C2C2C"

# Tier fill palette
tier_pal <- c("Lower third" = "#A8C5DA", "Middle third" = "#F5B97F", "Top third" = "#E05C5C")

p_scatter <- ggplot(team_summary, aes(x = elo, y = med_shot)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#AAAAAA", linewidth = 0.6) +
  geom_smooth(method = "lm", se = TRUE, colour = "#444444",
              fill = "#CCCCCC", linewidth = 0.9, alpha = 0.3) +
  geom_point(aes(fill = tier, size = n_matches), shape = 21,
             colour = "white", stroke = 0.4, alpha = 0.9) +
  geom_text(
    data = team_summary %>%
      group_by(half) %>%
      filter(med_shot == max(med_shot) | med_shot == min(med_shot) |
             rank(-med_shot) <= 4 | rank(med_shot) <= 3),
    aes(label = team),
    size = 2.5, colour = text_col, vjust = -0.9, fontface = "italic"
  ) +
  facet_wrap(~ half, ncol = 2) +
  scale_fill_manual(values = tier_pal, name = "Elo tier") +
  scale_size_continuous(range = c(3, 6), name = "Group matches\nwith shot data") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title    = "Does Team Quality Predict Post-Break Momentum Boost?",
    subtitle = "Each point = one 2026 group-stage team | Y-axis = median shot rate change (post − pre) across group matches\nPositive = more shots after break; dashed line = no change",
    x        = "Elo rating",
    y        = "Median shot rate delta (shots/min)",
    caption  = "Source: TheStatsAPI shot data · Elo ratings: eloratings.net"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background    = element_rect(fill = bg_color, colour = NA),
    panel.background   = element_rect(fill = bg_color, colour = NA),
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(colour = "#E4E4E4"),
    strip.text         = element_text(face = "bold", size = 12),
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(size = 9, colour = "#555555", margin = margin(b = 10)),
    plot.caption       = element_text(size = 8, colour = "#888888"),
    legend.position    = "right"
  )

out1 <- file.path(OUT_DIR, "quality_elo_vs_delta_scatter.png")
ggsave(out1, plot = p_scatter, width = 12, height = 6, dpi = 180, bg = bg_color)
cat(sprintf("Saved: %s\n", out1))

# =============================================================================
# Plot 2: Tier comparison — grouped bars across all three metrics
# =============================================================================

tier_long <- team_summary %>%
  pivot_longer(cols = c(med_shot, med_sot, med_xg),
               names_to = "metric", values_to = "delta") %>%
  mutate(metric = recode(metric,
    med_shot = "Shot rate\n(shots/min)",
    med_sot  = "SOT rate\n(on-target/min)",
    med_xg   = "xG rate\n(xG/min) †"
  ),
  metric = factor(metric, levels = c("Shot rate\n(shots/min)",
                                      "SOT rate\n(on-target/min)",
                                      "xG rate\n(xG/min) †")))

tier_summary <- tier_long %>%
  group_by(tier, half, metric) %>%
  summarise(
    mean_delta = mean(delta, na.rm = TRUE),
    se         = sd(delta,   na.rm = TRUE) / sqrt(sum(!is.na(delta))),
    n          = sum(!is.na(delta)),
    .groups    = "drop"
  )

p_tiers <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#AAAAAA", linewidth = 0.5) +
  geom_point(
    data     = tier_long,
    aes(x = tier, y = delta, colour = tier),
    position = position_jitter(width = 0.15, height = 0),
    size = 1.4, alpha = 0.35
  ) +
  geom_col(
    data     = tier_summary,
    aes(x = tier, y = mean_delta, fill = tier),
    width    = 0.55, alpha = 0.85
  ) +
  geom_errorbar(
    data  = tier_summary,
    aes(x = tier, ymin = mean_delta - se, ymax = mean_delta + se),
    width = 0.2, linewidth = 0.7, colour = text_col
  ) +
  geom_text(
    data  = tier_summary,
    aes(x = tier, y = ifelse(mean_delta >= 0, 0.002, -0.002),
        label = paste0("n=", n)),
    vjust = ifelse(tier_summary$mean_delta >= 0, -0.3, 1.3),
    size  = 2.4, colour = text_col
  ) +
  facet_grid(metric ~ half, scales = "free_y", switch = "y") +
  scale_fill_manual(values   = tier_pal, guide = "none") +
  scale_colour_manual(values = tier_pal, guide = "none") +
  labs(
    title    = "Post-Break Momentum by Team Quality Tier",
    subtitle = "Teams split into thirds by Elo rating | Bars = mean delta across all group matches  ·  dots = individual team medians  ·  error bars = ±1 SE",
    x        = NULL,
    y        = NULL,
    caption  = "† xG uses TheStatsAPI model · Elo ratings: eloratings.net"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background    = element_rect(fill = bg_color, colour = NA),
    panel.background   = element_rect(fill = bg_color, colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "#E4E4E4"),
    strip.text.x       = element_text(face = "bold", size = 11),
    strip.text.y.left  = element_text(face = "bold", size = 9, angle = 90),
    strip.placement    = "outside",
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(size = 9, colour = "#555555", margin = margin(b = 10)),
    plot.caption       = element_text(size = 8, colour = "#888888", hjust = 0),
    axis.text.x        = element_text(size = 9),
    panel.spacing.y    = unit(1.0, "lines")
  )

out2 <- file.path(OUT_DIR, "quality_tier_comparison.png")
ggsave(out2, plot = p_tiers, width = 10, height = 10, dpi = 180, bg = bg_color)
cat(sprintf("Saved: %s\n", out2))

# =============================================================================
# Quick console summary
# =============================================================================

cat("\n", strrep("=", 60), "\n", sep = "")
cat("ELO CORRELATION WITH POST-BREAK SHOT DELTA\n")
cat(strrep("=", 60), "\n")
for (h in levels(team_summary$half)) {
  sub <- team_summary[team_summary$half == h, ]
  r   <- cor(sub$elo, sub$med_shot, use = "complete.obs")
  p   <- cor.test(sub$elo, sub$med_shot)$p.value
  cat(sprintf("  %-35s r = %+.3f  (p=%.3f)\n", h, r, p))
}
cat(strrep("=", 60), "\n")
