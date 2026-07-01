library(dplyr)
library(here)

source(here("config.R"))
source(here("standardization", "elo.R"))
source(here("standardization", "team_names.R"))

# =============================================================================
# scotland_r32.R
#
# Simulates all possible matchday-3 outcomes for the 6 pending groups (G-L)
# and determines which combinations allow Scotland to survive as a top-8
# third-place finisher, plus the overall probability of advancement.
#
# Scotland's locked stats (Group C, all matches played):
#   3 pts | GD -3 | GF 1
#
# Ranking: Points -> GD -> GF  (GD rules, no H2H sub-table)
# =============================================================================

SCOTLAND_PTS <- 3L
SCOTLAND_GD  <- -3L
SCOTLAND_GF  <- 1L

# ── helpers ──────────────────────────────────────────────────────────────────

beats_scotland <- function(pts, gd, gf) {
  if (pts > SCOTLAND_PTS) return(TRUE)
  if (pts < SCOTLAND_PTS) return(FALSE)
  if (gd  > SCOTLAND_GD)  return(TRUE)
  if (gd  < SCOTLAND_GD)  return(FALSE)
  gf > SCOTLAND_GF
}

outcome_label <- function(h, a, code) {
  switch(as.character(code),
    "0" = sprintf("%s W", h),
    "1" = sprintf("Draw"),
    "2" = sprintf("%s W", a)
  )
}

# Elo-based match probabilities (Poisson model, same as rest of pipeline)
calc_probs <- function(h_name, a_name, elo_db) {
  h_elo <- get_elo(h_name, elo_db); if (is.na(h_elo)) h_elo <- ELO_MODEL$default_rating
  a_elo <- get_elo(a_name, elo_db); if (is.na(a_elo)) a_elo <- ELO_MODEL$default_rating
  hosts <- TOURNAMENT$host_nations
  if (elo_name(h_name) %in% hosts && !elo_name(a_name) %in% hosts) h_elo <- h_elo + ELO_MODEL$host_bonus
  if (elo_name(a_name) %in% hosts && !elo_name(h_name) %in% hosts) a_elo <- a_elo + ELO_MODEL$host_bonus
  dr  <- h_elo - a_elo
  hxg <- ELO_MODEL$xg_multiplier * 10^( dr / ELO_MODEL$elo_divisor)
  axg <- ELO_MODEL$xg_multiplier * 10^(-dr / ELO_MODEL$elo_divisor)
  g   <- 0:ELO_MODEL$max_goals
  mat <- outer(dpois(g, hxg), dpois(g, axg))
  ph  <- sum(mat[lower.tri(mat)]); pd <- sum(diag(mat)); pa <- sum(mat[upper.tri(mat)])
  tot <- ph + pd + pa
  c(home = ph/tot, draw = pd/tot, away = pa/tot)
}

# Apply a canonical outcome (1-0 / 0-0 / 0-1) to a group standings data.frame
# Columns required: Team, Points, GF, GA, GD
apply_outcome <- function(standings, home_team, away_team, outcome) {
  if (outcome == 0) { hg <- 1L; ag <- 0L }
  if (outcome == 1) { hg <- 0L; ag <- 0L }
  if (outcome == 2) { hg <- 0L; ag <- 1L }
  hi <- which(standings$Team == home_team)
  ai <- which(standings$Team == away_team)
  standings$GF[hi] <- standings$GF[hi] + hg; standings$GA[hi] <- standings$GA[hi] + ag
  standings$GF[ai] <- standings$GF[ai] + ag; standings$GA[ai] <- standings$GA[ai] + hg
  standings$GD[hi] <- standings$GF[hi] - standings$GA[hi]
  standings$GD[ai] <- standings$GF[ai] - standings$GA[ai]
  if (outcome == 0) standings$Points[hi] <- standings$Points[hi] + 3L
  if (outcome == 1) { standings$Points[hi] <- standings$Points[hi] + 1L
                      standings$Points[ai] <- standings$Points[ai] + 1L }
  if (outcome == 2) standings$Points[ai] <- standings$Points[ai] + 3L
  standings
}

get_third <- function(standings) {
  s <- standings[order(-standings$Points, -standings$GD, -standings$GF), ]
  s[3, ]
}

# ── load data ─────────────────────────────────────────────────────────────────

fx       <- read.csv(here("data","fixtures","clean_fixtures.csv"), stringsAsFactors=FALSE)
st_all   <- read.csv(here("data","standings","group_standings_R.csv"), stringsAsFactors=FALSE)
elo_db   <- get_elo_database()

PENDING_GROUPS <- c("G","H","I","J","K","L")

md3_fx <- fx[fx$type=="group" & fx$matchday==3 &
             fx$group %in% PENDING_GROUPS &
             (is.na(fx$home_score) | fx$home_score==""), ]
md3_fx <- md3_fx[order(md3_fx$group), ]

OUTCOMES <- 0:2   # 0=home win, 1=draw, 2=away win
combos   <- expand.grid(m1=OUTCOMES, m2=OUTCOMES)   # 9 rows per group

# ── per-group simulation ───────────────────────────────────────────────────────

group_results <- list()   # will hold per-group summary

for (g in PENDING_GROUPS) {
  g_fx <- md3_fx[md3_fx$group==g, ]
  if (nrow(g_fx) != 2) { warning(sprintf("Group %s: expected 2 MD3 fixtures, got %d", g, nrow(g_fx))); next }

  h1 <- g_fx$home_team_name_en[1]; a1 <- g_fx$away_team_name_en[1]
  h2 <- g_fx$home_team_name_en[2]; a2 <- g_fx$away_team_name_en[2]

  probs1 <- calc_probs(h1, a1, elo_db)
  probs2 <- calc_probs(h2, a2, elo_db)

  get_p <- function(match_idx, code) {
    p <- if (match_idx == 1) probs1 else probs2
    switch(as.character(code), "0"=p["home"], "1"=p["draw"], "2"=p["away"])
  }

  cur_st <- st_all[st_all$Group==g, c("Team","Points","GF","GA","GD")]

  scenario_rows <- list()
  for (i in seq_len(nrow(combos))) {
    o1 <- combos$m1[i]; o2 <- combos$m2[i]
    sim <- apply_outcome(cur_st, h1, a1, o1)
    sim <- apply_outcome(sim,    h2, a2, o2)
    third <- get_third(sim)
    p_scenario <- get_p(1, o1) * get_p(2, o2)
    threatened <- beats_scotland(third$Points, third$GD, third$GF)
    scenario_rows[[i]] <- data.frame(
      Group      = g,
      M1_Home    = h1, M1_Away = a1, M1_Code = o1,
      M2_Home    = h2, M2_Away = a2, M2_Code = o2,
      M1_Label   = outcome_label(h1, a1, o1),
      M2_Label   = outcome_label(h2, a2, o2),
      Third_Team = third$Team,
      Third_Pts  = third$Points,
      Third_GD   = third$GD,
      Third_GF   = third$GF,
      Probability = round(as.numeric(p_scenario), 4),
      Threatens_Scotland = threatened,
      stringsAsFactors = FALSE
    )
  }
  scen_df <- bind_rows(scenario_rows)

  p_threatens  <- sum(scen_df$Probability[scen_df$Threatens_Scotland])
  p_safe       <- sum(scen_df$Probability[!scen_df$Threatens_Scotland])
  group_results[[g]] <- list(scenarios=scen_df, p_threatens=p_threatens, p_safe=p_safe,
                              m1=c(h1,a1), m2=c(h2,a2))
}

# ── joint probability (Poisson binomial) ─────────────────────────────────────
# Scotland advances if ≤ 2 of the 6 groups threaten it.
# Locked groups above Scotland: Sweden, Ecuador, Bosnia, Paraguay, South Korea (5 teams)
# → Scotland starts at rank 6; can absorb at most 2 more teams above it.

threat_probs <- sapply(group_results, function(x) x$p_threatens)

# DP over number of groups threatening Scotland
n <- length(threat_probs)
dp <- numeric(n + 1); dp[1] <- 1   # dp[k+1] = P(exactly k groups threaten)
for (p in threat_probs) {
  dp_new <- numeric(n + 1)
  for (k in 0:n) {
    dp_new[k+1] <- dp_new[k+1] + dp[k+1] * (1-p)
    if (k < n) dp_new[k+2] <- dp_new[k+2] + dp[k+1] * p
  }
  dp <- dp_new
}

p_survival <- sum(dp[1:3])   # k=0,1,2 groups threaten → Scotland in top 8

# ── print results ─────────────────────────────────────────────────────────────

cat("\n")
cat("=================================================================\n")
cat(" SCOTLAND R32 PATH SIMULATION\n")
cat(" Scotland locked: 3 pts | GD -3 | GF 1\n")
cat(" 5 locked teams above: Sweden, Ecuador, Bosnia, Paraguay, S.Korea\n")
cat(" Scotland advances if ≤ 2 of the 6 pending groups displace them\n")
cat("=================================================================\n\n")

for (g in PENDING_GROUPS) {
  res   <- group_results[[g]]
  scen  <- res$scenarios
  m1str <- paste(res$m1, collapse=" vs ")
  m2str <- paste(res$m2, collapse=" vs ")

  cat(sprintf("GROUP %s  ──  %s  |  %s\n", g, m1str, m2str))
  cat(sprintf("  P(this group threatens Scotland): %.1f%%\n", res$p_threatens*100))
  cat(sprintf("  %-25s  %-25s  %-20s  %6s  %6s\n",
              "Match 1", "Match 2", "3rd Place", "P(%)", "Scotland"))
  cat(sprintf("  %s\n", strrep("-", 88)))

  scen_sorted <- scen[order(scen$Probability, decreasing=TRUE), ]
  for (i in seq_len(nrow(scen_sorted))) {
    r   <- scen_sorted[i,]
    sco <- if (r$Threatens_Scotland) "THREATENED" else "SAFE"
    cat(sprintf("  %-25s  %-25s  %-20s  %5.1f%%  %s\n",
                r$M1_Label, r$M2_Label,
                sprintf("%s (%dpts,GD%+d)", r$Third_Team, r$Third_Pts, r$Third_GD),
                r$Probability*100, sco))
  }
  cat("\n")
}

# Safe scenarios summary
cat("=================================================================\n")
cat(" SCENARIOS WHERE SCOTLAND IS SAFE IN EACH GROUP\n")
cat("=================================================================\n")
for (g in PENDING_GROUPS) {
  safe_scen <- group_results[[g]]$scenarios %>%
    filter(!Threatens_Scotland) %>%
    mutate(desc = paste0(M1_Label, " + ", M2_Label)) %>%
    arrange(desc(Probability))
  p_safe <- group_results[[g]]$p_safe
  cat(sprintf("\nGroup %s — P(safe) %.1f%%\n", g, p_safe*100))
  if (nrow(safe_scen) == 0) {
    cat("  No safe scenarios — this group ALWAYS displaces Scotland\n")
  } else {
    for (i in seq_len(nrow(safe_scen))) {
      r <- safe_scen[i,]
      cat(sprintf("  [%4.1f%%] %s → 3rd: %s (pts:%d, GD:%+d)\n",
                  r$Probability*100, r$desc, r$Third_Team, r$Third_Pts, r$Third_GD))
    }
  }
}

cat("\n=================================================================\n")
cat(" OVERALL PROBABILITY BREAKDOWN\n")
cat("=================================================================\n")
for (k in 0:(n)) {
  label <- if (k <= 2) sprintf("k=%d  → SCOTLAND ADVANCES", k) else sprintf("k=%d  → SCOTLAND ELIMINATED", k)
  cat(sprintf("  P(%d groups displace Scotland): %5.1f%%  %s\n", k, dp[k+1]*100, label))
}
cat(sprintf("\n  🏴󠁧󠁢󠁳󠁣󠁴󠁿  P(Scotland advances to R32): %.1f%%\n", p_survival*100))
cat(sprintf("  ❌  P(Scotland eliminated):       %.1f%%\n", (1-p_survival)*100))
cat("=================================================================\n")
