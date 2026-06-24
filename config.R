library(here)

# =============================================================================
# config.R — Central configuration for all World Cup analyses
#
# Source this at the top of any script that needs constants.
# To adapt for a new tournament: edit TOURNAMENT and add to HISTORICAL.
# =============================================================================

TOURNAMENT <- list(
  season                = 2026,
  worldcupir_url        = "https://worldcup26.ir/get/games",
  thestatsapi_comp_id   = "comp_6107",
  thestatsapi_season_id = "sn_118868",
  host_nations          = c("united states", "mexico", "canada")
)

HISTORICAL <- list(
  qatar_2022 = list(
    thestatsapi_comp_id   = "comp_6107",
    thestatsapi_season_id = "sn_326766",
    statsbomb_comp_id     = 43L,
    statsbomb_season_id   = 106L
  )
)

ELO_MODEL <- list(
  host_bonus     = 100,
  default_rating = 1500L,
  xg_multiplier  = 1.25,
  elo_divisor    = 1000,
  max_goals      = 10L,
  epsilon        = 0.1   # small advantage added to resolve ties in H2H sub-tables
)

HYDRATION <- list(
  break_1h_default = 30L,
  break_2h_default = 75L
)

API <- list(
  timeout_secs  = 15,
  sleep_between = 6,
  page_size     = 100
)
