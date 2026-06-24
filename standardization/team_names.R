# =============================================================================
# standardization/team_names.R
#
# Two canonical normalization functions for the two contexts where team name
# matching is needed across this project:
#
#   norm_name()  — compact alphanumeric token for joining across data sources
#                  (TheStatsAPI <-> StatsBomb)
#
#   elo_name()   — maps any known variant to eloratings.net's naming convention
#                  for Elo database lookups
# =============================================================================

# Cross-source matching token.
# Collapses all known name variants to a single compact lowercase token,
# then strips all remaining non-alphanumeric characters.
# Apply explicit aliases BEFORE stripping so encoding variants collapse first.
norm_name <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("democratic republic of the congo|dr congo|dr\\.? congo", "drcongo",           x)
  x <- gsub("bosnia and herzegovina|bosnia & herzegovina|bosnia.*herzeg",  "bosniaherzegovina", x)
  x <- gsub("c.te d.ivoire|ivory coast|cote d.ivoire",                    "ivorycoast",     x)
  x <- gsub("united states|u\\.s\\.a\\.|\\busa\\b",                      "usa",            x)
  x <- gsub("t.rkiye|\\bturkey\\b",                                       "turkiye",        x)
  x <- gsub("korea republic|south korea",                                 "southkorea",     x)
  x <- gsub("czechia|czech republic",                                     "czechrepublic",  x)
  x <- gsub("\\bir iran\\b",                                              "iran",           x)
  x <- gsub("cura.ao",                                                    "curacao",        x)
  x <- gsub("[^a-z0-9]", "", x)
  x
}

# eloratings.net name mapping.
# Maps any known API or display variant to the string eloratings.net uses,
# so get_elo() can do an exact match before falling back to fuzzy search.
elo_name <- function(x) {
  x <- tolower(trimws(x))
  x <- sub("\\busa\\b|^u\\.s\\.a\\.$|united states of america", "united states", x)
  x <- sub("\\bir iran\\b",                                       "iran",         x)
  x <- sub("korea republic|south korea",                          "south korea",  x)
  x <- sub("c.te d.ivoire|ivory coast|cote d.ivoire",             "ivory coast",  x)
  x <- sub("t.rkiye|\\bturkiye\\b",                               "turkey",       x)
  x <- sub("dr congo|democratic republic.*congo",                  "congo dr",     x)
  x <- sub("czech republic",                                       "czechia",      x)
  x <- sub("bosnia.*herzeg.*",                                     "bosnia and herzegovina", x)
  x <- sub("cura.ao",                                              "curacao",      x)
  x <- sub("trinidad.*tobago",                                     "trinidad and tobago", x)
  x
}
