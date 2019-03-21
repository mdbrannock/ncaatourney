options(stringsAsFactors = F)
library(tidyverse)
library(here)

# Read in the data
team_stats <- readRDS(here("data/team_stats.rds"))
games <- readRDS(here("data/games.rds"))

# Teams don't match up
# team_stats uses full school names, games does not
schools <- unique(team_stats$school)
teams <- unique(games$team)

# How many already match? 250. Take those out
shared <- intersect(schools, teams)
length(shared)
schools2 <- setdiff(schools, shared)
teams2 <- setdiff(teams, shared)

# Create dictionary matching long and short names
name_conv <- data.frame(
  long = c(sort(schools2), shared),
  short = c(NA, NA, "BYU", "Central Connecticut", "UCF", "UConn", "Detroit",
            "ETSU", NA, "UIC", "LIU-Brooklyn", "LSU", "UMBC", "UMass", 
            "Ole Miss", "UNLV", "UNC", "UNC Asheville", "UNC Greensboro", 
            "UNC Wilmington", "NC State", NA, "Penn", "Pitt", "St. Joseph's", 
            "Saint Mary's", "St. Peter's", "USC", "SMU", "Southern Miss", 
            "UTEP", "UTSA", "TCU", "UCSB", "California", "VCU", shared)
)

# Replace with short names
team_stats2 <- team_stats %>% 
  left_join(name_conv, by = c("school" = "long")) %>% 
  mutate(school = short) %>% select(-short)

# Check to make sure all the games have a matching name
setdiff(games$team, team_stats2$school)

# What differences in seeds are most common
table(games$seed - games$oseed)

# Add some variables to games
# Splitting into analysis groups
games2 <- games %>% 
  mutate(seeddiff = seed - oseed,
         ptsdiff = pts - opts,
         win = ptsdiff > 0) %>% 
  select(year, team, seed, seeddiff, ptsdiff, win)

# Merge in team_stats
stats_games <- games2 %>% 
  left_join(team_stats2, by = c("team" = "school", "year" = "year"))

# How are we doing on missing values? I know some metrics weren't always tracked
# Handful missing turnover-related stuff, and a bunch missing points for somehow
# Other than that, doing mostly okay except for the ones that weren't tracked.
lapply(names(stats_games), function(var){
  paste0(var, ": ", sum(is.na(stats_games[[var]])))
}) %>% unlist()

# Save dataset
saveRDS(stats_games, here("data/stats_games.rds"))
