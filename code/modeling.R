options(stringsAsFactors = F)
library(tidyverse)
library(here)
library(Boruta)

# Read in data
stats_games <- readRDS(here("data/stats_games.rds"))

# There are so many things I'm about to do that are terrible. Should probably
# put this in a markdown to at least give the semblance of credibility. But I
# won't. To avoid some ugly complications, we'll consider one seed 
# discrepancy at a time. Such as only 3-seed upsets (such as a 10 vs a 7 or a
# 2 vs a 5). 3-seed upsets aren't too dissimilar from 2 or 4 seed upsets, so 
# I'll lump those in there as well. Sample size here will be important, given
# How many things I'll be testing. We'll see if different things are important
# for different degrees of upsets.

# Test with one version
temp <- stats_games %>% 
#  filter(year >= 2010) %>% 
  filter(between(seeddiff, 6, 8), !is.na(tov), !is.na(pf)) %>% 
  select_if(funs(sum(is.na(.)) == 0))

bb <- Boruta(select(temp, -win, -ptsdiff, -team, -year, -seed, -seeddiff), 
             as.factor(temp$win))

history <- t(bb$ImpHistory)
sort(history[, ncol(history)])

# Spot check if effect was negative or positive
glm(as.numeric(win) ~ fg, temp, family = "binomial") %>% summary

# Playing upspeed makes is less likely to upset? Really? I'd love to believe
# that but it seems too easy.
