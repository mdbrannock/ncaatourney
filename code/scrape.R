library(tidyverse)
library(rvest)
options(stringsAsFactors = F)

# Define the years we care about
start <- 2005
years <- start:2019

# Get team stats for all years
team_stats <- map_dfr(years, ~{
  
  print(.x)
  
  # Get basic stats for each team
  basic_text <- paste0("https://www.sports-reference.com/cbb/seasons/",
                       .x, "-school-stats.html") %>% 
    read_html() %>% 
    html_nodes("td") %>% 
    html_text()
  
  # Get advanced stats for each team
  adv_text <- paste0("https://www.sports-reference.com/cbb/seasons/",
                     .x, "-advanced-school-stats.html") %>% 
    read_html() %>% 
    html_nodes("td") %>% 
    html_text()
  
  # Check to make sure same schools, same order
  check <- identical(basic_text[seq(1,  length(basic_text), 33)],
                     adv_text[seq(1,  length(adv_text), 29)])
  
  print(check)
  if(!check) return(NA)
  
  # Munge basic text into dataframe
  year_df <- data.frame(
    year = .x,
    school  = basic_text[seq(1,  length(basic_text), 33)],
    fg      = basic_text[seq(18, length(basic_text), 33)],
    fgp     = basic_text[seq(20, length(basic_text), 33)],
    threep  = basic_text[seq(21, length(basic_text), 33)],
    threepp = basic_text[seq(23, length(basic_text), 33)],
    ft      = basic_text[seq(24, length(basic_text), 33)],
    ftp     = basic_text[seq(26, length(basic_text), 33)],
    orb     = basic_text[seq(27, length(basic_text), 33)],
    trb     = basic_text[seq(28, length(basic_text), 33)],
    ast     = basic_text[seq(29, length(basic_text), 33)],
    stl     = basic_text[seq(30, length(basic_text), 33)],
    blk     = basic_text[seq(31, length(basic_text), 33)],
    tov     = basic_text[seq(32, length(basic_text), 33)],
    pf      = basic_text[seq(33, length(basic_text), 33)],
    winloss = adv_text[seq(5,  length(adv_text), 29)],
    srs     = adv_text[seq(6,  length(adv_text), 29)],
    sos     = adv_text[seq(7,  length(adv_text), 29)],
    awayw   = adv_text[seq(12, length(adv_text), 29)],
    awayl   = adv_text[seq(13, length(adv_text), 29)],
    pointsf = adv_text[seq(14, length(adv_text), 29)],
    pointsa = adv_text[seq(15, length(adv_text), 29)],
    pace    = adv_text[seq(17, length(adv_text), 29)],
    ortg    = adv_text[seq(18, length(adv_text), 29)],
    ftr     = adv_text[seq(19, length(adv_text), 29)],
    threepr = adv_text[seq(20, length(adv_text), 29)],
    totsp   = adv_text[seq(21, length(adv_text), 29)],
    trebp   = adv_text[seq(22, length(adv_text), 29)],
    astp    = adv_text[seq(23, length(adv_text), 29)],
    stlp    = adv_text[seq(24, length(adv_text), 29)],
    blkp    = adv_text[seq(25, length(adv_text), 29)],
    efgp    = adv_text[seq(26, length(adv_text), 29)],
    tovp    = adv_text[seq(27, length(adv_text), 29)],
    orbp    = adv_text[seq(28, length(adv_text), 29)]
  )
})

# Get tournament results
years = start:2018
games <- map_dfr(years, ~{
  print(.x)
  
  tourn_text <- paste0("https://www.sports-reference.com/cbb/postseason/",
                       .x, "-ncaa.html") %>% 
    read_html() %>% 
    html_nodes(".round div div span , .round div div a") %>% 
    html_text()
  
  # Complicated somewhat by the final four being partioned out separately
  # Drop out the the teams that don't have scores
  tourn_text2 <- tourn_text[-c(91, 92, 183, 184, 275, 276, 367, 368, 387, 388)]
  
  # Need both teams, both scores, and both seeds
  year_games <- data.frame(
    year = .x,
    seed1 = tourn_text2[seq(1, length(tourn_text2), 6)],
    team1 = tourn_text2[seq(2, length(tourn_text2), 6)],
    scor1 = tourn_text2[seq(3, length(tourn_text2), 6)],
    seed2 = tourn_text2[seq(4, length(tourn_text2), 6)],
    team2 = tourn_text2[seq(5, length(tourn_text2), 6)],
    scor2 = tourn_text2[seq(6, length(tourn_text2), 6)]
  )
})

# Convert everything but team names into numbers
team_stats2 <- team_stats %>% 
  mutate_at(vars(-school), as.numeric)
games2 <- games %>% 
  mutate_at(vars(-team1,-team2), as.numeric)