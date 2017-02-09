# development
library(tidyverse)
library(stringr)
library(rvest)

source("./R/scrape_fns.R")

year = 14
division = 1
stage = 1
page = 1
score_type = "points"

url <- create_url(year = 14, division = 1, stage = 1, page = 1)
html_page <- read_page(url)

athlete_urls <- get_athlete_urls(html_page)
athlete_ids <- get_athlete_ids(athlete_urls)

rank_scores <- get_rank_scores(html_page, stage = 1)

# split_scores


lapply(rank_scores, function(x) {str_split(x, "[\\(\\)]")[[1]]})
# parse rank
rank_scores %>%
  mutate(rank = if_else(rank_scores == "--\n                No score", 
                        as.integer(NA),
                        as.integer(sub(" .*", "", .))))
as.integer(sub(" .*", "", rank_scores))
# parse scores


# is_scaled

leaderboard[rank_score == "--\n                No score", rank_score := NA]
leaderboard[,rank  := as.integer(sub(" .*", "", rank_score))]
leaderboard[,score_pre := unlist(str_split(rank_scores, "[\\(\\)]"))[2], by=athlete_id]
leaderboard[,scaled := as.integer(substring(rank_score, 
                                            nchar(rank_score)-3,
                                            nchar(rank_score)) == " - s")]

# split the scores into rank and score

# create the leaderboard
leaderboard <- data_frame(year       = year,
                          division   = division,
                          stage      = stage,
                          athlete_id = get_athlete_ids(get_athlete_urls(html_page)))
