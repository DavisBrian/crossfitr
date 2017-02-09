# development
library(tidyverse)
library(rvest)


year = 14
division = 1
stage = 1
page = 1
score_type = "points"

html_page <- tryCatch({read_html(create_url(year, division, stage, page))},
  error = function(cond) { return(NULL) })


url <- create_url()


html_page <- read_page(url)

aurls <- get_athlete_urls(html_page)


athlete.ids <- as.integer(unlist(lapply(athlete.urls, GetAthleteID)))

get_scores(html_page, stage)

# create the leaderboard
leaderboard <- data_frame(year       = year,
                          division   = division,
                          stage      = stage,
                          athlete_id = get_athlete_ids(get_athlete_urls(html_page)))
