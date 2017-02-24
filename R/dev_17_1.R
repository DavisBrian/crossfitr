# Registration FAQ / Deadlines
# https://games.crossfit.com/help/article/40

# development
library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

source("./R/scrape_leaderboard_fns.R")


library(splashr)

# set up the splasher docker container
install_splash()  # this takes a few minutes
splash_svr <- start_splash()

# test to see if the server is active:
splash("localhost") %>% splash_active()
## Status of splash instance on [http://localhost:8050]: ok. Max RSS: 70443008

# lets scrape
url <- "https://games.crossfit.com/leaderboard?competition=1&year=2017&division=2&scaled=0&sort=0&fittest=1&fittest1=0&occupation=0&page=1"
pg <- splash("localhost") %>% 
  render_html(url, wait=5) 

saveRDS(pg, file="./data/2017pg1.rds")
athlete_link <-  html_attr(html_nodes(pg, "td.name a"), "href")
athlete_name <-  html_text(html_nodes(pg, "td .full-name"))

html_table(pg, fill=TRUE) %>% View()

# grab 17.1
string <- ".total-points+ .score .rank-result"
html_text(html_nodes(pg, string))

# grab all the scores
string <- ".rank-result"
html_text(html_nodes(pg, string))

# grap 17.2
string <- ".score:nth-child(5) .rank-result"
html_text(html_nodes(pg, string))


# get the score
get_rank_scores <- function(html_page, stage = 0) {
  rs <- html_table(html_page, fill=TRUE)[[1]]
  as_tibble(rs[paste0("Workout0",stage)])
}

# close session
stop_splash(splash_svr)
