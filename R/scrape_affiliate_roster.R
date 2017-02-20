# scrape affilate

library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

source("./R/scrape_leaderboard_fns.R")
source("./R/scrape_athlete_fns.R")

create_affiliate_url <- function(id = 9244) {
  paste0("https://games.crossfit.com/affiliate/",id)
}

get_athlete_name_roster <- function(page, css = "#affiliates .col-sm-3 a") {
  html_text(html_nodes(page, css))
}

get_athlete_urls_roster <- function(page, css = "#affiliates .col-sm-3 a") {
  urls <- html_attr(html_nodes(page, css), "href")
  if(length(urls) == 0){
    stop("No athlete urls found.")
  }
  urls
}


# get affiliate roster
get_affiliate_roster <- function(id = 9244) {
  url <- create_affiliate_url(id)
  profile_page <- read_page(url)
  
  roster <- data_frame(athlete = get_athlete_name_roster(profile_page),
                       athlete_urls = get_athlete_urls_roster(profile_page),
                       athlete_ids  = get_athlete_ids(athlete_urls)) %>%
    mutate(retrieved_datetime = Sys.time()) %>%
    select(- athlete_urls)
  
  roster
}

get_athletes <- function(ids) {
  bind_rows(lapply(ids, get_athlete))
}

