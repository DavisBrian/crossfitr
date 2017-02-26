# New Version of getting the affiliate

library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)
library(splashr)

# Pulling Functions ------------------------------------------------------------

create_affiliate_url <- function(id = 9244) {
  paste0("https://games.crossfit.com/affiliate/",id)
}

read_page <- function(xfit_url) {
  tryCatch({read_html(xfit_url)},
           error = function(cond) { return(NULL) })
}

# basic html helper functions / wrappers
get_html_text <- function(page, css) {
  html_text(html_nodes(page, css))
}


# Roster Functions -------------------------------------------------------------

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


get_athlete_id <- function(url){ 
  url.vector <- strsplit(url,"/")[[1]]
  id <- url.vector[length(url.vector)]
  return(id)
}

get_athlete_ids <- function(athlete_urls) {
  if (length(athlete_urls) == 0) {
    stop("No althlete urls to get")
  }
  as.integer(sapply(athlete_urls, get_athlete_id))
}

get_roster <- function(page) {
  roster <- data_frame(athlete = get_athlete_name_roster(page),
                       athlete_urls = get_athlete_urls_roster(page),
                       athlete_ids  = get_athlete_ids(athlete_urls)) %>%
    mutate(retrieved_datetime = Sys.time()) %>%
    select(- athlete_urls)
  
  roster
}


# Leaderboard functions --------------------------------------------------------
get_athlete_name <- function(page, css = ".full-name") {
  get_html_text(page, css)
}

get_header <- function(page, css = "th") {
  get_html_text(page, css)
}

get_workout <- function(page, css = ".rank-result", workout) {
  header <- get_header(page, css = "th a")
  if (all(workout %in% header)) {
    df <- get_html_text(page, css) %>%
      matrix(., ncol = length(header), byrow = TRUE) %>%
      as_tibble() %>%
      select(match(workout, header))
  } else {
    stop("workout not found!")
  }
  
  df
}

get_athlete_urls <- function(page, css = ".profile-link") {
  urls <- html_attr(html_nodes(page, css), "href")
  if(length(urls) == 0){
    stop("No athlete urls found.")
  }
  urls
}

# Scrape Functions -------------------------------------------------------------

# set up the splasher docker container
install_splash()  # this takes a few minutes
splash_svr <- start_splash()
pond <- splash("localhost")

# test to see if the server is active:
pond %>% splash_active()
# splash("localhost") %>% splash_active()
## Status of splash instance on [http://localhost:8050]: ok. Max RSS: 70443008



aurl <- create_affiliate_url(id=9244)
affiliate_page <- read_page(url)

page1 <- render_html(pond, aurl) 

# get the leaderboard ----------------------------------------------------------

pl1 <- get_workout(page1, workout = "17.1") %>% 
  mutate(name = get_athlete_name(page1, css = ".full-name"),
         id   = get_athlete_ids(get_athlete_urls(page1, css = ".profile-link")))

lb_file <- "./data/Propel/propel_17_1_test.rds"
saveRDS(pl1, file = lb_file)

# Figure out how to parse the score

# Load the athlete profiles --------------------------------------------------------
# roster <- get_roster(affiliate_page)
# get_roster(page1)

ap_file <- "./data/Propel/propel_athlete_profiles_2017_complete.rds"
athlete_profiles <- readRDS(ap_file)
