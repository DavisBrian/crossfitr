# Registration FAQ / Deadlines
# https://games.crossfit.com/help/article/40

# development
library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)
library(splashr)

# source("./R/scrape_leaderboard_fns.R")
## Functions -------------------------------------------------------------------

# basic html helper functions / wrappers
get_html_text <- function(page, css) {
  html_text(html_nodes(page, css))
}

# read the page at games.crossfit.com 
read_page <- function(xfit_url) {
  tryCatch({read_html(xfit_url)},
           error = function(cond) { return(NULL) })
}

# crossfit specific functions --------------------------------------------------

# need to handle legacy board  (css = ".button" maybe?)
get_page_count <- function(page, css = ".nums a") {
  max(as.integer(get_html_text(page, css)))
}

get_athlete_urls <- function(page, css = "td.name a") {
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


get_athlete_name <- function(page, css = "td .full-name") {
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


# set up the splasher docker container
install_splash()  # this takes a few minutes
splash_svr <- start_splash()
pond <- splash("localhost")

# test to see if the server is active:
pond %>% splash_active()
# splash("localhost") %>% splash_active()
## Status of splash instance on [http://localhost:8050]: ok. Max RSS: 70443008


generate_leaderboard_url <- function(year, division, workout, scaled = 0, page = 1) {
  # check parameters and transform as needed
  # after 17.2 comes out at sort by workout
  
  paste0("https://games.crossfit.com/leaderboard?competition=1&year=", year, 
         "&division=", division,
         "&scaled=", as.integer(scaled),
         "&sort=0&fittest=1&fittest1=0&occupation=0&page=", page)
}


scrape_leaderboard_page <- function(year, division, workout, scaled = 0, page = 1) {
  page <- rener_html(page)
}

# lets scrape
# parameters
workout <- "17.1"
yr <- "2017"
division <- 1
scaled <- FALSE
 # page <- 1

base_url <- "https://games.crossfit.com/leaderboard?competition=1&year=2017&division=1&scaled=0&sort=0&fittest=1&fittest1=0&occupation=0&page="

url1 <- paste0(base_url, 1)
url2 <- paste0(base_url, 2)

page1 <- render_html(pond, url1) 
page2 <- render_html(pond, url2) 

head(html_text(html_nodes(page1, css = "td .full-name")))
head(html_text(html_nodes(page1, css = "td .full-name")))

pg <- 1
url1 <- generate_leaderboard_url("2017", division = 1, workout = "17.1", scaled = FALSE, page = pg)
page1 <- render_html(pond, url1) 

read_page <- function(xfit_url) {
  tryCatch({read_html(xfit_url)},
           error = function(cond) { return(NULL) })
}


pg <- 2
url2 <- generate_leaderboard_url("2017", division = 1, workout = "17.1", scaled = FALSE, page = pg)


pg <- 3
url3 <- generate_leaderboard_url("2017", division = 1, workout = "17.1", scaled = FALSE, page = pg)


page1 <- render_html(pond, url1, resource_timeout=20) 
page2 <- render_html(pond, url2, resource_timeout=20) 
page3 <- render_html(pond, url3, resource_timeout=20) 

head(get_athlete_name(page1))
head(get_athlete_name(page2))
head(get_athlete_name(page3))

get_html_text <- function(page, css) {
  html_text(html_nodes(page, css = "td .full-name"))
}


get_athlete_name <- function(page, css = "td .full-name") {
  get_html_text(page, css)
}


num_pages    <- get_page_count(page)

athlete_url  <- get_athlete_urls(page)
athlete_name <- get_athlete_name(page)
athlete_id   <- get_athlete_ids(athlete_url)
rank_score   <- get_workout(page, workout = "17.1")



# close session
stop_splash(splash_svr)
