# scrape helper functions
library(tidyverse)
library(rvest)

# create the url at games.crossfit.com to get the scores -----------------------
# stage=0&
# sort=1&
# division=1&
# region=0&
# regional=5
# &numberperpage=100&
# page=0&
# competition=0&
# frontpage=0&
# expanded=0&
# full=1&
# year=15&
# showtoggles=0&
# hidedropdowns=1&
# showathleteac=1&
# athletename=&
# fittest=1&
# fitSelect=0&
# scaled=0&
# occupation=0

# DIVISION
# SORT
# 
create_url <- function(year = 14, division = 1, stage = 1, page = 1){
  paste0(
    "http://games.crossfit.com/scores/leaderboard.php?stage=",round(stage),"&sort=",round(stage),"&page=",page,
    "&division=",division,"&region=0&numberperpage=100&competition=0&frontpage=0",
    "&expanded=0&year=",year,"&full=1&showtoggles=0&hidedropdowns=1",
    "&showathleteac=1&is_mobile=1")
}

# read the page at games.crossfit.com ------------------------------------------
read_page <- function(xfit_url) {
  tryCatch({read_html(xfit_url)},
           error = function(cond) { return(NULL) })
}

# Get the urls for the athlete pages -------------------------------------------
get_athlete_urls <- function(html_page) {
  urls <- html_attr(html_nodes(html_page, "td.name a"), "href")
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

get_rank_scores <- function(html_page, stage = 0) {
  rs <- html_table(html_page, fill=TRUE)[[1]]
  if(stage == 1.1){
    as_tibble(rs[paste0("Workout01A")])
  } else {
    as_tibble(rs[paste0("Workout0",stage)])
  }
}

page2df <- function(html_page) {
  p <- html_table(html_page, fill=TRUE)[[1]]
  if (colnames(p)[1] == "") {
    colnames(p)[1] <- "Rank"
  } else {
    stop("Unexpected name in column 1.")
  }
  
  as_tibble(p)
}


convert_ranks <- function(rank_str) {
  x <- rep_len(integer(), length(rank_str))
  idx <- which(rank_str != "-- ")
  x[idx] <- as.integer(rank_str[idx])
  x
}

convert_scores <-function(scores_str) {
  if (any(grepl(":", scores_str))) {
    ms(scores_str, quiet = TRUE)
  } else {
    x <- rep_len(integer(), length(scores_str))
    idx <- which(scores_str != "--")
    x[idx] <- as.integer(scores_str[idx])
    x
  }
}
