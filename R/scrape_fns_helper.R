# scrape helper functions
library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)



# get text from a css string
get_html_text <- function(page, string) {
  html_text(html_nodes(page, string))
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

get_page_count <- function(html_page) {
  # as.integer(html_text(html_nodes(html_page, "div#leaderboard-pager a.button")[[1]]))
  as.integer(html_text(html_nodes(html_page, "div#leaderboard-pager a.button")[1]))
}


get_rank_scores <- function(html_page, stage = 0) {
  rs <- html_table(html_page, fill=TRUE)[[1]]
  as_tibble(rs[paste0("Workout0",stage)])
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

convert_scaled <- function(scaled_str) {
  case_when(
    scaled_str == " - s" ~ TRUE,
    scaled_str == ""     ~ FALSE,
    TRUE                 ~ NA
    # scaled_str == "\n                    No score" ~ NA,
  )
}

is_scaled <- function(x) { !is.na(x) & x }
is_rxd    <- function(x) { !is.na(x) & !x }


get_year <- function(workout) {
  yr <- str_split(workout, "\\.")[[1]][1]
  if (!(yr %in% c("12", "13", "14", "15", "16", "17"))) {
    stop(paste0(yr, " is not a valid year to pull from."))
  }
  yr
}

get_stage <- function(workout) {
  yr <- get_year(workout)
  stg <- toupper(str_split(workout, "\\.")[[1]][2])
  
  if (yr == "15") {
    stage_list <- c("1", "1A", "2", "3", "4", "5")
  } else {
    stage_list <- c("1", "2", "3", "4", "5")
  }
  
  if (!(stg %in% stage_list)) {
    stop(paste0("Stage: ", stg, " is not a valid stage for 20", yr))
  } else {
    stage = which(stage_list %in% stg)
  }
  
  list(stage_pos = as.character(stage),
       stage     = stg)
}


check_division <- function(division, year = NULL) {
  if (is.null(year)) {
    if (!between(division, 1, 17)) {
      stop("Invalid division.")
    }
  }
  
  year_int <- as.integer(year)
  
  if (year_int >= 15) {
    if (!between(division, 1, 17)) {
      stop(paste0("Invalid division for 20", year, "."))
    } 
  } else if (year_int == 14) {
    if (!between(division, 1, 13)) {
      stop(paste0("Invalid division for 20", year, "."))
    } 
  } else if (year_int < 14) {
    if (!between(division, 1, 11)) {
      stop(paste0("Invalid division for 20", year, "."))
    } 
  }
  
  invisible(NULL)
}

