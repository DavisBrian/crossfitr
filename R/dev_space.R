# development
library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

source("./R/scrape_fns.R")

workout <- "15.3"
division <- 1
scaled <- FALSE
page <- 1

params <- get_params(workout, division)
url <- create_leaderboard_url(params)
gender <- get_gender(params$division)

html_page <- read_page(url)
npages <- get_page_count(html_page)
athlete_urls <- get_athlete_urls(html_page)
athlete_ids <- get_athlete_ids(athlete_urls)
# p1 <- page2df(html_page)

rank_scores <- get_rank_scores(html_page, params$stage$stage)

leaderboard <- data_frame(workout     = params$workout,
                          year        = params$year,
                          division    = params$division,
                          athlete_url = get_athlete_urls(html_page),
                          athlete_id  = get_athlete_ids(athlete_urls),
                          rank_score = get_rank_scores(html_page, params$stage$stage)[[1]]) %>%
  separate(rank_score, c("rank_pre", "score_pre", "scaled_pre"), sep = "[\\(\\)]", remove = FALSE) %>%
  mutate(rank       = convert_ranks(rank_pre),
         scores     = convert_scores(score_pre),
         scaled_flg = convert_scaled(scaled_pre),
         retrieved_datetime = Sys.time()) %>%
  select(-rank_score, -ends_with("pre"))


# n <- 8
# rank_scores <- p1[ , n]
# colnames(rank_scores) <- "WOD"

rs <- separate(rank_scores, WOD, c("rank_pre", "score_pre", "scaled_pre"), sep = "[\\(\\)]", remove = FALSE) %>%
  mutate(rank       = convert_ranks(rank_pre),
         scores     = convert_scores(score_pre),
         scaled_flg = convert_scaled(scaled_pre),
         retrieved_datetime = Sys.time())

rs %>% View()

leaderboard <- html_page %>%
  

rp <- convert_rank(rs$rank_pre)
scores <- convert_scores(rs$score_pre) 
scores <- rs$score_pre

scaled <- convert_scaled(rs$scaled_pre)
identical(scaled, TRUE)

scaled_str <- rs$scaled_pre

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

# split_scores

separate(rank_scores, "Workout05", c("rank", "score", "scaled"), sep = "[\\(\\)]") %>%
  mutate(rank = if_else(rank == "-- ", as.integer(NA), as.integer(rank)),
         scaled = (scaled == " - s"),
         score  = ms(score))

convert_rank <- function(rank) {
  if_else(rank == "--", as.integer(NA), as.integer(rank))
}

# is_scaled

leaderboard[rank_score == "--\n                No score", rank_score := NA]
leaderboard[,rank  := as.integer(sub(" .*", "", rank_score))]
leaderboard[,score_pre := unlist(str_split(rank_scores, "[\\(\\)]"))[2], by=athlete_id]
leaderboard[,scaled := as.integer(substring(rank_score, 
                                            nchar(rank_score)-3,
                                            nchar(rank_score)) == " - s")]


