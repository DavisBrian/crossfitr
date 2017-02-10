# development
library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

source("./R/scrape_leaderboard_fns.R")

workout <- "15.3"
division <- 1
scaled <- FALSE
page <- 1

# params <- get_params(workout, division, scaled)
# x <- get_leaderboard_page(params, page = 1) %>% View()



t1 <- Sys.time()

open15_3 <- get_leaderboard(workout, division, scaled)

t2 <- Sys.time()

t2-t1

