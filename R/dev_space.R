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
open15_5 <- get_leaderboard(workout, division, scaled)
t2 <- Sys.time()

t2-t1

workout <- "12.1"
division <- 1
scaled <- FALSE
page <- 1

t1 <- Sys.time()
open121 <- get_leaderboard(workout, division, scaled)
t2 <- Sys.time()
t2-t1

workout <- "13.2"
division <- 1
scaled <- FALSE
page <- 1

t1 <- Sys.time()
open132 <- get_leaderboard(workout, division, scaled)
t2 <- Sys.time()
t2-t1

workout <- "14.3"
division <- 1
scaled <- FALSE
page <- 1

t1 <- Sys.time()
open143 <- get_leaderboard(workout, division, scaled)
t2 <- Sys.time()
t2-t1

workout <- "15.4"
division <- 1
scaled <- FALSE
page <- 1

t1 <- Sys.time()
open154 <- get_leaderboard(workout, division, scaled)
t2 <- Sys.time()
t2-t1

workout <- "16.5"
division <- 1
scaled <- FALSE
page <- 1

t1 <- Sys.time()
open165 <- get_leaderboard(workout, division, scaled)
t2 <- Sys.time()
t2-t1