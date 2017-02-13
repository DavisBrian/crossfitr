# development
library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

source("./R/scrape_leaderboard_fns.R")

open2016 <- paste0("16.", seq(1:5))
open2015 <- c(paste0("15.", seq(1:5)), "15.1A)")
open2014 <- paste0("14.", seq(1:5))
open2013 <- paste0("13.", seq(1:5))
open2012 <- paste0("12.", seq(1:5))

open <- c(open2016, open2015, open2014, open2013, open2012)

t1 <- Sys.time()

for (o in open) {
  workout <- o
  for(division in 1:10) {
    file_name <- paste0("./data/open", gsub("[\\.]", "_", o), "_div", division, "_scale0", ".rds")
    leaderboard <- get_leaderboard(workout, division, scaled = FALSE)
    saveRDS(leaderboard, file = file_name)
  }
}

t2 <- Sys.time()
t2-t1


# 
# 
# 
# t1 <- Sys.time()
# open15_5 <- get_leaderboard(workout, division, scaled)
# t2 <- Sys.time()
# 
# t2-t1
# 
# workout <- "12.1"
# division <- 1
# scaled <- FALSE
# page <- 1
# 
# t1 <- Sys.time()
# open121 <- get_leaderboard(workout, division, scaled)
# t2 <- Sys.time()
# t2-t1
# 
# workout <- "13.2"
# division <- 1
# scaled <- FALSE
# page <- 1
# 
# t1 <- Sys.time()
# open132 <- get_leaderboard(workout, division, scaled)
# t2 <- Sys.time()
# t2-t1
# 
# workout <- "14.3"
# division <- 1
# scaled <- FALSE
# page <- 1
# 
# t1 <- Sys.time()
# open143 <- get_leaderboard(workout, division, scaled)
# t2 <- Sys.time()
# t2-t1
# 
# workout <- "15.4"
# division <- 1
# scaled <- FALSE
# page <- 1
# 
# t1 <- Sys.time()
# open154 <- get_leaderboard(workout, division, scaled)
# t2 <- Sys.time()
# t2-t1
# 
# workout <- "16.5"
# division <- 1
# scaled <- FALSE
# page <- 1
# 
# t1 <- Sys.time()
# open165 <- get_leaderboard(workout, division, scaled)
# t2 <- Sys.time()
# t2-t1