# Registration FAQ / Deadlines
# https://games.crossfit.com/help/article/40

# development
library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

source("./R/scrape_leaderboard_fns.R")

# workout <- "15.1a"
# division <- 1
# scaled <- FALSE
# 
# params <- get_params(workout, division, scaled)
# url <- create_leaderboard_url(params)
# 
# lb <- get_leaderboard_page(params, page = 12)

open2016 <- paste0("16.", seq(1:5))
open2015 <- c(paste0("15.", seq(1:5)), "15.1A")
open2014 <- paste0("14.", seq(1:5))
open2013 <- paste0("13.", seq(1:5))
open2012 <- paste0("12.", seq(1:5))

open <- c(open2015, open2014, open2013, open2012)


t1 <- Sys.time()
# Get most of the open results.
for (o in open) {
  workout <- o
  
  out_dir <- paste0("./data/20", get_year(workout))
  if (!dir.exists(out_dir)){
    dir.create(out_dir, recursive = TRUE)
  }
  t0 <- Sys.time()
  for(division in 1:2) {
    file_name <- paste0(out_dir, "/open", gsub("[\\.]", "_", o), "_div", division, "_scale0", ".rds")
    leaderboard <- get_leaderboard(workout, division, scaled = FALSE)
    saveRDS(leaderboard, file = file_name)
  }
  t0b <-  Sys.time()
  message(paste0(workout, " took ", t0b-t0, " sec to run\n"))
}

t2 <- Sys.time()
t2-t1

