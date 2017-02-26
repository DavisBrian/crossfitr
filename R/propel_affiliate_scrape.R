# Basics of scrape of the Propel affiliate

library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

source("./R/scrape_affiliate_roster.R")
source("./R/lookup_tables.R")

propel_id <- 9244

roster <- get_affiliate_roster(propel_id) 
athlete_profiles <- roster$athlete_ids %>% get_athletes()

# add in gender (from Division)
athlete_profiles <- athlete_profiles %>% 
  left_join(., divisions, by = c("division" = "name")) %>% 
  mutate(gender = get_gender(div_id))

# save off the data set so we don't have to scrape again
out_dir <- "./data/Propel/"
if (!dir.exists(out_dir)){
  dir.create(out_dir, recursive = TRUE)
}

out_file <- "propel_athlete_profiles_2017.rds"
saveRDS(athlete_profiles, file = paste0(out_dir, out_file))

# Update the athlete profiles
ap <- readRDS(paste0(out_dir, out_file))
new_athletes <- setdiff(roster$athlete_ids, ap$id) %>%
  get_athletes()

athlete_profiles <- bind_rows(ap, new_athletes)

out_file <- "propel_athlete_profiles_2017_complete.rds"
saveRDS(athlete_profiles, file = paste0(out_dir, out_file))


