# Looks like the "New" website is Dynamically generated
# going ot have to use splashr or RSelenium
#
# https://www.r-bloggers.com/diving-into-dynamic-website-content-with-splashr/

# test out on a local html version of the site
file <- "./data/htmlTest/Leaderboard _ CrossFit Games.html"

html_page <- read_html(file)
html_table(html_nodes(html_page, "table"))

athlete_link <-  html_attr(html_nodes(html_page, "td.name a"), "href")
athlete_name <-  html_text(html_nodes(html_page, "td .full-name"))

# Install Docker

# devtools::install_github("wch/harbor")

# devtools::install_github("hrbrmstr/splashr")

# 2017 Open Roster Scrape

library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

# scrape functions
source("./R/scrape_fns.R")
source("./R/scrape_leaderboard_fns.R")
source("./R/lookup_tables.R")

# Currently 2017 redirects to:
# https://games.crossfit.com/leaderboard?page=1&competition=1&year=2017&division=2&scaled=0&sort=0&fittest=1&fittest1=0&occupation=0
create_2017_roster_url <- function(division, page = 1, scaled = FALSE) {
  paste0("https://games.crossfit.com/leaderboard?page=", page, 
         "&competition=1&year=2017&division=", division, 
         "&scaled=", scaled, 
         "&sort=0fittest=1&fittest1=0&occupation=0")
}

workout <- "17.0"
division <- 1

url <- create_2017_roster_url(division, page= 1, scaled = FALSE)
page <- read_page(url) 
s1 <- ".nums" 
s2 <- "#leaderboard .span+ a" 
s3 <- "tr .full-name"
s4 <- ".name , .full-name"

html_nodes(page, ".collapsed .name")
html_nodes(page, "div .full-name")
s3 <- "tr .full-name"
html_nodes(page, "td.full-name div")
urls <- html_attr(html_nodes(page, "td .full-name div"), "href")

html_text(html_nodes(page, "td .profile-link"))
html_attr(html_nodes(page, "td profile-link a"), "href")

html_text(html_nodes(page, ".profile-link a"))

npages <- url %>%
  read_page() %>%
  get_html_text(string = s2)

get_html_text <- function(page, string) {
  html_text(html_nodes(page, string))
}


url <- create_leaderboard_url(params, page)
html_page <- read_page(url)


# compare the 2
library(rvest)

# old site
old_url <- "https://games.crossfit.com/scores/leaderboard.php?stage=1&sort=1&page=1&division=1&region=0&numberperpage=100&competition=0&frontpage=0&expanded=0&year=16&scaled=0&full=1&showtoggles=0&hidedropdowns=1&showathleteac=1&is_mobile=1"
old_page <- read_html(old_url)

# get the athletes profile url
athlete_link <-  html_attr(html_nodes(old_page, "td.name a"), "href")
athlete_name <-  html_text(html_nodes(old_page, "td.name a"))

head(athlete_link)
head(athlete_name)

# new site
new_url <- "https://games.crossfit.com/leaderboard?page=1&competition=1&year=2017&division=2&scaled=0&sort=0&fittest=1&fittest1=0&occupation=0"
new_page <- read_html(new_url)

# get the athletes profile url
html_attr(html_nodes(new_page, "td.name a.profile-link"), "href")
html_text(html_nodes(new_page, "td.name div.full-name"))


"//*[contains(concat( " ", @class, " " ), concat( " ", "full-name", " " ))]"

"//*[contains(concat( " ", @class, " " ), concat( " ", "full-name", " " ))]"
html_table(html_nodes(new_page, "table"))

.total-points+ th , .total-points+ .score .rank-result
html_nodes(new_page, "td.rank-result")


ifile <- 