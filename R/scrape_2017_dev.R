library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

source("./R/scrape_leaderboard_fns.R")
source("./R/scrape_athlete_fns.R")

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

get_athlete_urls <- function(page, css = "td.name a") {
  urls <- html_attr(html_nodes(page, css), "href")
  if(length(urls) == 0){
    stop("No athlete urls found.")
  }
  urls
}


# need to handle legacy board  (css = ".button" maybe?)
get_page_count <- function(page, css = ".nums a") {
  max(as.integer(get_html_text(pg, css)))
}

get_rank_scores <- function(page, css)  {
  get_html_text(page, css)
  
}




## Code ------------------------------------------------------------------------


# Looks like the "New" website is Dynamically generated
# going ot have to use splashr or RSelenium
#
# https://www.r-bloggers.com/diving-into-dynamic-website-content-with-splashr/

# test out on a local html version of the site
file <- "./data/htmlTest/Leaderboard _ CrossFit Games.html"

pg <- read_html(file)

get_header <- function(page, css = "th") {
  get_html_text(page, css)
}

num_pages    <- get_page_count(pg)

athlete_url  <- get_athlete_urls(pg)
athlete_name <- get_html_text(pg, "td .full-name")
athlete_id   <- get_athlete_ids(athlete_url)
rank_score   <- get_rank_scores(pg, ".total-points+ .score .rank-result") 


# grab all the scores
header       <- get_header(pg, css = "th a")
css <- ".rank-result"
as_tibble(matrix(html_text(html_nodes(page, css)), ncol = length(header), byrow = TRUE))
# games specifc functiosn

# get leader board

string <- ".total-points+ .score .rank-result"
get_html_text(pg, string)

# grab all the scores
string <- ".rank-result"
html_text(html_nodes(pg, string))

# grap 17.2
string <- ".score:nth-child(5) .rank-result"
html_text(html_nodes(pg, string))


# table olf all scores
html_table(html_nodes(pg, "table")) %>% View()



# get the athletes profile url
html_attr(html_nodes(pg, "td.name a.profile-link"), "href")
html_text(html_nodes(pg, "td.name div.full-name"))




athlete_link <-  html_attr(html_nodes(pg, "td.name a"), "href")
athlete_name <-  html_text(html_nodes(pg, "td .full-name"))

html_table(pg, fill=TRUE) %>% View()

# grab 17.1
string <- ".total-points+ .score .rank-result"
html_text(html_nodes(pg, string))



# grap 17.2
string <- ".score:nth-child(5) .rank-result"
html_text(html_nodes(pg, string))


# get the score
get_rank_scores <- function(html_page, stage = 0) {
  rs <- html_table(html_page, fill=TRUE)[[1]]
  as_tibble(rs[paste0("Workout0",stage)])
}
