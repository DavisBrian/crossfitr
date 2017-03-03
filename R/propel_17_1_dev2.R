# propel 17_1 
# development
library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

indir <- "./data/Propel/"
page1_file <- "./data/Propel/Propel_17_1_pg1.html"
page2_file <- "./data/Propel/Propel_17_1_pg2.html"

# Pulling Functions ------------------------------------------------------------

create_affiliate_url <- function(id = 9244) {
  paste0("https://games.crossfit.com/affiliate/",id)
}

read_page <- function(xfit_url) {
  tryCatch({read_html(xfit_url)},
           error = function(cond) { return(NULL) })
}

# basic html helper functions / wrappers
get_html_text <- function(page, css) {
  html_text(html_nodes(page, css))
}


# Roster Functions -------------------------------------------------------------

get_athlete_name_roster <- function(page, css = "#affiliates .col-sm-3 a") {
  html_text(html_nodes(page, css))
}

get_athlete_urls_roster <- function(page, css = "#affiliates .col-sm-3 a") {
  urls <- html_attr(html_nodes(page, css), "href")
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

get_roster <- function(page) {
  roster <- data_frame(athlete = get_athlete_name_roster(page),
                       athlete_urls = get_athlete_urls_roster(page),
                       athlete_ids  = get_athlete_ids(athlete_urls)) %>%
    mutate(retrieved_datetime = Sys.time()) %>%
    select(- athlete_urls)
  
  roster
}


# Leaderboard functions --------------------------------------------------------
get_athlete_name <- function(page, css = ".full-name") {
  get_html_text(page, css)
}

get_header <- function(page, css = "th") {
  get_html_text(page, css)
}

get_workout <- function(page, css = ".rank-result", workout) {
  header <- get_header(page, css = "th a")
  if (all(workout %in% header)) {
    df <- get_html_text(page, css) %>%
      matrix(., ncol = length(header), byrow = TRUE) %>%
      as_tibble() %>%
      select(match(workout, header))
    colnames(df) <- if_else(length(workout) == 1L, 
                            "rank_score",
                            paste0("rank_score", 1:length(workout)))
  } else {
    stop("workout not found!")
  }
  
  df 
}

get_athlete_urls <- function(page, css = ".profile-link") {
  urls <- html_attr(html_nodes(page, css), "href")
  if(length(urls) == 0){
    stop("No athlete urls found.")
  }
  urls
}

get_leaderboard_page <- function(page, workout) {
  get_workout(page, workout = workout) %>% 
    mutate(name = get_athlete_name(page, css = ".full-name"),
           id   = get_athlete_ids(get_athlete_urls(page, css = ".profile-link")))
}

# Convert rank Score Strings Functions -----------------------------------------
convert_ranks <- function(rank_str) {
  x <- rep_len(integer(), length(rank_str))
  idx <- which(rank_str != "--")
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

# max time in minutes:seconds
get_scores_time <-function(scores_str, max_time = "20:00") {
  scores_str %>%
    gsub(" - s", '', .) %>%
    gsub("^(.)+reps", max_time, .)
}

get_scores_reps <-function(scores_str, max_reps = "225") {
  scores_str %>%
    gsub(" - s", '', .) %>%
    gsub("([0-9]{1,2}:[0-9]{2})", max_reps, .) %>%
    gsub(" reps", '', .)
}

is_scaled <- function(scores_str) {
  grepl("- s", scores_str)
}

# Scrape page into a data frame ------------------------------------------------

page1 <- read_html(page1_file)
page2 <- read_html(page2_file)

wo <- bind_rows(get_leaderboard_page(page1, workout = "17.1"), 
                get_leaderboard_page(page2, workout = "17.1")) %>%
  separate(rank_score, c("rank_pre", "score_pre", "scaled_pre"), sep = "[\\(\\)]", remove = FALSE) %>% 
  mutate(rank     = convert_ranks(rank_pre),
         time     = convert_scores(get_scores_time(score_pre)),
         reps     = convert_scores(get_scores_reps(score_pre)),
         scaled   = is_scaled(score_pre)) %>%
  select(id, name, rank_score, rank, time, reps, scaled)


# basic starts
# - How many scaled
# - how many didn't enter their data / have it validated


# time plot
# rep plot
# propel 17_1 
# development
library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

indir <- "./data/Propel/"
page1_file <- "./data/Propel/Propel_17_1_pg1.html"
page2_file <- "./data/Propel/Propel_17_1_pg2.html"

# Pulling Functions ------------------------------------------------------------

create_affiliate_url <- function(id = 9244) {
  paste0("https://games.crossfit.com/affiliate/",id)
}

read_page <- function(xfit_url) {
  tryCatch({read_html(xfit_url)},
           error = function(cond) { return(NULL) })
}

# basic html helper functions / wrappers
get_html_text <- function(page, css) {
  html_text(html_nodes(page, css))
}


# Roster Functions -------------------------------------------------------------

get_athlete_name_roster <- function(page, css = "#affiliates .col-sm-3 a") {
  html_text(html_nodes(page, css))
}

get_athlete_urls_roster <- function(page, css = "#affiliates .col-sm-3 a") {
  urls <- html_attr(html_nodes(page, css), "href")
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

get_roster <- function(page) {
  roster <- data_frame(athlete = get_athlete_name_roster(page),
                       athlete_urls = get_athlete_urls_roster(page),
                       athlete_ids  = get_athlete_ids(athlete_urls)) %>%
    mutate(retrieved_datetime = Sys.time()) %>%
    select(- athlete_urls)
  
  roster
}


# Leaderboard functions --------------------------------------------------------
get_athlete_name <- function(page, css = ".full-name") {
  get_html_text(page, css)
}

get_header <- function(page, css = "th") {
  get_html_text(page, css)
}

get_workout <- function(page, css = ".rank-result", workout) {
  header <- get_header(page, css = "th a")
  if (all(workout %in% header)) {
    df <- get_html_text(page, css) %>%
      matrix(., ncol = length(header), byrow = TRUE) %>%
      as_tibble() %>%
      select(match(workout, header))
    colnames(df) <- if_else(length(workout) == 1L, 
                            "rank_score",
                            paste0("rank_score", 1:length(workout)))
  } else {
    stop("workout not found!")
  }
  
  df 
}

get_athlete_urls <- function(page, css = ".profile-link") {
  urls <- html_attr(html_nodes(page, css), "href")
  if(length(urls) == 0){
    stop("No athlete urls found.")
  }
  urls
}

get_leaderboard_page <- function(page, workout) {
  get_workout(page, workout = workout) %>% 
    mutate(name = get_athlete_name(page, css = ".full-name"),
           id   = get_athlete_ids(get_athlete_urls(page, css = ".profile-link")))
}

# Convert rank Score Strings Functions -----------------------------------------
convert_ranks <- function(rank_str) {
  x <- rep_len(integer(), length(rank_str))
  idx <- which(rank_str != "--")
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

# max time in minutes:seconds
get_scores_time <-function(scores_str, max_time = "20:00") {
  scores_str %>%
    gsub(" - s", '', .) %>%
    gsub("^(.)+reps", max_time, .)
}

get_scores_reps <-function(scores_str, max_reps = "225") {
  scores_str %>%
    gsub(" - s", '', .) %>%
    gsub("([0-9]{1,2}:[0-9]{2})", max_reps, .) %>%
    gsub(" reps", '', .)
}

is_scaled <- function(scores_str) {
  grepl("- s", scores_str)
}

# Scrape page into a data frame ------------------------------------------------

page1 <- read_html(page1_file)
page2 <- read_html(page2_file)

# p1 <- get_leaderboard_page(page1, workout = "17.1")
# p2 <- get_leaderboard_page(page2, workout = "17.1")

# combine into 1

wo <- bind_rows(get_leaderboard_page(page1, workout = "17.1"), 
                get_leaderboard_page(page2, workout = "17.1"))

df <- wo %>%
separate(rank_score, c("rank_pre", "score_pre", "scaled_pre"), sep = "[\\(\\)]", remove = FALSE) %>% 
mutate(rank     = convert_ranks(rank_pre),
       time     = convert_scores(get_scores_time(score_pre)),
       reps     = convert_scores(get_scores_reps(score_pre)),
       scaled   = is_scaled(score_pre))

df %>% View()
saveRDS(df, file = paste0(indir, "propel_17_1_results.rds"))
# How many people didn't submit scores
nrow(df)
sum(is.na(df$time))
mean(is.na(df$time))

# Participation and scaling

# rep plot
reps <- cumsum(c(10,15,20,15,30,15,40,15,50,15))+1
x_min <- reps[seq(1,9,2)]
x_max <- reps[seq(2,10,2)]-1

breaks.rx <- tibble("reps" = cumsum(c(10,15,20,15,30,15,40,15,50,15))+1)
# rounds.rx <- tibble("x_min" = breaks.rx[seq(1,9,2), reps],
#                     "x_max" = breaks.rx[seq(2,10,2), reps-1])
rounds.rx <- tibble("x_min" = reps[seq(1,9,2)],
                    "x_max" = reps[seq(2,10,2)]-1)


  ggplot(data = df[!is.na(df$time), ]) +
    geom_rect(data=rounds.rx, aes(xmin=x_min, xmax=x_max+1), 
              ymin=0, ymax=10000, alpha=0.2, fill="grey60") +
  geom_vline(data=breaks.rx, aes(xintercept = reps), color="darkgrey", alpha=.7) + 
  geom_histogram(aes(x=reps), binwidth=1) +
  scale_x_continuous(limits = c(0,226),
                     breaks = breaks.rx$reps) +
  labs(x="17.1 Score",y="# of Athletes", title="Crossfit Open 17.1 Scores (Rx)") +
  # facet_grid(gender~.,scale="free_y") +
  theme_bw(base_size=14) +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))



ggplot(leaderboard.15[wod=="15.4" & scaled =="Rx" & !is.na(score),]) +
  geom_rect(data=rounds.rx, aes(xmin=x_min, xmax=x_max+1), 
            ymin=0, ymax=10000, alpha=0.2, fill="grey60") +
  geom_vline(data=breaks.rx, aes(xintercept = reps), color="darkgrey", alpha=.7) + 
  geom_histogram(aes(x=score, fill=gender), binwidth=1)+
  scale_x_continuous(limits = c(0,148),
                     breaks = breaks.rx[,reps])+
  labs(x="15.4 Score",y="# of Athletes", title="Crossfit Open 15.4 Scores (Rx)") +
  facet_grid(gender~.,scale="free_y") +
  theme_bw(base_size=14) +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
# time plot
