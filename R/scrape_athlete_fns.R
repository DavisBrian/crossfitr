# scrape_athlete_data
library(purrr)

# scrape functions
source("./R/scrape_fns.R")

create_athlete_url <- function(id) {
  paste0("https://games.crossfit.com/athlete/",id)
}

DeUnitWeight <- function(x){
  if(x %in% c("--","", NA)) {return(as.integer(NA))}
  unit  <- substr(x,
                  nchar(x)-1,
                  nchar(x))
  value <- as.numeric(substr(x,
                             1, nchar(x)-3))
  value <- switch(unit,
                  kg = value * 2.20462,
                  lb = value,
                  NA)
  return(value)
}

DeUnitHeight <- function(x) {
  if(x %in% c("--","", NA)) {return(as.numeric(NA))}
  
  n.char <- nchar(x)
  if(substr(x,2,2) == "'"){
    feet <- as.numeric(substr(x,1,1))
    inch <- as.numeric(substr(x,3,n.char-1))
    return(feet*12 + inch)
  } else if(substr(x, n.char-1, n.char) == "cm"){
    cm <- substr(x, 1, n.char-3)
    return(as.numeric(cm) * 0.393701)
  } else {
    return(NA)
  }
}

# basic stats
clean_up_text <- function(x) {
  trimws(gsub("\\n", '', x))
}


get_athlete_name <- function(page) {
  athlete_name <- page %>%
    html_nodes(".c-heading-page-cover") %>%
    html_text() %>% 
    clean_up_text() %>%
    gsub("[ ]+", " ", x=.)
  
  athlete_name
}

get_basic_stats <- function(page) {
  labels <- clean_up_text(html_text(html_nodes(page, "ul.infobar .item-label")))
  text   <- clean_up_text(html_text(html_nodes(page, "ul.infobar .text")))
  
  if (length(labels) == length(text)) {
    x <- data_frame(stat  = tolower(labels),
                    value = text) %>%
      spread(stat, value) %>%
      mutate(age    = as.integer(age),
             height = DeUnitHeight(height),
             weight = DeUnitWeight(weight))
  }
  else {
    stop("Basic Stats parsing error.")
  } 
  
  x
}

MinSecToSec <- function(x){
  if(x %in% c("--","","0:00",NA)) {
    return(ms(NA, quiet = TRUE))
  } else {
    return(ms(x))
  }
}

fix_benchmark_labels <- function(labels) {
  # gsub("-", '', gsub(" ", "_", tolower(labels)))
  labels %>%
    tolower() %>%
    gsub(" ", "_", .) %>%
    gsub("-", '', .)
}

get_benchmark_labels <- function(page) {
  labels <- clean_up_text(html_text(html_nodes(page, "table.stats th")))
  
  if (all(labels %in% "")) {
    labels <- c("back_squat",
                "clean_and_jerk",
                "deadlift",
                "fight_gone_bad",
                "filthy_50",
                "fran",
                "grace",
                "helen",
                "max_pullups",
                "run_5k",
                "snatch",
                "sprint_400m")
  }
  
  return(labels)
}

get_benchmark_stats <- function(page) {
  labels <- get_benchmark_labels(page)
  
  #make sure we have a Benchmark Stats section
  hdr <- get_html_text(page, "#profileStats+ .page-section .c-heading-data-sub")
  if (length(hdr) == 0L) {
    text <- rep(NA, length(labels))
  } else if (length(hdr) > 1L) {
    stop("Error parsing Benchmark Stats Header")
  } else if (hdr == " Benchmark Stats ") {
    # text   <- clean_up_text(html_text(html_nodes(page, "table.stats td")))
    text <- clean_up_text(get_html_text(page, "table.stats td"))
    
    if (length(labels) != length(text)) {
      stop("Benchmark Stats parsing error. Length of labels and values do not match.")
      
    }
    
  } else {
    stop("Error parsing Benchmark Stats.")
  }

  x <- data_frame(stat  = fix_benchmark_labels(labels),
                  value = text) %>%
    spread(stat, value) %>%
    mutate(back_squat     = DeUnitWeight(back_squat),
           clean_and_jerk = DeUnitWeight(clean_and_jerk),
           deadlift       = DeUnitWeight(deadlift),
           fight_gone_bad = as.integer(fight_gone_bad),
           filthy_50      = MinSecToSec(filthy_50),
           fran           = MinSecToSec(fran),
           grace          = MinSecToSec(grace),
           helen          = MinSecToSec(helen),
           max_pullups    = as.integer(max_pullups),
           run_5k         = MinSecToSec(run_5k),
           snatch         = DeUnitWeight(snatch),
           sprint_400m    = MinSecToSec(sprint_400m))
  x
}

get_athlete <- function(athlete_id){
  
  url <- create_athlete_url(athlete_id)
  profile_page <- read_page(url)
  
  if (is.null(profile_page)) {
    return(NULL)
  }
  
  basic_stats <- get_basic_stats(profile_page)
  bench_stats <- get_benchmark_stats(profile_page)
  
  
  athlete <- data_frame(id           = athlete_id,
                        athlete_name = get_athlete_name(profile_page)) %>%
    cbind(basic_stats, bench_stats) %>%
    mutate(retrieved_datetime = Sys.time())
  
  return(athlete)
}

