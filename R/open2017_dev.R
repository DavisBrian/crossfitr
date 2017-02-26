# New Version

library(httr)
library(jsonlite)
library(tidyverse)

# TBD: add sort
get_leaderboard <- function(year=2017L, division=1L, scaled = 0L, page=1L) {
  httr::GET(url = "https://games.crossfit.com",
            path = "/competitions/api/v1/competitions/open/2017/leaderboards",
            query = list(competition=1L, year=year, division=division, scaled=scaled,
                         sort=0, fittest=1, fittest1=0, occupation=0, page=page)) -> res
  
  res <- httr::stop_for_status(res)
  
  res <- httr::content(res, as="text", encoding="UTF-8")
  
  res <- jsonlite::fromJSON(res, simplifyDataFrame = TRUE, flatten = TRUE)
  
  res_cols <- cols(name = col_character(), userid = col_character(),
                   overallrank = col_integer(), overallscore = col_integer(),
                   regionid = col_integer(), region = col_character(), affiliateid = col_character(),
                   affiliate = col_character(), height = col_character(), weight = col_character(),
                   profilepic = col_character() )
  
  res$athletes <- readr::type_convert(res$athletes, col_types = res_cols)
  res$athletes <- tibble::as_tibble(res$athletes)
  
  res
}

pull_leaderbaord <- function(division = 1L, scaled = 0L, chunk_size = 100L, chunk_start  = 1L) {
  
  first <- get_leaderboard(division = division, scaled = scaled)
  first$currentpage
  first$totalpages
  
  n_pages <- first$totalpages
  nchunks <- ceiling(n_pages / chunk_size)
  # nchunks <- 4
  
  for (i in chunk_start:nchunks) {
    start <- 1+((i-1)*chunk_size)
    end   <- min(i*chunk_size, n_pages)
    
    message(paste0("chunk ", i, "\n"))
    pb <- progress_estimated(end-start+1)
    map_df(start:end, function(pg) {
      pb$tick()$print() # progress meter when used interactively
      res <- get_leaderboard(page = pg)
      res <- res$athletes
      Sys.sleep(sample(60, 120, 1)) # pretend you're a human so you don't get IP banned
      res
    }) -> athletes
    
    leaderboard <- athletes %>%
      unnest(scores) %>% 
      group_by(userid) %>%
      mutate(workout = paste0("17.", row_number())) %>%
      filter(scoreidentifier != "--")
    
    ofile <- paste0("./data/", "2017_17_1_dv_", division, "scale_", scaled, "chunk_", i)
    saveRDS(leaderboard, file = ofile)
    Sys.sleep(sample(3, 7, 1)) # pretend you're a human so you don't get IP banned
  }
  
  return(TRUE)
}

pull_leaderbaord(division = 1L, chunk_start = 4)

x1 <- get_leaderboard(year=2017L, division=1L, scaled = 0L, page=334L)
x2 <- get_leaderboard(year=2017L, division=1L, scaled = 0L, page=335L)
x3 <- get_leaderboard(year=2017L, division=1L, scaled = 0L, page=336L)

page<- 342L
x <- purrr::safely(get_leaderboard(year=2017L, division=1L, scaled = 0L, page=336L))

pull_leaderbaord(division = 2L)

