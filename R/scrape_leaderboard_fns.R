# scrape functions
source("./R/scrape_fns.R")

create_leaderboard_url <- function(params, page = 1) {
  stage = params$stage$stage_pos
  sort  = params$stage$stage_pos
  div   = params$division
  yr    = params$year
  scaled  = params$scaled
  
  paste0("https://games.crossfit.com/scores/leaderboard.php?stage=", stage,
         "&sort=", sort,"&page=", page,"&division=", div,
         "&region=0&numberperpage=100&competition=0&frontpage=0&expanded=0",
         "&year=", yr, "&scaled=", scaled,
         "&full=1&showtoggles=0&hidedropdowns=1&showathleteac=1&is_mobile=1")
}


get_leaderboard_page <- function(params, page) {
  url <- create_leaderboard_url(params, page)
  html_page <- read_page(url)
  
  leaderboard <- data_frame(workout     = params$workout,
                            year        = params$year,
                            division    = params$division,
                            athlete_url = get_athlete_urls(html_page),
                            athlete_id  = get_athlete_ids(athlete_url),
                            rank_score = get_rank_scores(html_page, params$stage$stage)[[1]]) %>%
    separate(rank_score, c("rank_pre", "score_pre", "scaled_pre"), sep = "[\\(\\)]", remove = FALSE) %>%
    mutate(rank       = convert_ranks(rank_pre),
           score      = convert_scores(score_pre),
           scaled_flg = if_else(year >= 15L, convert_scaled(scaled_pre), FALSE),
           retrieved_datetime = Sys.time()) %>%
    select(- athlete_url, -rank_score, -ends_with("pre"))
  
  leaderboard
}


get_leaderboard <- function(workout, division, scaled = FALSE) {
  params <- get_params(workout, division, scaled)
  url <- create_leaderboard_url(params)
  
  npages <- url %>%
    read_page() %>%
    get_page_count()
  
  leaderboard <- plyr::ldply(seq(1:npages), get_leaderboard_page, params = params)
  
  # only return those with a score
  as_tibble(leaderboard) %>%
    filter(!is.na(score))
}
