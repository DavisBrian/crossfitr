# scrape functions
source("./R/scrape_fns_helper.R")

get_params <- function(workout, division, scaled = FALSE) {
  
  year <- get_year(workout)
  stage <- get_stage(workout)
  
  check_division(division, year)
  
  if (!is.logical(scaled)) {
    stop("scaled must be TRUE or FALSE")
  }
  if (length(scaled) != 1L) {
    stop("Only one value for scaled allowed.")
  }
  if (as.integer(year) < 15L) {
    stop(paste0("Scaled was not an otion in 20", year))
  }
  scaled <- as.character(as.integer(scaled))
  
  list(workout  = workout,
       year     = year, 
       stage    = stage,
       division = as.integer(division),
       scaled   = scaled,
       npages   = as.integer(NA))
}


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
