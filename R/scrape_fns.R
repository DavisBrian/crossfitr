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
  if (as.integer(year) < 15L & scaled) {
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


