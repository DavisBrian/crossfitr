# scrape_athlete_data
library(purrr)


create_athlete_url <- function(id) {
  paste0("https://games.crossfit.com/athlete/",id)
}

athlete_id <- 132552
athlete_id <- 574825  # no rep
athlete_id <- 692214  # missing basic stats

url <- create_athlete_url(athlete_id)
profile_page <- read_page(url)

athlete_name <- html_text(html_nodes(profile_page, ".c-heading-page-cover")) %>%
  gsub("\\n", '', x=.)  %>% 
  trimws() %>%
  gsub("[ ]+", " ", x=.)
  

# basic stats
clean_up_text <- function(x) {
  trimws(gsub("\\n", '', x))
}

labels <-clean_up_text(html_text(html_nodes(profile_page, "ul.infobar .item-label")))
text   <- clean_up_text(html_text(html_nodes(profile_page, "ul.infobar .text")))


get_athlete <- function(athlete_id){
  
  url <- create_athlete_url(athlete_id)
  profile_page <- read_page(url)
  
  # if(html_text(html_nodes(profile_page, "h2#page-title")) == "Athlete: Not found"){
  #   print(url)
  #   stop("Special case 1 in get_athlete that needs to be handled.")
  #   return(data.table("athlete_id" = athlete_id))
  # }
  # athlete not found.  Skip them
  if (is.null(profile_page)) {
    return(NULL)
  }
  
  athlete_name <- html_text(html_nodes(profile_page, ".c-heading-page-cover"))
  region <- html_text(html_nodes(profile_page, ".infobar-container ul"))
  html_text(html_nodes(profile_page, ".sm-inline:nth-child(1) .text"))
  html_text(html_nodes(profile_page, ".sm-inline"))
  html_name(html_nodes(profile_page, ".sm-inline"))
  # labels  <- html_text(html_nodes(profile_page, "div.profile-details dl dt"))
  # demo    <- html_text(html_nodes(profile.page, "div.profile-details dl dd"))
  # stats   <- html_text(html_nodes(profile_page, "div.profile-stats td"))
  # history    <- html_nodes(profile.page, "div.history")
  # 
  # eat        <- c(html_text(html_nodes(history[[1]], "h4")),"")
  # train      <- c(html_text(html_nodes(history[[2]], "h4")),"")
  # background <- c(html_text(html_nodes(history[[3]], "h4")),"")
  # experience <- c(html_text(html_nodes(history[[4]], "h4")),"")
  # schedule   <- c(html_text(html_nodes(history[[5]], "h4")),"")
  # howlong    <- c(html_text(html_nodes(history[[6]], "h4")),"")
  
  
  athlete <- data.table(
    "athlete_id" = athlete.id,
    "name"       = substring(html_text(html_nodes(profile_page, "h2#page-title")),10),
    "region"     = paste(demo[which(labels == "Region:")],"",sep=""),
    "team"       = paste(demo[which(labels == "Team:")],"", sep=""),
    "affiliate"  = paste(demo[which(labels == "Affiliate:")],"",sep=""),
    "gender"     = paste(demo[which(labels == "Gender:")],"",sep=""),
    "age"        = paste(demo[which(labels == "Age:")],"",sep=""),
    "height"     = DeUnitHeight(paste(demo[which(labels == "Height:")],"",sep="")),
    "weight"     = DeUnitWeight(paste(demo[which(labels == "Weight:")],"",sep="")),
    "fran"       = MinSecToSec(stats[2]),
    "helen"      = MinSecToSec(stats[4]),
    "grace"      = MinSecToSec(stats[6]),
    "filthy50"   = MinSecToSec(stats[8]),
    "fgonebad"   = as.integer(stats[10]),
    "run400"     = MinSecToSec(stats[12]),
    "run5k"      = MinSecToSec(stats[14]),
    "candj"      = DeUnitWeight(stats[16]),
    "snatch"     = DeUnitWeight(stats[18]),
    "deadlift"   = DeUnitWeight(stats[20]),
    "backsq"     = DeUnitWeight(stats[22]),
    "pullups"    = as.integer(stats[24]),
    "eat"        = paste(eat, collapse="|"),
    "train"      = paste(train, collapse="|"),
    "background" = paste(background, collapse="|"),
    "experience" = paste(experience, collapse="|"),
    "schedule"   = paste(schedule, collapse="|"),
    "howlong"    = paste(howlong, collapse="|"),
    "retrieved_datetime" = Sys.time())
  
  return(athlete)
}