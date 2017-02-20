# look up tables

# division
divisions <- frame_data(
  ~div_id , ~name,
  #-- / ------ 
   1L , "Individual Men",
   2L , "Individual Women",
   3L , "Masters Men (45-49)",
   4L , "Masters Women (45-49)",
   5L , "Masters Men (50-54)",
   6L , "Masters Women (50-54)",
   7L , "Masters Men (55-59)",
   8L , "Masters Women (55-59)",
   9L , "Masters Men (60+)",
  10L , "Masters Women (60+)",
  11L , "Team",
  12L , "Masters Men (40-44)",
  13L , "Masters Women (40-44)",
  14L , "Teenage Boys (14-15)",
  15L , "Teenage Girls (14-15)",
  16L , "Teenage Boys (16-17)",
  17L , "Teenage Girls (16-17)",
  18L , "Masters Men (35-39)",
  19L , "Masters Women (35-39)"
)

games <- frame_data(
  ~year, ~scaled, ~divisions,
  16, TRUE,  c(1:17),
  15, TRUE,  c(1:17),
  14, FALSE, c(1:13),
  13, FALSE, c(1:11),
  12, FALSE, c(1:11),
  11, FALSE, c(1:2)
)


get_gender <- function(division) {
  coed_divs   <- c(11)
  male_divs   <- c(1, 3, 5, 7, 9, 12, 14, 16, 18)
  female_divs <- c(2, 4, 6, 8, 10, 13, 15, 17, 19)
  case_when(
    division %in% coed_divs   ~ "COED",
    division %in% male_divs   ~ "M",    
    division %in% female_divs ~ "F",
    TRUE                      ~ as.character(NA)
  )
}
