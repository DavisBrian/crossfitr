# look up tables

# division
#  1 = "Individual Men"
#  2 = "Individual Women"
#  3 = "Masters Men 45-49"
#  4 = "Masters Women 45-49"
#  5 = "Masters Men 50-54"
#  6 = "Masters Women 50-54"
#  7 = "Masters Men 55-59"
#  8 = "Masters Women 55-59"
#  9 = "Masters Men 60+"
# 10 = "Masters Women 60+"
# 11 = "Team"
# 12 = "Masters Men 40-44"
# 13 = "Masters Women 40-44"
# 14 = "Teenage Boys 14-15"
# 15 = "Teenage Girls 14-15"
# 16 = "Teenage Boys 16-17"
# 17 = "Teenage Girls 16-17"
divisions <- frame_data(
  ~id , ~name,
  #-- / ------ 
   1L , "Individual Men",
   2L , "Individual Women",
   3L , "Masters Men 45-49",
   4L , "Masters Women 45-49",
   5L , "Masters Men 50-54",
   6L , "Masters Women 50-54",
   7L , "Masters Men 55-59",
   8L , "Masters Women 55-59",
   9L , "Masters Men 60+",
  10L , "Masters Women 60+",
  11L , "Team",
  12L , "Masters Men 40-44",
  13L , "Masters Women 40-44",
  14L , "Teenage Boys 14-15",
  15L , "Teenage Girls 14-15",
  16L , "Teenage Boys 16-17",
  17L , "Teenage Girls 16-17"
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


