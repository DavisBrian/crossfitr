# 2017 has divisions 1-18, and all have the ability to scale

open2017 <- divisions %>%
  filter(name  != "Team") %>%
  mutate(scaled = TRUE)

# 17.1 Analysis

library(tidyverse)
library(lubridate)
library(stringr)

workout <- frame_data(
  ~round , ~rep1 , ~rep2,
  #----- / ----- / ----- 
  1L     , 10L   , 15L,
  2L     , 20L   , 15L,
  3L     , 30L   , 15L,
  4L     , 40L   , 15L,
  5L     , 50L   , 15L
) %>%
  mutate(round_total = rep1 + rep2,
         total       = cumsum(round_total))

