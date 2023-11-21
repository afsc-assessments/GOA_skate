## Update SARA file ##


library(here)
library(tidyverse)
library(dbplyr)

cur_yr <- 2023


# read in SARA file file
sara <- suppressWarnings(readLines(here(cur_yr,"SARA file","BIGSKATEGOA2023_HQ.dat")))

sara[grep("#ASMT_YEAR",sara)+1] <- cur_yr


bg_index <- read.csv(here::here(cur_yr, "data","output",paste(cur_yr,"Skate_Total_index.csv",sep = "-")))

skate_bio <- bg_index %>%
  filter(year >= 2005)
  
bg_catch <- read.csv(here(cur_yr,"data","output",paste(cur_yr,"Big_Skate_OFL_catch.csv", sep = "-")))

# Get the catch and exploitation rate
bg_exploit <- bg_catch %>%
  select(year,total_catch) %>%
  left_join(skate_bio %>%
              select(year,big_pred), by = "year") %>%
  mutate(xrate = total_catch/big_pred) %>%
  add_column(Species = "Big Skate") %>%
  rename(pred_bio = big_pred)

bg_index <- read.csv(here::here(cur_yr, "data","output",paste(cur_yr,"Skate_Total_index.csv",sep = "-")))

# F_YEAR
sara[grep("#F_YEAR",sara) +1 ] <- cur_yr-1

# BEST_F_ESTIMATE
sara[grep("#BEST_F_ESTIMATE",sara) +1] <- round(bg_exploit$xrate[grep(cur_yr-1, bg_exploit$year)],3)

# F_LIMIT
sara[grep("#F_LIMIT",sara)[1] +1] <- bg_catch$OFL[grep(cur_yr-1, bg_catch$year)]

# B_YEAR
sara[grep("#B_YEAR",sara) +1] <- cur_yr

# BEST_B_ESTIMATE
sara[grep("#BEST_B_ESTIMATE",sara) +1] <- round(bg_index$big_pred[grep(cur_yr, bg_index$year)])

# FISHERYYEAR
sara[grep("#FISHERYYEAR",sara) +1] <- paste(bg_catch$year, collapse = "  ")

# TOTALBIOMASS
sara[grep("#TOTALBIOMASS",sara) +1] <- paste(round(bg_index$big_pred[grep(min(bg_catch$year), bg_index$year):grep(max(bg_catch$year), bg_index$year)]), collapse = "  ")

# TOTALCATCH
sara[grep("#TOTALCATCH",sara) +1] <- paste(round(bg_catch$total_catch), collapse = "  ")

write(sara, file = here(cur_yr, "SARA file","BIGSKATEGOA2023_HQ.dat"))





