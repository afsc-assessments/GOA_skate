## Update SARA file ##


library(here)
library(tidyverse)
library(dbplyr)

cur_yr <- 2023


# read in SARA file file
sara <- suppressWarnings(readLines(here(cur_yr,"SARA file","SKATEGOA2023_HQ.dat")))

sara[grep("#ASMT_YEAR",sara)+1] <- cur_yr


ot_index <- read.csv(here::here(cur_yr, "data","output",paste(cur_yr,"Skate_Total_index.csv",sep = "-")))

skate_bio <- ot_index %>%
  filter(year >= 2005)
  
ot_catch <- read.csv(here(cur_yr,"data","output",paste(cur_yr,"Other_Skate_OFL_catch.csv", sep = "-")))

# Get the catch and exploitation rate
ot_exploit <- ot_catch %>%
  select(year,total_catch) %>%
  left_join(skate_bio %>%
              select(year,ot_pred), by = "year") %>%
  mutate(xrate = total_catch/ot_pred) %>%
  add_column(Species = "Longnose Skate") %>%
  rename(pred_bio = ot_pred)

# bg_index <- read.csv(here::here(cur_yr, "data","output",paste(cur_yr,"Skate_Total_index.csv",sep = "-")))

# F_YEAR
sara[grep("#F_YEAR",sara) +1 ] <- cur_yr-1

# BEST_F_ESTIMATE
sara[grep("#BEST_F_ESTIMATE",sara) +1] <- round(ot_exploit$xrate[grep(cur_yr-1, ot_exploit$year)],3)

# F_LIMIT
sara[grep("#F_LIMIT",sara)[1] +1] <- ot_catch$OFL[grep(cur_yr-1, ot_catch$year)]

# B_YEAR
sara[grep("#B_YEAR",sara) +1] <- cur_yr

# BEST_B_ESTIMATE
sara[grep("#BEST_B_ESTIMATE",sara) +1] <- round(ot_index$ot_pred[grep(cur_yr, ot_index$year)])

# FISHERYYEAR
sara[grep("#FISHERYYEAR",sara) +1] <- paste(ot_catch$year, collapse = "  ")

# TOTALBIOMASS
sara[grep("#TOTALBIOMASS",sara) +1] <- paste(round(ot_index$ot_pred[grep(min(ot_catch$year), ot_index$year):grep(max(ot_catch$year), ot_index$year)]), collapse = "  ")

# TOTALCATCH
sara[grep("#TOTALCATCH",sara) +1] <- paste(round(ot_catch$total_catch), collapse = "  ")

write(sara, file = here(cur_yr, "SARA file","SKATEGOA2023_HQ.dat"))





