## Update SARA file ##


library(here)
library(tidyverse)
library(dbplyr)

cur_yr <- 2023


# read in SARA file file
sara <- suppressWarnings(readLines(here(cur_yr,"SARA file","LONGSKATEGOA2023_HQ.dat")))

sara[grep("#ASMT_YEAR",sara)+1] <- cur_yr


ln_index <- read.csv(here::here(cur_yr, "data","output",paste(cur_yr,"Skate_Total_index.csv",sep = "-")))

skate_bio <- ln_index %>%
  filter(year >= 2005)
  
ln_catch <- read.csv(here(cur_yr,"data","output",paste(cur_yr,"Longnose_Skate_OFL_catch.csv", sep = "-")))

# Get the catch and exploitation rate
ln_exploit <- ln_catch %>%
  select(year,total_catch) %>%
  left_join(skate_bio %>%
              select(year,ln_pred), by = "year") %>%
  mutate(xrate = total_catch/ln_pred) %>%
  add_column(Species = "Longnose Skate") %>%
  rename(pred_bio = ln_pred)

# bg_index <- read.csv(here::here(cur_yr, "data","output",paste(cur_yr,"Skate_Total_index.csv",sep = "-")))

# F_YEAR
sara[grep("#F_YEAR",sara) +1 ] <- cur_yr-1

# BEST_F_ESTIMATE
sara[grep("#BEST_F_ESTIMATE",sara) +1] <- round(ln_exploit$xrate[grep(cur_yr-1, ln_exploit$year)],3)

# F_LIMIT
sara[grep("#F_LIMIT",sara)[1] +1] <- ln_catch$OFL[grep(cur_yr-1, ln_catch$year)]

# B_YEAR
sara[grep("#B_YEAR",sara) +1] <- cur_yr

# BEST_B_ESTIMATE
sara[grep("#BEST_B_ESTIMATE",sara) +1] <- round(ln_index$ln_pred[grep(cur_yr, ln_index$year)])

# FISHERYYEAR
sara[grep("#FISHERYYEAR",sara) +1] <- paste(ln_catch$year, collapse = "  ")

# TOTALBIOMASS
sara[grep("#TOTALBIOMASS",sara) +1] <- paste(round(ln_index$ln_pred[grep(min(ln_catch$year), ln_index$year):grep(max(ln_catch$year), ln_index$year)]), collapse = "  ")

# TOTALCATCH
sara[grep("#TOTALCATCH",sara) +1] <- paste(round(ln_catch$total_catch), collapse = "  ")

write(sara, file = here(cur_yr, "SARA file","LONGSKATEGOA2023_HQ.dat"))





