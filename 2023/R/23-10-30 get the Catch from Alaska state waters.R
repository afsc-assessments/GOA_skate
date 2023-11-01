## Alaska state Catch time series for Skates

library(here)
library(tidyverse)
library(dplyr)

year <- 2023

st_cat <- read.csv(here(year, "data","user_inputs","Skate harvest time series Area 649 OceanAK 10_20_2023.csv")) %>%
  rename_all(tolower) %>%
  rename(year = dol.year,
         wght_lbs = whole.weight..lbs.) %>%
  mutate(assess_group = case_when(species.code == "701" ~ "longnose_skate",
                                  species.code == "702" ~ "big_skate",
                                  .default = "other_skates")) %>%
  select(year,species.code, species.name, assess_group,wght_lbs,mgt.area.district)%>%
  group_by(year,assess_group, mgt.area.district) %>%
  summarise(catch_wght_lbs = sum(wght_lbs)) %>%
  ungroup() %>%
  mutate(catch_wght_t = catch_wght_lbs/2240)
