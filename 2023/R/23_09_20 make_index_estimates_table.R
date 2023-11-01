##=============================================================##
#     Make the Biomass Indices Table and plot for all Skates    #
#      This is Table  in the 2023 SAFE document                 #
#                First Created: 09/20/23                        #
#                Last Updated: 10/19/23                         #
##=============================================================##

library(here)
library(janitor)
library(tidyverse)
library(dplyr)
# library(readr)


##------------------##
#    Manual Inputs   #
##------------------##
export_dat <- T                  # Do you want to export the data
current_yr <- 2023               # The current year


##-----------------##
#  Modify raw data  #
##-----------------##
# Get Big skate estimates and observed
tot_est <- read.csv(here(current_yr,"data","output", paste(current_yr, "Big_Estimate_Biomass_all_GOA.csv", sep = "-"))) %>%
  select(year, obs,obs_cv,pred,pred_lci,pred_uci) %>%
  rename(big_obs = obs,
         big_obs_cv = obs_cv,
         big_pred = pred,
         big_pred_lci = pred_lci,
         big_pred_uci = pred_uci) %>%
  left_join(read.csv(here(current_yr,"data","output", paste(current_yr, "Longnose_Estimate_Biomass_all_GOA.csv", sep = "-"))) %>%
              select(year, obs,obs_cv,pred,pred_lci,pred_uci) %>%
              rename(ln_obs = obs,
                     ln_obs_cv = obs_cv,
                     ln_pred = pred,
                     ln_pred_lci = pred_lci,
                     ln_pred_uci = pred_uci), by = join_by(year)) %>%
  left_join(read.csv(here(current_yr,"data","output", paste(current_yr, "Other_Estimate_Biomass_all_GOA.csv", sep = "-"))) %>%
              select(year, obs,obs_cv,pred,pred_lci,pred_uci) %>%
              rename(ot_obs = obs,
                     ot_obs_cv = obs_cv,
                     ot_pred = pred,
                     ot_pred_lci = pred_lci,
                     ot_pred_uci = pred_uci), by = join_by(year))

if(export_dat == T) write_csv(tot_est, here::here(current_yr, "data","output",paste(current_yr,"Skate_Total_index.csv",sep = "-")))


##------------##
#  Make plots  #
##------------##
pl_bio_tot <- read.csv(here(current_yr,"data","output", paste(current_yr, "Big_Estimate_Biomass_all_GOA.csv", sep = "-"))) %>%
  select(year,model_name,obs,obs_lci,obs_uci,pred,pred_lci,pred_uci) %>%
  bind_rows(read.csv(here(current_yr,"data","output", paste(current_yr, "Longnose_Estimate_Biomass_all_GOA.csv", sep = "-"))) %>%
              select(year,model_name,obs,obs_lci,obs_uci,pred,pred_lci,pred_uci)) %>%
  bind_rows(read.csv(here(current_yr,"data","output", paste(current_yr, "Other_Estimate_Biomass_all_GOA.csv", sep = "-"))) %>%
              select(year,model_name,obs,obs_lci,obs_uci,pred,pred_lci,pred_uci)) %>%
  mutate(sp = case_when(model_name == "big_skate_one" ~ "Big Skate",
                        model_name == "longnose_skate_one" ~ "Longnose Skate",
                        model_name == "other_skates_one" ~ "Other Skates"))
  

yr_mm <- c(min(pl_bio_tot$year),max(pl_bio_tot$year))

ggplot2::theme_set(afscassess::theme_report())

p_survey <- ggplot(data = pl_bio_tot) + 
  facet_grid(rows = vars(sp), scales = "free_y") +
  geom_ribbon(aes(x = year, ymin=pred_lci, ymax=pred_uci), alpha=0.5, fill = "grey") +
  geom_point(aes(x = year, y = obs), position=position_dodge(width=0.6)) +
  geom_errorbar(aes(x = year, y = obs,ymin=obs_lci, ymax=obs_uci),
                width=0.2, position=position_dodge(width=0.6)) +
  geom_line(aes(x = year, y = pred)) +
  theme(panel.grid.major = element_line(linewidth = 0.5)) +
  theme(axis.line = element_line()) +
  theme(axis.ticks = element_line(colour = "black")) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(from = yr_mm[1], to = yr_mm[2], by = 5)) +
  labs(x = "Year", y = 'Biomass (t)', title = '') 
print(p_survey)

ggsave(here::here(current_yr, "figs", paste(current_yr,"Skate_Biomass_Estimate.png",sep = "_")), dpi = 300, units = 'in',
       height = 7, width = 9,bg = 'white')



# AAA
# # read in the raw survey data
# raw_dat <- read_csv(here(current_yr,"data","raw", paste(current_yr, "Survey_biomass_by_strata.csv", sep = "-"))) %>%
#   janitor::clean_names(case = "snake") %>%
#   mutate(strata = case_when(regulatory_area_name == "EASTERN GOA" ~ "EGOA",
#                             regulatory_area_name == "WESTERN GOA" ~ "WGOA",
#                             regulatory_area_name == "CENTRAL GOA" ~ "CGOA"),
#          common_name = case_when(species_code == 420 ~ "big_skate",
#                                  species_code == 440 ~ "longnose_skate",
#                                  .default =  "other_skates"))
# 
# # Sort data by strata and sum over all the "other_skates" groups
# strat_bio <- raw_dat %>%
#   group_by(common_name,year,strata) %>%
#   summarise(biomass = sum(area_biomass),
#             cv = sqrt(sum(biomass_var, na.rm = TRUE)) / biomass) %>%
#   ungroup()
# 
# 
# # sort data by total biomass
# total_bio <- raw_dat %>%
#   group_by(common_name, year) %>%
#   summarise(biomass = sum(area_biomass),
#             cv = sqrt(sum(biomass_var, na.rm = TRUE)) / biomass) %>%
#   ungroup()
# 
# 
# ##-------------------------------------##
# # GOA-wide Biomass Index for each group #
# ##-------------------------------------##
# tot1 <- total_bio %>%
#   filter(year >= 1990,
#          year != 2001)
# 
# b_tot <- tot1 %>%
#   filter(common_name == "big_skate") %>%
#   mutate(index = round(biomass),
#          CV = round(cv,2)) %>%
#   select(year,index,CV)
# 
# ln_tot <- tot1 %>%
#   filter(common_name == "longnose_skate") %>%
#   mutate(index = round(biomass),
#          CV = round(cv,2)) %>%
#   select(index,CV)
# 
# ot_tot <- tot1 %>%
#   filter(common_name == "other_skates") %>%
#   mutate(index = round(biomass),
#          CV = round(cv,2)) %>%
#   select(index,CV)
# 
# tot_index <- cbind(b_tot,ln_tot,ot_tot)
# 
# if(export_dat == T) write_csv(as.data.frame(tot_index), here::here(dat_loc,paste(current_yr,"Skate_Total_index.csv",sep = "-")))

