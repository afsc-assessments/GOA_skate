##=========================================##
#     Run the REMA Model for Other Skates   #
#       Date Created: 09/05/23              #
#       Last Modified: 10/16/23             #
##=========================================##

library(here)
library(janitor)
library(tidyverse)
library(rema)  # The rema package
library(readr)

##------------------##
#    Manual Inputs   #
##------------------##
export_dat <- T            # Do you want to export the data
get_old_summary_values <- T      # Do you want to export the data
current_yr <- 2023         # The current year
m <- 0.1                   # Natural mortality
tier <- 5                  # Specified Tier
mult_ofl_max_abc <- 0.75   # multiplier used to determine max_ABC from OFL
mult_max_abc_abc <- 1      # multiplier used to determine recommended ABC from max ABC



#===================#
## Modify raw data ##
#===================#
# read in the raw survey data
raw_dat <- read_csv(here(current_yr,"data","raw",paste(current_yr,"Survey_biomass_by_strata.csv", sep = "-"))) %>%
  janitor::clean_names(case = "snake") %>%
  mutate(strata = case_when(regulatory_area_name == "EASTERN GOA" ~ "EGOA",
                            regulatory_area_name == "WESTERN GOA" ~ "WGOA",
                            regulatory_area_name == "CENTRAL GOA" ~ "CGOA"),
         common_name = case_when(species_code == 420 ~ "big_skate",
                                 species_code == 440 ~ "longnose_skate",
                                 .default =  "other_skates"))


# sort data by total biomass
total_bio <- raw_dat %>%
  group_by(common_name, year) %>%
  summarise(biomass = sum(area_biomass),
            cv = sqrt(sum(biomass_var, na.rm = TRUE)) / biomass) %>%
  ungroup()

#==========================================================================#
##                           Run the REMA Model                           ##
#==========================================================================#  

#------------------------------------------------------------#
##               All Other Skates (ot_one)                ##
##          Run for Other Skates across all of GOA        ## 
#------------------------------------------------------------#

ot_one_dat <- total_bio %>%
  filter(common_name == "other_skates",
         year >= 1990,
         year != 2001) %>%
  select(-common_name) %>%
  add_column("strata" = "GOA", .after = "year")

ot_one_input <- prepare_rema_input(biomass_dat = ot_one_dat,
                                   end_year = current_yr + 2,
                                   model_name = 'other_skates_one')
ot_one_mod <- fit_rema(ot_one_input)
ot_one_out <- tidy_rema(ot_one_mod)
ot_one_plots <- plot_rema(ot_one_out, biomass_ylab = 'Biomass (t)')
(ot_one_p1 <- ot_one_plots$biomass_by_strata + ggtitle('Other skates'))
(ot_one_p2 <- ot_one_plots$total_predicted_biomass + expand_limits(y = 0) + ggtitle('Other skates'))

ot_one_bio <- ot_one_out$total_predicted_biomass
ot_one_strata <- ot_one_out$biomass_by_strata


#==========================================================================#
##                   Create Summary of Results Table                      ##
#==========================================================================#  
# S_of_R  = Summary of Results Table

tot_bio <- ot_one_bio %>%
  filter(year >= current_yr+1) %>%
  select(year, pred) %>%
  rename(total_GOA_bio = pred)

temp <- ot_one_bio %>%
  filter(year >= current_yr+1) %>%
  select(year, pred) %>%
  rename(total_GOA_bio = pred)%>%
  mutate_if(is.numeric,round) %>%
  add_column("M" = m,
             "Tier" = tier,
             .before = "total_GOA_bio") %>%
  add_column("F_OFL" = m, 
             "max_F_ABC" = mult_ofl_max_abc*F_OFL,
             "F_ABC" = mult_max_abc_abc*max_F_ABC) %>%
  mutate(OFL = round(total_GOA_bio*F_OFL),
         max_ABC = round(total_GOA_bio*max_F_ABC),
         r_ABC = round(total_GOA_bio*F_ABC)) %>%
  add_column("blank" = NA,
             "Status" = c(current_yr-1,current_yr),
             "Overfishing?" = "n/a") %>%
  mutate(across(everything(), as.character))

S_of_R <- as.data.frame(temp)
rownames(S_of_R) <- S_of_R$year
S_of_R <- S_of_R[,-1]
S_of_R <- t(S_of_R)


##---------------------------------------------##
#  Combine Summary Table with old summary Table #
##---------------------------------------------##
if(get_old_summary_values == T){
  old_table <- as_tibble(read.csv(here(current_yr,"data","user_inputs", paste(current_yr-1,"Summary_of_Results_other_skate.csv",sep = "-")))) %>%
    select(tail(names(.), 2))  %>%
    mutate(across(everything(), as.character))
  old_table_df <- as.data.frame(old_table)
  S_of_R <- cbind(old_table_df,S_of_R)
}


catch_OFL <- as_tibble(read.csv(here(current_yr,"data","user_inputs","Other_Skate_OFL_ABC_TAC.csv")))
catch_tot <- as_tibble(read.csv(here(current_yr,"data","output", paste(current_yr,"Skate_total_catch.csv",sep = "-"))))
##----------------------------------------------##
#  Check if over fishing occurred the year prior #
##----------------------------------------------##
if(get_old_summary_values == T){
  cat("\n")
  cat(current_yr-1, "\n")
  ## Check total catch from last year ##
  prior_catch <- catch_tot %>%
    filter(assess_group == "other_skates",
           year == (current_yr-1))
  
  prior_OFL <- catch_OFL %>%
    filter(year == current_yr -1)
  
  old_ofl <- prior_OFL$OFL
  old_catch <- prior_catch$catch_wght
  if(old_ofl > old_catch){
    cat("Overfishing did NOT occur in", current_yr-1,": OFL = ",old_ofl, ", Catch = ", old_catch, "\n")
    S_of_R[grep("Overfishing",rownames(S_of_R)),3] <- "No"
  }else{
    cat("!!!!!!!!!!!!!!!!!Overfishing DID occur in", current_yr-1,": OFL = ",old_ofl, ", Catch = ", old_catch, "\n")
    S_of_R[grep("Overfishing",rownames(S_of_R)),3] <- "Yes"
  }
  
  ## Check if catch is greater thanthe ABC from last year ##
  old_abc <- prior_OFL$ABC
  if(old_abc > old_catch){
    cat("Other Skate catch did NOT exceed the ABC in", current_yr-1,": ABC = ",old_abc, ", Catch = ", old_catch, "\n")
  }else{
    cat("!!!!!!!!!!!!Other Skate catch DID exceed the ABC in", current_yr-1,": ABC = ",old_abc, ", Catch = ", old_catch, "\n")
  }
}


##------------------------------------------------##
#  Check if over fishing occurring in current year #
##------------------------------------------------##
## Get the date when the date was lasted pulled ##
current_yr_date <- readLines(here::here(current_yr,"data","raw", "catch_data_last_pulled.out"))
current_yr_date <- unlist(strsplit(current_yr_date,split = ": "))[2]

cat("\n")
cat(current_yr, "\n")
## Check total catch from last year ##
current_catch <- catch_tot %>%
  filter(assess_group == "other_skates",
         year == (current_yr))

current_OFL <- catch_OFL %>%
  filter(year == current_yr)

cur_ofl <- current_OFL$OFL
cur_catch <- current_catch$catch_wght
if(cur_ofl > cur_catch){
  cat("Overfishing has NOT occur in as of", current_yr_date,": OFL = ",cur_ofl, ", Catch = ", cur_catch, "\n")
}else{
  cat("!!!!!!!!!!!Overfishing DID occur in", current_yr,": OFL = ",cur_ofl, ", Catch = ", cur_catch, "\n")
}


if(export_dat == T){
  write.csv(S_of_R, here::here(current_yr,"data","output", paste(current_yr,"Summary_of_Results_other_skate.csv",sep = "-")))
  write.csv(ot_one_out$biomass_by_strata, here::here(current_yr,"data","output", paste(current_yr,"Other_Estimate_Biomass_all_GOA.csv",sep = "-")))
} 
















