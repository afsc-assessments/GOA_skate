##=============================================================##
#     Make the OFL, ABC, TAC and Catch table for Skates         #
#      This is Table 1/2 in the 2023 SAFE document              #
#                First Created: 09/20/23                        #
#                Last Updated: 10/11/23                         #
##=============================================================##


library(readr)
library(tidyverse)
library(dplyr)
library(here)

##---------------##
#  Manual Inputs  #
##---------------##
export_dat <- T        # T = export pulled data, F = do not export pulled data
current_yr <- 2023     # the current year
big_ofl_file <- "Big_Skate_OFL_ABC_TAC.csv" # name of file that has the raw survey data by strata
long_ofl_file <- "Longnose_Skate_OFL_ABC_TAC.csv" # name of file that has the raw survey data by strata
ot_ofl_file <- "Other_Skate_OFL_ABC_TAC.csv" # name of file that has the raw survey data by strata
catch_strata_file <- paste(current_yr,"Skate_strata_catch.csv", sep = "-")  



catch_strata <- read.csv(here(current_yr, "data", "output",catch_strata_file))
yrs <- seq(from = 2005, to = current_yr, by = 1)


##--------------------------------------##
# Get the OFL, ABC and TAC for Big Skate #
##--------------------------------------##
big_ofl <- read.csv(here(current_yr,"data", "user_inputs",big_ofl_file)) %>%
  relocate(OFL, .before = W_ABC)


big_cat_OFL <- catch_strata %>%
  filter(assess_group == "big_skate",
         year >= 2005) %>%
  arrange(factor(strata, levels = c("WGOA","CGOA","EGOA"))) %>%
  pivot_wider(names_from = strata, values_from = catch_wght) %>%
  mutate(total_catch = sum(WGOA, CGOA, EGOA, na.rm=T),  .by = c(year)) %>%
  left_join(big_ofl,by = "year")

if(export_dat == T){
  write_csv(big_cat_OFL, here(current_yr, "data","output",paste(current_yr,"Big_Skate_OFL_catch2.csv",sep = "-")))
}



# for(i in 1:length(yrs))
# {
#   # get the catch for a specific year
#   big_catch <- catch_strata %>%
#     filter(assess_group == "big_skate",
#            year == yrs[i]) %>%
#     arrange(factor(strata, levels = c("WGOA","CGOA","EGOA")))
#   
#   big_c1 <- round(c(big_catch$catch_wght,sum(big_catch$catch_wght)))
#   
#   # get the ABC and ABC and TAC for a specific year
#   temp_abc <- big_ofl %>%
#     filter(year == yrs[i]) %>%
#     select(w_abc,c_abc,e_abc,tot_abc)
#   
#   temp_tac <- big_ofl %>%
#     filter(year == yrs[i]) %>%
#     select(w_tac,c_tac,e_tac,tot_tac)
#   
#   temp_ofl <- big_ofl %>%
#     filter(year == yrs[i]) %>%
#     select(ofl)
#   temp_ofl <- c("","","",temp_ofl$ofl)
#   
#   temp <- cbind(rep(yrs[i],4),c("W","C","E","GOA-wide"), temp_ofl,t(temp_abc),t(temp_tac),big_c1)
#   rownames(temp) <- NULL
#   
#   if(i == 1){
#     temp_mat_b <- temp
#     colnames(temp_mat_b) <- c("Year","Location","OFL","ABC","TAC","Catch")
#   }else{
#     temp_mat_b <- rbind(temp_mat_b,temp)
#   }
# }
# 
# if(export_dat == T) write_csv(as.data.frame(temp_mat_b), here::here(dat_loc,paste(current_yr,"Big_Skate_OFL_catch.csv",sep = "-")))


##-------------------------------------------##
# Get the OFL, ABC and TAC for Longnose Skate #
##-------------------------------------------##
long_ofl <- read.csv(here(current_yr,"data", "user_inputs",long_ofl_file)) %>%
  relocate(OFL, .before = W_ABC)

long_cat_OFL <- catch_strata %>%
  filter(assess_group == "longnose_skate",
         year >= 2005) %>%
  arrange(factor(strata, levels = c("WGOA","CGOA","EGOA"))) %>%
  pivot_wider(names_from = strata, values_from = catch_wght) %>%
  mutate(total_catch = sum(WGOA, CGOA, EGOA, na.rm=T),  .by = c(year)) %>%
  left_join(long_ofl,by = "year")

if(export_dat == T){
  write_csv(long_cat_OFL, here(current_yr, "data","output",paste(current_yr,"Longnose_Skate_OFL_catch2.csv",sep = "-")))
}

# for(i in 1:length(yrs))
# {
#   # get the catch for a specific year
#   long_catch <- catch_strata %>%
#     filter(assess_group == "longnose_skate",
#            year == yrs[i]) %>%
#     arrange(factor(strata, levels = c("WGOA","CGOA","EGOA")))
#   long_c1 <- round(c(long_catch$catch_wght,sum(long_catch$catch_wght)))
#   
#   # get the ABC and ABC and TAC for a specific year
#   temp_abc <- long_ofl %>%
#     filter(year == yrs[i]) %>%
#     select(w_abc,c_abc,e_abc,tot_abc)
#   
#   temp_tac <- long_ofl %>%
#     filter(year == yrs[i]) %>%
#     select(w_tac,c_tac,e_tac,tot_tac)
#   
#   temp_ofl <- long_ofl %>%
#     filter(year == yrs[i]) %>%
#     select(ofl)
#   temp_ofl <- c("","","",temp_ofl$ofl)
#   
#   temp <- cbind(rep(yrs[i],4),c("W","C","E","GOA-wide"), temp_ofl,t(temp_abc),t(temp_tac),long_c1)
#   rownames(temp) <- NULL
#   
#   if(i == 1){
#     temp_mat_ln <- temp
#     colnames(temp_mat_ln) <- c("Year","Location","OFL","ABC","TAC","Catch")
#   }else{
#     temp_mat_ln <- rbind(temp_mat_ln,temp)
#   }
# }
# 
# if(export_dat == T) write_csv(as.data.frame(temp_mat_ln), here::here(dat_loc,paste(current_yr,"Longnose_Skate_OFL_catch.csv",sep = "-")))


##----------------------------------------##
# Get the OFL, ABC and TAC for other Skate #
##----------------------------------------##
ot_ofl <- read.csv(here(current_yr,"data", "user_inputs",ot_ofl_file)) %>%
  relocate(OFL, .before = ABC)

ot_cat_OFL <- catch_strata %>%
  filter(assess_group == "other_skates",
         year >= 2005)  %>%
  group_by(year,assess_group) %>%
  summarise(catch_wght = sum(catch_wght)) %>%
  ungroup() %>%
  left_join(ot_ofl,by = "year")

if(export_dat == T){
  write_csv(ot_cat_OFL, here(current_yr, "data","output",paste(current_yr,"Other_Skate_OFL_catch2.csv",sep = "-")))
}


# # i <- 1
# for(i in 1:length(yrs))
# {
#   # get the catch for a specific year
#   ot_catch <- catch_strata %>%
#     filter(assess_group == "other_skates",
#            year == yrs[i]) %>%
#     arrange(factor(strata, levels = c("WGOA","CGOA","EGOA")))
#   ot_c1 <- round(c(ot_catch$catch_wght,sum(ot_catch$catch_wght)))
#   
#   # get the ABC and ABC and TAC for a specific year
#   temp_abc <- ot_ofl %>%
#     filter(year == yrs[i]) %>%
#     select(abc)
#   temp_abc <- c("","","",temp_abc$abc)
#   
#   temp_tac <- ot_ofl %>%
#     filter(year == yrs[i]) %>%
#     select(tac)
#   temp_tac <- c("","","",temp_tac$tac)
#   
#   temp_ofl <- ot_ofl %>%
#     filter(year == yrs[i]) %>%
#     select(ofl)
#   temp_ofl <- c("","","",temp_ofl$ofl)
#   
#   temp <- cbind(rep(yrs[i],4),c("W","C","E","GOA-wide"), temp_ofl,temp_abc,temp_tac,ot_c1)
#   rownames(temp) <- NULL
#   
#   if(i == 1){
#     temp_mat_ot <- temp
#     colnames(temp_mat_ot) <- c("Year","Location","OFL","ABC","TAC","Catch")
#   }else{
#     temp_mat_ot <- rbind(temp_mat_ot,temp)
#   }
# }
# 
# if(export_dat == T) write_csv(as.data.frame(temp_mat_ot), here::here(dat_loc,paste(current_yr,"Other_Skate_OFL_catch.csv",sep = "-")))

##-------------------------------------------------##
# OFL, ABC and TAC for all three assessments groups #
##-------------------------------------------------##
all_mat <- cbind(temp_mat_b,temp_mat_ln[,3:6],temp_mat_ot[,3:6])  # order is big skate, longnose skate and other skate

if(export_dat == T) write_csv(as.data.frame(all_mat), here::here(dat_loc,paste(current_yr,"All_Skate_OFL_catch.csv",sep = "-")))
