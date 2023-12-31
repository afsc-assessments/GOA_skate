##=========================================##
#     Run the REMA Model for Big  Skates    #
#       Date Created: 08/18/23              #
#       Last Modified: 10/19/23             #
##=========================================##


library(here)
library(janitor)
library(tidyverse)
library(rema)  # The rema package
library(readr)


##------------------##
#    Manual Inputs   #
##------------------##
export_dat <- T                  # Do you want to export the data
pres_pl   <- T         # T = Create plots for presentation, F = don't create plots for presentations 
get_old_summary_values <- T      # Do you want get the old summary values
current_yr <- 2023               # The current year
m <- 0.1                         # Natural mortality
tier <- 5                        # Specified Tier
mult_ofl_max_abc <- 0.75         # multiplier used to determine max_ABC from OFL
mult_max_abc_abc <- 1            # multiplier used to determine recommended ABC from max ABC



##-----------------##
#  Modify raw data  #
##-----------------##
# read in the raw survey data
raw_dat <- read_csv(here(current_yr,"data","raw",paste(current_yr,"Survey_biomass_by_strata.csv", sep = "-"))) %>%
  janitor::clean_names(case = "snake") %>%
  mutate(strata = case_when(regulatory_area_name == "EASTERN GOA" ~ "EGOA",
                            regulatory_area_name == "WESTERN GOA" ~ "WGOA",
                            regulatory_area_name == "CENTRAL GOA" ~ "CGOA"),
         common_name = case_when(species_code == 420 ~ "big_skate",
                                 species_code == 440 ~ "longnose_skate",
                                 .default =  "other_skates"))

# Sort data by strata and sum over all the "other_skates" groups
strat_bio <- raw_dat %>%
  group_by(common_name,year,strata) %>%
  summarise(biomass = sum(area_biomass),
            cv = sqrt(sum(biomass_var, na.rm = TRUE)) / biomass) %>%
  ungroup()


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
##               All Big Skate (bs_one)                     ##
##          Run for Big Skate across all of GOA             ## 
#------------------------------------------------------------#

bs_one_dat <- total_bio %>%
  filter(common_name == "big_skate",
         year >= 1990,
         year != 2001) %>%
  select(-common_name) %>%
  add_column("strata" = "GOA", .after = "year")

bs_one_input <- prepare_rema_input(biomass_dat = bs_one_dat,
                                   end_year = current_yr + 2,
                                   model_name = 'big_skate_one')
bs_one_mod <- fit_rema(bs_one_input)
bs_one_out <- tidy_rema(bs_one_mod)
bs_one_plots <- plot_rema(bs_one_out, biomass_ylab = 'Biomass (t)')
(bs_one_p1 <- bs_one_plots$biomass_by_strata + ggtitle('Big skates'))
(bs_one_p2 <- bs_one_plots$total_predicted_biomass + expand_limits(y = 0) + ggtitle('Big skates'))

bs_one_bio <- bs_one_out$total_predicted_biomass
bs_one_strata <- bs_one_out$biomass_by_strata



#------------------------------------------------------------#
##        All Big Skate with strata (bs_all)                ##
##  Run for Big Skate including different parameters for    ## 
##  each strata                                             ##
#------------------------------------------------------------#

bs_all_dat <- strat_bio %>%
  filter(common_name == "big_skate",
         year >= 1990,
         year != 2001) %>%
  select(-common_name)

bs_all_input <- prepare_rema_input(biomass_dat = bs_all_dat,
                                 end_year = current_yr + 2,
                                 model_name = 'big_skate_all')
bs_all_mod <- fit_rema(bs_all_input)
bs_all_out <- tidy_rema(bs_all_mod)
bs_all_plots <- plot_rema(bs_all_out, biomass_ylab = 'Biomass (t)')
(bs_all_p1 <- bs_all_plots$biomass_by_strata + ggtitle('Big skates'))
(bs_all_p2 <- bs_all_plots$total_predicted_biomass + expand_limits(y = 0) + ggtitle('Big skates'))

bs_all_bio <- bs_all_out$total_predicted_biomass
bs_all_strata <- bs_all_out$biomass_by_strata
bs_all_prop <- bs_all_out$proportion_biomass_by_strata


#==========================================================================#
##     Create table with biomass by area with the assumption that         ##
##                    SE(log(B_tot*P1)) ~ SE(log(B1))                     ##
##     where B_tot is the total estimated biomass, P1 is the proportion   ##
##     of the biomass in area 1 (estimated in a separate REMA model)      ##
##     and B1 is the estimated biomass in area 1 from the separate REMA   ##
##     model. I have issues with this method but this is how it was done  ## 
##     in the previous assessment.                                        ##  
#==========================================================================#  

# get the bio_one*proportion in each area
bio_strata_calc <- bs_one_bio %>%
  select(year,pred) %>%
  left_join(bs_all_prop %>%
              select(year,WGOA,CGOA,EGOA), by = "year") %>%
  left_join(bs_all_strata %>%
              select(year,strata,sd_log_pred) %>%
              pivot_wider(names_from = strata, values_from = sd_log_pred) %>%
              rename(W_sd_log = WGOA,
                     C_sd_log = CGOA,
                     E_sd_log = EGOA), by = "year") %>%
  mutate(W_bio = pred*WGOA,
         C_bio = pred*CGOA,
         E_bio = pred*EGOA) %>%
  mutate(W_lci = exp(log(W_bio) - 1.96*W_sd_log),
         C_lci = exp(log(C_bio) - 1.96*C_sd_log),
         E_lci = exp(log(E_bio) - 1.96*E_sd_log),
         W_uci = exp(log(W_bio) + 1.96*W_sd_log),
         C_uci = exp(log(C_bio) + 1.96*C_sd_log),
         E_uci = exp(log(E_bio) + 1.96*E_sd_log),) %>%
  left_join(bs_all_strata %>%
              select(year,strata,obs,obs_cv) %>%
              pivot_wider(names_from = strata, values_from = c(obs, obs_cv)) %>%
              rename(obs_W = obs_WGOA,
                     obs_C = obs_CGOA,
                     obs_E = obs_EGOA,
                     obs_cv_W = obs_cv_WGOA,
                     obs_cv_C = obs_cv_CGOA,
                     obs_cv_E = obs_cv_EGOA,), by = "year") %>%
  select(year,obs_W,obs_cv_W,W_bio,W_lci,W_uci,obs_C,obs_cv_C,C_bio,C_lci,C_uci,obs_E,obs_cv_E,E_bio,E_lci,E_uci)


if(export_dat == T){
  write.csv(bio_strata_calc, here::here(current_yr,"data","output", paste(current_yr,"Calculate_biomass_strata_big_skate.csv",sep = "-")),row.names = F)
}


#==========================================================================#
##                   Create Summary of Results Table                      ##
#==========================================================================#  
# S_of_R  = Summary of Results Table

w_bio <- bs_all_strata %>%
  filter(strata == "WGOA",
         year >= current_yr+1) %>%
  select(year, pred) %>%
  rename(W_pred = pred)

c_bio <- bs_all_strata %>%
  filter(strata == "CGOA",
         year >= current_yr+1) %>%
  select(year, pred) %>%
  rename(C_pred = pred)

e_bio <- bs_all_strata %>%
  filter(strata == "EGOA",
         year >= current_yr+1) %>%
  select(year, pred) %>%
  rename(E_pred = pred)

tot_bio <- bs_one_bio %>%
  filter(year >= current_yr+1) %>%
  select(year, pred) %>%
  rename(tot_pred_one = pred)

temp <- bs_all_bio %>%
  filter(year >= current_yr+1) %>%
  select(year, pred) %>%
  rename(tot_pred = pred) %>%
  left_join(w_bio, by = c("year")) %>%
  left_join(c_bio, by = c("year")) %>%
  left_join(e_bio, by = c("year")) %>%
  left_join(tot_bio, by = c("year")) %>%
  mutate(w_prop = W_pred/tot_pred,
         c_prop = C_pred/tot_pred,
         e_prop = E_pred/tot_pred,
         W_pred_one = tot_pred_one*w_prop,
         C_pred_one = tot_pred_one*c_prop,
         E_pred_one = tot_pred_one*e_prop) %>%
  mutate_if(is.numeric,round) %>%
  select(-tot_pred,-W_pred,-C_pred,-E_pred,-w_prop,-c_prop,-e_prop)%>%
  add_column("M" = m,
             "Tier" = tier,
             .before = "tot_pred_one") %>%
  relocate(c(W_pred_one,C_pred_one,E_pred_one), .before = "tot_pred_one") %>%
  rename(total_GOA_bio = tot_pred_one,
         W_bio_pred = W_pred_one,
         C_bio_pred = C_pred_one,
         E_bio_pred = E_pred_one) %>%
  add_column("F_OFL" = m, 
             "max_F_ABC" = mult_ofl_max_abc*F_OFL,
             "F_ABC" = mult_max_abc_abc*max_F_ABC) %>%
  mutate(OFL = round(total_GOA_bio*F_OFL),
         W_max_ABC = round(W_bio_pred*max_F_ABC),
         C_max_ABC = round(C_bio_pred*max_F_ABC),
         E_max_ABC = round(E_bio_pred*max_F_ABC),
         W_r_ABC = round(W_bio_pred*F_ABC),
         C_r_ABC = round(C_bio_pred*F_ABC),
         E_r_ABC = round(E_bio_pred*F_ABC)) %>%
  add_column("blank" = NA,
             "Status" = c(current_yr-1,current_yr),
             "Overfishing?" = "n/a") %>%
  mutate(across(everything(), as.character))

S_of_R <- as.data.frame(temp)
rownames(S_of_R) <- S_of_R$year
S_of_R <- S_of_R[,-1]
S_of_R <- t(S_of_R)

# AAA
##---------------------------------------------##
#  Combine Summary Table with old summary Table #
##---------------------------------------------##
if(get_old_summary_values == T){
  old_table <- as_tibble(read.csv(here(current_yr,"data","user_inputs", paste(current_yr-1,"Summary_of_Results_big_skate.csv",sep = "-")))) %>%
    select(tail(names(.), 2))  %>%
    mutate(across(everything(), as.character))
  old_table_df <- as.data.frame(old_table)
  S_of_R <- cbind(old_table_df,S_of_R)
  
}


catch_OFL <- as_tibble(read.csv(here(current_yr,"data","user_inputs","Big_Skate_OFL_ABC_TAC.csv")))
catch_tot <- as_tibble(read.csv(here(current_yr,"data","output", paste(current_yr,"Skate_total_catch.csv",sep = "-"))))
catch_strata <- as_tibble(read.csv(here(current_yr,"data","output", paste(current_yr,"Skate_strata_catch.csv",sep = "-"))))
##----------------------------------------------##
#  Check if over fishing occurred the year prior #
##----------------------------------------------##
if(get_old_summary_values == T){
  cat("\n")
  cat(current_yr-1, "\n")
  ## Check total catch from last year ##
  prior_catch <- catch_tot %>%
    filter(assess_group == "big_skate",
           year == (current_yr-1))
  
  prior_OFL <- catch_OFL %>%
    filter(year == current_yr -1)
  
  old_ofl <- prior_OFL$OFL  #as.numeric(prior_OFL[grep("OFL",colnames(catch_OFL))])
  old_catch <- prior_catch$catch_wght
  if(old_ofl > old_catch){
    cat("Overfishing did NOT occur in", current_yr-1,": OFL = ",old_ofl, ", Catch = ", old_catch, "\n")
    S_of_R[grep("Overfishing",rownames(S_of_R)),3] <- "No"
  }else{
    cat("!!!!!!!!!!!!!!!!!Overfishing DID occur in", current_yr-1,": OFL = ",old_ofl, ", Catch = ", old_catch, "\n")
    S_of_R[grep("Overfishing",rownames(S_of_R)),3] <- "Yes"
  }

  ## Check the catch by strata for last year ##
  prior_catch_strata <- catch_strata %>%
    filter(assess_group == "big_skate",
           year == (current_yr-1))
  
  goa_loc <- colnames(prior_OFL)[grep("ABC",colnames(catch_OFL))[1:3]]
  old_abc <- as.numeric(prior_OFL[grep("ABC",colnames(catch_OFL))])[1:3]
  
  # i <- 1
  for(i in 1:length(goa_loc))
  {
    loc_nam <- paste(unlist(strsplit(goa_loc[i], split = "_"))[1],"GOA",sep = "")
    catch_val <- prior_catch_strata$catch_wght[grep(loc_nam,prior_catch_strata$strata)]
    if(old_abc[i] > catch_val){
      cat("Big Skate catch in ", loc_nam, " did NOT exceed the ABC in", current_yr-1,": ABC = ",old_abc[i], ", Catch = ", catch_val, "\n")
    }else{
      cat("!!!!!!!!!!!!Big Skate catch in ", loc_nam, " DID exceed the ABC in", current_yr-1,": ABC = ",old_abc[i], ", Catch = ", catch_val, "\n")
    }
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
  filter(assess_group == "big_skate",
         year == (current_yr))

current_OFL <- catch_OFL %>%
  filter(year == current_yr)

cur_ofl <- current_OFL$OFL 
if(cur_ofl > current_catch$catch_wght){
  cat("Overfishing has NOT occurred as of", current_yr_date,": OFL = ",cur_ofl, ", Catch = ", current_catch$catch_wght, "\n")
}else{
  cat("!!!!!!!!!!!Overfishing DID occur in", current_yr,": OFL = ",cur_ofl, ", Catch = ", current_catch$catch_wght, "\n")
}

# cat("\n")
## Check the catch by strata for last year ##
current_catch <- catch_strata %>%
  filter(assess_group == "big_skate",
         year == (current_yr))

goa_loc <- colnames(current_OFL)[grep("ABC",colnames(catch_OFL))[1:3]]
cur_abc <- as.numeric(current_OFL[grep("ABC",colnames(catch_OFL))])[1:3]

# i <- 1
for(i in 1:length(goa_loc))
{
  loc_nam <- paste(unlist(strsplit(goa_loc[i], split = "_"))[1],"GOA",sep = "")
  catch_val <- current_catch$catch_wght[grep(loc_nam,current_catch$strata)]
  if(cur_abc[i] > catch_val){
    cat("Big Skate Catch in ", loc_nam, " does NOT exceed the ABC as of", current_yr_date,": ABC = ",cur_abc[i], ", Catch = ", catch_val, "\n")
  }else{
    cat("!!!!!!Big Skate Catch in ", loc_nam, " DOES exceed the ABC as of", current_yr_date,": Max_ABC = ",cur_abc[i], ", Catch = ", catch_val, "\n")
  }
}


if(export_dat == T){
  write.csv(S_of_R, here::here(current_yr,"data","output", paste(current_yr,"Summary_of_Results_big_skate.csv",sep = "-")))
  write.csv(bs_one_out$biomass_by_strata, here::here(current_yr,"data","output", paste(current_yr,"Big_Estimate_Biomass_all_GOA.csv",sep = "-")))
  write.csv(bs_all_out$biomass_by_strata, here::here(current_yr,"data","output", paste(current_yr,"Big_biomass_strata.csv",sep = "-")))
} 


##-----------##
#   Plots     # 
##-----------##
## Plot using ggplot
pl_bio_strata_calc <- bs_all_prop %>%
  select(year,WGOA,CGOA,EGOA) %>%
  pivot_longer(cols = WGOA:EGOA, values_to =  c("prop"),names_to = "strata")  %>%
  left_join(bs_one_strata %>%
              select(year,pred), by = "year", relationship = "many-to-one") %>%
  left_join(bs_all_strata %>%
              select(year,strata,sd_log_pred,obs,obs_lci,obs_uci), by = c("year","strata")) %>%
  mutate(pred_strata = prop*pred,
         pred_strata_lci = exp(log(pred_strata) - 1.96*sd_log_pred),
         pred_strata_uci = exp(log(pred_strata) + 1.96*sd_log_pred)) %>%
  rename(tot_pred = pred) %>%
  mutate(strata2 = case_when(strata == "WGOA" ~ "Western",
                             strata == "CGOA" ~ "Central",
                             strata == "EGOA" ~ "Eastern"))  %>%
  mutate(across(strata2, factor, levels=c("Western","Central","Eastern"))) %>%
  mutate(across(strata, factor, levels=c("WGOA","CGOA","EGOA")))

yr_mm <- c(min(pl_bio_strata_calc$year),max(pl_bio_strata_calc$year))
ggplot2::theme_set(afscassess::theme_report())

p_survey <- ggplot(data = pl_bio_strata_calc) + 
  facet_grid(rows = vars(strata2), scales = "free_y") +
  geom_ribbon(aes(x = year, ymin=pred_strata_lci, ymax=pred_strata_uci), alpha=0.5, fill = "grey") +
  geom_point(aes(x = year, y = obs), position=position_dodge(width=0.6)) +
  geom_errorbar(aes(x = year, y = obs,ymin=obs_lci, ymax=obs_uci),
                width=0.2, position=position_dodge(width=0.6)) +
  geom_line(aes(x = year, y = pred_strata)) +
  theme(panel.grid.major = element_line(linewidth = 0.5)) +
  theme(axis.line = element_line()) +
  theme(axis.ticks = element_line(colour = "black")) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(from = yr_mm[1], to = yr_mm[2], by = 5)) +
  labs(x = "Year", y = 'Biomass (t)', title = '') 
print(p_survey)

ggsave(here::here(current_yr, "figs", paste(current_yr,"Big_Regional_Biomass_Estimate.png",sep = "_")), dpi = 300, units = 'in',
       height = 7, width = 9,bg = 'white')


##===================================##
#  plots for Plan Team presentation   #
##===================================##
if(pres_pl == T){
  p_survey2 <- ggplot(data = pl_bio_strata_calc) + 
    facet_grid(rows = vars(strata), scales = "free_y") +
    geom_ribbon(aes(x = year, ymin=pred_strata_lci, ymax=pred_strata_uci), alpha=0.5, fill = "grey") +
    geom_point(aes(x = year, y = obs), position=position_dodge(width=0.6)) +
    geom_errorbar(aes(x = year, y = obs,ymin=obs_lci, ymax=obs_uci),
                  width=0.2, position=position_dodge(width=0.6)) +
    geom_line(aes(x = year, y = pred_strata)) +
    theme(panel.grid.major = element_line(linewidth = 0.5)) +
    theme(axis.line = element_line()) +
    theme(axis.ticks = element_line(colour = "black")) +
    theme(axis.text = element_text(size =12),
          axis.title = element_text(size =12),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 12)) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(from = yr_mm[1], to = yr_mm[2], by = 5)) +
    labs(x = "Year", y = 'Biomass (t)', title = '') 
  print(p_survey2)
  
  # Same Y axis with manually entered Y axis
  p_survey2_same_small <- ggplot(data = pl_bio_strata_calc) + 
    facet_grid(rows = vars(strata)) +
    geom_ribbon(aes(x = year, ymin=pred_strata_lci, ymax=pred_strata_uci), alpha=0.5, fill = "grey") +
    geom_point(aes(x = year, y = obs), position=position_dodge(width=0.6)) +
    geom_errorbar(aes(x = year, y = obs,ymin=obs_lci, ymax=obs_uci),
                  width=0.2, position=position_dodge(width=0.6)) +
    geom_line(aes(x = year, y = pred_strata)) +
    theme(panel.grid.major = element_line(linewidth = 0.5)) +
    theme(axis.line = element_line()) +
    theme(axis.ticks = element_line(colour = "black")) +
    theme(axis.text = element_text(size =12),
          axis.title = element_text(size =12),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 12)) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(from = yr_mm[1], to = yr_mm[2], by = 5)) +
    labs(x = "Year", y = 'Biomass (t)', title = '') +
    coord_cartesian(ylim=c(0, 55000)) # allows Error Bars to go off the page!!!!!!!!!!!!!!
  print(p_survey2_same_small)
  
  ggsave(here::here(current_yr, "figs", "Presentations", paste(current_yr,"Big_Regional_Biomass_Estimate_same_small.png",sep = "_")), dpi = 300, units = 'in',
         height = 7, width = 9,bg = 'white')
  
  # Same Y axis
  p_survey2_same <- ggplot(data = pl_bio_strata_calc) + 
    facet_grid(rows = vars(strata)) +
    geom_ribbon(aes(x = year, ymin=pred_strata_lci, ymax=pred_strata_uci), alpha=0.5, fill = "grey") +
    geom_point(aes(x = year, y = obs), position=position_dodge(width=0.6)) +
    geom_errorbar(aes(x = year, y = obs,ymin=obs_lci, ymax=obs_uci),
                  width=0.2, position=position_dodge(width=0.6)) +
    geom_line(aes(x = year, y = pred_strata)) +
    theme(panel.grid.major = element_line(linewidth = 0.5)) +
    theme(axis.line = element_line()) +
    theme(axis.ticks = element_line(colour = "black")) +
    theme(axis.text = element_text(size =12),
          axis.title = element_text(size =12),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 12)) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(from = yr_mm[1], to = yr_mm[2], by = 5)) +
    labs(x = "Year", y = 'Biomass (t)', title = '') 
  print(p_survey2_same)
  
  ggsave(here::here(current_yr, "figs", "Presentations", paste(current_yr,"Big_Regional_Biomass_Estimate_same.png",sep = "_")), dpi = 300, units = 'in',
         height = 7, width = 9,bg = 'white')
  
  
  
  p_survey_E <- ggplot(data = pl_bio_strata_calc %>%
                         filter(strata == 'EGOA')) +
    geom_ribbon(aes(x = year, ymin=pred_strata_lci, ymax=pred_strata_uci), alpha=0.5, fill = "grey") +
    geom_point(aes(x = year, y = obs), position=position_dodge(width=0.6)) +
    geom_errorbar(aes(x = year, y = obs,ymin=obs_lci, ymax=obs_uci),
                  width=0.2, position=position_dodge(width=0.6)) +
    geom_line(aes(x = year, y = pred_strata)) +
    theme(panel.grid.major = element_line(linewidth = 0.5)) +
    theme(axis.line = element_line()) +
    theme(axis.ticks = element_line(colour = "black")) +
    theme(axis.text = element_text(size =12),
          axis.title = element_text(size =12),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 12),
          plot.title = element_text(size=24)) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(from = yr_mm[1], to = yr_mm[2], by = 5)) +
    labs(x = "Year", y = 'Biomass (t)', title = 'Big - EGOA') +
    coord_cartesian(ylim=c(0, 45000)) # allows Error Bars to go off the page!!!!!!!!!!!!!!
  print(p_survey_E)
  
  ggsave(here::here(current_yr, "figs", "Presentations", paste(current_yr,"Big_EGOA_Biomass_Estimate.png",sep = "_")), dpi = 300, units = 'in',
         height = 7, width = 9,bg = 'white')
  
  
}





## Plot using R code
# est_bio_strata <- bs_all_out$biomass_by_strata
# 
# reg_nam <- c("WGOA","CGOA","EGOA")
# y_low <- c(0,0,0)
# y_up <- c(30000,60000,60000)
# c_axis <- 1.7
# save_figs <- T
# 
# # par(mfrow=c(3,1),mar = c(.1,2.5,.1,.1),oma = c(5,5,.5,.5))
# 
# if(save_figs == T){
#   png(here(current_yr, "figs",paste(current_yr,"Big_Regional_Biomass_Estimate.png", sep = "_")),
#       width = 899, height = 705, units = "px")
#   
#   par(mfrow=c(3,1),mar = c(.1,2.7,.1,.1),oma = c(5,5,.5,.5))
# } 
# for(i in 1:length(reg_nam))
# {
#   temp <- est_bio_strata %>%
#     filter(strata == reg_nam[i])
#   
#   if((i%%length(reg_nam))==0){
#     plot(temp$year,temp$pred,ylim = c(y_low[i],y_up[i]), type = "l", col = "white",xlab = "", ylab = "",
#          cex.axis = c_axis, yaxt = "n")
#     polygon(c(temp$year,rev(temp$year)),c(temp$pred_uci,rev(temp$pred_lci)),col="grey",
#             border = "white")
#     grid()
#     points(temp$year,temp$obs, pch = 16)
#     arrows(x0=temp$year, y0=temp$obs_lci, x1=temp$year, y1=temp$obs_uci, code=3, angle=90, length=0.1)
#     lines(temp$year,temp$pred,type="l",col="black", lwd = 2)
#     temp_axis_loc <- axTicks(2)
#     temp_axis_lab <- formatC(temp_axis_loc, format="d", big.mark=",")
#     axis(2, at = temp_axis_loc, labels = temp_axis_lab, cex.axis = c_axis, las = 1)
#     mtext(reg_nam[i], side=3,line=-2,cex = 1.5)
#   }else{
#     plot(temp$year,temp$pred,ylim = c(y_low[i],y_up[i]), type = "l", col = "white",xlab = "", ylab = "",
#          xaxt = "n", yaxt = "n",cex.axis = c_axis)
#     polygon(c(temp$year,rev(temp$year)),c(temp$pred_uci,rev(temp$pred_lci)),col="grey",
#             border = "white")
#     grid()
#     points(temp$year,temp$obs, pch = 16)
#     arrows(x0=temp$year, y0=temp$obs_lci, x1=temp$year, y1=temp$obs_uci, code=3, angle=90, length=0.1)
#     lines(temp$year,temp$pred,type="l",col="black", lwd = 2)
#     temp_axis_loc <- axTicks(2)
#     temp_axis_lab <- formatC(temp_axis_loc, format="d", big.mark=",")
#     axis(2, at = temp_axis_loc, labels = temp_axis_lab, cex.axis = c_axis, las = 1)
#     mtext(reg_nam[i], side=3,line=-2,cex = 1.7)
#   }
# }
# mtext("Year", side=1,line=3,cex = 1.7, outer = T)
# mtext("Biomass (t)", side=2,line=3,cex = 1.7, outer = T)
# 
# if(save_figs == T) dev.off()





  

