##=============================================##
#     Obtaining the Data for GOA Skates         #
#        First Created: 08/15/23                #
#        Last Updated: 10/16/23                 #
##=============================================##

library(here)
library(keyring)  # accessing my username and passwords for database
library(RODBC)    # used to access database in R
library(readr)
library(tidyverse)
library(dplyr)


##---------------##
#  Manual Inputs  #
##---------------##
export_dat <- F        # T = export pulled data, F = do not export pulled data
quary_dat <- F         # query data from SQL
pres_pl   <- T         # T = Create plots for presentation, F = don't create plots for presentations    
current_yr <- 2023     # the current year



##-----------------------##
#   Accessing Databases   #
##-----------------------##
if(quary_dat == T){
  # Get the date
  temp <- as.numeric(unlist(strsplit(as.character(Sys.Date()), split = "-")))
  cur_dat_word <- paste(month.name[temp[2]], " ", temp[3],", ",temp[1],sep = "" ) # the current date in word format
  
  # AKFIN database #
  database_akfin <- 'akfin'
  akfin_user_nam <- key_list(database_akfin)$username  # Username
  akfin_password <- key_get(database_akfin, keyring::key_list(database_akfin)$username) # Password
  
  channel_akfin <- RODBC::odbcConnect(database_akfin, uid = akfin_user_nam,
                                      pwd = key_get(database_akfin, keyring::key_list(database_akfin)$username),
                                      believeNRows=FALSE)  # gains access to database
  
  # AFSC database #
  # database_afsc <- 'afsc'
  # channel_afsc <- RODBC::odbcConnect(database_afsc, uid = key_list(database_afsc)$username,
  #                                    pwd = key_get(database_afsc, keyring::key_list(database_afsc)$username),
  #                                    believeNRows=FALSE)  # gains access to database
}



##-----------------------##
#   Collect Survey Data   #
##-----------------------##
if(quary_dat == T){
  ## Setting up query for AKFIN ##  (You can use either AKFIN or AFSC, they should give the same data)
  query_sur <- "select  survey, year, regulatory_area_name, species_code, area_biomass, biomass_var
             from     afsc.race_biomassareaaigoa
             where    species_code in (400,420,435,440,455,460,471,472,480,485) and
                      survey = 'GOA' and
                      year <= 2023"
  temp_channel <- channel_akfin
  
  ## Setting up query for AFSC ##
  # query_sur <- "select  survey, year, regulatory_area_name, species_code, area_biomass, biomass_var
  #              from     goa.biomass_area
  #              where    species_code in (400,420,435,440,455,460,471,472,480,485) and
  #                       survey = 'GOA' and
  #                       year <= 2023"
  # temp_channel <- channel_afsc
  
  
  ## Get Survey Data ## 
  tot_sur_biomass <- RODBC::sqlQuery(temp_channel, query = query_sur)
  
  
  
  if(export_dat == T){
    write_csv(tot_sur_biomass, here::here(current_yr,"data", "raw", paste(current_yr,"Survey_biomass_by_strata.csv",sep = "-")))
    cat("Survey data last pulled on:", cur_dat_word, file = here(current_yr, "data","raw","survey_data_last_pulled.out"))
  } 
}else {
  tot_sur_biomass <- read.csv(here(current_yr, "data","raw",paste(current_yr,"Survey_biomass_by_strata.csv",sep = "-")))
}



##----------------------##
#   Collect Catch Data   #
##----------------------##
if(quary_dat == T){
  ## Collect Catch Data from AKFIN ##
  query_cat <- "select   year, week_end_date, ves_akr_name, FMP_Area, fmp_subarea, reporting_area_code, trip_target_group, agency_species_code, species_group_code, species_group_name, species_name, weight_posted, RETAINED_OR_DISCARDED 
          from     council.comprehensive_blend_ca
          where    agency_species_code in (700,701,702,703,704) and
                   fmp_area = 'GOA' and
                   year <= 2023"
  
  
  catch <- RODBC::sqlQuery(channel_akfin, query_cat)
  
  catch <- catch %>%
    rename_all(tolower) %>%
    mutate(strata = case_when(fmp_subarea == "CG" ~ "CGOA",
                              fmp_subarea %in% c("WY","SE") ~ "EGOA",
                              fmp_subarea == "WG" ~ "WGOA"),
           assess_group = case_when(species_name == "skate, longnose" ~ "longnose_skate",
                                    species_name == "skate, big" ~ "big_skate",
                                    .default = "other_skates"))
  
  if(export_dat == T){
    write_csv(catch, here::here(current_yr,"data","raw",  paste(current_yr,"Skate_catch.csv",sep = "-")))
    cat("Catch data was last pulled on:", cur_dat_word, "\n", file = here(current_yr, "data","raw","catch_data_last_pulled.out"))
  }
} else{
  catch <- read.csv(here(current_yr, "data","raw",paste(current_yr,"Skate_catch.csv",sep = "-")))
}

# Could potentially get ride of this
catch_strata <- catch %>%
  group_by(year,assess_group, strata) %>%
  summarise(catch_wght = sum(weight_posted)) %>%
  ungroup()

if(export_dat == T) write_csv(catch_strata, here::here(current_yr, "data", "output",paste(current_yr,"Skate_strata_catch.csv",sep = "-")))

catch_total <- catch %>%
  group_by(year,assess_group) %>%
  summarise(catch_wght = sum(weight_posted)) %>%
  ungroup()

if(export_dat == T) write_csv(catch_total, here::here(current_yr, "data", "output",paste(current_yr,"Skate_total_catch.csv",sep = "-")))

# yrs <- seq(from = 2005, to = current_yr, by = 1)
# state_cat <- read.csv(here(current_yr, "data", "user_inputs","State_Catch.csv")) # Alaska State Catch

# Catch by species, strata and percent retained
catch_s_r <- catch %>%
  filter(year >= 2005) %>%  # the strata divide of skates in assessments started in 2005
  summarise(catch = sum(weight_posted, na.rm = T),
            .by = c(year, assess_group, strata)) %>%
  arrange(factor(strata, levels = c("WGOA","CGOA","EGOA"))) %>%
  pivot_wider(names_from = strata, values_from = catch)  %>% 
  mutate(total_catch = sum(WGOA, CGOA, EGOA, na.rm=T), .by=c(year, assess_group)) %>%
  left_join(catch %>%
              filter(year >= 2005) %>%
              summarise(catch = sum(weight_posted, na.rm = T),
                        .by = c(year, assess_group, retained_or_discarded)) %>%
              mutate(total = sum(catch, na.rm=T),
                     .by = c(year, assess_group)) %>%
              pivot_wider(names_from = retained_or_discarded, values_from = catch)   %>% 
              mutate(retained = sum(R, na.rm=T) / total * 100, .by=c(year, assess_group)) %>%
              select(-c(D, R,total)),
            by = join_by(year, assess_group))

##--------------------------------------##
# Get the OFL, ABC and TAC for Big Skate #
##--------------------------------------##
big_ofl <- read.csv(here(current_yr,"data", "user_inputs", "Big_Skate_OFL_ABC_TAC.csv")) %>%
  relocate(OFL, .before = W_ABC)

big_cat_OFL <- catch_s_r %>%
  filter(assess_group == "big_skate") %>%
  # left_join((state_cat %>%
  #              select(year,Big)), by = "year") %>%
  # rename(E2 = Big) %>%
  # relocate(E2, .before = total_catch) %>%
  left_join(big_ofl,by = "year")

if(export_dat == T){
  write_csv(big_cat_OFL, here(current_yr, "data","output",paste(current_yr,"Big_Skate_OFL_catch.csv",sep = "-")))
}

##-------------------------------------------##
# Get the OFL, ABC and TAC for Longnose Skate #
##-------------------------------------------##
long_ofl <- read.csv(here(current_yr,"data", "user_inputs","Longnose_Skate_OFL_ABC_TAC.csv")) %>%
  relocate(OFL, .before = W_ABC)

long_cat_OFL <- catch_s_r %>%
  filter(assess_group == "longnose_skate") %>%
  # left_join((state_cat %>%
  #              select(year,Longnose)), by = "year") %>%
  # rename(E2 = Longnose) %>%
  # relocate(E2, .before = total_catch) %>%
  left_join(long_ofl,by = "year")


if(export_dat == T){
  write_csv(long_cat_OFL, here(current_yr, "data","output",paste(current_yr,"Longnose_Skate_OFL_catch.csv",sep = "-")))
}

##----------------------------------------##
# Get the OFL, ABC and TAC for other Skate #
##----------------------------------------##
ot_ofl <- read.csv(here(current_yr,"data", "user_inputs","Other_Skate_OFL_ABC_TAC.csv")) %>%
  relocate(OFL, .before = ABC)

ot_cat_OFL <- catch_s_r %>%
  filter(assess_group == "other_skates") %>%
  select(-WGOA, -CGOA, -EGOA) %>%
  # left_join((state_cat %>%
  #              select(year,Other)), by = "year") %>%
  # rename(E2 = Other) %>%
  # relocate(E2, .before = total_catch) %>%
  left_join(ot_ofl,by = "year")

if(export_dat == T){
  write_csv(ot_cat_OFL, here(current_yr, "data","output",paste(current_yr,"Other_Skate_OFL_catch.csv",sep = "-")))
}

# ot_ofl_file <- "Other_Skate_OFL_ABC_TAC.csv" # name of file that has the raw survey data by strata
# ot_ofl <- read_csv(here(current_yr, "data", "user_inputs" ,ot_ofl_file)) %>%
#   rename_all(tolower) 

##--------##
#  Plots   #
##--------##

ggplot2::theme_set(afscassess::theme_report())


tot_specs <- big_cat_OFL %>%
  add_column(sp = "Big Skate") %>%
  select(year,sp,total_catch,tot_ABC,tot_TAC) %>%
  rename(Catch = total_catch,
         ABC = tot_ABC,
         TAC = tot_TAC) %>%
  pivot_longer(cols = c('TAC', 'ABC')) %>%
  add_row(long_cat_OFL %>%
            add_column(sp = "Longnose Skate") %>%
            select(year,sp,total_catch,tot_ABC,tot_TAC) %>%
            rename(Catch = total_catch,
                   ABC = tot_ABC,
                   TAC = tot_TAC) %>%
            pivot_longer(cols = c('TAC', 'ABC'))) %>%
  add_row(ot_cat_OFL %>%
            add_column(sp = "Other Skates") %>%
            select(year,sp,total_catch,ABC,TAC) %>%
            rename(Catch = total_catch) %>%
            pivot_longer(cols = c('TAC', 'ABC')))

tot_tac_abc_pl <- ggplot(tot_specs %>%
                        filter(year <= current_yr),
                      aes(x = year)) +
  geom_bar(aes(y = value, fill = name),
           stat = 'identity', position = 'identity',
           col = 'black', alpha = .25) +
  # facet_wrap(~ assess_group) +
  facet_grid(rows = vars(sp)) +
  geom_point(aes(y = Catch, shape = 'Catch'))  +
  theme(panel.grid.major = element_line(linewidth = 0.5)) +
  theme(axis.line = element_line()) +
  theme(axis.ticks = element_line(colour = "black")) +
  geom_line(aes(y = Catch, lty = 'Catch')) +
  scale_y_continuous(labels = scales::comma)  +
  scale_fill_manual(values = c('white', 'black'))  +
  theme(legend.position = "top") +
  labs(x = NULL, y = 'Biomass (t)', fill = NULL, shape = NULL, lty = NULL)
print(tot_tac_abc_pl)

ggsave(here::here(current_yr, "figs", paste(current_yr,"All_Skate_catch_specs.png",sep = "_")), dpi = 300, units = 'in',
       height = 7, width = 9,bg = 'white')


area_specs <- big_cat_OFL %>%
  add_column(sp = "Big Skate") %>%
  select(year,sp,WGOA,CGOA,EGOA,W_ABC,C_ABC,E_ABC,W_TAC,C_TAC,E_TAC) %>%
  rename(W_Catch = WGOA,
         C_Catch = CGOA,
         E_Catch = EGOA)  %>%
  pivot_longer(cols = 3:11,
               names_to = c("Area","Type"),
               names_sep = "_") %>%
  pivot_wider(names_from = Type, values_from = value) %>%
  pivot_longer(cols = c('TAC', 'ABC')) %>%
  arrange(factor(Area, levels = c("W","C","E"))) %>%
  add_row(long_cat_OFL %>%
            add_column(sp = "Longnose Skate") %>%
            select(year,sp,WGOA,CGOA,EGOA,W_ABC,C_ABC,E_ABC,W_TAC,C_TAC,E_TAC) %>%
            rename(W_Catch = WGOA,
                   C_Catch = CGOA,
                   E_Catch = EGOA)  %>%
            pivot_longer(cols = 3:11,
                         names_to = c("Area","Type"),
                         names_sep = "_") %>%
            pivot_wider(names_from = Type, values_from = value) %>%
            pivot_longer(cols = c('TAC', 'ABC')) %>%
            arrange(factor(Area, levels = c("W","C","E")))) %>%
  mutate(across(Area, factor, levels=c("W","C","E"))) %>%
  mutate(Area2 = case_when(Area == "W" ~ "WGOA",
                           Area == "C" ~ "CGOA",
                           Area == "E" ~ "EGOA")) %>%
  mutate(across(Area2, factor, levels=c("WGOA","CGOA","EGOA")))


area_tac_abc_pl <- ggplot(area_specs %>%
                        filter(year <= current_yr),
                      aes(x = year)) +
  geom_bar(aes(y = value, fill = name),
           stat = 'identity', position = 'identity',
           col = 'black', alpha = .25) +
  # facet_wrap(~ assess_group) +
  facet_grid(rows = vars(Area), cols = vars(sp), scales = "free_y") +
  geom_point(aes(y = Catch, shape = 'Catch'))  +
  theme(panel.grid.major = element_line(linewidth = 0.5)) +
  theme(axis.line = element_line()) +
  theme(axis.ticks = element_line(colour = "black")) +
  geom_line(aes(y = Catch, lty = 'Catch')) +
  scale_y_continuous(labels = scales::comma)  +
  scale_fill_manual(values = c('white', 'black'))  +
  theme(legend.position = "right") +
  # guides(shape = guide_legend(override.aes = list(size = 0.8))) +
  # theme(legend.title = element_text(size = 3), 
  #         legend.text = element_text(size = 3)) +
  labs(x = NULL, y = 'Biomass (t)', fill = NULL, shape = NULL, lty = NULL)
print(area_tac_abc_pl)

ggsave(here::here(current_yr, "figs", paste(current_yr,"Area_Skate_catch_specs.png",sep = "_")), dpi = 300, units = 'in',
       height = 7, width = 9,bg = 'white')

##===================================##
#  plots for Plan Team presentation   #
##===================================##
if(pres_pl == T){
  tot_tac_abc_pl2 <- ggplot(tot_specs %>%
                              filter(year <= current_yr),
                            aes(x = year)) +
    geom_bar(aes(y = value, fill = name),
             stat = 'identity', position = 'identity',
             col = 'black', alpha = .25) +
    # facet_wrap(~ assess_group) +
    facet_grid(rows = vars(sp)) +
    geom_point(aes(y = Catch, shape = 'Catch'))  +
    theme(panel.grid.major = element_line(linewidth = 0.5)) +
    theme(axis.line = element_line()) +
    theme(axis.ticks = element_line(colour = "black")) +
    theme(axis.text = element_text(size =12),
          axis.title = element_text(size =12),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 12)) +
    geom_line(aes(y = Catch, lty = 'Catch')) +
    scale_y_continuous(labels = scales::comma)  +
    scale_fill_manual(values = c('white', 'black'))  +
    theme(legend.position = "top") +
    labs(x = NULL, y = 'Biomass (t)', fill = NULL, shape = NULL, lty = NULL)
  print(tot_tac_abc_pl2)
  
  ggsave(here::here(current_yr, "figs", "Presentations", paste(current_yr,"All_Skate_catch_specs.png",sep = "_")), dpi = 300, units = 'in',
         height = 7, width = 9,bg = 'white')
  
  
  # Big skate area specific specs
  area_tac_abc_big <- ggplot(area_specs %>%
                               filter(year <= current_yr,
                                      sp == 'Big Skate'),
                             aes(x = year)) +
    geom_bar(aes(y = value, fill = name),
             stat = 'identity', position = 'identity',
             col = 'black', alpha = .25) +
    # facet_wrap(~ Area2) +
    facet_grid(rows = vars(Area2), scales = "free_y") +
    geom_point(aes(y = Catch, shape = 'Catch'))  +
    theme(panel.grid.major = element_line(linewidth = 0.5)) +
    theme(axis.line = element_line()) +
    theme(axis.ticks = element_line(colour = "black")) +
    theme(axis.text = element_text(size =12),
          axis.title = element_text(size =12),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 12)) +
    geom_line(aes(y = Catch, lty = 'Catch')) +
    scale_y_continuous(labels = scales::comma)  +
    scale_fill_manual(values = c('white', 'black'))  +
    theme(legend.position = "right") +
    # guides(shape = guide_legend(override.aes = list(size = 0.8))) +
    # theme(legend.title = element_text(size = 3), 
    #         legend.text = element_text(size = 3)) +
    labs(x = NULL, y = 'Biomass (t)', fill = NULL, shape = NULL, lty = NULL)
  print(area_tac_abc_big)
  
  ggsave(here::here(current_yr, "figs", "Presentations", paste(current_yr,"Big_Skate_area_catch_specs.png",sep = "_")), dpi = 300, units = 'in',
         height = 7, width = 9,bg = 'white')
  
  
  area_tac_abc_big_same <- ggplot(area_specs %>%
                               filter(year <= current_yr,
                                      sp == 'Big Skate'),
                             aes(x = year)) +
    geom_bar(aes(y = value, fill = name),
             stat = 'identity', position = 'identity',
             col = 'black', alpha = .25) +
    # facet_wrap(~ Area2) +
    facet_grid(rows = vars(Area2)) +
    geom_point(aes(y = Catch, shape = 'Catch'))  +
    theme(panel.grid.major = element_line(linewidth = 0.5)) +
    theme(axis.line = element_line()) +
    theme(axis.ticks = element_line(colour = "black")) +
    theme(axis.text = element_text(size =12),
          axis.title = element_text(size =12),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 12)) +
    geom_line(aes(y = Catch, lty = 'Catch')) +
    scale_y_continuous(labels = scales::comma)  +
    scale_fill_manual(values = c('white', 'black'))  +
    theme(legend.position = "right") +
    # guides(shape = guide_legend(override.aes = list(size = 0.8))) +
    # theme(legend.title = element_text(size = 3), 
    #         legend.text = element_text(size = 3)) +
    labs(x = NULL, y = 'Biomass (t)', fill = NULL, shape = NULL, lty = NULL)
  print(area_tac_abc_big_same)
  
  ggsave(here::here(current_yr, "figs", "Presentations", paste(current_yr,"Big_Skate_area_catch_specs_same.png",sep = "_")), dpi = 300, units = 'in',
         height = 7, width = 9,bg = 'white')
  
  
  # Longnose skate area specific specs
  area_tac_abc_ln <- ggplot(area_specs %>%
                              filter(year <= current_yr,
                                     sp == 'Longnose Skate'),
                            aes(x = year)) +
    geom_bar(aes(y = value, fill = name),
             stat = 'identity', position = 'identity',
             col = 'black', alpha = .25) +
    # facet_wrap(~ Area2) +
    facet_grid(rows = vars(Area2), scales = "free_y") +
    geom_point(aes(y = Catch, shape = 'Catch'))  +
    theme(panel.grid.major = element_line(linewidth = 0.5)) +
    theme(axis.line = element_line()) +
    theme(axis.ticks = element_line(colour = "black")) +
    theme(axis.text = element_text(size =12),
          axis.title = element_text(size =12),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 12)) +
    geom_line(aes(y = Catch, lty = 'Catch')) +
    scale_y_continuous(labels = scales::comma)  +
    scale_fill_manual(values = c('white', 'black'))  +
    theme(legend.position = "right") +
    # guides(shape = guide_legend(override.aes = list(size = 0.8))) +
    # theme(legend.title = element_text(size = 3), 
    #         legend.text = element_text(size = 3)) +
    labs(x = NULL, y = 'Biomass (t)', fill = NULL, shape = NULL, lty = NULL)
  print(area_tac_abc_ln)
  
  ggsave(here::here(current_yr, "figs", "Presentations", paste(current_yr,"Longnose_Skate_area_catch_specs.png",sep = "_")), dpi = 300, units = 'in',
         height = 7, width = 9,bg = 'white')
  
  area_tac_abc_ln_same <- ggplot(area_specs %>%
                              filter(year <= current_yr,
                                     sp == 'Longnose Skate'),
                            aes(x = year)) +
    geom_bar(aes(y = value, fill = name),
             stat = 'identity', position = 'identity',
             col = 'black', alpha = .25) +
    facet_grid(rows = vars(Area2)) +
    geom_point(aes(y = Catch, shape = 'Catch'))  +
    theme(panel.grid.major = element_line(linewidth = 0.5)) +
    theme(axis.line = element_line()) +
    theme(axis.ticks = element_line(colour = "black")) +
    theme(axis.text = element_text(size =12),
          axis.title = element_text(size =12),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 12)) +
    geom_line(aes(y = Catch, lty = 'Catch')) +
    scale_y_continuous(labels = scales::comma)  +
    scale_fill_manual(values = c('white', 'black'))  +
    theme(legend.position = "right") +
    # guides(shape = guide_legend(override.aes = list(size = 0.8))) +
    # theme(legend.title = element_text(size = 3), 
    #         legend.text = element_text(size = 3)) +
    labs(x = NULL, y = 'Biomass (t)', fill = NULL, shape = NULL, lty = NULL)
  print(area_tac_abc_ln_same)
  
  ggsave(here::here(current_yr, "figs", "Presentations", paste(current_yr,"Longnose_Skate_area_catch_specs_same.png",sep = "_")), dpi = 300, units = 'in',
         height = 7, width = 9,bg = 'white')

}



