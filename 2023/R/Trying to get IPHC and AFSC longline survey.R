# Test IPHC data pull

library(here)
library(tidyverse)
library(janitor)

cpue_dat <- read_csv(here::here('C:/Users/lee.cronin-fine/Job/Assessments/GOA_skate/Jane advice on REMA/goa_skates_rema-main_from_jane', 'data', 'raw_csv', 'all_skate_iphc_rpns.csv')) %>% 
  janitor::clean_names(case = 'snake') %>% 
  mutate(species_group = dplyr::case_when(species == 'Big skate' ~ 'big_skate',
                                          species == 'Longnose skate' ~ 'longnose_skate',
                                          .default = 'other_skates'),
         strata = dplyr::case_when(fmp_sub_area %in% c('EY/SE', 'WY') ~ 'EGOA',
                                   .default = fmp_sub_area)) 

cpue_by_strata <- cpue_dat %>% 
  group_by(species_group, year, strata) %>% 
  summarise(cpue = sum(boot_strata_rpn, na.rm = TRUE),
            cv = sqrt(sum(boot_sd^2, na.rm = TRUE)) / cpue) %>% 
  ungroup() # %>% 
  # write_csv(here::here('data', 'rema_inputs', 'goa_skates_strata_cpue.csv'))



# AFSC longline survey
# AKFIN database #
database_akfin <- 'akfin'
akfin_user_nam <- key_list(database_akfin)$username  # Username
akfin_password <- key_get(database_akfin, keyring::key_list(database_akfin)$username) # Password

channel_akfin <- RODBC::odbcConnect(database_akfin, uid = akfin_user_nam,
                                    pwd = key_get(database_akfin, keyring::key_list(database_akfin)$username),
                                    believeNRows=FALSE)  # gains access to database


query_sur <- "select  year, council_management_area, council_management_area_ID, country, species, species_code, cpue, cpue_var, rpn, rpn_var
             from     afsc.lls_area_rpn_all_strata
             where    species_code in (400,420,440,455,460, 475,480,485,99995) and
                      year >= 1992 and
                      country in 'United States' and
                      council_management_area_ID in (3,4,6)"
temp_channel <- channel_akfin


## Get Longline Survey Data ## 
ll_afsc <- RODBC::sqlQuery(temp_channel, query = query_sur)
