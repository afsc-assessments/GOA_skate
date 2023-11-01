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

state_cat <- read.csv(here(current_yr, "data", "user_inputs","State_Catch.csv"))


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
  left_join((state_cat %>%
              select(year,Big)), by = "year") %>%
  rename(E2 = Big) %>%
  relocate(E2, .before = total_catch) %>%
  left_join(big_ofl,by = "year")

if(export_dat == T){
  write_csv(big_cat_OFL, here(current_yr, "data","output",paste(current_yr,"Big_Skate_OFL_catch2.csv",sep = "-")))
}


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
  left_join((state_cat %>%
               select(year,Longnose)), by = "year") %>%
  rename(E2 = Longnose) %>%
  relocate(E2, .before = total_catch) %>%
  left_join(long_ofl,by = "year")

if(export_dat == T){
  write_csv(long_cat_OFL, here(current_yr, "data","output",paste(current_yr,"Longnose_Skate_OFL_catch2.csv",sep = "-")))
}

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
  left_join((state_cat %>%
               select(year,Other)), by = "year") %>%
  rename(E2 = Other) %>%
  relocate(E2, .before = catch_wght) %>%
  left_join(ot_ofl,by = "year")

if(export_dat == T){
  write_csv(ot_cat_OFL, here(current_yr, "data","output",paste(current_yr,"Other_Skate_OFL_catch2.csv",sep = "-")))
}

##--------##
#  Plots   #
##--------##


ggplot2::theme_set(afscassess::theme_report())

big_specs <- big_cat_OFL %>%
  add_column(sp = "Big Skate") %>%
  select(year,sp,total_catch,tot_ABC,tot_TAC) %>%
  rename(Catch = total_catch,
          ABC = tot_ABC,
          TAC = tot_TAC) %>%
  pivot_longer(cols = c('TAC', 'ABC'))  

# tac_abc_pl <- ggplot(big_specs %>%
#                        filter(year <= current_yr),
#                      aes(x = year)) +
#   geom_bar(aes(y = value, fill = name),
#            stat = 'identity', position = 'identity',
#            col = 'black', alpha = .25) +
#   geom_point(aes(y = Catch, shape = 'Catch'))  +
#   theme(panel.grid.major = element_line(linewidth = 0.5)) +
#   theme(axis.line = element_line()) +
#   theme(axis.ticks = element_line(colour = "black")) +
#   geom_line(aes(y = Catch, lty = 'Catch')) +
#   scale_y_continuous(labels = scales::comma)  +
#   scale_fill_manual(values = c('white', 'black'))  +
#   theme(legend.position = c(.9, .85)) +
#   labs(x = NULL, y = 'Biomass (t)', fill = NULL, shape = NULL, lty = NULL)
# print(tac_abc_pl)

long_specs <- long_cat_OFL %>%
  add_column(sp = "Longnose Skate") %>%
  select(year,sp,total_catch,tot_ABC,tot_TAC) %>%
  rename(Catch = total_catch,
         ABC = tot_ABC,
         TAC = tot_TAC) %>%
  pivot_longer(cols = c('TAC', 'ABC'))  
# 
# tac_abc_pl1 <- ggplot(long_specs %>%
#                        filter(year <= current_yr),
#                      aes(x = year)) +
#   geom_bar(aes(y = value, fill = name),
#            stat = 'identity', position = 'identity',
#            col = 'black', alpha = .25) +
#   geom_point(aes(y = Catch, shape = 'Catch'))  +
#   theme(panel.grid.major = element_line(linewidth = 0.5)) +
#   theme(axis.line = element_line()) +
#   theme(axis.ticks = element_line(colour = "black")) +
#   geom_line(aes(y = Catch, lty = 'Catch')) +
#   scale_y_continuous(labels = scales::comma)  +
#   scale_fill_manual(values = c('white', 'black'))  +
#   theme(legend.position = c(.9, .85)) +
#   labs(x = NULL, y = 'Biomass (t)', fill = NULL, shape = NULL, lty = NULL)
# print(tac_abc_pl1)

ot_specs <- ot_cat_OFL %>%
  add_column(sp = "Other Skates") %>%
  select(year,sp,catch_wght,ABC,TAC) %>%
  rename(Catch = catch_wght) %>%
  pivot_longer(cols = c('TAC', 'ABC'))

tot_spec <- rbind(big_specs,long_specs,ot_specs) 
  
AAA
tac_abc_pl2 <- ggplot(tot_spec %>%
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
print(tac_abc_pl2)
  
ggsave(here::here(current_yr, "figs", paste(current_yr,"All_Skate_catch_specs.png",sep = "_")), dpi = 300, units = 'in',
       height = 4, width = 7,bg = 'white')



big_spec_area <- big_cat_OFL %>%
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
  arrange(factor(Area, levels = c("W","C","E"))) 

tac_abc_pl3 <- ggplot(big_spec_area %>%
                        filter(year <= current_yr),
                      aes(x = year)) +
  geom_bar(aes(y = value, fill = name),
           stat = 'identity', position = 'identity',
           col = 'black', alpha = .25) +
  # facet_wrap(~ assess_group) +
  facet_grid(rows = vars(Area)) +
  geom_point(aes(y = Catch, shape = 'Catch'))  +
  theme(panel.grid.major = element_line(linewidth = 0.5)) +
  theme(axis.line = element_line()) +
  theme(axis.ticks = element_line(colour = "black")) +
  geom_line(aes(y = Catch, lty = 'Catch')) +
  scale_y_continuous(labels = scales::comma)  +
  scale_fill_manual(values = c('white', 'black'))  +
  theme(legend.position = "top") +
  labs(x = NULL, y = 'Biomass (t)', fill = NULL, shape = NULL, lty = NULL)
print(tac_abc_pl3)

# ggsave(here::here(current_yr, "figs", paste(current_yr,"All_Skate_catch_specs.png",sep = "_")), dpi = 300, units = 'in',
#        height = 4, width = 7,bg = 'white')



long_spec_area <- long_cat_OFL %>%
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
  arrange(factor(Area, levels = c("W","C","E"))) 

tac_abc_pl4 <- ggplot(long_spec_area %>%
                        filter(year <= current_yr),
                      aes(x = year)) +
  geom_bar(aes(y = value, fill = name),
           stat = 'identity', position = 'identity',
           col = 'black', alpha = .25) +
  # facet_wrap(~ assess_group) +
  facet_grid(rows = vars(Area)) +
  geom_point(aes(y = Catch, shape = 'Catch'))  +
  theme(panel.grid.major = element_line(linewidth = 0.5)) +
  theme(axis.line = element_line()) +
  theme(axis.ticks = element_line(colour = "black")) +
  geom_line(aes(y = Catch, lty = 'Catch')) +
  scale_y_continuous(labels = scales::comma)  +
  scale_fill_manual(values = c('white', 'black'))  +
  theme(legend.position = "top") +
  labs(x = NULL, y = 'Biomass (t)', fill = NULL, shape = NULL, lty = NULL)
print(tac_abc_pl4)

area_tot <- rbind(big_spec_area,long_spec_area) %>%
  mutate(across(Area, factor, levels=c("W","C","E")))

tac_abc_pl5 <- ggplot(area_tot %>%
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
print(tac_abc_pl5)

ggsave(here::here(current_yr, "figs", paste(current_yr,"Area_Skate_catch_specs.png",sep = "_")), dpi = 300, units = 'in',
       height = 4, width = 7,bg = 'white')
