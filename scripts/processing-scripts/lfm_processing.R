
#____________________________________________________________
######## LFM DATA PROCESSIONG ###################
#____________________________________________________________

# script for quickly ingesting
# LFM
# started 10.12.2022 by IB

# last updated: 10.12.2022

library(tidyverse) # all the tidyverse data handling functions
library(lubridate) #Dealing with dates. 
library(janitor) # cleaning up column names, etc.
require(ggplot2)
library(here) #for easier/less buggy file organization
library(readxl)
library(gridExtra)
library(MetBrewer)

## Data file version (so it's not hard coded in every read_excel() call)
#datver <- "10182022"
#dataversion <- paste0("Data_", datver)

lfm_raw <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), 
                     sheet="LFM DATA", skip=0, na = "NA") %>%
  clean_names()

lfm_df <- lfm_raw %>% 
  # filter(!tree_id %in% c( ##Ignoring shrubs for now, but lets not always do this!
  #   "ARCA_ch",
  #   "ATLE",
  #   "LEU_cucu",
  #   "ARCA_CH",
  #   "ARCA_cucu",
  #   "ARCA",
  #   "BAPI",
  #   "ARCA near 2380",
  #   "LEU_CUCU", 
  #   "LL_LEU"
  # )) %>%
  mutate(tree = case_when(
    tree_id %in% c("ARCA_ch",
                   "ARCA_CH") ~ 10, 
    tree_id %in% c("ARCA",
                  "ARCA near 2380") ~ 11, 
    tree_id %in% c("ARCA_cucu") ~ 12,
    tree_id %in% c("LEU_cucu"
                  # "LEU_CUCU"
                   ) ~ 20, 
    tree_id %in% c("LL_LEU") ~ 21, 
    tree_id %in% c("ATLE") ~ 30, 
    tree_id %in% c("BAPI") ~ 40, 
    tree_id %in% c("ARCA") ~ 40, 
    TRUE ~ as.numeric(tree_id))) %>% 
  drop_na(date_updated) %>% 
  mutate( lfm_wet_per_dry_g = ((wet_wt_g - dry_wt_g)/dry_wt_g), #do calculations
         lfm_percent = 100 * ((wet_wt_g - dry_wt_g)/dry_wt_g),
         lfm_wet_wt_g = wet_wt_g, 
         lfm_dry_wt_g = dry_wt_g,
         year = as.numeric(age), 
         time  = case_when(
           time %in% c("PD") ~ "pd", 
           time %in% c("MD") ~ "md", 
           TRUE ~"NA" ), 
         date_lfm = ymd(date_updated), 
         week = week(date_lfm)) %>% 
  select(-site, -date, -date_updated,
         -tree_id, -age, -container_g, 
         -cont_wet_g, -cont_dry_g, -wet_wt_g, -dry_wt_g)
# %>% 
#   mutate(date_lfm_old = date_lfm) %>% 
#   mutate(date_lfm = case_when(
#   date_lfm_old %in% c("2022-03-11") ~ "2022-03-08", 
#   TRUE ~ as.character(date_lfm_old))) %>% 
#   mutate(date_lfm = ymd(date_lfm))

write.csv(lfm_df, here("processed-data", paste0("lfm_alldates_",datver,".csv")))


