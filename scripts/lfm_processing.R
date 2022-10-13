
#____________________________________________________________
######## LFM DATA PROCESSIONG ###################
#____________________________________________________________

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

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
datver <- "08182022"
dataversion <- paste0("Data_", datver)

lfm_raw <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), 
                     sheet="LFM DATA", skip=0, na = "NA") %>%
  clean_names()

lfm_df <- lfm_raw %>% 
  filter(!tree_id %in% c(
    "ARCA_ch",
    "ATLE",
    "LEU_cucu",
    "ARCA_CH",
    "ARCA_cucu",
    "ARCA",
    "BAPI",
    "ARCA near 2380",
    "LEU_CUCU"
  )) %>%
  drop_na(date) %>% 
  mutate(tree = as.numeric(tree_id), #make columns match WP, WC, and RWC data
         lfm_wet_per_dry_g = ((wet_wt_g - dry_wt_g)/dry_wt_g), #do calculations
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

write.csv(lfm_df, here("processed-data", paste0("lfm_alldates_",datver,".csv")))


