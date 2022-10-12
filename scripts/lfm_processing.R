
#____________________________________________________________
######## LFM DATA PROCESSIONG ###################
#____________________________________________________________

# script for quickly ingesting
# LWC

# started 06.11.2022 by IB

# last updated: 
#    


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

lfm_df <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), 
                     sheet="LFM DATA", skip=0, na = "NA") %>%
  clean_names() %>% 
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
  select(-x14, -site, -date, -date_updated,
         -tree_id, -age, -container_g, 
         -cont_wet_g, -cont_dry_g, -wet_wt_g, -dry_wt_g) 



write.csv(lfm_df, here("processed-data", paste0("lfm_alldates_",datver,".csv")))


