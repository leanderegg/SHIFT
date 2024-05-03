
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


lfm_raw <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), 
                     sheet="LFM DATA", skip=0, na = "NA") %>%
  clean_names()

lfm_raw$age[which(lfm_raw$age=="missing")] <- 3 # if missing, assumed it was bulk
lfm_raw$age[which(is.na(lfm_raw$age))] <- 3
lfm_raw$age[which(lfm_raw$age=="1,2")] <- 1 # anything yr 1 had yr 1 or 2+


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
  mutate(lfm_wet_per_dry_g = ((wet_wt_g - dry_wt_g)/dry_wt_g), #do calculations
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

# do some data cleaning:
lfm_df$tree[which(lfm_df$tree==1472)] <- 1478 # one mislabeled leaf
lfm_df$tree[which(lfm_df$tree==9382)] <- 2382
lfm_df$tree[which(lfm_df$tree==2847)] <- 2347
lfm_df$tree[which(lfm_df$tree==2323)] <- 2023
lfm_df$tree[which(lfm_df$tree==2387)] <- 2381
lfm_df$tree[which(lfm_df$tree==2847)] <- 2347
lfm_df$tree[which(lfm_df$tree==2080)] <- 2380
lfm_df$tree[which(lfm_df$tree==2077)] <- 2377
lfm_df$tree[which(lfm_df$tree==2393)] <- 2093
lfm_df$tree[which(lfm_df$tree==2385)] <- 2085
# can't place: 2802, 2303, 2322, 2378, 3477, NA's, 2127, (Other), 2070, 2080,2081,2082
badnames <- c(2802, 2303, 2322, 2378, 3477, 2127, "(Other)", 2070, 2080,2081,2082)
  # 57 samples from 11 trees with unidentifiable names
length(which(lfm_df$tree %in% badnames))

lfm_df <- lfm_df[-which(lfm_df$tree %in% badnames),]

write.csv(lfm_df, here("processed-data", paste0("lfm_alldates_cleaned",datver,".csv")))

# Averaging obs since we have many duplicates and I can't figure out how this happened. Repeated Measures of the same container?
  # we'll just sum the wet weights and dry weights
lfm_df_avg <- lfm_df %>% group_by(time, type, year, tree, date_lfm, week) %>% summarise(lfm_wet_per_dry_g = mean(wet_wt_g_tot = sum(lfm_wet_wt_g, na.rm=T), dry_wt_g_tot = sum(lfm_dry_wt_g, na.rm=T), lfm_wet_per_dry_g, na.rm=T), lfm_percent = mean(lfm_percent, na.rm=T))
  # this averages ~250 duplicate measurements
write.csv(lfm_df, here("processed-data", paste0("lfm_alldates_avgd",datver,".csv")))


# and summing years and tissue types to get one bulk lfm value per ind per time
  # the only question is whether I should sum all values, including duplicates
lfm_bulk_df <- 

