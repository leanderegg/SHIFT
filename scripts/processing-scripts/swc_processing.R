library(data.table)
library(janitor)
library(here)
library(tidyverse)
library(lubridate)
library(readxl)
library(gridExtra)
library(MetBrewer)
#install.packages("data.table")

## Data file version (so it's not hard coded in every read_excel() call)
#datver <- "10282022"
#dataversion <- paste0("Data_", datver)


#Field data: 

#Read in water potential data:
  
#Note: we measured Ks on stems collected on "2022-04-25" "2022-03-27" "2022-04-12" and "2022-05-23"

rwc_df <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), 
                     sheet="RWC DATA", skip=0, na = "NA") %>% 
  clean_names() %>% 
  mutate(date = ymd(date), 
         swc_g = wet_wt_g - dry_wt_g,
         swc_per_dry_g = (wet_wt_g - dry_wt_g)/dry_wt_g, #grams water per gram dry weight
         swc_percent = swc_per_dry_g * 100, 
         tree = tree_id, 
         year = as.character(year), 
         week = week(date), 
         swc_ww_g = wet_wt_g, 
         swc_dw_g = dry_wt_g) %>% 
  select(date, tree, swc_g, swc_per_dry_g, swc_percent, year, week, swc_ww_g, swc_dw_g, type, rep) %>% 
  # mutate(timing = dplyr::case_when(
  #   week <= 14 ~ "early", 
  #   week >= 15 ~ "late", 
  #   TRUE ~ NA_real_
  # )) %>% 
mutate(tree = case_when(
    tree == 1475 ~ 1478, 
    TRUE ~ as.numeric(tree)
  ))

write.csv(rwc_df, here("processed-data", paste0("rwc_alldates_",datver,".csv")))








