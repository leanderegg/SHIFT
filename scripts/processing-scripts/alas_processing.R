####Al:As processing####

library(janitor)
library(here)
library(tidyverse)
library(lubridate)
library(readxl)
library(gridExtra)
library(MetBrewer)

###LIT VALUES for QUDO:

#SLA: 7.6 mm2/mg
#stem diameter: 3.75mm
#Huber value: 0.077 mm2/cm2

alas_df_raw <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="ALAS DATA", skip=0, na = "NA") %>% 
  clean_names() %>% 
  mutate(date = ymd(date), 
         area_scanned_cm = area_if_leaf_from_scan, 
         #length_mm = case_when
         ) 

unique(alas_df_raw$date)

alas_df <- alas_df_raw %>% 
   mutate(
     #area_cm = ((area_scanned_cm/sdm_g)*ldm_g), 
  #        area_mm = area_cm/10,
  #        dm_g = sdm_g + ldm_g,
  #        dm_mg = dm_g/1000,
  #        lma = (sdm_g + ldm_g)/area_cm, 
  #        d_cm = d_mm/10,
  #        r_cm = d_cm/2,
  #        area_stem_cm = (pi*(r_cm)^2), 
  #        alas = (area_mm/area_stem_cm), 
  #        sla = area_mm/(dm_mg), 
         tree_id = as.factor(tree_id), 
         branch = as.factor(branch), 
         date_alas = date, 
         year_alas = year, 
         week = week(date)
         ) %>% 
#drop_na(alas) %>% 
  select(-date, 
         -year
         )

unique(alas_df$date_alas)

#read in leaf areas: 
leaf_area_df <- read_csv(here("processed-data", paste0("leaf_area_alldates_",datver,".csv")), show_col_types = FALSE) 
##Combine: 

alas_leaf_area_df_week <- merge(leaf_area_df, alas_df, by = c("tree_id", "branch", "species", "week")) %>% 
  mutate(area_cm2_total = ((area_cm2/sdm_g)*ldm_g), 
         area_mm2 = area_cm2_total*10,
         dm_g = sdm_g + ldm_g,
         dm_mg = dm_g*1000,
         lma = (sdm_g + ldm_g)/area_cm2, 
         d_cm = d_mm/10,
         r_cm = d_cm/2,
         area_stem_cm2 = (pi*(r_cm)^2), 
         alas = (area_mm2/area_stem_cm2), 
         sla_mm_mg = area_mm2/(dm_mg))


