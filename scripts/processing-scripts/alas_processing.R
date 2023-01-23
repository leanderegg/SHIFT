####Al:As processing####

library(janitor)
library(here)
library(tidyverse)
library(lubridate)
library(readxl)
library(gridExtra)
library(MetBrewer)

alas_df_raw <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="ALAS DATA", skip=0, na = "NA") %>% 
  clean_names() %>% 
  mutate(date = ymd(date), 
         area_scanned_cm = area_if_leaf_from_scan, 
         #length_mm = case_when
         ) 

alas_df <- alas_df_raw %>% 
  mutate(area_cm = ((area_scanned_cm/sdm_g)*ldm_g), 
         area_mm = area_cm/10,
         dm_g = sdm_g + ldm_g,
         dm_mg = dm_g/1000,
         lma = (sdm_g + ldm_g)/area_cm, 
         d_cm = d_mm/10,
         r_cm = d_cm/2,
         area_stem_cm = (pi*(r_cm)^2), 
         alas = (area_mm/area_stem_cm), 
         sla = area_mm/(dm_mg)) %>% 
drop_na(alas)

#Lit values: 

#SLA: 7.6 mm2/mg
#stem diameter: 3.75mm
#Huber value: 0.077 mm2/cm2
