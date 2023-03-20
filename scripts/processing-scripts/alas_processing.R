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

alas_noareas_df <- alas_df_raw %>% 
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
         #branch_stem_info = as.factor(branch), 
         branch = as.factor(branch), 
         date_alas = date, 
        # year_alas = year, 
         week = week(date),
       # area_cm2 = area_scanned_cm
         ) %>% 
#drop_na(alas) %>% 
  select(-date, 
        # -year,
         -area_if_leaf_from_scan, 
         -area_scanned_cm, 
        # -branch
         )


##Just after July: 

alas_post_july <- alas_df_raw %>% 
  mutate(
    tree_id = as.factor(tree_id), 
    branch = as.factor(branch), 
    date_alas = date, 
    week = week(date),
    area_cm2 = area_scanned_cm
  ) %>% 
  filter(week > 29) %>% 
  #drop_na(alas) %>% 
  select(-date, 
         # -year,
         -area_if_leaf_from_scan, 
         -area_scanned_cm, 
         # -branch
  )

#read in leaf areas: 

alas_leaf_area_df <- read_csv(here("processed-data", paste0("leaf_area_alldates_",datver,".csv")), show_col_types = FALSE) %>% 
  mutate(#branch_leaf_area = as.factor(branch), 
         week = as.integer(week)) #%>% 
 # select(-branch)


##Combine wall without post-July: 

alas_leaf_area_df_week_nopostjuly <- merge(alas_leaf_area_df, alas_noareas_df, by = c("tree_id", 
                                                                   "branch", 
                                                                   "species",
                                                                   "year",
                                                                   "week"), 
                                all = T) 


##Add in July: 

alas_leaf_area_df_week <- bind_rows(alas_leaf_area_df_week_nopostjuly, alas_post_july)



alas_df_final <-alas_leaf_area_df_week %>% 
 # select() %>% 
  mutate(#area_cm2_total = ((area_cm2/sdm_g)*ldm_g), 
         #area_cm2_total = area_cm2,
         area_mm2 = area_cm2*10,
        # dm_g = sdm_g + ldm_g,
        # dm_mg = dm_g*1000,
        # lma = (sdm_g + ldm_g)/area_cm2, 
         lma_g_cm2 = ldm_g/area_cm2,
         d_cm = d_mm/10,
          r_mm = d_mm, 
         r_cm = d_cm/2,
         area_stem_mm2 = (pi*((r_mm)^2)), 
         alas_cm2_per_mm2 = (area_cm2/area_stem_mm2), 
        # sla_mm_mg = area_mm2/(dm_mg),
        sla_cm_g = area_cm2/ldm_g)

write.csv(alas_df_final, here("processed-data", paste0("alas_leaf_area_df_week",datver,".csv")))


  

