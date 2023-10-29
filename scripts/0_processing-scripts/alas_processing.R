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

#-----------------------------------

##Read in raw data: 

alas_df_raw <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="ALAS DATA", skip=0, na = "NA") %>% 
  clean_names() %>% 
  mutate(date = ymd(date), 
         ) 

unique(alas_df_raw$date)

alas_noareas_df <- alas_df_raw %>% 
   mutate(
         tree_id = as.factor(tree_id), 
         branch = as.factor(branch), 
         date_alas = date, 
         week = week(date)
         ) %>% 
  select( -area_if_leaf_from_scan_fresh_weight_scan,
        -dry_scan,
        -date_alas
         ) %>% 
  filter(week <= 29) %>% 
  select(-x17)

#Weeks/dates are good. 


##Just after July, WITH areas entered correctly: 

alas_post_july <- alas_df_raw %>% 
  mutate(
    tree_id = as.factor(tree_id), 
    branch = as.factor(branch), 
    week = week(date),
  ) %>% 
  filter(week > 29) %>% 
  rename(area_cm2 = area_if_leaf_from_scan_fresh_weight_scan) 

#Read in processed leaf areas from leaf area processing script:

alas_leaf_area_fromscans_df <- read_csv(here("processed-data", paste0("leaf_area_alldates_",datver,".csv")), show_col_types = FALSE) %>% 
 mutate(year = year_leaf_area) %>% 
  select(-day, -date_new, -'...1', -x17, -file, -species, -year_leaf_area) %>% 
  mutate(tree_id = as.factor(tree_id), 
         branch = as.factor(branch)) %>% 
  mutate(sub_branch = case_when(
    sub_branch %in% NA_real_ ~ notes_leaf_area, 
    TRUE ~ as.character(sub_branch)
  )) %>% 
  mutate(sub_branch = case_when(
    sub_branch %in% c('y0', "FUNKYBAGLABLE", "dry scan area only; need to use conversion from 4/4 scans") ~ NA, 
    TRUE ~ as.character(sub_branch)
  )) %>% 
  mutate(year = case_when(
    sub_branch %in% c('y0') ~ 0, 
    TRUE ~ as.numeric(year)
  )) %>% 
  filter(!tree_id %in% c('234x')) 


##Combine all without post-July: 

alas_leaf_area_df_week_nopostjuly <- merge(alas_leaf_area_fromscans_df, alas_noareas_df, by = c("tree_id", 
                                                                   "branch", 
                                                                 #  "species",
                                                                   "year",
                                                                   "week", 
                                                                  # "date",
                                                                   "sub_branch"), 
                                all = T) 

##Add in July: 

alas_leaf_area_df <- bind_rows(alas_leaf_area_df_week_nopostjuly, alas_post_july) %>% 
  group_by(tree_id, branch, sub_branch, year, week) %>% 
  fill(c(6:21),.direction = "downup") %>% 
  mutate(area_mm2 = area_cm2*10,
         lma_g_cm2 = ldm_g/area_cm2,
         d_cm = d_mm/10, #diameter is in mm, convert to cm
         r_mm = d_mm/2, #turn diameter (mm) into radius
         r_cm = d_cm/2, #do same, but with cm.
         area_stem_mm2 = (pi*((r_mm)^2)), #calculate area of stem in mm2.
         alas_cm2_per_mm2 = (area_cm2/area_stem_mm2), #
         sla_cm_g = area_cm2/ldm_g) %>% 
  select(-dry_scan) %>% 
  distinct() %>% 
  select(species, tree_id, branch, sub_branch, year, week, area_cm2, 
         area_stem_mm2, alas_cm2_per_mm2, sla_cm_g, lma_g_cm2, ldm_g)
  
missing_stem_areas <- alas_leaf_area_df %>% 
  filter(is.na(area_stem_mm2))
#Look at it: 

alas_leaf_area_df %>% 
  #filter(alas_cm2_per_mm2 < 20) %>% 
  ggplot(aes(x= week, y = alas_cm2_per_mm2, color = species)) +
  geom_point(alpha = .2)

write.csv(alas_leaf_area_df, here("processed-data", paste0("alas_leaf_area_df_week",datver,".csv")))


  

