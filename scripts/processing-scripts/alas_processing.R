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
  filter(week <= 29)

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
 mutate(date = date_leaf_area, year = year_leaf_area) %>% 
  select(-day, -week, -date_new, -date_leaf_area, -'...1', -year_leaf_area, -week, -file, -species) %>% 
  mutate(tree_id = as.factor(tree_id), 
         branch = as.factor(branch)) %>% 
  mutate(sub_branch = case_when(
    sub_branch %in% NA_real_ ~ notes_leaf_area, 
    TRUE ~ as.character(sub_branch)
  )) %>% 
  mutate(sub_branch = case_when(
    sub_branch %in% c('y0', "FUNKYBAGLABLE") ~ NA, 
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
                                                                  # "week", 
                                                                   "date",
                                                                   "sub_branch"), 
                                all.y = T) 

##Add in July: 

alas_leaf_area_df <- bind_rows(alas_leaf_area_df_week_nopostjuly, alas_post_july) %>% 
  mutate(area_mm2 = area_cm2*10,
         lma_g_cm2 = ldm_g/area_cm2,
         d_cm = d_mm/10,
          r_mm = d_mm, 
         r_cm = d_cm/2,
         area_stem_mm2 = (pi*((r_mm)^2)), 
         alas_cm2_per_mm2 = (area_cm2/area_stem_mm2), 
        sla_cm_g = area_cm2/ldm_g) %>% 
  distinct()

#Look at it: 

alas_df_final %>% 
  filter(alas_cm2_per_mm2 < 20) %>% 
  ggplot(aes(x= week, y = alas_cm2_per_mm2, color = species)) +
  geom_jitter(alpha = .2)

write.csv(alas_df_final, here("processed-data", paste0("alas_leaf_area_df_week",datver,".csv")))


  

