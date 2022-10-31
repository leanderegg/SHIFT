
##setup##
  
library(data.table)
library(janitor)
library(here)
library(tidyverse)
library(lubridate)
library(readxl)
library(gridExtra)
#install.packages("data.table")
#devtools::install_github("an-bui/calecopal")
library(calecopal)


## Data file version (so it's not hard coded in every read_excel() call)
datver <- "08182022"
dataversion <- paste0("Data_", datver)




#Rerun processing:####

###Run all scripts for processing to make sure those are updated: 
  
 # (NOTE: if changing datver, will have to change in each of these scripts)

list.files("processing-scripts", full.names = TRUE) %>% walk(source)



####WC:####

# Water content data looks like this: 
# md_bulk_wet = as.numeric(md_bulk_wet), 
# lwc_md_bulk = ((md_bulk_wet-md_bulk_dry)/md_bulk_dry)
# lwc_pd_bulk = ((pd_bulk_wet - pd_bulk_dry)/pd_bulk_dry)
# lwc_md_leaf = ((ww_g_md - dw_g_md)/dw_g_md)
# lwc_pd_leaf = ((ww_g_pd - dw_g_pd)/dw_g_pd)) 


wc_df <- read_csv(here("processed-data", paste0("wc_alldates_",datver,".csv"))) %>% #contains LWC data
  mutate(date = lubridate::ymd(date))%>% 
  select(-...1) %>% 
  mutate(timing = case_when(
    week %in% c(9, 10, 11, 12, 13, 14) ~ "early", 
    week %in% c(15, 16, 17, 18, 19, 20, 21, 29, 33) ~ "late"
  ))%>% 
  mutate(date_wc = date) %>% 
  #select(-date) %>% 
  #drop_na(date_wc)  %>% 
  drop_na(mpa)  %>% 
  mutate(time  = case_when(
    time %in% c("PD") ~ "pd", 
    time %in% c("MD") ~ "md", 
    TRUE ~"NA"
  ), 
  tree = case_when(
    tree == 2127 ~ 2327, #weird tree
    TRUE ~ tree
  ), 
  lwc_leaf = case_when(
    lwc_leaf >= 5 ~ NA_real_, #weird value
    TRUE ~ lwc_leaf
  )) %>% 
  select(-date)

#WP:####

wp_df <- read_csv(here("processed-data", paste0("wp_alldates_long_",datver,".csv"))) %>%
  #mutate(date = lubridate::ymd(date)) %>% 
  select(-...1, -rep) %>% 
  mutate(timing = case_when(
    week %in% c(9, 10, 11, 12, 13, 14) ~ "early", 
    week %in% c(15, 16, 17, 18, 19, 20, 21, 29, 33) ~ "late"
  )) %>%  
  mutate(tree = as.numeric(tree), 
         tree = case_when(
           tree == 2127 ~ 2327, #weird tree
           TRUE ~ tree), 
         date_wp = date) %>% 
  select(-tag, -plot_number, -date )

#WC + WP:####

wp_wc_df <- merge(wp_df, wc_df, 
                  by = c("tree","plot", "site", "species", "time", "timing", "week", "mpa"), 
                  all = T) %>% 
  filter(species != "ARCA", 
         species != "LEU") %>% 
  rowwise() %>% 
  mutate(lwc_mean = mean(c(lwc_bulk, lwc_leaf), na.rm = T))

#tree info###

trees_sites_df <- wp_wc_df %>% 
  select(tree, plot, site, species) %>% 
  distinct()


#RWC:###


rwc_df <- read.csv(here("processed-data", paste0("rwc_alldates_",datver,".csv")))

rwc_df_leaves_y0 <- rwc_df %>% 
  filter(type == "leaf", 
         year %in% c(0, "NA")) %>% 
  group_by(tree, date) %>% 
  mutate(mean_swc_per_dry_g = mean(swc_per_dry_g)
         , mean_swc_g = mean(swc_g)) %>% 
  ungroup() %>% 
  mutate(date_rwc = date, 
         week = as.numeric(week)) %>% 
  select(-date) %>% 
  distinct() %>% 
  select(mean_swc_per_dry_g, mean_swc_g, tree, week, date_rwc) %>% 
  as.data.frame()

rwc_df_leaves_y1 <- rwc_df %>% 
  filter(type == "leaf", 
         year %in% c(1, "NA")) %>% 
  group_by(tree, date) %>% 
  mutate(mean_swc_per_dry_g = mean(swc_per_dry_g)
         , mean_swc_g = mean(swc_g)) %>% 
  ungroup() %>% 
  mutate(date_rwc = date, 
         week = as.numeric(week)) %>% 
  select(-date) %>% 
  distinct() %>% 
  select(mean_swc_per_dry_g, mean_swc_g, tree, week, date_rwc) %>% 
  as.data.frame()


####RWC + WC + WP:####

wp_wc_rwc_df <- merge(wp_wc_df, rwc_df_leaves_y0 ,
                      by = c("tree",
                             "week"
                             # "rep"
                      ),
                      #all.x = T,
                      all.x = T
)

# wp_wc_rwc_summary_df <- wp_wc_rwc_df %>% 
#   group_by(week, tree) %>% 
#   summarise(mean_md = mean(mpa_md), 
#             mean_pd = mean(mpa_pd), 
#             mean_lwc_md_bulk = mean(lwc_md_bulk), 
#             mean_lwc_pd_bulk = mean(lwc_pd_bulk), 
#             mean_swc_g = mean(mean_swc_g), 
#             mean_swc_per_dry_g = mean(mean_swc_per_dry_g))


####LFM:####

#Here we are splitting the LFM into different categories: stem vs. leaves, and y1 vs. y0. 


lfm_leaves_y0_df <- read.csv( here("processed-data", paste0("lfm_alldates_",datver,".csv"))) %>% 
  filter(type == "leaf", 
         year == 0) %>% 
  mutate(tree = as.numeric(tree))  %>% 
  select(-year)

lfm_leaves_y1_df <- read.csv( here("processed-data", paste0("lfm_alldates_",datver,".csv"))) %>% 
  filter(type == "leaf", 
         year == 1) %>% 
  mutate(lfm_wet_wt_y1_g = lfm_wet_wt_g, 
         lfm_wet_per_dry_y1_g = lfm_wet_per_dry_g, 
         lfm_dry_wt_y1_g = lfm_dry_wt_g, 
         lfm_y1_percent = lfm_percent, 
         tree = as.numeric(tree)) %>% 
  select(-year, -lfm_wet_wt_g, -lfm_dry_wt_g, -lfm_percent, -lfm_wet_per_dry_g, -X)

lfm_stem_y0_df <- read.csv( here("processed-data", paste0("lfm_alldates_",datver,".csv"))) %>% 
  filter(type == "stem", 
         year == 0)

lfm_stem_y1_df <- read.csv( here("processed-data", paste0("lfm_alldates_",datver,".csv"))) %>% 
  filter(type == "leaf",
         year == 1)


lfm_both_both_df <- read.csv( here("processed-data", paste0("lfm_alldates_",datver,".csv"))) %>% 
  filter(type == "both",
         year == 3)  %>% 
  mutate(lfm_wet_wt_both_g = lfm_wet_wt_g, 
         lfm_wet_per_dry_both_g = lfm_wet_per_dry_g, 
         lfm_dry_wt_both_g = lfm_dry_wt_g, 
         lfm_both_percent = lfm_percent, 
         tree = as.numeric(tree), 
         date_lfm_both = date_lfm) %>% 
  select(-year, -lfm_wet_wt_g, -lfm_dry_wt_g, -lfm_percent, -lfm_wet_per_dry_g, -X)

lfm_leaves_df <- merge(lfm_leaves_y0_df, lfm_leaves_y1_df, 
                       by = c("tree", "time", "date_lfm", "week", "type"), all = T) %>% 
  distinct() %>% 
  select(-X)

lfm_leaves_both_df <- merge(lfm_leaves_df, lfm_both_both_df,
                            by = c("tree", "time", "date_lfm", "week", "type"), all = T) %>% 
  distinct() 


####LFM + RWC + WC + WP (wide):####


wp_wc_rwc_lfm_df_wide <- merge(wp_wc_rwc_df, lfm_leaves_both_df, 
                               by = c("tree", "week", "time"), 
                               all = T) %>% 
  distinct() %>% 
  select(-site, -species, -plot)

wp_wc_rwc_lfm_df_wide_trees <- merge(trees_sites_df, wp_wc_rwc_lfm_df_wide, 
                                     by = c("tree"))

#make a shorter name because the long one will be a pain to write out!


all_water_df <- wp_wc_rwc_lfm_df_wide_trees %>% 
  mutate(rwc_percent = ((lwc_mean/mean_swc_per_dry_g)*100))

write.csv(all_water_df, here("processed-data", paste0("LWC_LFM_WP_dates",datver,".csv")))







#What are we missing?####


missings_df <- wp_wc_rwc_lfm_df_wide_trees %>% 
  select(tree, week, species, site, plot, time, date_wp, date_wc, date_lfm, date_rwc) %>% 
  distinct()

write.csv(missings_df, here("processed-data", paste0("missing_LWC_LFM_WP_dates",datver,".csv")))


