# datver <- "20231128" #date downloaded
# dataversion <- paste0("Data_", datver)

library(dlookr)

#Data from LEAF_Lab_Sedgwick  google sheet: https://docs.google.com/spreadsheets/d/1zBABEvgD2qqwJpY3slrIFePbNCgTdh8vVs4PHMzit0w/edit?pli=1#gid=817120000 

wp_23_df <- read_csv(here(paste0("Data_",datver_wp), "wp_2023.csv"),
                     show_col_types = FALSE) %>% 
  janitor::clean_names() %>%
  mutate(date = ymd(date), #lubridate package, 
         year = year(date)) %>% 
  filter(year == c(2023)) %>% #2022 is cleaned up in other areas of R. 
  mutate(date = case_when(
    date %in% c("2023-05-15") ~ "2023-05-12", #changed becuase entered wrong
    date %in% c("2023-07-27") ~ "2023-07-28", 
    TRUE ~ as.character(date)
  )) %>% 
  mutate(tag = case_when(
    tag %in% c(1748) ~ 1478, 
    tag %in% c(2879) ~ 2379, 
    TRUE ~ as.numeric(tag)
  )) %>% 
  select(tag, date, pd_md, water_potential_mpa, year) %>% 
  filter(water_potential_mpa < 10) #removing outlier in predawn water potential
  # mutate(species = case_when(
  #   tag %in% c(2013, 2014, 2015, 2016) ~ "blue oak", #need to find other labels
  #   TRUE ~ as.character(species)
  # )) %>% 
  # group_by(tag) %>% 
  # fill(c(1:4), .direction = "downup")

wp_23_nas <- wp_23_df %>% 
  filter(is.na(species))

wp_23_pd <- wp_23_df %>% 
  filter(pd_md == c("pd"))

wp_23_md <- wp_23_df %>% 
  filter(pd_md == c("md"))

wp_23_df %>% 
  ggplot(aes(y = water_potential_mpa, 
             x = date)) +
  geom_point() + 
  facet_wrap(~pd_md)


#add in tree info:
tree_info_all <- read_csv(here::here("data", "all_sedgwick_tree_info.csv")) %>% 
  janitor::clean_names()

wp_23_trees_df <- merge(tree_info_all, wp_23_df, by = c("tag")) %>% 
  select(tag, site, plot, species, date, pd_md, water_potential_mpa, year)

write_csv(wp_23_trees_df, here::here("processed-data", "2023_wp.csv"))

