library(tidyverse) # all the tidyverse data handling functions
library(lubridate) #Dealing with dates. 
library(janitor) # cleaning up column names, etc.
require(ggplot2)
library(here) #for easier/less buggy file organization
library(readxl)
library(gridExtra)
library(MetBrewer)
library(here)

####-----------------------
# all csv files
leaf_area_data_csv <- list.files(path = here("data", "leaf_area_scans"),  # Identify all CSV files
                         pattern = "*.csv", full.names = TRUE, include.dirs = T) %>% 
    lapply(read_csv) %>%                              # Store all files in list
    bind_rows 

leaf_area_data_csv2 <- leaf_area_data_csv %>%                                        # Combine data sets into one data set 
  clean_names() %>% 
  mutate(id = str_remove(slice, ".jpg"),
         id = str_remove(id, ".png"),
         area = total_area, 
         file = "csv", 
         id = gsub(pattern="_", replacement="-", id, fixed = TRUE),
         id = gsub(pattern=".b", replacement="-b", id, fixed = TRUE)) %>% 
  select(id, area, file) 

####-----------------------
# all txt files with just AREA

leaf_area_data_files_txt <- list.files(path = here("data", "leaf_area_scans"),  # Identify all txt files
                         pattern = "*.txt", full.names = TRUE, include.dirs = T)

leaf_area_data_files_txt_tables <- lapply(leaf_area_data_files_txt, read.table, sep = "\t") #get into a table

leaf_area_with_names <- mapply(cbind, leaf_area_data_files_txt_tables, 
                               "source"= leaf_area_data_files_txt,
                               SIMPLIFY = FALSE)  #get the names of the files

leaf_area_data_txt0  <- data.table::rbindlist(leaf_area_with_names, fill= T) 

leaf_area_data_txt <- leaf_area_data_txt0 %>% #this will bind columns of different class 
  select(V2, V3, source) %>% 
  filter(!V2 == "Area") %>% 
  mutate(area = V2) %>% 
  select(-V2)


leaf_area_data_txt2 <- leaf_area_data_txt %>% 
  clean_names() %>% 
  mutate(id = str_remove(source, ".*\\/"), # make this a regex to remove everything before
         id = str_remove(id, "/"),
         id = str_remove(id, ".txt"),
         id = gsub(pattern="_", replacement="-", id, fixed = TRUE),
         id = gsub(pattern=".b", replacement="-b", id, fixed = TRUE),
         id = gsub(pattern=".Y", replacement="-Y", id, fixed = TRUE),
         id = gsub(pattern="a-", replacement="-a-", id, fixed = TRUE),
         id = gsub(pattern="b-", replacement="-b-", id, fixed = TRUE),
         id = gsub(pattern="c-", replacement="-c-", id, fixed = TRUE),
         id = gsub(pattern=".y", replacement="-y", id, fixed = TRUE),
         id = gsub(pattern="bo", replacement="b0", id, fixed = TRUE),
         file = "txt"
         ) %>% 
  select(id, area, file) %>% 
  filter(!area == "Count") %>% 
  mutate(area = as.numeric(area)) %>% 
  group_by(id, file) %>% 
  summarise(area = sum(area))

####-----------------------

# all txt files with just TOTAL AREA


leaf_area_data_files_txt3 <- list.files(path = here("data", "leaf_area_scans", "scans_with_total_area"),  # Identify all txt files
                                       pattern = "*.txt", full.names = TRUE, include.dirs = T)

leaf_area_data_files_txt_tables3 <- lapply(leaf_area_data_files_txt3, read.table, sep = "\t") #get into a table

leaf_area_with_names3 <- mapply(cbind, leaf_area_data_files_txt_tables3, 
                               "source"= leaf_area_data_files_txt3,
                               SIMPLIFY = FALSE)  #get the names of the files

leaf_area_data_txt03  <- data.table::rbindlist(leaf_area_with_names3, fill= T) 

leaf_area_data_txt3 <- leaf_area_data_txt03 %>% #this will bind columns of different class 
  select(V2, V3, source) %>% 
  filter(!V3 == "Total Area") %>% 
  mutate(area = V3) %>% 
  select(-V2, -V3)

leaf_area_data_txt33 <- leaf_area_data_txt3 %>% 
  clean_names() %>% 
  mutate(id = str_remove(source,".*\\/"),
         id = str_remove(id, ".txt"),
         id = gsub(pattern="_", replacement="-", id, fixed = TRUE),
         id = gsub(pattern=".b", replacement="-b", id, fixed = TRUE),
         id = gsub(pattern=".Y", replacement="-Y", id, fixed = TRUE),
         id = gsub(pattern="a-", replacement="-a-", id, fixed = TRUE),
         id = gsub(pattern="b-", replacement="-b-", id, fixed = TRUE),
         id = gsub(pattern="c-", replacement="-c-", id, fixed = TRUE),
         id = gsub(pattern=".y", replacement="-y", id, fixed = TRUE),
         id = gsub(pattern="bo", replacement="b0", id, fixed = TRUE),
         file = "txt"
  ) %>% 
  select(id, area, file) %>% 
  mutate(area = as.double(area))

####-----------------------

#Combine: 

leaf_area_df0 <- rbind(leaf_area_data_txt2, leaf_area_data_txt33, leaf_area_data_csv2) %>% 
  mutate(id = gsub(pattern="_", replacement="-", id, fixed = TRUE)) %>% 
  separate_wider_delim(id, delim = c("-"),
           names = c("spp", "tree", "date", "branch", "year", "extra_info", "more_info"),
           too_few = "align_start")


leaf_area_df1 <- leaf_area_df0 %>% 
  mutate(date_new = mdy(date), 
         week = week(date_new), 
         day = day(date_new), 
         date_old = date, 
         date = date_new, 
         branch = str_remove(branch, "b"),
         branch = str_remove(branch, "B"),
         year = str_remove(year, "Y"),
         year = str_remove(year, "y"),
        # branch = str_remove(year, "y"),
         tree_id = as.factor(tree), 
         notes_leaf_area = extra_info, 
         more_notes_leaf_area = more_info, 
         area_cm2 = area, 
         species = spp,
         branch = gsub(pattern = "a", replacement = "1", branch, fixed = T),
         branch = gsub(pattern = "b", replacement = "2", branch,fixed = T),
         branch = gsub(pattern = "c", replacement = "3", branch,fixed = T),
         branch = gsub(pattern = "o", replacement = "0", branch,fixed = T),
         branch = gsub(pattern = "0 (2)", replacement = "0", branch, fixed = T),
        # branch = gsub(pattern = "", replacement = NA_real_, branch, fixed = T),
         year = gsub(pattern = "a", replacement = "1", year, fixed = T),
         year = gsub(pattern = "b", replacement = "2", year,fixed = T),
         year = gsub(pattern = "c", replacement = "3", year,fixed = T),
         year = gsub(pattern = "o", replacement = "0", year,fixed = T),
         year = gsub(pattern = "0 (2)", replacement = "0", year, fixed = T),
         year = as.factor(year), 
         date_leaf_area = date,
         year_leaf_area = year,
         week = week(date)
         ) %>% 
  select(#date_new, 
    -extra_info, -more_info, -tree, -area, -spp, -more_notes_leaf_area,
        # -year, 
         -date_old, -date
         )

unique(leaf_area_df1$branch)
unique(leaf_area_df1$year)

date_weird <- leaf_area_df1 %>% 
  filter(is.na(date))

####-----------------------

#Leaf area scans: dry mass scans from march and earlier: 

leaf_area_dry_scans <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="ALAS DATA", skip=0, na = "NA") %>% 
  clean_names() %>% 
  mutate(date = ymd(date), 
         day = day(date), 
         week = week(date), 
         file = "from dry", 
         branch = as.character(branch), 
         tree_id = as.factor(tree_id)) %>% 
 drop_na(dry_scan) %>% 
  rename(area_cm2 = dry_scan, 
         date_leaf_area = date,
         year_leaf_area = year, 
         notes_leaf_area = notes) %>% 
  select(-lwm_g, -ldm_g, -swm_g, -sdm_g, -l_cm, -d_mm, -area_if_leaf_from_scan_fresh_weight_scan, -length_new_cm) %>% 
  mutate(year_leaf_area = as.factor(year_leaf_area))
         

leaf_area_df2 <- bind_rows(leaf_area_df1, leaf_area_dry_scans)

dates_alas <- leaf_area_df2 %>% select(date_new, date_leaf_area)

####----------

#Conversion factor from dry scans to wet scans: 

dry_to_wet_conversion <- leaf_area_df2 %>% 
  filter(date_leaf_area == "2022-04-04") %>% 
  group_by(tree_id, branch, sub_branch, file) %>% 
  mutate(area_cm2 = mean(area_cm2)) %>% 
  ungroup() %>% 
  #distinct() %>% 
  select(branch, year_leaf_area, file, tree_id, area_cm2, species) %>% 
  pivot_wider(names_from = file, values_from = area_cm2, values_fn = mean 
              ) %>% 
  rename(from_dry = 'from dry') %>% 
  mutate(conversion = (txt/from_dry)) %>% 
  mutate(test = from_dry * conversion) %>% 
  filter(conversion < 2) %>% #only use realistic conversion (likly lost some leaves between wet and dry scanning)
  group_by(species) %>% 
  summarise(mean = mean(conversion)) 

dry_to_wet_conversion

##Convert dry to wet for all using conversion factor from above
leaf_area_df3 <- leaf_area_df2 %>% 
  filter(!(date_leaf_area %in% c("2022-04-04") & file %in% c("from dry"))) %>% 
  mutate(leaf_area_new = case_when(
    file %in% c("from dry") ~ area_cm2*dry_to_wet_conversion, 
    TRUE ~ as.numeric(area_cm2)
  )) %>% 
  select(-area_cm2) %>% 
  rename(area_cm2 = leaf_area_new)
  

####-----------------------

write.csv(leaf_area_df3, here("processed-data", paste0("leaf_area_alldates_",datver,".csv")))

####-----------------------
leaf_area_df3 %>% 
  filter(area_cm2 <100) %>% 
ggplot(aes(y = area_cm2, 
           x= week, 
           color = species)) +
  geom_jitter(alpha= .3)


###All leaf areas from dates after July are in the SHIFT data collection folder and can be picked up there. 












