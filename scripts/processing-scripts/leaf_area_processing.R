library(tidyverse) # all the tidyverse data handling functions
library(lubridate) #Dealing with dates. 
library(janitor) # cleaning up column names, etc.
require(ggplot2)
library(here) #for easier/less buggy file organization
library(readxl)
library(gridExtra)
library(MetBrewer)
library(here)


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

####
# all txt files with just AREA
####
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
  mutate(id = str_remove(source,"/Users/user/Desktop/github/SHIFT/data/leaf_area_scans/"),
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

####
# all txt files with just TOTAL AREA
####
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
  mutate(id = str_remove(source,"/Users/user/Desktop/github/SHIFT/data/leaf_area_scans/scans_with_total_area/"),
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



#Combine: 

leaf_area_df0 <- rbind(leaf_area_data_txt2, leaf_area_data_txt33, leaf_area_data_csv2) %>% 
  separate(id, sep = c("-", "_"),
           into = c("spp", "tree", "date", "branch", "year", "extra_info", "more_info")) 

leaf_area_df1 <- leaf_area_df0 %>% 
 # filter(!id == "2354337") %>% 
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
  select(-date_new, -extra_info, -more_info, -tree, -area, -spp, -date, 
        # -year, 
         -date_old
         )

unique(leaf_area_df1$branch)
unique(leaf_area_df1$year)

date_weird <- leaf_area_df1 %>% 
  filter(is.na(date))
  

write.csv(leaf_area_df1, here("processed-data", paste0("leaf_area_alldates_",datver,".csv")))

#####
leaf_area_df1 %>% 
ggplot(aes(y = area_cm2, 
           x= week, 
           color = species)) +
  geom_jitter()

###All dates after July are in the SHIFT data collection folder and can be picked up there. 


##MISSING: Leaf areas for March! Weeks 10-12.










