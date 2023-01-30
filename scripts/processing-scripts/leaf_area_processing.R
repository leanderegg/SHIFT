library(tidyverse) # all the tidyverse data handling functions
library(lubridate) #Dealing with dates. 
library(janitor) # cleaning up column names, etc.
require(ggplot2)
library(here) #for easier/less buggy file organization
library(readxl)
library(gridExtra)
library(MetBrewer)
library(here)

## Data file version (so it's not hard coded in every read_excel() call)
#datver <- "10182022"
#dataversion <- paste0("Data_", datver)

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


# all txt files
  
leaf_area_data_files_txt <- list.files(path = here("data", "leaf_area_scans"),  # Identify all txt files
                         pattern = "*.txt", full.names = TRUE, include.dirs = T)

leaf_area_data_files_txt_tables <- leaf_area_data_files_txt %>% #read in all .txt files as a table
                        #  lapply(read_delim)
                         lapply(read_table)  

leaf_area_with_names <- mapply(cbind, leaf_area_data_files_txt_tables, "source"= leaf_area_data_files_txt, SIMPLIFY = FALSE) #get the names of the files

leaf_area_data_txt  <- data.table::rbindlist(leaf_area_with_names, fill= T) #this will bind columns of different class


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
  select(id, area, file)

#Combine: 

leaf_area_df0 <- rbind(leaf_area_data_txt2, leaf_area_data_csv2) %>% 
  separate(id, sep = c("-", "_"),
           into = c("spp", "tree", "date", "branch", "year", "extra_info", "more_info")) 

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
         branch = str_remove(year, "y"),
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
         -year, -date_old
         )

unique(leaf_area_df1$branch)
unique(leaf_area_df1$year)

date_weird <- leaf_area_df1 %>% 
  filter(is.na(date))
  

write.csv(leaf_area_df1, here("processed-data", paste0("leaf_area_alldates_",datver,".csv")))

#####
#Try bluebs!
#####

# txt_data <- read_delim(txt_files)  
# 
# hobo_files <- list.files(here("raw-data", "HOBOS_SEKI"),
#                          pattern = ".csv$", recursive = TRUE, full.names = TRUE)

# hobo_data <- read_csv(moisture_files, skip = 1,
#                       id = "id") %>% 
#   rename(date_time = `Date Time, GMT-07:00`) %>% 
#   rename(temp_c = `Temp, °C (LGR S/N: 21445506, SEN S/N: 21445506)`) %>% 
#   rename(relative_humidity = `RH, % (LGR S/N: 21445506, SEN S/N: 21445506)`) %>% 
#   select(id, date_time, temp_c, relative_humidity) %>%
#   mutate(id = str_remove(id,
#                          "/Users/user/Desktop/github/SEKI_Data_Analysis_IB/raw-data/HOBOS_SEKI/"),
#          id = str_remove(id, ".csv"))










