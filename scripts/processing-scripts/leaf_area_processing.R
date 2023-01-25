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
alas_data_csv <- list.files(path = here("data", "leaf_area_scans"),  # Identify all CSV files
                         pattern = "*.csv", full.names = TRUE, include.dirs = T) %>% 
    lapply(read_csv) %>%                              # Store all files in list
    bind_rows 

alas_data_csv2 <- alas_data_csv %>%                                        # Combine data sets into one data set 
  clean_names() %>% 
  mutate(id = str_remove(slice, ".jpg"),
         id = str_remove(id, ".png"),
         area = total_area, 
         file = "csv", 
         id = gsub(pattern="_", replacement="-", id, fixed = TRUE)) %>% 
  select(id, area, file) 


# all txt files
  
alas_data_files_txt <- list.files(path = here("data", "leaf_area_scans"),  # Identify all txt files
                         pattern = "*.txt", full.names = TRUE, include.dirs = T)

alas_data_files_txt_tables <- alas_data_files_txt %>% #read in all .txt files as a table
                        #  lapply(read_delim)
                         lapply(read_table)  

alas_with_names <- mapply(cbind, alas_data_files_txt_tables, "source"= alas_data_files_txt, SIMPLIFY = FALSE) #get the names of the files

alas_data_txt  <- data.table::rbindlist(alas_with_names, fill= T) #this will bind columns of different class


alas_data_txt2 <- alas_data_txt %>% 
  clean_names() %>% 
  mutate(id = str_remove(source,
                         "/Users/user/Desktop/github/SHIFT/data/leaf_area_scans/"),
         id = str_remove(id, ".txt"
        ),
        id = gsub(pattern="_", replacement="-", id, fixed = TRUE),
        file = "txt"
         ) %>% 
  select(id, area, file)

#Combine: 

alas_df <- rbind(alas_data_txt2, alas_data_csv2) %>% 
  separate(id, sep = c("-", "_"),
           into = c("spp", "tree", "date", "branch", "year", "extra_info", "more_info")) %>% 
  mutate(date_new = mdy(date), 
         week = week(date_new), 
         day = day(date_new), 
         date_old = date, 
         date = date_new) %>% 
  select(-date_new)

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










