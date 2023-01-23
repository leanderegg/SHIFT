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
data_csv <- list.files(path = here("data", "leaf_area_scans"),  # Identify all CSV files
                         pattern = "*.csv", full.names = TRUE, include.dirs = T) %>% 
    lapply(read_csv) %>%                              # Store all files in list
    bind_rows                                         # Combine data sets into one data set 
  data_csv 
  
data_files_txt <- list.files(path = here("data", "leaf_area_scans"),  # Identify all txt files
                         pattern = "*.txt", full.names = TRUE) %>%
                         lapply(read_table)  

data_txt_new <- data.table::rbindlist(data_files_txt, fill= T)





file.info(data_files_txt)

df <- data_files_txt %>%
  set_names(.) %>%
  map_df(read_table2, .id = "FileName")


#%>% 
   # lapply(read_table)                                        # Combine data sets into one data set 

df2 <- vroom::vroom(data_files_txt
                    #, .id = "FileName"
                   )



name_files <- setNames(data_files_txt, data_files_txt )









