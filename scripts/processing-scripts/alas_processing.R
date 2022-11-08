####Al:As processing####

library(janitor)
library(here)
library(tidyverse)
library(lubridate)
library(readxl)
library(gridExtra)
library(MetBrewer)

alas_df_raw <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="ALAS DATA", skip=0, na = "NA") %>% 
  clean_names() %>% 
  mutate(date = ymd(date)) 

