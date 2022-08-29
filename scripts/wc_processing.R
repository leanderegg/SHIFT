
#____________________________________________________________
######## LEAF WATER CONTENT DATA PROCESSIONG ###################
#____________________________________________________________

# script for quickly ingesting
# LWC

# started 06.11.2022 by IB

# last updated: 
#    


library(tidyverse) # all the tidyverse data handling functions
library(lubridate) #Dealing with dates. 
library(janitor) # cleaning up column names, etc.
require(ggplot2)
library(here) #for easier/less buggy file organization
library(readxl)
library(gridExtra)
library(MetBrewer)



## Data file version (so it's not hard coded in every read_excel() call)
datver <- "08182022"
dataversion <- paste0("Data_", datver)

#______________________________________________________________
############### Begin: WATER CONTENT - Load and Clean #######################################
#______________________________________________________________

#Date: 218 WP + LWC
wc218 <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="218-221 WP + LWC", skip=5, na = "NA") %>% clean_names() %>% 
  mutate(date = mdy("02-18-2022")) 


#Date: 228 WP + LWC
wc228 <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="228 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("02-28-2022")) 

#Date: 33-34 WP + LWC
wc303 <-  read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="33-34 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("03-03-2022")) 


#Date: 38-311 WP + LWC
wc38 <-  read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="38-311 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("03-11-2022")) 

# remove mislabeled tree 2567 which was actually 2367 
wc38 <- wc38[-which(wc38$tag=="2567"),]

#Date: 315 WP + LWC
wc315 <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="315 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("03-15-2022")) 


#Date: 325 WP + LWC
wc325 <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="325-327 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("03-25-2022")) 


#Date: 330 WP + LWC (core)
wc330 <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="330 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("03-30-2022")) 


#Date: 44WP + LWC (satellite)
wc44 <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="44 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("04-04-2022")) 


#Date: 46 WP + LWC (satellite)
wc46 <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="46 WP + LWC", skip=5, na = "NA") %>% clean_names() %>% 
  mutate(date = mdy("04-06-2022")) 


#Date: 411-412 WP + LWC
wc411 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="411-412 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("04-11-2022")) 


#Date: 413-414 WP + LWC
wc413 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="413-414 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("04-13-2022")) 


#Date: 425 WP + LWC
wc425 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="425 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("04-25-2022")) 


#Date: 427 WP + LWC
wc427 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="427 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("04-27-2022")) 

#Date: 504WP + LWC
wc0504 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="54 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("05-04-2022")) 


#Date: 523-525 WP + LWC
wc523 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="523 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("05-23-2022")) 


wc525 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="525 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("05-25-2022")) 

#Date: 719 WP + LWC

wc719 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="719 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("07-19-2022")) 

#Date: 818 WP + LWC

wc818 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="818 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("08-18-2022")) 



#Fix that data to be long:
  

#______________________________________________________________
############### MIDDAYS #######################################
#______________________________________________________________

  
#This works! A lot of code though...can we do this with a loop? 

#______________________________________________________________
############### 02-28-2022 #######################################
#______________________________________________________________

wc228md <- wc228 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("md")) %>% 
  dplyr::select(!matches("pd")) 

wc228_long_md <- wc228md %>%  #has MPa data and 
  dplyr::select(!matches("md[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa_md"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("02-28-2022")) 

wc228_long_dry <- wc228md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_dry","md_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(md  = case_when(
    measure_dw == "md1_g_dry" ~ "md1"
    ,measure_dw == "md2_g_dry" ~ "md2"
    ,measure_dw == "md3_g_dry" ~ "md3"
    ,measure_dw == "md4_g_dry" ~ "md4"
    ,measure_dw == "md5_g_dry" ~ "md5"
    ,measure_dw == "md6_g_dry" ~ "md6"
    ,measure_dw == "md7_g_dry" ~ "md7"
    ,measure_dw == "md8_g_dry" ~ "md8"
    ,measure_dw == "md9_g_dry" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)

wc228_long_wet <- wc228md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_wet", "md_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(md  = case_when(
    measure_ww == "md1_g_wet" ~ "md1"
    ,measure_ww == "md2_g_wet" ~ "md2"
    ,measure_ww == "md3_g_wet" ~ "md3"
    ,measure_ww == "md4_g_wet" ~ "md4"
    ,measure_ww == "md5_g_wet" ~ "md5"
    ,measure_ww == "md6_g_wet" ~ "md6"
    ,measure_ww == "md7_g_wet" ~ "md7"
    ,measure_ww == "md8_g_wet" ~ "md8"
    ,measure_ww == "md9_g_wet" ~ "md9"
    )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)


wc228_long_dry_premerge <- merge(wc228_long_md, wc228_long_dry, all.x = T) #combine MPa data with dry data

wc_long_228 <- merge(wc228_long_dry_premerge, wc228_long_wet, all.x = T) #combine MPa, dry, and wet data

#______________________________________________________________
############### 03-03-2022 #######################################
#______________________________________________________________

wc303md <- wc303 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("md")) %>% 
  dplyr::select(!matches("pd")) 

wc303_long_md <- wc303md %>% 
  dplyr::select(!matches("md[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa_md"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("03-03-2022")) 

wc303_long_dry <- wc303md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_dry","md_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(md  = case_when(
    measure_dw == "md1_g_dry" ~ "md1"
    ,measure_dw == "md2_g_dry" ~ "md2"
    ,measure_dw == "md3_g_dry" ~ "md3"
    ,measure_dw == "md4_g_dry" ~ "md4"
    ,measure_dw == "md5_g_dry" ~ "md5"
    ,measure_dw == "md6_g_dry" ~ "md6"
    ,measure_dw == "md7_g_dry" ~ "md7"
    ,measure_dw == "md8_g_dry" ~ "md8"
    ,measure_dw == "md9_g_dry" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)

wc303_long_wet <- wc303md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_wet", "md_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(md  = case_when(
    measure_ww == "md1_g_wet" ~ "md1"
    ,measure_ww == "md2_g_wet" ~ "md2"
    ,measure_ww == "md3_g_wet" ~ "md3"
    ,measure_ww == "md4_g_wet" ~ "md4"
    ,measure_ww == "md5_g_wet" ~ "md5"
    ,measure_ww == "md6_g_wet" ~ "md6"
    ,measure_ww == "md7_g_wet" ~ "md7"
    ,measure_ww == "md8_g_wet" ~ "md8"
    ,measure_ww == "md9_g_wet" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)


wc303_long_dry_premerge <- merge(wc303_long_md, wc303_long_dry, all.x = T)

wc_long_303 <- merge(wc303_long_dry_premerge, wc303_long_wet)

#______________________________________________________________
############### 03-15-2022 #######################################
#______________________________________________________________

wc315md <- wc315 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("md")) %>% 
  dplyr::select(!matches("pd")) 

wc315_long_md <- wc315md %>% 
  dplyr::select(!matches("md[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa_md"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("03-15-2022")) 

wc315_long_dry <- wc315md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_dry","md_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(md  = case_when(
    measure_dw == "md1_g_dry" ~ "md1"
    ,measure_dw == "md2_g_dry" ~ "md2"
    ,measure_dw == "md3_g_dry" ~ "md3"
    ,measure_dw == "md4_g_dry" ~ "md4"
    ,measure_dw == "md5_g_dry" ~ "md5"
    ,measure_dw == "md6_g_dry" ~ "md6"
    ,measure_dw == "md7_g_dry" ~ "md7"
    ,measure_dw == "md8_g_dry" ~ "md8"
    ,measure_dw == "md9_g_dry" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)

wc315_long_wet <- wc315md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_wet", "md_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(md  = case_when(
    measure_ww == "md1_g_wet" ~ "md1"
    ,measure_ww == "md2_g_wet" ~ "md2"
    ,measure_ww == "md3_g_wet" ~ "md3"
    ,measure_ww == "md4_g_wet" ~ "md4"
    ,measure_ww == "md5_g_wet" ~ "md5"
    ,measure_ww == "md6_g_wet" ~ "md6"
    ,measure_ww == "md7_g_wet" ~ "md7"
    ,measure_ww == "md8_g_wet" ~ "md8"
    ,measure_ww == "md9_g_wet" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)


wc315_long_dry_premerge <- merge(wc315_long_md, wc315_long_dry, all.x = T)

wc_long_315 <- merge(wc315_long_dry_premerge, wc315_long_wet, all.x = T)

#______________________________________________________________
############### 03-25-2022 #######################################
#______________________________________________________________

wc325md <- wc325 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("md")) %>% 
  dplyr::select(!matches("pd")) 

wc325_long_md <- wc325md %>% 
  dplyr::select(!matches("md[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa_md"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("03-25-2022")) 

wc325_long_dry <- wc325md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_dry","md_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  # mutate(md  = case_when(
  #   measure_dw == "md1_g_dry" ~ "md1"
  #   ,measure_dw == "md2_g_dry" ~ "md2"
  #   ,measure_dw == "md3_g_dry" ~ "md3"
  #   ,measure_dw == "md4_g_dry" ~ "md4"
  #   ,measure_dw == "md5_g_dry" ~ "md5"
  #   ,measure_dw == "md6_g_dry" ~ "md6"
  #   ,measure_dw == "md7_g_dry" ~ "md7"
  #   ,measure_dw == "md8_g_dry" ~ "md8"
  #   ,measure_dw == "md9_g_dry" ~ "md9", 
  #  # ,measure_dw == "md_bulk_dry" ~ "md_bulk" 
    # TRUE ~ measure_dw)) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)

wc325_long_wet <- wc325md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_wet", "md_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  # mutate(md  = case_when(
  #   measure_ww == "md1_g_wet" ~ "md1"
  #   ,measure_ww == "md2_g_wet" ~ "md2"
  #   ,measure_ww == "md3_g_wet" ~ "md3"
  #   ,measure_ww == "md4_g_wet" ~ "md4"
  #   ,measure_ww == "md5_g_wet" ~ "md5"
  #   ,measure_ww == "md6_g_wet" ~ "md6"
  #   ,measure_ww == "md7_g_wet" ~ "md7"
  #   ,measure_ww == "md8_g_wet" ~ "md8"
  #   ,measure_ww == "md9_g_wet" ~ "md9"
  # )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)


wc325_long_dry_premerge <- merge(wc325_long_md, wc325_long_dry, all = T)

wc_long_325 <- merge(wc325_long_dry_premerge, wc325_long_wet, all= T)

##No specific LWC

#______________________________________________________________
############### 03-30-2022 #######################################
#______________________________________________________________

wc330md <- wc330 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("md")) %>% 
  dplyr::select(!matches("pd")) 

wc330_long_md <- wc330md %>% 
  dplyr::select(!matches("md[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa_md"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("03-30-2022")) 

wc330_long_dry <- wc330md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_dry","md_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(md  = case_when(
    measure_dw == "md1_g_dry" ~ "md1"
    ,measure_dw == "md2_g_dry" ~ "md2"
    ,measure_dw == "md3_g_dry" ~ "md3"
    ,measure_dw == "md4_g_dry" ~ "md4"
    ,measure_dw == "md5_g_dry" ~ "md5"
    ,measure_dw == "md6_g_dry" ~ "md6"
    ,measure_dw == "md7_g_dry" ~ "md7"
    ,measure_dw == "md8_g_dry" ~ "md8"
    ,measure_dw == "md9_g_dry" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)

wc330_long_wet <- wc330md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_wet", "md_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(md  = case_when(
    measure_ww == "md1_g_wet" ~ "md1"
    ,measure_ww == "md2_g_wet" ~ "md2"
    ,measure_ww == "md3_g_wet" ~ "md3"
    ,measure_ww == "md4_g_wet" ~ "md4"
    ,measure_ww == "md5_g_wet" ~ "md5"
    ,measure_ww == "md6_g_wet" ~ "md6"
    ,measure_ww == "md7_g_wet" ~ "md7"
    ,measure_ww == "md8_g_wet" ~ "md8"
    ,measure_ww == "md9_g_wet" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)


wc330_long_dry_premerge <- merge(wc330_long_md, wc330_long_dry, all.x = T)

wc_long_330 <- merge(wc330_long_dry_premerge, wc330_long_wet, all.x = T)

##no MIDDAY 

#______________________________________________________________
############### 04-11-2022 #######################################
#______________________________________________________________

wc411md <- wc411 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("md")) %>% 
  dplyr::select(!matches("pd")) 

wc411_long_md <- wc411md %>% 
  dplyr::select(!matches("md[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa_md"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("04-11-2022")) 

wc411_long_dry <- wc411md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_dry","md_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(md  = case_when(
    measure_dw == "md1_g_dry" ~ "md1"
    ,measure_dw == "md2_g_dry" ~ "md2"
    ,measure_dw == "md3_g_dry" ~ "md3"
    ,measure_dw == "md4_g_dry" ~ "md4"
    ,measure_dw == "md5_g_dry" ~ "md5"
    ,measure_dw == "md6_g_dry" ~ "md6"
    ,measure_dw == "md7_g_dry" ~ "md7"
    ,measure_dw == "md8_g_dry" ~ "md8"
    ,measure_dw == "md9_g_dry" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)

wc411_long_wet <- wc411md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_wet", "md_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(md  = case_when(
    measure_ww == "md1_g_wet" ~ "md1"
    ,measure_ww == "md2_g_wet" ~ "md2"
    ,measure_ww == "md3_g_wet" ~ "md3"
    ,measure_ww == "md4_g_wet" ~ "md4"
    ,measure_ww == "md5_g_wet" ~ "md5"
    ,measure_ww == "md6_g_wet" ~ "md6"
    ,measure_ww == "md7_g_wet" ~ "md7"
    ,measure_ww == "md8_g_wet" ~ "md8"
    ,measure_ww == "md9_g_wet" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)


wc411_long_dry_premerge <- merge(wc411_long_md, wc411_long_dry, all = T)

wc_long_411 <- merge(wc411_long_dry_premerge, wc411_long_wet, all = T)

#Just Piney

#______________________________________________________________
############### 04-13-2022 #######################################
#______________________________________________________________

wc413md <- wc413 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("md")) %>% 
  dplyr::select(!matches("pd")) 

wc413_long_md <- wc413md %>% 
  dplyr::select(!matches("md[1-9]_g_[dry,wet]")) %>% 
  #mutate(as.character("md[1-9]")) %>% 
  pivot_longer(cols=matches("md[1-9]"),
               values_transform = as.character
               , names_to="md"
               , values_to="mpa_md"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("04-13-2022")) 

wc413_long_dry <- wc413md %>% 
  mutate(md5_g_dry = as.numeric(md5_g_dry)) %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_dry","md_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(md  = case_when(
    measure_dw == "md1_g_dry" ~ "md1"
    ,measure_dw == "md2_g_dry" ~ "md2"
    ,measure_dw == "md3_g_dry" ~ "md3"
    ,measure_dw == "md4_g_dry" ~ "md4"
    ,measure_dw == "md5_g_dry" ~ "md5"
    ,measure_dw == "md6_g_dry" ~ "md6"
    ,measure_dw == "md7_g_dry" ~ "md7"
    ,measure_dw == "md8_g_dry" ~ "md8"
    ,measure_dw == "md9_g_dry" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)

wc413_long_wet <- wc413md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_wet", "md_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(md  = case_when(
    measure_ww == "md1_g_wet" ~ "md1"
    ,measure_ww == "md2_g_wet" ~ "md2"
    ,measure_ww == "md3_g_wet" ~ "md3"
    ,measure_ww == "md4_g_wet" ~ "md4"
    ,measure_ww == "md5_g_wet" ~ "md5"
    ,measure_ww == "md6_g_wet" ~ "md6"
    ,measure_ww == "md7_g_wet" ~ "md7"
    ,measure_ww == "md8_g_wet" ~ "md8"
    ,measure_ww == "md9_g_wet" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)


wc413_long_dry_premerge <- merge(wc413_long_md, wc413_long_dry)

wc_long_413 <- merge(wc413_long_dry_premerge, wc413_long_wet)


##Have bulk and individual leaves

#______________________________________________________________
############### 04-25-2022 #######################################
#______________________________________________________________

wc425md <- wc425 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("md")) %>% 
  dplyr::select(!matches("pd")) 

wc425_long_md <- wc425md %>% 
  dplyr::select(!matches("md[1-9]_g_[dry,wet]")) %>% 
  mutate(md1 = as.numeric(md1)) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa_md"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("04-25-2022")) 

wc425_long_dry <- wc425md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_dry","md_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(md  = case_when(
    measure_dw == "md1_g_dry" ~ "md1"
    ,measure_dw == "md2_g_dry" ~ "md2"
    ,measure_dw == "md3_g_dry" ~ "md3"
    ,measure_dw == "md4_g_dry" ~ "md4"
    ,measure_dw == "md5_g_dry" ~ "md5"
    ,measure_dw == "md6_g_dry" ~ "md6"
    ,measure_dw == "md7_g_dry" ~ "md7"
    ,measure_dw == "md8_g_dry" ~ "md8"
    ,measure_dw == "md9_g_dry" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)

wc425_long_wet <- wc425md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_wet", "md_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(md  = case_when(
    measure_ww == "md1_g_wet" ~ "md1"
    ,measure_ww == "md2_g_wet" ~ "md2"
    ,measure_ww == "md3_g_wet" ~ "md3"
    ,measure_ww == "md4_g_wet" ~ "md4"
    ,measure_ww == "md5_g_wet" ~ "md5"
    ,measure_ww == "md6_g_wet" ~ "md6"
    ,measure_ww == "md7_g_wet" ~ "md7"
    ,measure_ww == "md8_g_wet" ~ "md8"
    ,measure_ww == "md9_g_wet" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)


wc425_long_dry_premerge <- merge(wc425_long_md, wc425_long_dry, all.x = T)

wc_long_425 <- merge(wc425_long_dry_premerge, wc425_long_wet, all.x = T)

#only bulk

#______________________________________________________________
############### 05-04-2022 #######################################
#______________________________________________________________

wc504md <- wc0504 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("md")) %>% 
  dplyr::select(!matches("pd")) 

wc504_long_md <- wc504md %>% 
  dplyr::select(!matches("md[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa_md"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("05-04-2022")) 

wc504_long_dry <- wc504md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_dry","md_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(md  = case_when(
    measure_dw == "md1_g_dry" ~ "md1"
    ,measure_dw == "md2_g_dry" ~ "md2"
    ,measure_dw == "md3_g_dry" ~ "md3"
    ,measure_dw == "md4_g_dry" ~ "md4"
    ,measure_dw == "md5_g_dry" ~ "md5"
    ,measure_dw == "md6_g_dry" ~ "md6"
    ,measure_dw == "md7_g_dry" ~ "md7"
    ,measure_dw == "md8_g_dry" ~ "md8"
    ,measure_dw == "md9_g_dry" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)

wc504_long_wet <- wc504md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_wet", "md_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(md  = case_when(
    measure_ww == "md1_g_wet" ~ "md1"
    ,measure_ww == "md2_g_wet" ~ "md2"
    ,measure_ww == "md3_g_wet" ~ "md3"
    ,measure_ww == "md4_g_wet" ~ "md4"
    ,measure_ww == "md5_g_wet" ~ "md5"
    ,measure_ww == "md6_g_wet" ~ "md6"
    ,measure_ww == "md7_g_wet" ~ "md7"
    ,measure_ww == "md8_g_wet" ~ "md8"
    ,measure_ww == "md9_g_wet" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)


wc504_long_dry_premerge <- merge(wc504_long_md, wc504_long_dry, all.x = T)

wc_long_504 <- merge(wc504_long_dry_premerge, wc504_long_wet, all.x = T)

#only bulk

#______________________________________________________________
############### 05-23-2022 #######################################
#______________________________________________________________

wc523md <- wc523 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("md")) %>% 
  dplyr::select(!matches("pd")) 

wc523_long_md <- wc523md %>% 
  dplyr::select(!matches("md[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa_md"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("05-23-2022")) 

wc523_long_dry <- wc523md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_dry","md_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(md  = case_when(
    measure_dw == "md1_g_dry" ~ "md1"
    ,measure_dw == "md2_g_dry" ~ "md2"
    ,measure_dw == "md3_g_dry" ~ "md3"
    ,measure_dw == "md4_g_dry" ~ "md4"
    ,measure_dw == "md5_g_dry" ~ "md5"
    ,measure_dw == "md6_g_dry" ~ "md6"
    ,measure_dw == "md7_g_dry" ~ "md7"
    ,measure_dw == "md8_g_dry" ~ "md8"
    ,measure_dw == "md9_g_dry" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)

wc523_long_wet <- wc523md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_wet", "md_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(md  = case_when(
    measure_ww == "md1_g_wet" ~ "md1"
    ,measure_ww == "md2_g_wet" ~ "md2"
    ,measure_ww == "md3_g_wet" ~ "md3"
    ,measure_ww == "md4_g_wet" ~ "md4"
    ,measure_ww == "md5_g_wet" ~ "md5"
    ,measure_ww == "md6_g_wet" ~ "md6"
    ,measure_ww == "md7_g_wet" ~ "md7"
    ,measure_ww == "md8_g_wet" ~ "md8"
    ,measure_ww == "md9_g_wet" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)


wc523_long_dry_premerge <- merge(wc523_long_md, wc523_long_dry, all.x = T)

wc_long_523 <- merge(wc523_long_dry_premerge, wc523_long_wet, all.x = T) %>% 
  #mutate(md_bulk_dry = mean("md_bulk_dry_y0"), "md_bulk_dry_y1"), 
      #   md_bulk_wet = mean("md_bulk_wet_y0", "md_bulk_wet_y1")) %>% ##y1 is only shrubs
  mutate(md_bulk_dry = md_bulk_dry_y0, 
         md_bulk_wet = md_bulk_wet_y0) %>% 
  select(-md_bulk_dry_y1, -md_bulk_wet_y1, -md_bulk_dry_y0, -md_bulk_wet_y0)

#______________________________________________________________
############### 07-19-2022 #######################################
#______________________________________________________________

wc719md <- wc719 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("md")) %>% 
  dplyr::select(!matches("pd")) 

wc719_long_md <- wc719md %>%  #has MPa data and 
  dplyr::select(!matches("md[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa_md"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("07-19-2022")) 

wc719_long_dry <- wc719md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_dry","md_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(md  = case_when(
    measure_dw == "md1_g_dry" ~ "md1"
    ,measure_dw == "md2_g_dry" ~ "md2"
    ,measure_dw == "md3_g_dry" ~ "md3"
    ,measure_dw == "md4_g_dry" ~ "md4"
    ,measure_dw == "md5_g_dry" ~ "md5"
    ,measure_dw == "md6_g_dry" ~ "md6"
    ,measure_dw == "md7_g_dry" ~ "md7"
    ,measure_dw == "md8_g_dry" ~ "md8"
    ,measure_dw == "md9_g_dry" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)

wc719_long_wet <- wc719md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_wet", "md_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(md  = case_when(
    measure_ww == "md1_g_wet" ~ "md1"
    ,measure_ww == "md2_g_wet" ~ "md2"
    ,measure_ww == "md3_g_wet" ~ "md3"
    ,measure_ww == "md4_g_wet" ~ "md4"
    ,measure_ww == "md5_g_wet" ~ "md5"
    ,measure_ww == "md6_g_wet" ~ "md6"
    ,measure_ww == "md7_g_wet" ~ "md7"
    ,measure_ww == "md8_g_wet" ~ "md8"
    ,measure_ww == "md9_g_wet" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)


wc719_long_dry_premerge <- merge(wc719_long_md, wc719_long_dry, all.x = T) #combine MPa data with dry data

wc_long_719 <- merge(wc719_long_dry_premerge, wc719_long_wet, all.x =T) #combine MPa, dry, and wet data

#______________________________________________________________
############### 08-18-2022 #######################################
#______________________________________________________________

wc818md <- wc818 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("md")) %>% 
  dplyr::select(!matches("pd")) 

wc818_long_md <- wc818md %>%  #has MPa data and 
  dplyr::select(!matches("md[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa_md"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("08-18-2022")) 

wc818_long_dry <- wc818md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_dry","md_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(md  = case_when(
    measure_dw == "md1_g_dry" ~ "md1"
    ,measure_dw == "md2_g_dry" ~ "md2"
    ,measure_dw == "md3_g_dry" ~ "md3"
    ,measure_dw == "md4_g_dry" ~ "md4"
    ,measure_dw == "md5_g_dry" ~ "md5"
    ,measure_dw == "md6_g_dry" ~ "md6"
    ,measure_dw == "md7_g_dry" ~ "md7"
    ,measure_dw == "md8_g_dry" ~ "md8"
    ,measure_dw == "md9_g_dry" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)

wc818_long_wet <- wc818md %>% 
  pivot_longer(cols=matches(c("md[1-9]_g_wet", "md_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(md  = case_when(
    measure_ww == "md1_g_wet" ~ "md1"
    ,measure_ww == "md2_g_wet" ~ "md2"
    ,measure_ww == "md3_g_wet" ~ "md3"
    ,measure_ww == "md4_g_wet" ~ "md4"
    ,measure_ww == "md5_g_wet" ~ "md5"
    ,measure_ww == "md6_g_wet" ~ "md6"
    ,measure_ww == "md7_g_wet" ~ "md7"
    ,measure_ww == "md8_g_wet" ~ "md8"
    ,measure_ww == "md9_g_wet" ~ "md9"
  )) %>% 
  dplyr::select(!matches("md[1-9]"), -md_avg)


wc818_long_dry_premerge <- merge(wc818_long_md, wc818_long_dry, all.x = T) #combine MPa data with dry data

wc_long_818 <- merge(wc818_long_dry_premerge, wc818_long_wet, all.x = T) #combine MPa, dry, and wet data

##Not entered yet, as of 8-28-2022
#______________________________________________________________
############### All MDs #######################################
#______________________________________________________________


wc_alldates_md <- rbind(wc_long_523, 
                        wc_long_411, 
                        wc_long_303, 
                        wc_long_325, 
                        wc_long_330, 
                        wc_long_315, 
                        wc_long_425, 
                        wc_long_413, 
                        wc_long_504, 
                        wc_long_228, 
                        wc_long_719, 
                        wc_long_818) %>% 
  mutate(week = week(date)) %>% 
  mutate(tree = as.numeric(tag)) %>% 
 # mutate(md_bulk_wet = as.numeric(md_bulk_wet)) %>% 
  mutate(dw_g_md = dw_g, 
            ww_g_md = ww_g) %>% 
  dplyr::select(-dw_g, -ww_g, -measure_ww, -measure_dw) %>% 
  mutate(rep = str_sub(md, 3, -1), 
         date_md = date) %>% 
  select(-date, -md_avg) %>% 
  mutate(time = "MD")


#______________________________________________________________
############### PREDAWNS #######################################
#______________________________________________________________



#This works! A lot of code though...can we do this with a loop? 

#______________________________________________________________
############### 02-28-2022 #######################################
#______________________________________________________________

wc228pd <- wc228 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("pd")) %>% 
  dplyr::select(!matches("md")) 

wc228_long_pd <- wc228pd %>%  #has MPa data and 
  dplyr::select(!matches("pd[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa_pd"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("02-28-2022")) 

wc228_long_dry <- wc228pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_dry","pd_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(pd  = case_when(
    measure_dw == "pd1_g_dry" ~ "pd1"
    ,measure_dw == "pd2_g_dry" ~ "pd2"
    ,measure_dw == "pd3_g_dry" ~ "pd3"
    ,measure_dw == "pd4_g_dry" ~ "pd4"
    ,measure_dw == "pd5_g_dry" ~ "pd5"
    ,measure_dw == "pd6_g_dry" ~ "pd6"
    ,measure_dw == "pd7_g_dry" ~ "pd7"
    ,measure_dw == "pd8_g_dry" ~ "pd8"
    ,measure_dw == "pd9_g_dry" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)

wc228_long_wet <- wc228pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_wet", "pd_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(pd  = case_when(
    measure_ww == "pd1_g_wet" ~ "pd1"
    ,measure_ww == "pd2_g_wet" ~ "pd2"
    ,measure_ww == "pd3_g_wet" ~ "pd3"
    ,measure_ww == "pd4_g_wet" ~ "pd4"
    ,measure_ww == "pd5_g_wet" ~ "pd5"
    ,measure_ww == "pd6_g_wet" ~ "pd6"
    ,measure_ww == "pd7_g_wet" ~ "pd7"
    ,measure_ww == "pd8_g_wet" ~ "pd8"
    ,measure_ww == "pd9_g_wet" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)


wc228_long_dry_premerge <- merge(wc228_long_pd, wc228_long_dry, all.x = T) #combine MPa data with dry data

wc_long_228 <- merge(wc228_long_dry_premerge, wc228_long_wet, all.x = T) #combine MPa, dry, and wet data

#______________________________________________________________
############### 03-03-2022 #######################################
#______________________________________________________________

wc303pd <- wc303 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("pd")) %>% 
  dplyr::select(!matches("md")) 

wc303_long_pd <- wc303pd %>% 
  dplyr::select(!matches("pd[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa_pd"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("03-03-2022")) 

wc303_long_dry <- wc303pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_dry","pd_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(pd  = case_when(
    measure_dw == "pd1_g_dry" ~ "pd1"
    ,measure_dw == "pd2_g_dry" ~ "pd2"
    ,measure_dw == "pd3_g_dry" ~ "pd3"
    ,measure_dw == "pd4_g_dry" ~ "pd4"
    ,measure_dw == "pd5_g_dry" ~ "pd5"
    ,measure_dw == "pd6_g_dry" ~ "pd6"
    ,measure_dw == "pd7_g_dry" ~ "pd7"
    ,measure_dw == "pd8_g_dry" ~ "pd8"
    ,measure_dw == "pd9_g_dry" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)

wc303_long_wet <- wc303pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_wet", "pd_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(pd  = case_when(
    measure_ww == "pd1_g_wet" ~ "pd1"
    ,measure_ww == "pd2_g_wet" ~ "pd2"
    ,measure_ww == "pd3_g_wet" ~ "pd3"
    ,measure_ww == "pd4_g_wet" ~ "pd4"
    ,measure_ww == "pd5_g_wet" ~ "pd5"
    ,measure_ww == "pd6_g_wet" ~ "pd6"
    ,measure_ww == "pd7_g_wet" ~ "pd7"
    ,measure_ww == "pd8_g_wet" ~ "pd8"
    ,measure_ww == "pd9_g_wet" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)


wc303_long_dry_premerge <- merge(wc303_long_pd, wc303_long_dry, all.x = T)

wc_long_303 <- merge(wc303_long_dry_premerge, wc303_long_wet, all.x = T)

#______________________________________________________________
############### 03-15-2022 #######################################
#______________________________________________________________

wc315pd <- wc315 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("pd")) %>% 
  dplyr::select(!matches("md")) 

wc315_long_pd <- wc315pd %>% 
  dplyr::select(!matches("pd[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa_pd"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("03-15-2022")) 

wc315_long_dry <- wc315pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_dry","pd_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(pd  = case_when(
    measure_dw == "pd1_g_dry" ~ "pd1"
    ,measure_dw == "pd2_g_dry" ~ "pd2"
    ,measure_dw == "pd3_g_dry" ~ "pd3"
    ,measure_dw == "pd4_g_dry" ~ "pd4"
    ,measure_dw == "pd5_g_dry" ~ "pd5"
    ,measure_dw == "pd6_g_dry" ~ "pd6"
    ,measure_dw == "pd7_g_dry" ~ "pd7"
    ,measure_dw == "pd8_g_dry" ~ "pd8"
    ,measure_dw == "pd9_g_dry" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)

wc315_long_wet <- wc315pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_wet", "pd_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(pd  = case_when(
    measure_ww == "pd1_g_wet" ~ "pd1"
    ,measure_ww == "pd2_g_wet" ~ "pd2"
    ,measure_ww == "pd3_g_wet" ~ "pd3"
    ,measure_ww == "pd4_g_wet" ~ "pd4"
    ,measure_ww == "pd5_g_wet" ~ "pd5"
    ,measure_ww == "pd6_g_wet" ~ "pd6"
    ,measure_ww == "pd7_g_wet" ~ "pd7"
    ,measure_ww == "pd8_g_wet" ~ "pd8"
    ,measure_ww == "pd9_g_wet" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)


wc315_long_dry_premerge <- merge(wc315_long_pd, wc315_long_dry, all.x = T)

wc_long_315 <- merge(wc315_long_dry_premerge, wc315_long_wet, all.x = T)

#______________________________________________________________
############### 03-25-2022 #######################################
#______________________________________________________________

wc325pd <- wc325 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("pd")) %>% 
  dplyr::select(!matches("md")) 

wc325_long_pd <- wc325pd %>% 
  dplyr::select(!matches("pd[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa_pd"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("03-25-2022")) 

wc325_long_dry <- wc325pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_dry","pd_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  # mutate(pd  = case_when(
  #   measure_dw == "pd1_g_dry" ~ "pd1"
  #   ,measure_dw == "pd2_g_dry" ~ "pd2"
  #   ,measure_dw == "pd3_g_dry" ~ "pd3"
  #   ,measure_dw == "pd4_g_dry" ~ "pd4"
  #   ,measure_dw == "pd5_g_dry" ~ "pd5"
  #   ,measure_dw == "pd6_g_dry" ~ "pd6"
  #   ,measure_dw == "pd7_g_dry" ~ "pd7"
  #   ,measure_dw == "pd8_g_dry" ~ "pd8"
  #   ,measure_dw == "pd9_g_dry" ~ "pd9", 
  #  # ,measure_dw == "pd_bulk_dry" ~ "pd_bulk" 
# TRUE ~ measure_dw)) %>% 
dplyr::select(!matches("pd[1-9]"), -pd_avg)

wc325_long_wet <- wc325pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_wet", "pd_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  # mutate(pd  = case_when(
  #   measure_ww == "pd1_g_wet" ~ "pd1"
  #   ,measure_ww == "pd2_g_wet" ~ "pd2"
  #   ,measure_ww == "pd3_g_wet" ~ "pd3"
  #   ,measure_ww == "pd4_g_wet" ~ "pd4"
  #   ,measure_ww == "pd5_g_wet" ~ "pd5"
  #   ,measure_ww == "pd6_g_wet" ~ "pd6"
  #   ,measure_ww == "pd7_g_wet" ~ "pd7"
  #   ,measure_ww == "pd8_g_wet" ~ "pd8"
  #   ,measure_ww == "pd9_g_wet" ~ "pd9"
  # )) %>% 
dplyr::select(!matches("pd[1-9]"), -pd_avg)


wc325_long_dry_premerge <- merge(wc325_long_pd, wc325_long_dry, all = T)

wc_long_325 <- merge(wc325_long_dry_premerge, wc325_long_wet, all= T)

##No specific LWC

#______________________________________________________________
############### 03-30-2022 #######################################
#______________________________________________________________

wc330pd <- wc330 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("pd")) %>% 
  dplyr::select(!matches("md")) 

wc330_long_pd <- wc330pd %>% 
  dplyr::select(!matches("pd[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa_pd"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("03-30-2022")) 

wc330_long_dry <- wc330pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_dry","pd_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(pd  = case_when(
    measure_dw == "pd1_g_dry" ~ "pd1"
    ,measure_dw == "pd2_g_dry" ~ "pd2"
    ,measure_dw == "pd3_g_dry" ~ "pd3"
    ,measure_dw == "pd4_g_dry" ~ "pd4"
    ,measure_dw == "pd5_g_dry" ~ "pd5"
    ,measure_dw == "pd6_g_dry" ~ "pd6"
    ,measure_dw == "pd7_g_dry" ~ "pd7"
    ,measure_dw == "pd8_g_dry" ~ "pd8"
    ,measure_dw == "pd9_g_dry" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)

wc330_long_wet <- wc330pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_wet", "pd_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(pd  = case_when(
    measure_ww == "pd1_g_wet" ~ "pd1"
    ,measure_ww == "pd2_g_wet" ~ "pd2"
    ,measure_ww == "pd3_g_wet" ~ "pd3"
    ,measure_ww == "pd4_g_wet" ~ "pd4"
    ,measure_ww == "pd5_g_wet" ~ "pd5"
    ,measure_ww == "pd6_g_wet" ~ "pd6"
    ,measure_ww == "pd7_g_wet" ~ "pd7"
    ,measure_ww == "pd8_g_wet" ~ "pd8"
    ,measure_ww == "pd9_g_wet" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)


wc330_long_dry_premerge <- merge(wc330_long_pd, wc330_long_dry, all.x = T)

wc_long_330 <- merge(wc330_long_dry_premerge, wc330_long_wet, all.x = T)

##no MIDDAY 

#______________________________________________________________
############### 04-11-2022 #######################################
#______________________________________________________________

wc411pd <- wc411 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("pd")) %>% 
  dplyr::select(!matches("md")) 

wc411_long_pd <- wc411pd %>% 
  dplyr::select(!matches("pd[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa_pd"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("04-11-2022")) 

wc411_long_dry <- wc411pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_dry","pd_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(pd  = case_when(
    measure_dw == "pd1_g_dry" ~ "pd1"
    ,measure_dw == "pd2_g_dry" ~ "pd2"
    ,measure_dw == "pd3_g_dry" ~ "pd3"
    ,measure_dw == "pd4_g_dry" ~ "pd4"
    ,measure_dw == "pd5_g_dry" ~ "pd5"
    ,measure_dw == "pd6_g_dry" ~ "pd6"
    ,measure_dw == "pd7_g_dry" ~ "pd7"
    ,measure_dw == "pd8_g_dry" ~ "pd8"
    ,measure_dw == "pd9_g_dry" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)

wc411_long_wet <- wc411pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_wet", "pd_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(pd  = case_when(
    measure_ww == "pd1_g_wet" ~ "pd1"
    ,measure_ww == "pd2_g_wet" ~ "pd2"
    ,measure_ww == "pd3_g_wet" ~ "pd3"
    ,measure_ww == "pd4_g_wet" ~ "pd4"
    ,measure_ww == "pd5_g_wet" ~ "pd5"
    ,measure_ww == "pd6_g_wet" ~ "pd6"
    ,measure_ww == "pd7_g_wet" ~ "pd7"
    ,measure_ww == "pd8_g_wet" ~ "pd8"
    ,measure_ww == "pd9_g_wet" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)


wc411_long_dry_premerge <- merge(wc411_long_pd, wc411_long_dry, all = T)

wc_long_411 <- merge(wc411_long_dry_premerge, wc411_long_wet, all = T)

#Just Piney

#______________________________________________________________
############### 04-13-2022 #######################################
#______________________________________________________________

wc413pd <- wc413 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("pd")) %>% 
  dplyr::select(!matches("md")) 

wc413_long_pd <- wc413pd %>% 
  dplyr::select(!matches("pd[1-9]_g_[dry,wet]")) %>% 
  #mutate(as.character("pd[1-9]")) %>% 
  pivot_longer(cols=matches("pd[1-9]"),
               values_transform = as.character
               , names_to="pd"
               , values_to="mpa_pd"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("04-13-2022")) 

wc413_long_dry <- wc413pd %>% 
  mutate(pd5_g_dry = as.numeric(pd5_g_dry)) %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_dry","pd_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(pd  = case_when(
    measure_dw == "pd1_g_dry" ~ "pd1"
    ,measure_dw == "pd2_g_dry" ~ "pd2"
    ,measure_dw == "pd3_g_dry" ~ "pd3"
    ,measure_dw == "pd4_g_dry" ~ "pd4"
    ,measure_dw == "pd5_g_dry" ~ "pd5"
    ,measure_dw == "pd6_g_dry" ~ "pd6"
    ,measure_dw == "pd7_g_dry" ~ "pd7"
    ,measure_dw == "pd8_g_dry" ~ "pd8"
    ,measure_dw == "pd9_g_dry" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)

wc413_long_wet <- wc413pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_wet", "pd_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(pd  = case_when(
    measure_ww == "pd1_g_wet" ~ "pd1"
    ,measure_ww == "pd2_g_wet" ~ "pd2"
    ,measure_ww == "pd3_g_wet" ~ "pd3"
    ,measure_ww == "pd4_g_wet" ~ "pd4"
    ,measure_ww == "pd5_g_wet" ~ "pd5"
    ,measure_ww == "pd6_g_wet" ~ "pd6"
    ,measure_ww == "pd7_g_wet" ~ "pd7"
    ,measure_ww == "pd8_g_wet" ~ "pd8"
    ,measure_ww == "pd9_g_wet" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)


wc413_long_dry_premerge <- merge(wc413_long_pd, wc413_long_dry)

wc_long_413 <- merge(wc413_long_dry_premerge, wc413_long_wet)


##Have bulk and individual leaves

#______________________________________________________________
############### 04-25-2022 #######################################
#______________________________________________________________

wc425pd <- wc425 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("pd")) %>% 
  dplyr::select(!matches("md")) 

wc425_long_pd <- wc425pd %>% 
  dplyr::select(!matches("pd[1-9]_g_[dry,wet]")) %>% 
  mutate(pd1 = as.numeric(pd1)) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa_pd"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("04-25-2022")) 

wc425_long_dry <- wc425pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_dry","pd_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(pd  = case_when(
    measure_dw == "pd1_g_dry" ~ "pd1"
    ,measure_dw == "pd2_g_dry" ~ "pd2"
    ,measure_dw == "pd3_g_dry" ~ "pd3"
    ,measure_dw == "pd4_g_dry" ~ "pd4"
    ,measure_dw == "pd5_g_dry" ~ "pd5"
    ,measure_dw == "pd6_g_dry" ~ "pd6"
    ,measure_dw == "pd7_g_dry" ~ "pd7"
    ,measure_dw == "pd8_g_dry" ~ "pd8"
    ,measure_dw == "pd9_g_dry" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)

wc425_long_wet <- wc425pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_wet", "pd_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(pd  = case_when(
    measure_ww == "pd1_g_wet" ~ "pd1"
    ,measure_ww == "pd2_g_wet" ~ "pd2"
    ,measure_ww == "pd3_g_wet" ~ "pd3"
    ,measure_ww == "pd4_g_wet" ~ "pd4"
    ,measure_ww == "pd5_g_wet" ~ "pd5"
    ,measure_ww == "pd6_g_wet" ~ "pd6"
    ,measure_ww == "pd7_g_wet" ~ "pd7"
    ,measure_ww == "pd8_g_wet" ~ "pd8"
    ,measure_ww == "pd9_g_wet" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)


wc425_long_dry_premerge <- merge(wc425_long_pd, wc425_long_dry, all.x = T)

wc_long_425 <- merge(wc425_long_dry_premerge, wc425_long_wet, all.x = T)

#only bulk

#______________________________________________________________
############### 05-04-2022 #######################################
#______________________________________________________________

wc504pd <- wc0504 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("pd")) %>% 
  dplyr::select(!matches("md")) 

wc504_long_pd <- wc504pd %>% 
  dplyr::select(!matches("pd[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa_pd"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("05-04-2022")) 

wc504_long_dry <- wc504pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_dry","pd_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(pd  = case_when(
    measure_dw == "pd1_g_dry" ~ "pd1"
    ,measure_dw == "pd2_g_dry" ~ "pd2"
    ,measure_dw == "pd3_g_dry" ~ "pd3"
    ,measure_dw == "pd4_g_dry" ~ "pd4"
    ,measure_dw == "pd5_g_dry" ~ "pd5"
    ,measure_dw == "pd6_g_dry" ~ "pd6"
    ,measure_dw == "pd7_g_dry" ~ "pd7"
    ,measure_dw == "pd8_g_dry" ~ "pd8"
    ,measure_dw == "pd9_g_dry" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)

wc504_long_wet <- wc504pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_wet", "pd_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(pd  = case_when(
    measure_ww == "pd1_g_wet" ~ "pd1"
    ,measure_ww == "pd2_g_wet" ~ "pd2"
    ,measure_ww == "pd3_g_wet" ~ "pd3"
    ,measure_ww == "pd4_g_wet" ~ "pd4"
    ,measure_ww == "pd5_g_wet" ~ "pd5"
    ,measure_ww == "pd6_g_wet" ~ "pd6"
    ,measure_ww == "pd7_g_wet" ~ "pd7"
    ,measure_ww == "pd8_g_wet" ~ "pd8"
    ,measure_ww == "pd9_g_wet" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)


wc504_long_dry_premerge <- merge(wc504_long_pd, wc504_long_dry, all.x = T)

wc_long_504 <- merge(wc504_long_dry_premerge, wc504_long_wet, all.x = T)

#only bulk

#______________________________________________________________
############### 05-23-2022 #######################################
#______________________________________________________________

wc523pd <- wc523 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("pd")) %>% 
  dplyr::select(!matches("md")) 

wc523_long_pd <- wc523pd %>% 
  dplyr::select(!matches("pd[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa_pd"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("05-23-2022")) 

wc523_long_dry <- wc523pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_dry","pd_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(pd  = case_when(
    measure_dw == "pd1_g_dry" ~ "pd1"
    ,measure_dw == "pd2_g_dry" ~ "pd2"
    ,measure_dw == "pd3_g_dry" ~ "pd3"
    ,measure_dw == "pd4_g_dry" ~ "pd4"
    ,measure_dw == "pd5_g_dry" ~ "pd5"
    ,measure_dw == "pd6_g_dry" ~ "pd6"
    ,measure_dw == "pd7_g_dry" ~ "pd7"
    ,measure_dw == "pd8_g_dry" ~ "pd8"
    ,measure_dw == "pd9_g_dry" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)

wc523_long_wet <- wc523pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_wet", "pd_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(pd  = case_when(
    measure_ww == "pd1_g_wet" ~ "pd1"
    ,measure_ww == "pd2_g_wet" ~ "pd2"
    ,measure_ww == "pd3_g_wet" ~ "pd3"
    ,measure_ww == "pd4_g_wet" ~ "pd4"
    ,measure_ww == "pd5_g_wet" ~ "pd5"
    ,measure_ww == "pd6_g_wet" ~ "pd6"
    ,measure_ww == "pd7_g_wet" ~ "pd7"
    ,measure_ww == "pd8_g_wet" ~ "pd8"
    ,measure_ww == "pd9_g_wet" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)


wc523_long_dry_premerge <- merge(wc523_long_pd, wc523_long_dry, all.x = T)

wc_long_523 <- merge(wc523_long_dry_premerge, wc523_long_wet, all.x = T) %>% 
  #mutate(pd_bulk_dry = mean("pd_bulk_dry_y0"), "pd_bulk_dry_y1"), 
  #   pd_bulk_wet = mean("pd_bulk_wet_y0", "pd_bulk_wet_y1")) %>% ##y1 is only shrubs
  mutate(pd_bulk_dry = pd_bulk_dry_y0, 
         pd_bulk_wet = pd_bulk_wet_y0) %>% 
  select(-pd_bulk_dry_y1, -pd_bulk_wet_y1, -pd_bulk_dry_y0, -pd_bulk_wet_y0)

#______________________________________________________________
############### 07-19-2022 #######################################
#______________________________________________________________

wc719pd <- wc719 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("pd")) %>% 
  dplyr::select(!matches("md")) 

wc719_long_pd <- wc719pd %>%  #has MPa data and 
  dplyr::select(!matches("pd[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa_pd"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("07-19-2022")) 

wc719_long_dry <- wc719pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_dry","pd_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(pd  = case_when(
    measure_dw == "pd1_g_dry" ~ "pd1"
    ,measure_dw == "pd2_g_dry" ~ "pd2"
    ,measure_dw == "pd3_g_dry" ~ "pd3"
    ,measure_dw == "pd4_g_dry" ~ "pd4"
    ,measure_dw == "pd5_g_dry" ~ "pd5"
    ,measure_dw == "pd6_g_dry" ~ "pd6"
    ,measure_dw == "pd7_g_dry" ~ "pd7"
    ,measure_dw == "pd8_g_dry" ~ "pd8"
    ,measure_dw == "pd9_g_dry" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)

wc719_long_wet <- wc719pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_wet", "pd_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(pd  = case_when(
    measure_ww == "pd1_g_wet" ~ "pd1"
    ,measure_ww == "pd2_g_wet" ~ "pd2"
    ,measure_ww == "pd3_g_wet" ~ "pd3"
    ,measure_ww == "pd4_g_wet" ~ "pd4"
    ,measure_ww == "pd5_g_wet" ~ "pd5"
    ,measure_ww == "pd6_g_wet" ~ "pd6"
    ,measure_ww == "pd7_g_wet" ~ "pd7"
    ,measure_ww == "pd8_g_wet" ~ "pd8"
    ,measure_ww == "pd9_g_wet" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)


wc719_long_dry_premerge <- merge(wc719_long_pd, wc719_long_dry, all.x = T) #combine MPa data with dry data

wc_long_719 <- merge(wc719_long_dry_premerge, wc719_long_wet, all.x =T) #combine MPa, dry, and wet data

#______________________________________________________________
############### 08-18-2022 #######################################
#______________________________________________________________

wc818pd <- wc818 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(1:5, matches("pd")) %>% 
  dplyr::select(!matches("md")) 

wc818_long_pd <- wc818pd %>%  #has MPa data and 
  dplyr::select(!matches("pd[1-9]_g_[dry,wet]")) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa_pd"
               , values_drop_na=TRUE)%>% 
  mutate(date = mdy("08-18-2022")) 

wc818_long_dry <- wc818pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_dry","pd_bulk_dry"))
               , names_to= c("measure_dw")
               , values_to= c("dw_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_wet"))  %>% 
  mutate(pd  = case_when(
    measure_dw == "pd1_g_dry" ~ "pd1"
    ,measure_dw == "pd2_g_dry" ~ "pd2"
    ,measure_dw == "pd3_g_dry" ~ "pd3"
    ,measure_dw == "pd4_g_dry" ~ "pd4"
    ,measure_dw == "pd5_g_dry" ~ "pd5"
    ,measure_dw == "pd6_g_dry" ~ "pd6"
    ,measure_dw == "pd7_g_dry" ~ "pd7"
    ,measure_dw == "pd8_g_dry" ~ "pd8"
    ,measure_dw == "pd9_g_dry" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)

wc818_long_wet <- wc818pd %>% 
  pivot_longer(cols=matches(c("pd[1-9]_g_wet", "pd_bulk_wet")) 
               , names_to= c("measure_ww")
               , values_to= c("ww_g")
               , values_drop_na=TRUE) %>% 
  dplyr::select(!matches("_dry")) %>% 
  mutate(pd  = case_when(
    measure_ww == "pd1_g_wet" ~ "pd1"
    ,measure_ww == "pd2_g_wet" ~ "pd2"
    ,measure_ww == "pd3_g_wet" ~ "pd3"
    ,measure_ww == "pd4_g_wet" ~ "pd4"
    ,measure_ww == "pd5_g_wet" ~ "pd5"
    ,measure_ww == "pd6_g_wet" ~ "pd6"
    ,measure_ww == "pd7_g_wet" ~ "pd7"
    ,measure_ww == "pd8_g_wet" ~ "pd8"
    ,measure_ww == "pd9_g_wet" ~ "pd9"
  )) %>% 
  dplyr::select(!matches("pd[1-9]"), -pd_avg)


wc818_long_dry_premerge <- merge(wc818_long_pd, wc818_long_dry, all.x = T) #combine MPa data with dry data

wc_long_818 <- merge(wc818_long_dry_premerge, wc818_long_wet, all.x = T) #combine MPa, dry, and wet data

##Not entered yet, as of 8-28-2022
#______________________________________________________________
############### All PDs #######################################
#______________________________________________________________


wc_alldates_pd <- rbind(wc_long_523, 
                        wc_long_411, 
                        wc_long_303, 
                        wc_long_325, 
                        wc_long_330, 
                        wc_long_315, 
                        wc_long_425, 
                        wc_long_413, 
                        wc_long_504, 
                        wc_long_228, 
                        wc_long_719, 
                        wc_long_818) %>% 
  mutate(week = week(date)) %>% 
  mutate(tree = as.numeric(tag)) %>% 
  mutate(dw_g_pd = dw_g, 
         ww_g_pd = ww_g) %>% 
  dplyr::select(-dw_g, -ww_g, -measure_ww, -measure_dw) %>% 
  mutate(rep = str_sub(pd, 3, -1), 
         date_pd = date) %>% 
  select(-date, -pd_avg) %>% 
  mutate(time = "PD")


#______________________________________________________________
############### ALL + write.csv #######################################
#______________________________________________________________

wc_alldates <- merge(wc_alldates_pd, wc_alldates_md, all = T, by = c(#"date",
                                                                     "tag",
                                                                     "plot_number",
                                                                     "tree",
                                                                     "species",
                                                                     "week",
                                                                     "rep", 
                                                                     "site", 
                                                                     "plot"
                                                                    )) %>%
  mutate(md_bulk_wet = as.numeric(md_bulk_wet), 
         lwc_md_bulk = ((md_bulk_wet-md_bulk_dry)/md_bulk_dry)
         , lwc_pd_bulk = ((pd_bulk_wet - pd_bulk_dry)/pd_bulk_dry)
         , lwc_md_leaf = ((ww_g_md - dw_g_md)/dw_g_md)
         , lwc_pd_leaf = ((ww_g_pd - dw_g_pd)/dw_g_pd)) %>%
  select(-tag, -plot_number, -md, -pd, -time) %>% 
  select("site", 
          "plot", 
          "tree", 
          "week", 
         #"time", 
          "date_pd", 
          "date_md", 
          "mpa_pd", 
          "mpa_md",
          "rep", 
          "lwc_pd_bulk", 
          "lwc_pd_leaf",
          "lwc_md_bulk", 
          "lwc_md_leaf", 
          "pd_bulk_dry", 
          "pd_bulk_wet", 
          "dw_g_pd", 
          "ww_g_pd", 
          "md_bulk_dry", 
          "md_bulk_wet", 
          "dw_g_md", 
          "ww_g_md"
          ) %>% 
  arrange("week", "tree", "rep")

####That is currently very wide, so make longer: 


wc_alldates_longer_mpa <- wc_alldates %>% 
  select("tree", "plot", "week", "site", "rep", "date", "species", 
         #date_md, date_pd, 
        # "time.x", "time.y",
         #"lwc_md_bulk", "lwc_md_leaf",
        # "lwc_pd_bulk", "lwc_pd_leaf",
          "mpa_pd", "mpa_md"
         ) %>% 
  mutate(mpa_md = as.numeric(mpa_md)) %>% 
  pivot_longer(cols = c("mpa_pd","mpa_md"), 
               names_to = "time", 
               values_to = "mpa")  %>% 
  mutate(time = case_when(
    time == "mpa_pd" ~ "PD", 
    time == "mpa_md" ~ "MD"
  ))

wc_alldates_longer_bulk <- wc_alldates %>% 
  select("tree", "plot", "week", "site", "rep",
         # "time.x", "time.y",
         "lwc_md_bulk", 
         #"lwc_md_leaf",
          "lwc_pd_bulk",
         #"lwc_pd_leaf",
        # "mpa_pd", "mpa_md"
  )%>% 
  pivot_longer(cols = c("lwc_pd_bulk","lwc_md_bulk"), 
               names_to = "time_lwc", 
               values_to = "lwc_bulk") %>% 
  mutate(time = case_when(
    time_lwc == "lwc_pd_bulk" ~ "PD", 
    time_lwc == "lwc_md_bulk" ~ "MD"
  )) %>% 
  select(-time_lwc)

wc_alldates_longer_leaf <- wc_alldates %>% 
  select("tree", "plot", "week", "site", "rep",
         # "time.x", "time.y",
         #"lwc_md_bulk", 
         "lwc_md_leaf",
        # "lwc_pd_bulk",
         "lwc_pd_leaf",
         # "mpa_pd", "mpa_md"
  )%>% 
  pivot_longer(cols = c("lwc_pd_leaf","lwc_md_leaf"), 
               names_to = "time_lwc", 
               values_to = "lwc_leaf") %>% 
  mutate(time = case_when(
    time_lwc == "lwc_pd_leaf" ~ "PD", 
    time_lwc == "lwc_md_leaf" ~ "MD"
  )) %>% 
  select(-time_lwc)

wc_alldates_longer_lwc <- merge(wc_alldates_longer_bulk, 
                                wc_alldates_longer_leaf, 
                            )

wc_alldates_longer_lwc_mpa <- merge(wc_alldates_longer_lwc,
                                 wc_alldates_longer_mpa 
                                 )


wc_alldates_longer_dates <- wc_alldates %>% 
  select("tree", "plot", "week", "site", "rep", "species", 
         "date_md", "date_pd") %>% 
  #mutate(mpa_md = as.numeric(mpa_md)) %>% 
  pivot_longer(cols = c("date_pd","date_md"), 
               names_to = "date_time", 
               values_to = "date")  %>% 
  mutate(time = case_when(
    date_time == "date_pd" ~ "PD", 
    date_time == "date_md" ~ "MD"
  )) %>% 
  select(-date_time)

wc_alldates_longer_lwc_mpa_dates <- merge(wc_alldates_longer_lwc_mpa,
                                          wc_alldates_longer_dates) %>% 
  distinct()
                                    
##super annoying name, so rename: 
wc_ad_lwc_mpa <- wc_alldates_longer_lwc_mpa_dates

write.csv(wc_ad_lwc_mpa, here("processed-data", paste0("wc_alldates_",datver,".csv")))



