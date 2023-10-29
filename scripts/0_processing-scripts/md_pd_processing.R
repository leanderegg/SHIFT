# Script for reading raw water potential/water content data, pulling out wp and exporting it

library(janitor)
library(here)
library(tidyverse)
library(lubridate)
library(readxl)
library(gridExtra)
library(MetBrewer)

#Field data: 

#Read in water potential data: ####

####### Water Potentials 
#Date: 218 WP + LWC
wpwc218 <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="218-221 WP + LWC", skip=5, na = "NA") %>% clean_names() %>% 
  mutate(date = mdy("02-18-2022")) 


#Date: 228 WP + LWC
wpwc228 <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="228 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("02-28-2022")) 

#Date: 33-34 WP + LWC
wpwc303 <-  read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="33-34 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("03-03-2022")) 


#Date: 38-311 WP + LWC
wpwc308 <-  read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="38-311 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("03-08-2022")) 


#Date: 315 WP + LWC
wpwc315 <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="315 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("03-15-2022")) 

#Date: 323 WP + LWC
wpwc323 <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="325-327 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("03-23-2022")) 

#Date: 325 WP + LWC
wpwc325 <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="325-327 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("03-25-2022")) 


#Date: 330 WP + LWC (core)
wpwc330 <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="330 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("03-30-2022")) 


#Date: 44WP + LWC (satellite)
wpwc44 <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="44 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("04-04-2022")) 


#Date: 46 WP + LWC (satellite)
wpwc46 <- read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="46 WP + LWC", skip=5, na = "NA") %>% clean_names() %>% 
  mutate(date = mdy("04-06-2022")) 


#Date: 411-412 WP + LWC
wpwc411 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="411-412 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("04-11-2022")) 


#Date: 413-414 WP + LWC
wpwc413 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="413-414 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("04-13-2022")) 
# remove 'n/a' and a bad value with a '?' from md3 and md6 and turn everything numeric in md3
wpwc413$md3<- as.numeric(wpwc413$md3)
wpwc413$md6[grep(' ', wpwc413$md6)] <- NA
wpwc413$md6<- as.numeric(wpwc413$md6)

#Date: 425 WP + LWC
wpwc425 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="425 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("04-25-2022")) 

#Date: 427 WP + LWC
wpwc427 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="427 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("04-27-2022")) 

#Date: 504WP + LWC
wpwc504 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="54 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("05-04-2022")) 

#Date: 509WP + LWC
wpwc509 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="59 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("05-09-2022")) 


#Date: 523-525 WP + LWC
wpwc523 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="523 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("05-23-2022")) 


wpwc525 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="525 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("05-25-2022")) 


#Date: 719 WP + LWC
wpwc719 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="719 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("07-19-2022")) 



wpwc719$Tag <- str_remove(wpwc719$tag,"\\.0")


#Date: 818 WP + LWC
wpwc818 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="818 WP + LWC", skip=5, na = "NA") %>% clean_names()  %>% 
  mutate(date = mdy("08-18-2022")) 

#Date: 915 WP (Differenr format, will have to add at the bottom)
wpwc912 <-read_excel(here(dataversion,"WP_WC", "SHIFT data collection 2022.xlsx"), sheet="912 -915 WP", skip=0, na = "NA") %>% clean_names()  %>%
  mutate(date = mdy("09-15-2022"))



###Fix that data to be long:
  

######Middays####

####For 2.28
wp228md <- wpwc228 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("md")) %>% 
  dplyr::select(-md_bulk_wet, -md_bulk_dry, -md_avg) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("02-28-2022")) 

####For 3.3

wp303md <- wpwc303 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("md")) %>% 
  dplyr::select(-md_bulk_wet, -md_bulk_dry, -md_avg) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("03-03-2022"))

####For 3.08

wp308md <- wpwc308 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("md")) %>% 
  dplyr::select(-md_bulk_wet, -md_bulk_dry, -md_avg) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("03-08-2022"))

####For 3.15

wp315md <- wpwc315 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("md")) %>% 
  dplyr::select(-md_bulk_wet, -md_bulk_dry, -md_avg) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("03-15-2022"))


####For 3.23

wp323md <- wpwc323 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("md")) %>% 
  dplyr::select(-md_bulk_wet, -md_bulk_dry, -md_avg) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("03-23-2022"))



####For 3.25

wp325md <- wpwc325 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("md")) %>% 
  dplyr::select(-md_bulk_wet, -md_bulk_dry, -md_avg) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("03-25-2022"))

####For 3.30
wp330md <- wpwc330 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("md")) %>% 
  dplyr::select(-md_bulk_wet, -md_bulk_dry, -md_avg) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("03-30-2022")) 


###For 4.04

wp44md <- wpwc44 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("md")) %>% 
  dplyr::select(-md_bulk_wet, -md_bulk_dry, -md_avg) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("04-04-2022"))



####For 4.6

wp46md <- wpwc46 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("md")) %>% 
  dplyr::select(-md_bulk_wet, -md_bulk_dry, -md_avg) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("04-06-2022")) 


####For 4.11
wp411md <- wpwc411 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("md")) %>% 
  dplyr::select(-md_bulk_wet, -md_bulk_dry, -md_avg) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("04-11-2022"))


####For 4.13
wp413md <- wpwc413 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("md")) %>% 
  dplyr::select(-md_bulk_wet, -md_bulk_dry, -md_avg) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("04-13-2022")) #switching date here too

####For 4.25
###Note: 2367 MD are from tree 2365, as we are missing it from 2367
wp425md <- wpwc425 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("md")) %>% 
  dplyr::select(-md_bulk_wet, -md_bulk_dry, -md_avg) %>% 
  mutate(md1 = as.numeric(md1)) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("04-25-2022")) 


####For 4.27
wp427md <- wpwc427 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("md")) %>% 
  dplyr::select(-md_bulk_wet, -md_bulk_dry, -md_avg) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("04-27-2022"))


wp504md <- wpwc504 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("md")) %>% 
  dplyr::select(-md_bulk_wet, -md_bulk_dry, -md_avg) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("05-04-2022"))

wp509md <- wpwc509 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("md")) %>% 
  dplyr::select(-md_bulk_wet_y0, -md_bulk_wet_y1, -md_bulk_dry_y0, -md_bulk_dry_y1, -md_avg) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("05-09-2022"))


wp523md <- wpwc523 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("md")) %>% 
  dplyr::select(-md_bulk_wet_y0, -md_bulk_wet_y1, -md_bulk_dry_y0, -md_bulk_dry_y1, -md_avg) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("05-23-2022"))


wp525md <- wpwc525 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("md")) %>% 
  dplyr::select(-md_bulk_wet, -md_bulk_dry, -md_avg) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("05-25-2022"))


wp719md <- wpwc719 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("md")) %>% 
  dplyr::select(-md_bulk_wet, -md_bulk_dry, -md_avg) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("07-19-2022"))


wp818md <- wpwc818 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("md")) %>% 
  dplyr::select(-md_bulk_wet, -md_bulk_dry, -md_avg) %>% 
  pivot_longer(cols=matches("md[1-9]") 
               , names_to="md"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("08-18-2022"))

###
###Combine all together: 
###

wp_alldates_md <- rbind( wp818md,
                         wp719md,
                        wp525md, 
                        wp523md, 
                        wp509md, 
                        wp504md, 
                        wp427md, 
                        wp425md, 
                        wp411md, 
                        wp413md,
                        wp46md,
                        wp44md,
                        wp330md,
                        wp323md, 
                        wp325md, 
                        wp315md,  
                        wp308md,
                        wp303md, 
                        wp228md) %>% 
  mutate(week = week(date)) %>% 
  mutate(tree = str_remove(tag,"\\.0")) #as.numeric(tag)) 


######Predawns:####
 
####For 2.28
wp228pd <- wpwc228 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("pd")) %>% 
  dplyr::select(-pd_bulk_wet, -pd_bulk_dry, -pd_avg) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("02-28-2022")) 


####For 3.3

wp303pd <- wpwc303 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("pd")) %>% 
  dplyr::select(-pd_bulk_wet, -pd_bulk_dry, -pd_avg) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("03-03-2022"))

####For 3.08

wp308pd <- wpwc308 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("pd")) %>% 
  dplyr::select(-pd_bulk_wet, -pd_bulk_dry, -pd_avg) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("03-08-2022"))

####For 3.15

wp315pd <- wpwc315 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("pd")) %>% 
  dplyr::select(-pd_bulk_wet, -pd_bulk_dry, -pd_avg) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("03-15-2022"))

####For 3.23

wp323pd <- wpwc323 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("pd")) %>% 
  dplyr::select(-pd_bulk_wet, -pd_bulk_dry, -pd_avg) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("03-23-2022"))

####For 3.25
wp325pd <- wpwc325 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("pd")) %>% 
  dplyr::select(-pd_bulk_wet, -pd_bulk_dry, -pd_avg) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("03-25-2022"))

####For 3.30
wp330pd <- wpwc330 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("pd")) %>% 
  dplyr::select(-pd_bulk_wet, -pd_bulk_dry, -pd_avg) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("03-30-2022")) 


####For 4.04 

wp44pd <- wpwc44 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("pd")) %>% 
  dplyr::select(-pd_bulk_wet, -pd_bulk_dry, -pd_avg) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("04-04-2022"))


####For 4.06
wp46pd <- wpwc46 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("pd")) %>% 
  dplyr::select(-pd_bulk_wet, -pd_bulk_dry, -pd_avg) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("04-06-2022"))


####For 4.11
wp411pd <- wpwc411 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("pd")) %>% 
  dplyr::select(-pd_bulk_wet, -pd_bulk_dry, -pd_avg) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("04-11-2022"))


####For 4.13
wp413pd <- wpwc413 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("pd")) %>% 
  dplyr::select(-pd_bulk_wet, -pd_bulk_dry, -pd_avg) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("04-13-2022"))

####For 4.25
###Note: 2367 pd are from tree 2365, as we are missing it from 2367
wp425pd <- wpwc425 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("pd")) %>% 
  dplyr::select(-pd_bulk_wet, -pd_bulk_dry, -pd_avg) %>% 
  mutate(pd1 = as.numeric(pd1)) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("04-25-2022")) 

####For 4.27

wp427pd <- wpwc427 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("pd")) %>% 
  dplyr::select(-pd_bulk_wet, -pd_bulk_dry, -pd_avg) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("04-27-2022"))

####For 5.04

wp504pd <- wpwc504 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("pd")) %>% 
  dplyr::select(-pd_bulk_wet, -pd_bulk_dry, -pd_avg) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("05-04-2022"))

wp509pd <- wpwc509 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("pd")) %>% 
  dplyr::select(-pd_bulk_wet, -pd_bulk_dry, -pd_avg) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("05-09-2022"))


wp523pd <- wpwc523 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("pd")) %>% 
  dplyr::select(-pd_bulk_wet_y0, -pd_bulk_wet_y1, -pd_bulk_dry_y0, -pd_bulk_dry_y1, -pd_avg) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("05-23-2022"))

wp525pd <- wpwc525 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("pd")) %>% 
  dplyr::select(-pd_bulk_wet, -pd_bulk_dry, -pd_avg) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("05-25-2022"))



wp719pd <- wpwc719 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("pd")) %>% 
  dplyr::select(-pd_bulk_wet, -pd_bulk_dry, -pd_avg) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("07-19-2022"))

####For 8.18

wp818pd <- wpwc818 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>%
  dplyr::select(!matches("_g_")) %>% 
  dplyr::select(1:5,matches("pd")) %>% 
  dplyr::select(-pd_bulk_wet, -pd_bulk_dry, -pd_avg) %>% 
  pivot_longer(cols=matches("pd[1-9]") 
               , names_to="pd"
               , values_to="mpa"
               , values_drop_na=TRUE) %>% 
  mutate(date = mdy("08-18-2022"))


#####All WPs#####


wp_alldates_pd <- rbind(wp818pd,
                        wp719pd,
                        wp525pd, 
                        wp523pd, 
                        wp509pd, 
                        wp504pd, 
                        wp425pd, 
                        wp427pd, 
                        wp411pd, 
                        wp413pd,
                        wp46pd,
                        wp44pd,
                        wp330pd,
                        wp323pd, 
                        wp325pd, 
                        wp315pd,  
                        wp308pd,
                        wp303pd, 
                        wp228pd) %>% 
  mutate(week = week(date)) %>% 
  mutate(tree = str_remove(tag,"\\.0"))#as.numeric(tag)) 


#### Indra's Summarizing ######



# wp_alldates_md_summary_premerge <- wp_alldates_md %>% 
#   group_by(date, tree, site, plot) %>% 
#   mutate(mean_md = mean(mpa), 
#          mpa_md = mpa) %>% 
# ungroup() %>% 
#   mutate(rep = str_sub(md, 3, -1), 
#          date_md = date) %>% 
#   select(-mpa, -md, -tag, -date)
# 
# wp_alldates_pd_summary_premerge <- wp_alldates_pd %>% 
#   group_by(date, tag) %>% 
#   mutate(mean_pd = mean(mpa), 
#          mpa_pd = mpa) %>% 
#   ungroup() %>% 
#   mutate(rep = str_sub(pd, 3, -1), 
#          date_pd = date) %>% 
#   select(-mpa, -pd, -tag, -date) 
# 
# # merge the predawn and midday. 
# 
# wp_alldates <- merge(wp_alldates_pd_summary_premerge, wp_alldates_md_summary_premerge, 
#                              by = c("site","plot","tree","week", "species", "rep"), 
#                      all = T) #%>% 
# #  select(-tag, -plot_number)
# 
# # I made this append a data version date to the file, since otherwise it overwrites past versions
# write.csv(wp_alldates, here("processed-data", paste0("wp_alldates_",datver,".csv")))
# 


 ##########`



 
####### Lee's summarizing ######

# LDLA: I'd be inclined to just stack them all on top of each other and keep them long, since 'rep' isn't a true shared unit
#- also there seem to be an ungodly number of duplicate values when I averaged to indivdual. so trying fully long

wp_ad_md <- wp_alldates_md
wp_ad_md$time <- "md"
wp_ad_md$rep <- as.numeric(str_remove(wp_ad_md$md, "md"))
wp_ad_md <- wp_ad_md %>% select(-md)

wp_ad_pd <- wp_alldates_pd
wp_ad_pd$time <- "pd"
wp_ad_pd$rep <- as.numeric(str_remove(wp_ad_pd$pd, "pd"))
wp_ad_pd <- wp_ad_pd %>% select(-pd)


wp_ad_no915 <- rbind(wp_ad_md, wp_ad_pd) %>% 
  mutate(tree = as.numeric(tree))

##Deal with the new format of sept dates: 
wpwc915_new <- wpwc912 %>% 
  mutate(tag = as.character(tree_id), 
         tree = as.numeric(tree_id), 
         mpa = as.numeric(m_pa)) %>% 
  select(-m_pa, -rep, - x6, -tree_id)
  
wp_ad <- bind_rows(wp_ad_no915, wpwc915_new) %>% 
  group_by(tree) %>% 
  fill(c(site, plot, plot_number, species), .direction = "downup") %>% 
  mutate(tree = case_when(
  tag %in% c(#"ARCA_ch",
                #"ARCA_CH",
    "Chamise ARCA",
              "Chamise-ARCA") ~ 10, 
  tag %in% c(#"ARCA",
              #"ARCA near 2380", 
    "LL ARCA" ,
    "ARCA - LL",
    "ARCA - CUCU" ,
              "LL-ARCA") ~ 11, 
  tag %in% c("ARCA-CUCU", 
              "ARCA - CUCU",
              "CUCU ARCA" ,
              "Cucu-ARCA") ~ 12, 
  tag %in% c(#"LEU_cucu",
    "CUCU LEU",
    "LEU- CUCU",
              "LEU-CUCU", 
              "Cucu-LEU") ~ 20, 
  tag %in% c("LL-LEU",
              "LEU - LL") ~ 21, 
 # tree %in% c("ATLE") ~ 30, 
 # tree %in% c("BAPI") ~ 40, 
 # tree %in% c("ARCA") ~ 40, 
  TRUE ~ as.numeric(tree))) %>% 
  mutate(week = week(date), 
         time = tolower(time)) %>% 
  select(-rep)

write.csv(wp_ad, here("processed-data", paste0("wp_alldates_long_",datver,".csv")))

dlookr::diagnose(wp_ad)

unique(wp_ad$tag)
unique(wp_ad$tree)

####### Phenology ######

pheno_218 <- wpwc218 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>% 
  select(tag, date, new_leaves_0_10) %>% 
  clean_names() %>% 
  mutate(new_pheno = new_leaves_0_10)

pheno_228 <- wpwc228 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>% 
  select(tag, date, new_leaves_0_10) %>% 
  clean_names() %>% 
  mutate(new_pheno = new_leaves_0_10)

pheno_315 <- wpwc315 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>% 
  select(tag, date, new_leaves_0_10) %>% 
  clean_names() %>% 
  mutate(new_pheno = new_leaves_0_10)

pheno_325 <- wpwc325 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>% 
  select(tag, date, new_leaves_0_10) %>% 
  clean_names() %>% 
  mutate(new_pheno = new_leaves_0_10)

pheno_46 <- wpwc46 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>% 
  select(tag, date, new_leaves_0_10) %>% 
  clean_names() %>% 
  mutate(new_pheno = new_leaves_0_10)

pheno_413 <- wpwc413 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>% 
  select(tag, date, new_leaves_0_10) %>% 
  clean_names() %>% 
  mutate(new_pheno = new_leaves_0_10)

pheno_425 <- wpwc425 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>% 
  select(tag, date, new_leaves_0_10) %>% 
  clean_names() %>% 
  mutate(new_pheno = new_leaves_0_10)

pheno_504 <- wpwc504 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>% 
  select(tag, date, new_leaves_0_10) %>% 
  clean_names() %>% 
  mutate(new_pheno = new_leaves_0_10)

pheno_525 <- wpwc525 %>%
  as.data.frame() %>% 
  filter(!is.na(tag)) %>% 
  select(tag, date, new_leaves_0_10) %>% 
  clean_names() %>% 
  mutate(new_pheno = new_leaves_0_10)

pheno_alldates <- rbind(pheno_218, pheno_228, pheno_315, pheno_325, pheno_413, pheno_46, pheno_425, pheno_504, pheno_525) %>% 
  mutate(tree = as.numeric(tag)) 

write.csv(pheno_alldates, here("processed-data", paste0("phenology",datver,".csv")))

