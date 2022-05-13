################################################################
######## INITIAL WATER POTENTIAL ANALYSIS ######################
################################################################


####### Load Packages ###########
#install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(car)
library(lubridate)


#+++++++++++++++++++++++++++++++++




#_______________________________________________________________
############# LOAD DATA ########################################

#### Tree Locations
latlon <- read.csv("Data_05042022/Trees_LatLon.csv")
# pull out 'B' and 'L' at start of tree names
latlon$Tree <- latlon$Name
latlon$Tree <- str_remove(pattern="B", string=latlon$Tree)
latlon$Tree <- str_remove(pattern="L", string=latlon$Tree)

latlon$Species <- NA
latlon$Species[grep("B",latlon$Name )] <- "B"
latlon$Species[grep("L",latlon$Name )] <- "L"
##### Water Potentials

#Date: 411-412 WP + LWC
wpwc4.11 <- read_excel("Data_05042022/WP_WC/SHIFT data collection 2022.xlsx",sheet="411-412 WP + LWC", skip=5, na = "NA")
# PD3 has problems
wpwc4.11$PD3


######### . Data cleaning ####################

# take wide form and hella messy data and start making in long

# pull out predawns and make them wide form
wp4.11pd <- wpwc4.11 %>% select(!matches("_g_")) %>% 
  select(1:3,6,matches("PD")) %>% 
  select(-PD_bulk_wet, -PD_bulk_dry, -PD_avg) %>% 
  pivot_longer(cols=matches("PD[1-9]") 
               , names_to="PD"
               , values_to="MPa"
               , values_drop_na=TRUE)
# add in sampling date
wp4.11pd$Date <- as_date("2022-04-11")

#average to tree
wp411pdind <- wp4.11pd %>% group_by(Tag) %>% summarise(PD_MPa = mean(MPa))

#add in lat lon
wp411 <- full_join(wp411pdind, latlon, by= c("Tag"="Tree"))







########### DATA VIZ ##############

ggplot(wp411[wp411$PD_MPa>0 & wp411$Species=="B",], aes(x=Longitude, y=Latitude, col=PD_MPa, size=PD_MPa)) +
  geom_point() + 
  geom_point(data=wp411[which(wp411$PD_MPa>0 & wp411$Species=="L"),],shape=15)
