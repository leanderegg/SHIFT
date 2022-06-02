################################################################
######## INITIAL WATER POTENTIAL ANALYSIS ######################
################################################################

#Notes:
# - currently just removing shrubs
# - currently combining everything by week for when PD and MD weren't measured on same day
# - main goal at present is to get Tree, Date, PD, MD at the ind level for plotting in space and time
# --> still need to work on individual leaf data for matching WC

####### Load Packages ###########
#install.packages("tidyverse")

library(sf)
library(raster)

# some namespace issue with raster and some of these packages.
library(tidyverse)
library(readxl)
library(car)
library(lubridate)
library(ggmap)


# get rid of horrible grid lines but don't have to set Theme each time
### sets default ggplot so that there's no brown grid in the background
ggplot <- function(...) { ggplot2::ggplot(...) + 
    theme(panel.background = element_rect(fill='white', colour='black'), # Make background white and border black
          panel.grid.major = element_blank(),  # Hide major gridlines
          panel.grid.minor = element_blank())  # Hide minor gridlines
}
#+++++++++++++++++++++++++++++++++




#_______________________________________________________________
############# LOAD DATA ########################################

###### Sedgwick DEM

sdem <- raster::raster("Data_05042022/geospatial/DEM_sedgwick_3m.tif")
#sdemll <- st_transform(sdem, crs= 4326)
#sdemll <- raster::projectRaster(sdem)
# make a df for plotting with ggplot
sdem_gg <- sdem %>% 
  rasterToPoints() %>% 
  data.frame()

#### Tree Locations
latlon <- read.csv("Data_05042022/Trees_LatLon_v2.csv")
colnames(latlon)[1:2] <- c("Longitude","Latitude")
# pull out 'B' and 'L' at start of tree names
latlon$Tree <- latlon$Name
latlon$Tree <- str_remove(pattern="B", string=latlon$Tree)
latlon$Tree <- str_remove(pattern="L", string=latlon$Tree)

latlon$Species <- NA
latlon$Species[grep("B",latlon$Name )] <- "B"
latlon$Species[grep("L",latlon$Name )] <- "L"

# make a latlon and utm spatial data version 
latlon.ll <- SpatialPointsDataFrame(coords = data.frame(latlon$Longitude, latlon$Latitude), proj4string = CRS("+proj=longlat +datum=WGS84"), data=latlon)
latlon.utm <- spTransform(latlon.ll, CRSobj = crs(sdem))


## table for linking tree numbers with sites
treesite <- read_excel("Data_05042022/WP_WC/SHIFT data collection 2022.xlsx",sheet="Tree_Sites", skip=0, na = "NA")


####### Water Potentials 
#Date: 228 WP + LWC
wpwc228 <- read_excel("Data_05042022/WP_WC/SHIFT data collection 2022.xlsx",sheet="228 WP + LWC", skip=5, na = "NA")

#Date: 38-311 WP + LWC
wpwc38 <- read_excel("Data_05042022/WP_WC/SHIFT data collection 2022.xlsx",sheet="38-311 WP + LWC", skip=5, na = "NA")
# remove mislabeled tree 2567 which was actually 2367 
wpwc38 <- wpwc38[-which(wpwc38$Tag=="2567"),]

#Date: 315 WP + LWC
wpwc315 <- read_excel("Data_05042022/WP_WC/SHIFT data collection 2022.xlsx",sheet="315 WP + LWC", skip=5, na = "NA")

#Date: 330 WP + LWC (core)
wpwc330 <- read_excel("Data_05042022/WP_WC/SHIFT data collection 2022.xlsx",sheet="330 WP + LWC", skip=5, na = "NA")

#Date: 411-412 WP + LWC (satellite)
wpwc44 <- read_excel("Data_05042022/WP_WC/SHIFT data collection 2022.xlsx",sheet="44 WP + LWC", skip=5, na = "NA")

#Date: 411-412 WP + LWC
wpwc411 <- read_excel("Data_05042022/WP_WC/SHIFT data collection 2022.xlsx",sheet="411-412 WP + LWC", skip=5, na = "NA")






######### . Data cleaning ####################

# take wide form and hella messy data and start making in long

# pull out predawns and make them wide form
####### ++ 2.28.22 #########
wp228pd <- wpwc228 %>%
  filter(!is.na(Tag)) %>%
  select(!matches("_g_")) %>% 
  select(1:3,6,matches("PD")) %>% 
  select(-PD_bulk_wet, -PD_bulk_dry, -PD_avg) %>% 
  pivot_longer(cols=matches("PD[1-9]") 
               , names_to="PD"
               , values_to="MPa"
               , values_drop_na=TRUE)
# add in sampling date
# assign 2/28 date to predawns appropriately
wp228pd$Date <- as_date("2022-02-28")


wp228md <- wpwc228 %>%
  filter(!is.na(Tag)) %>%
  select(!matches("_g_")) %>% 
  select(1:3,6,matches("MD")) %>% 
  select(-MD_bulk_wet, -MD_bulk_dry, -MD_avg) %>% 
  pivot_longer(cols=matches("MD[1-9]") 
               , names_to="MD"
               , values_to="MPa"
               , values_drop_na=TRUE)
# add in sampling date
# assign 3/8 date to middays appropriately
wp228md$Date <- as_date("2022-02-28")



#average to tree
wp228pdind <- wp228pd %>% group_by(Tag) %>% summarise(Date.PD=unique(Date),PD_MPa = mean(MPa))
wp228pdind$WOY <- week(wp228pdind$Date.PD)
wp228mdind <- wp228md %>% group_by(Tag) %>% summarise(Date.MD=unique(Date),MD_MPa = mean(MPa))
wp228mdind$Date.MD[which(is.na(wp228mdind$Date.MD))] <- as_date("2022-03-08") # missing date for one value
wp228mdind$WOY <- week(wp228mdind$Date.MD)


#add in lat lon
wp228all <- full_join(wp228pdind, wp228mdind, by=c("Tag", "WOY"))


wp228 <- left_join(wp228all, latlon, by= c("Tag"="Tree"))
#wp228$Edrop <- wp228$MD_MPa - wp228$PD_MPa




####### ++ 3.08.22 & 3.11.22 #########
wp38pd <- wpwc38 %>%
  filter(!is.na(Tag)) %>%
  select(!matches("_g_")) %>% 
  select(1:3,6,matches("PD")) %>% 
  select(-PD_bulk_wet, -PD_bulk_dry, -PD_avg) %>% 
  pivot_longer(cols=matches("PD[1-9]") 
               , names_to="PD"
               , values_to="MPa"
               , values_drop_na=TRUE)
# add in sampling date
# assign 3/11 date to predawns appropriately
wp38pd$Date[which(wp38pd$Date=="3/8 (MD); 3/11 (PD)")] <- "3/11/2022"
wp38pd$Date <- as_date(wp38pd$Date, format="%m/%d/%Y")


wp38md <- wpwc38 %>%
  filter(!is.na(Tag)) %>%
  select(!matches("_g_")) %>% 
  select(1:3,6,matches("MD")) %>% 
  select(-MD_bulk_wet, -MD_bulk_dry, -MD_avg) %>% 
  pivot_longer(cols=matches("MD[1-9]") 
               , names_to="MD"
               , values_to="MPa"
               , values_drop_na=TRUE)
# add in sampling date
# assign 3/8 date to middays appropriately
wp38md$Date[which(wp38md$Date=="3/8 (MD); 3/11 (PD)")] <- "3/8/2022"
wp38md$Date <- as_date(wp38md$Date, format="%m/%d/%Y")


#average to tree
wp38pdind <- wp38pd %>% group_by(Tag) %>% summarise(Date.PD=unique(Date),PD_MPa = mean(MPa))
wp38pdind$WOY <- week(wp38pdind$Date.PD)
wp38mdind <- wp38md %>% group_by(Tag) %>% summarise(Date.MD=unique(Date),MD_MPa = mean(MPa))
wp38mdind$Date.MD[which(is.na(wp38mdind$Date.MD))] <- as_date("2022-03-08") # missing date for one value
wp38mdind$WOY <- week(wp38mdind$Date.MD)


#add in lat lon
wp38all <- full_join(wp38pdind, wp38mdind, by=c("Tag", "WOY"))


wp38 <- left_join(wp38all, latlon, by= c("Tag"="Tree"))
#wp38$Edrop <- wp38$MD_MPa - wp38$PD_MPa


####### ++ 3.15.22 #########
wp315pd <- wpwc315 %>%
  filter(!is.na(Tag)) %>%
  select(!matches("_g_")) %>% 
  select(1:3,6,matches("PD")) %>% 
  select(-PD_bulk_wet, -PD_bulk_dry, -PD_avg) %>% 
  pivot_longer(cols=matches("PD[1-9]") 
               , names_to="PD"
               , values_to="MPa"
               , values_drop_na=TRUE)
# add in sampling date
# assign 3/11 date to predawns appropriately
wp315pd$Date <- as_date("2022-03-15")


wp315md <- wpwc315 %>%
  filter(!is.na(Tag)) %>%
  select(!matches("_g_")) %>% 
  select(1:3,6,matches("MD")) %>% 
  select(-MD_bulk_wet, -MD_bulk_dry, -MD_avg) %>% 
  pivot_longer(cols=matches("MD[1-9]") 
               , names_to="MD"
               , values_to="MPa"
               , values_drop_na=TRUE)
# add in sampling date
# assign 3/15 date to middays appropriately
wp315md$Date <- as_date("2022-03-15")


#average to tree
wp315pdind <- wp315pd %>% group_by(Tag) %>% summarise(Date.PD=unique(Date),PD_MPa = mean(MPa))
wp315pdind$WOY <- week(wp315pdind$Date.PD)
wp315mdind <- wp315md %>% group_by(Tag) %>% summarise(Date.MD=unique(Date),MD_MPa = mean(MPa))
wp315mdind$Date.MD[which(is.na(wp315mdind$Date.MD))] <- as_date("2022-03-08") # missing date for one value
wp315mdind$WOY <- week(wp315mdind$Date.MD)


#add in lat lon
wp315all <- full_join(wp315pdind, wp315mdind, by=c("Tag", "WOY"))


wp315 <- left_join(wp315all, latlon, by= c("Tag"="Tree"))
#wp315$Edrop <- wp315$MD_MPa - wp315$PD_MPa




####### ++ 3.30.22 #########
# currently just pd from core
wp330pd <- wpwc330 %>%
  filter(!is.na(Tag)) %>%
  select(!matches("_g_")) %>% 
  select(1:3,6,matches("PD")) %>% 
  select(-PD_bulk_wet, -PD_bulk_dry, -PD_avg) %>% 
  pivot_longer(cols=matches("PD[1-9]") 
               , names_to="PD"
               , values_to="MPa"
               , values_drop_na=TRUE)
# add in sampling date
wp330pd$Date <- as_date("2022-03-30")

# currently no MD values
# wp330md <- wpwc330 %>%
#   filter(!is.na(Tag)) %>%
#   select(!matches("_g_")) %>% 
#   select(1:3,6,matches("MP")) %>% 
#   select(-MP_bulk_wet, -MP_bulk_dry, -MP_avg) %>% 
#   pivot_longer(cols=matches("MP[1-9]") 
#                , names_to="MP"
#                , values_to="MPa"
#                , values_drop_na=TRUE)
# # add in sampling date
# wp330md$Date <- as_date("2022-04-11")



#average to tree
wp330pdind <- wp330pd %>% group_by(Tag) %>% summarise(Date.PD=unique(Date),PD_MPa = mean(MPa))
wp330pdind$WOY <- week(wp330pdind$Date.PD)
wp330pdind$Date.MD <- NA
wp330pdind$MD_MPa <- NA


#wp330mdind <- wp330md %>% group_by(Tag) %>% summarise(MD_MPa = mean(MPa))



#add in lat lon
wp330 <- left_join(wp330pdind, latlon, by= c("Tag"="Tree"))
#wp330all <- full_join(wp330pdind, wp330mdind)
#wp330 <- full_join(wp330all, latlon, by= c("Tag"="Tree"))
#wp330$Edrop <- wp330$MD_MPa - wp330$PD_MPa

# apply(wp330, FUN=function(x){ length(which(is.na(x)))}, MARGIN =2)
  #looks good





####### ++ 4.04.22 #########
wp44pd <- wpwc44 %>%
  filter(!is.na(Tag)) %>%
  select(!matches("_g_")) %>% 
  select(1:3,6,matches("PD")) %>% 
  select(-PD_bulk_wet, -PD_bulk_dry, -PD_avg) %>% 
  pivot_longer(cols=matches("PD[1-9]") 
               , names_to="PD"
               , values_to="MPa"
               , values_drop_na=TRUE)
# add in sampling date
wp44pd$Date <- as_date("2022-04-04")


wp44md <- wpwc44 %>%
  filter(!is.na(Tag)) %>%
  select(!matches("_g_")) %>% 
  select(1:3,6,matches("MD")) %>% 
  select(-MD_bulk_wet, -MD_bulk_dry, -MD_avg) %>% 
  pivot_longer(cols=matches("MD[1-9]") 
               , names_to="MD"
               , values_to="MPa"
               , values_drop_na=TRUE)
# add in sampling date
wp44md$Date <- as_date("2022-04-04")


#average to tree
wp44pdind <- wp44pd %>% group_by(Tag) %>% summarise(Date.PD=unique(Date),PD_MPa = mean(MPa))
wp44pdind$WOY <- week(wp44pdind$Date.PD)
wp44mdind <- wp44md %>% group_by(Tag) %>% summarise(Date.MD=unique(Date),MD_MPa = mean(MPa))
wp44mdind$WOY <- week(wp44mdind$Date.MD)


#add in lat lon
wp44all <- full_join(wp44pdind, wp44mdind, by=c("Tag", "WOY"))


wp44 <- left_join(wp44all, latlon, by= c("Tag"="Tree"))
#wp44$Edrop <- wp44$MD_MPa - wp44$PD_MPa

# apply(wp44, FUN=function(x){ length(which(is.na(x)))}, MARGIN =2)
  # 2013 and 2085 are missing lat/lons



####### ++ 4.11.22 #########
wp411pd <- wpwc411 %>%
  filter(!is.na(Tag)) %>%
  select(!matches("_g_")) %>% 
  select(1:3,6,matches("PD")) %>% 
  select(-PD_bulk_wet, -PD_bulk_dry, -PD_avg) %>% 
  pivot_longer(cols=matches("PD[1-9]") 
               , names_to="PD"
               , values_to="MPa"
               , values_drop_na=TRUE)
# add in sampling date
wp411pd$Date <- as_date("2022-04-11")

# currently no MD values
# wp411md <- wpwc411 %>%
#   filter(!is.na(Tag)) %>%
#   select(!matches("_g_")) %>% 
#   select(1:3,6,matches("MP")) %>% 
#   select(-MP_bulk_wet, -MP_bulk_dry, -MP_avg) %>% 
#   pivot_longer(cols=matches("MP[1-9]") 
#                , names_to="MP"
#                , values_to="MPa"
#                , values_drop_na=TRUE)
# # add in sampling date
# wp411md$Date <- as_date("2022-04-11")



#average to tree
wp411pdind <- wp411pd %>% group_by(Tag) %>% summarise(Date.PD=unique(Date),PD_MPa = mean(MPa))
wp411pdind$WOY <- week(wp411pdind$Date.PD)
wp411pdind$Date.MD <- NA
wp411pdind$MD_MPa <- NA


#wp411mdind <- wp411md %>% group_by(Tag) %>% summarise(MD_MPa = mean(MPa))



#add in lat lon
wp411 <- left_join(wp411pdind, latlon, by= c("Tag"="Tree"))
#wp411all <- full_join(wp411pdind, wp411mdind)
#wp411 <- full_join(wp411all, latlon, by= c("Tag"="Tree"))
#wp411$Edrop <- wp411$MD_MPa - wp411$PD_MPa






################# Combine Ind average WPs together ###########

wp.ind <- rbind(wp228,wp38, wp315, wp330, wp44, wp411)
wp.ind <- wp.ind[!is.na(wp.ind$Latitude),] 
wp.ind$Site <- treesite$Site[match(wp.ind$Tag, treesite$Tag)]
wp.ind$Plot <- treesite$Plot[match(wp.ind$Tag, treesite$Tag)]


latlonproj <- CRS("+proj=longlat +datum=WGS84")
wp.ind.ll <- SpatialPoints(coords = data.frame(wp.ind$Longitude, wp.ind$Latitude), proj4string = latlonproj)
# aeaproj <- projection(sdem)
# aeaproj <- CRS("+proj=aea +lat_1=34.0 +lat_2=40.5 +lat_0=0.0 +lon_0=-120 +x_0=0 +y_0=-4000000")
wp.ind.aea <- sp::spTransform(wp.ind.ll,CRSobj =  crs(sdem))
wp.indsp <- SpatialPointsDataFrame(wp.ind.aea, wp.ind)

########### DATA VIZ ##############


# quick look at how the sites compare.
ggplot(wp.ind[which(wp.ind$PD_MPa>0 & wp.ind$Species=="B"  & wp.ind$WOY %in% c(11,14)),], aes(x=Longitude, y=Latitude, col=PD_MPa, size=PD_MPa)) +
  geom_point() + 
  geom_point(data=wp.ind[which(wp.ind$PD_MPa>0 & wp.ind$Species=="L" & wp.ind$WOY %in% c(11,14)),],shape=15) +
  facet_wrap(facets = ~WOY)


### quick LL maps with terrain as background
ggmap(LLMap) + geom_point(data=wp.ind[wp.ind$PD_MPa>0 & wp.ind$Species=="B" & wp.ind$Latitude<34.692 & !is.na(wp.ind$WOY),], aes(x=Longitude, y=Latitude, col=PD_MPa, size=PD_MPa))+
  geom_point(data=wp.ind[which(wp.ind$PD_MPa>0 & wp.ind$Species=="L"),], aes(x=Longitude, y=Latitude, col=PD_MPa, size=PD_MPa)) +
  facet_wrap(facets = ~WOY)

ggmap(LLMap) + geom_point(data=wp.ind[wp.ind$MD_MPa>0 & wp.ind$Species=="B" & wp.ind$Latitude<34.692 & !is.na(wp.ind$WOY),], aes(x=Longitude, y=Latitude, col=MD_MPa, size=MD_MPa))+
  geom_point(data=wp.ind[which(wp.ind$MD_MPa>0 & wp.ind$Species=="L"),], aes(x=Longitude, y=Latitude, col=MD_MPa, size=MD_MPa)) +
  facet_wrap(facets = ~WOY)


##### look at site differences
ggplot(wp.ind, aes(x=Site, y=PD_MPa, fill=Site)) + geom_boxplot() + facet_wrap(facets = ~WOY)

ggplot(wp.ind, aes(x=Site, y=MD_MPa, fill=Site)) + geom_boxplot() + facet_wrap(facets = ~WOY)


### look at trees through time
ggplot(wp.ind, aes(x=WOY, y=PD_MPa, col=Species, Shape=Tag)) + geom_line() + facet_wrap(facets=~Site)


### Only LL trees through time
ggplot(wp.ind[wp.ind$Site=="LL",], aes(x=WOY, y=PD_MPa, col=Plot, Shape=Tag)) + geom_line(aes(linetype=Species))




#@########3 Failed mapping
# c(left = -97.1268, bottom = 31.536245, right = -97.099334, top = 31.559652))

summary(wp.ind.ll)
myMap <- get_map(location=c(left=-120.051, bottom=34.6853,right= - 120.0401, top=34.7187), source="google", maptype="terrain", zoom=12)

LLMap <-get_map(location=c(left=-120.0487, bottom=34.689,right= - 120.0455, top=34.6925), source="google", maptype="terrain", zoom=14)


plot(sdem,ylim=bbox(wp.ind.aea)[2,1:2], xlim=bbox(wp.ind.aea)[1,1:2]) 
points(wp.ind.aea)
#ylim=c(34.689,34.6925), xlim=c(-120.049, -120.045)

#ggplot() + geom_raster(data=sdem, aes(x=x, y=y, fill=DEM_sedgwick_3m))

