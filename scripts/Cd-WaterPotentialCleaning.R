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
library(here)
library(sf)
library(raster)
library(paletteer) 
library(RColorBrewer)

# some namespace issue with raster and some of these packages.
#library(dplyr)
library(tidyverse)
library(readxl)
library(car)
library(lubridate)
library(ggmap)
library(janitor)


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

datver <- "07192022"
dataversion <- paste0("Data_", datver)


### Processed water potentials (individual measurements)

# Indra's version
wp_alldates <- read.csv(here("processed-data", paste0("wp_alldates_",datver,".csv")))
# lee's version
wp_ad <- read.csv(here("processed-data", paste0("wp_alldates_long_",datver,".csv")))[,-1]
wp_ad$tag <- str_replace(wp_ad$tag, "\\.0","") # get rid of annoying .0 at end of numeric tags (thanks excel)

## table for linking tree numbers with sites, correct some mislabeling
treesite <- read_excel(here(dataversion,"WP_WC","SHIFT data collection 2022.xlsx"),sheet="Tree_Sites", skip=0, na = "NA", col_types = "text")
treesite$Tag <- str_replace(treesite$Tag, "\\.0","")


wp_ad$plot <- treesite$Plot[match(wp_ad$tag, treesite$Tag)]

### Problems to fix:
# 2084 doesn't exist. appears to be 2004 mislabeled (from week 19)
# all 2086 values are doubled because tree 2085 is mislabeled in the latlon spreadsheet
# 2370-25 are mislabeld weathertop in the original datasheet


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
# make this a seperate species column
latlon$Species <- NA
latlon$Species[grep("B",latlon$Name )] <- "B"
latlon$Species[grep("L",latlon$Name )] <- "L"
# fix shrub names
latlon$Species[grep("Art",latlon$Name)] <- "ARCA"
latlon$Species[grep("LE",latlon$Name)] <- "LEU"
latlon$Tree[which(latlon$Species=="ARCA")] <- c("Chamise-ARCA","Cucu-ARCA","LL-ARCA")
latlon$Tree[which(latlon$Species=="LEU")] <- c("Cucu-LEU","LL-LEU")
latlon$Tree[grep("Dont", latlon$Name)] <- "2373"


# make a latlon and utm spatial data version
latlon.ll <- SpatialPointsDataFrame(coords = data.frame(latlon$Longitude, latlon$Latitude), proj4string = CRS("+proj=longlat +datum=WGS84"), data=latlon)
latlon.utm <- spTransform(latlon.ll, CRSobj = crs(sdem))



P50doug <- 4.43 # average of all of our wild curves from Skelton et al. 2019 NP
P50ag <- 4.32 # from Skelton et al. 2018 Plant Phys

############## END: Load Data ##################3






#_______________________________________________________________
############# Average and combine data ########################################


# # average to individual:
# wp_ind <- wp_alldates %>% group_by(tree, week, species) %>% summarise(date_pd=unique(date_pd),pd_mpa = unique(mean_pd),
#                                                                       date_md=unique(date_md),md_mpa = unique(mean_md))
# # calculate delta Psi
# wp_ind$e_drop <- wp_ind$md_mpa-wp_ind$pd_mpa
# length(which(is.na(wp_ind$e_drop)))
#   # well over half of our obs either don't have a pd or don't have a md
#   # only 191 of 506 obs have both
# # 64 trees with >1 entry (so 11 trees only measured once, probably data entry issues)
# median(xtabs(~tree, wp_ind))
#   # median of 5 weeks with any wp
# median(xtabs(~tree, wp_ind[wp_ind$e_drop>0,]))
#   # median of only 2 weeks with both predawn and midday measurements...
# 
# xtabs(~week, wp_ind)


#### Lee's version which hopefully cuts down on replicate values

wp_ind_long <- wp_ad %>% group_by(tree,site,plot,species,week,date,time) %>% summarise(sd.mpa= sd(mpa),mpa = mean(mpa))

# rename midday and predawn columns so I can combine them
wp_ind_md <- wp_ind_long[wp_ind_long$time=="md",]
wp_ind_md <- wp_ind_md %>% rename(md_mpa = mpa, date_md=date, md_sd.mpa = sd.mpa)

wp_ind_pd <- wp_ind_long[wp_ind_long$time=="pd",]
wp_ind_pd <- wp_ind_pd %>% rename(pd_mpa = mpa, date_pd=date, pd_sd.mpa = sd.mpa)

wp_ind <- full_join(wp_ind_md %>% select(-time), wp_ind_pd %>% select(-time))
wp_ind <- left_join(wp_ind, latlon, by=c("tree"="Tree")) %>% clean_names()


# calculate delta Psi
wp_ind$e_drop <- wp_ind$md_mpa-wp_ind$pd_mpa

# add in lat lon and utm
wp_ind$lat_utm <- latlon.utm@coords[match( wp_ind$tree,latlon.utm$Tree),2]
wp_ind$lon_utm <- latlon.utm@coords[match( wp_ind$tree,latlon.utm$Tree),1]

# make a spatialdata version, dropping trees without coords
wp_ind_utm <- SpatialPointsDataFrame(coords = data.frame(wp_ind$lon_utm[-which(is.na(wp_ind$lon_utm))], wp_ind$lat_utm[-which(is.na(wp_ind$lon_utm))]), proj4string = crs(sdem), data=wp_ind[-which(is.na(wp_ind$lon_utm)),])


### making a spatial dataframe
latlonproj <- CRS("+proj=longlat +datum=WGS84")
wp_ind.ll <- SpatialPoints(coords = data.frame(wp_ind$longitude, wp_ind$latitude), proj4string = latlonproj)
# aeaproj <- projection(sdem)
# aeaproj <- CRS("+proj=aea +lat_1=34.0 +lat_2=40.5 +lat_0=0.0 +lon_0=-120 +x_0=0 +y_0=-4000000")
wp.ind.aea <- sp::spTransform(wp.ind.ll,CRSobj =  crs(sdem))
wp.indsp <- SpatialPointsDataFrame(wp.ind.aea, wp.ind)




##### Data Characterization:
length(which(is.na(wp_ind$e_drop)))
# half of our obs either don't have a pd or don't have a md
# only 191 of 385 obs have both

xtabs(~week, wp_ind)
# but at least I don't have more measurements than we have trees in any given week!
xtabs(~week, wp_ind[wp_ind$pd_mpa>0,])
  # Missing a lot of predawns from
# Week 15 (have 30 of 59)
# Week 18 (have 6 of 28)
# Week 19 (have 40 of 61)
# Week 21 (have 30 of 42)

xtabs(~week, wp_ind[wp_ind$md_mpa>0,])
# Missing a lot of middays from
# Week 11 (have 9 of 29)
# Week 13 (have 0 of 27)
# Week 21 (have 32 of 42)


### Make a master list of trees:
alltrees <- wp_ind %>% group_by(tree, site, plot, species) %>% summarise(n_pds = length(which(pd_mpa >0)), n_mds = length(which(md_mpa>0)))
write.csv(alltrees, here("processed-data", paste0("FullTreeList_fromwps",datver,".csv")))


################ END: Data loading, combining, cleaning ##############3







########### DATA VIZ ##############




################### Plot 'hydroscapes' ################

ggplot(wp_ind, aes(x=-1*pd_mpa, y=-1*md_mpa, col=log(week))) + geom_point() + 
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~species)

p1 <- ggplot(wp_ind, aes(x=-1*pd_mpa, y=-1*md_mpa, col=log(week))) + geom_point() +
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~species)
p1 + geom_line(data=wp_ind, aes(col=as.numeric(site), group=tree)) 

ggplot(wp_ind[which(wp_ind$md_mpa>0),], aes(x=week, y=md_mpa)) + geom_line(aes( col=site, group=tree) )+ facet_wrap(~species) + geom_hline(yintercept = 4.3)

ggplot(wp_ind[which(wp_ind$pd_mpa>0),], aes(x=week, y=pd_mpa)) + geom_line(aes( col=site, group=tree) )+ facet_wrap(~species)+ geom_hline(yintercept = 4.3)




# quick look at how the sites compare.

#predawn
ggplot(wp_ind[which(wp_ind$pd_mpa>0 & wp_ind$species=="blue oak" ),], aes(x=longitude, y=latitude, col=pd_mpa, size=pd_mpa)) +
                geom_point() + 
                geom_point(data=wp_ind[which(wp_ind$pd_mpa>0 & wp_ind$species=="live oak" ),],shape=15) +
                facet_wrap(facets = ~week)
#midday
ggplot(wp_ind[which(wp_ind$md_mpa>0 & wp_ind$species=="blue oak" ),], aes(x=longitude, y=latitude, col=md_mpa, size=md_mpa)) +
  geom_point() + 
  geom_point(data=wp_ind[which(wp_ind$pd_mpa>0 & wp_ind$species=="live oak" ),],shape=15) +
  facet_wrap(facets = ~week)



ggplot(wp_ind[grep("oak", wp_ind$species),], aes(x=species, y=pd_mpa, col=species)) + geom_boxplot() + facet_wrap(~week)


# ### quick LL maps with terrain as background
# ggmap(LLMap) + geom_point(data=wp.ind[wp.ind$PD_MPa>0 & wp.ind$Species=="B" & wp.ind$Latitude<34.692 & !is.na(wp.ind$WOY),], aes(x=Longitude, y=Latitude, col=PD_MPa, size=PD_MPa))+
#   geom_point(data=wp.ind[which(wp.ind$PD_MPa>0 & wp.ind$Species=="L"),], aes(x=Longitude, y=Latitude, col=PD_MPa, size=PD_MPa)) +
#   facet_wrap(facets = ~WOY)
# 
# ggmap(LLMap) + geom_point(data=wp.ind[wp.ind$MD_MPa>0 & wp.ind$Species=="B" & wp.ind$Latitude<34.692 & !is.na(wp.ind$WOY),], aes(x=Longitude, y=Latitude, col=MD_MPa, size=MD_MPa))+
#   geom_point(data=wp.ind[which(wp.ind$MD_MPa>0 & wp.ind$Species=="L"),], aes(x=Longitude, y=Latitude, col=MD_MPa, size=MD_MPa)) +
#   facet_wrap(facets = ~WOY)


##### look at site differences
ggplot(wp_ind, aes(x=site, y=pd_mpa, fill=site)) + geom_boxplot() + facet_wrap(facets = ~week)

ggplot(wp_ind, aes(x=site, y=md_mpa, fill=site)) + geom_boxplot() + facet_wrap(facets = ~week)


### look at trees through time
ggplot(wp_ind, aes(x=week, y=pd_mpa, col=species, shape=tree)) + geom_line() + facet_wrap(facets=~site)


### Only LL trees through time
ggplot(wp_ind[wp_ind$site=="LL",], aes(x=week, y=pd_mpa, col=plot, Shape=tree)) + geom_line(aes(linetype=species))

#only blue oak through time
ggplot(wp_ind[which(wp_ind$site=="LL" & wp_ind$species=="blue oak" & wp_ind$pd_mpa>0),], aes(x=week, y=pd_mpa, col=plot, group=tree)) + geom_line(aes(linetype=plot))


# blue oaks at different sites in July
ggplot(wp_ind[which(wp_ind$week==29 & wp_ind$species=="blue oak"),],aes(x=site, y=pd_mpa)) + geom_boxplot()


######### Failed mapping
# c(left = -97.1268, bottom = 31.536245, right = -97.099334, top = 31.559652))

summary(wp_ind_ll)
myMap <- get_map(location=c(left=-120.051, bottom=34.6853,right= - 120.0401, top=34.7187), source="google", maptype="terrain", zoom=12)

LLMap <-get_map(location=c(left=-120.0487, bottom=34.689,right= - 120.0455, top=34.6925), source="google", maptype="terrain", zoom=14)

### all sedgewick, plotted in UTM
plot(sdem,ylim=bbox(latlon.utm)[2,1:2], xlim=bbox(latlon.utm)[1,1:2]) 
points(latlon.utm)
#ylim=c(34.689,34.6925), xlim=c(-120.049, -120.045)

# just LL
LLbb <- bbox(wp_ind_utm[which(wp_ind_utm$site=="LL"),])
LLBB <- cbind(LLbb[,1]-50, LLbb[,2]+50)
# crop dem to just lower lisque
demLL <- crop(sdem, LLBB)



#plot(demLL, col=viridis::viridis(100)) 



#-------------------------------------------------
#### Predawn LL Map Comparison #######
quartz(width=8, height=5)
par(mfrow=c(1,2))

###### predawn from April
#bluecol <- paste0(brewer.pal(n=3,"Set1")[2],"AA")
#livecol <- paste0(brewer.pal(n=3,"Set1")[1],"88")
bluecol <-paletteer_d("awtools::mpalette")[3]# brewer.pal(n=3,"Set1")[2]
livecol <- brewer.pal(n=3,"Set1")[1] # "#FFC107"
p50col <- "#FFC107" # paletteer_d("awtools::spalette")[6] 

plot(demLL, col= grey(1:150/150)[51:150], legend=F
     , main="April\n"
     , xaxt="n",yaxt="n"
     , ylab="")
mtext(side=2, "Soil Dryness", font=2, line=1.5, cex=1.2)

contour(demLL, add=T)
points(wp_ind_utm[which(wp_ind_utm$species=="blue oak" & wp_ind_utm$week==15),]
       , pch=21
       , cex=wp_ind_utm$pd_mpa[which(wp_ind_utm$species=="blue oak" & wp_ind_utm$week==15)]*.8
       , bg=bluecol)
points(wp_ind_utm[which(wp_ind_utm$species=="live oak" & wp_ind_utm$week==15),]
       , pch=21
       , cex=wp_ind_utm$pd_mpa[which(wp_ind_utm$species=="live oak" & wp_ind_utm$week==15)]*.8
       , bg=livecol)
par(xpd=NA)
arrows(x0=-4100, y0=-369450, y1=-369380, xpd=NA, lwd=2,length=.2 )
text("N", font=2, x=-4100, y=-369360, cex=1.8)
par(xpd=T)

legend(x=-4150, y=-369600, xpd=NA
       , legend=c("-1","-3","-5")
       , pch=1, pt.cex=c(.8,3*.8,5*.8)
       , cex=1.1
       , col="black"
       , bty="n", title="Water\nPotential\n(MPa)")

legend(x=-4150, y=-369470, xpd=NA
       , legend=c("blue","live")
       , pch=16, pt.cex=1.5
       , cex=1.1
       , col=c(bluecol, livecol)
       , bty="n")


# predawn from July
# bluecol <- paste0(brewer.pal(n=3,"Set1")[2],"AA")
# livecol <- paste0(brewer.pal(n=3,"Set1")[1],"88")
plot(demLL, col= grey(1:150/150)[51:150], legend=F
     , main="July"
     , yaxt="n", xaxt="n")
contour(demLL, add=T)
points(wp_ind_utm[which(wp_ind_utm$species=="blue oak" & wp_ind_utm$week==29),]
       , pch=21
       , cex=wp_ind_utm$pd_mpa[which(wp_ind_utm$species=="blue oak" & wp_ind_utm$week==29)]*.8
       , bg=bluecol)
points(wp_ind_utm[which(wp_ind_utm$species=="live oak" & wp_ind_utm$week==29),]
       , pch=21
       , cex=wp_ind_utm$pd_mpa[which(wp_ind_utm$species=="live oak" & wp_ind_utm$week==29)]*.8
       , bg=livecol)
# legend(x=-4150, y=-369600, xpd=NA
#        , legend=c("-1","-3","-5")
#        , pch=1, pt.cex=c(.8,3*.8,5*.8)
#        , cex=1.1
#        , col="black"
#        , bty="n", title="Water\nPotential\n(MPa)")
# 
# legend(x=-4150, y=-369400, xpd=NA
#        , legend=c("blue","live")
#        , pch=16, pt.cex=1.5
#        , cex=1.1
#        , col=c(bluecol, livecol)
#        , bty="n")




##### Midday Map through Time #####
quartz(width=8, height=5)
par(mfrow=c(1,2))
# midday from April
# bluecol <- paste0(brewer.pal(n=3,"Set1")[2],"AA")
# livecol <- paste0(brewer.pal(n=3,"Set1")[1],"88")
plot(demLL, col= grey(1:150/150)[51:150], legend=F
     , main="April"
     , xaxt="n",yaxt="n"
     , ylab="")
mtext(side=2, "Tree Stress", font=2, line=1.5, cex=1.2)
contour(demLL, add=T)
points(wp_ind_utm[which(wp_ind_utm$species=="blue oak" & wp_ind_utm$week==15),]
       , pch=21
       , cex=wp_ind_utm$md_mpa[which(wp_ind_utm$species=="blue oak" & wp_ind_utm$week==15)]*.8
       , bg=bluecol)
points(wp_ind_utm[which(wp_ind_utm$species=="live oak" & wp_ind_utm$week==15),]
       , pch=21
       , cex=wp_ind_utm$md_mpa[which(wp_ind_utm$species=="live oak" & wp_ind_utm$week==15)]*.8
       , bg=livecol)
par(xpd=NA)
arrows(x0=-4100, y0=-369450, y1=-369380, xpd=NA, lwd=2,length=.2 )
text("N", font=2, x=-4100, y=-369360, cex=1.8)
par(xpd=T)
legend(x=-4150, y=-369600, xpd=NA
       , legend=c("-1","-3","-5")
       , pch=1, pt.cex=c(.8,3*.8,5*.8)
       , cex=1.1
       , col="black"
       , bty="n", title="Water\nPotential\n(MPa)")

legend(x=-4150, y=-369470, xpd=NA
       , legend=c("blue","live")
       , pch=16, pt.cex=1.5
       , cex=1.1
       , col=c(bluecol, livecol)
       , bty="n")


# midday from July
# bluecol <- paste0(brewer.pal(n=3,"Set1")[2],"AA")
# livecol <- paste0(brewer.pal(n=3,"Set1")[1],"88")
plot(demLL, col= grey(1:150/150)[51:150], legend=F
     , main="July"
     , xaxt="n",yaxt="n")
contour(demLL, add=T)
points(wp_ind_utm[which(wp_ind_utm$species=="blue oak" & wp_ind_utm$week==29),]
       , pch=21
       , cex=wp_ind_utm$md_mpa[which(wp_ind_utm$species=="blue oak" & wp_ind_utm$week==29)]*.8
       , bg=bluecol)
points(wp_ind_utm[which(wp_ind_utm$species=="live oak" & wp_ind_utm$week==29),]
       , pch=21
       , cex=wp_ind_utm$md_mpa[which(wp_ind_utm$species=="live oak" & wp_ind_utm$week==29)]*.8
       , bg=livecol)

# Highlight trees past P50
points(wp_ind_utm[which(wp_ind_utm$species=="live oak" & wp_ind_utm$week==29 & wp_ind_utm$md_mpa> P50ag),]
       , pch=21, lwd=3
       , cex=wp_ind_utm$md_mpa[which(wp_ind_utm$species=="live oak" & wp_ind_utm$week==29& wp_ind_utm$md_mpa> P50ag)]*.8
       , bg=livecol, col=p50col)#brewer.pal(n=3,"Set1")[1]) #"#FFC107")
points(wp_ind_utm[which(wp_ind_utm$species=="blue oak" & wp_ind_utm$week==29 & wp_ind_utm$md_mpa> P50doug),]
       , pch=21, lwd=3
       , cex=wp_ind_utm$md_mpa[which(wp_ind_utm$species=="blue oak" & wp_ind_utm$week==29& wp_ind_utm$md_mpa> P50doug)]*.8
       , bg=bluecol , col= p50col)#brewer.pal(n=3,"Set1")[1])

# legend(x=-4150, y=-369600, xpd=NA
#        , legend=c("-1","-3","-5")
#        , pch=1, pt.cex=c(.8,3*.8,5*.8)
#        , cex=1.1
#        , col="black"
#        , bty="n", title="Water\nPotential\n(MPa)")
# 
# legend(x=-4150, y=-369400, xpd=NA
#        , legend=c("blue","live")
#        , pch=16, pt.cex=1.5
#        , cex=1.1
#        , col=c(bluecol, livecol)
#        , bty="n")




### trying with ggplot
ggplot() + geom_raster(data=sdem_gg, aes(x=x, y=y, fill=DEM_sedgwick_3m)) + xlim(LLBB[1,1:2]) + ylim(LLBB[2,1:2])










#__________________________________________
#+++++++++ Boxplot for Holly's fungi ++++++++####

# subset to week 15 to just plot data from around when roots were sampled
tmp <- wp_ind %>% filter(week==15, site=="LL", species=="blue oak", plot != "Ridge", plot != "Rohan")
tmp <- wp_ind %>% filter(week==29, site=="LL", species=="blue oak", plot != "Ridge", plot != "Rohan")


# create a color palette
library(RColorBrewer)
c <- brewer.pal(n=4, name="Set2")
col <- paste(c, "55", sep="") #make it opaque
#cols <- rep(col, each=3)
coldark <- paste(c, "AA", sep="") #make the middays less opaque
#colsdark <- rep(coldark, each=3)

quartz(width=4.5, height=3)
par(mar=c(4,5,1,1), mgp=c(2.7,1,0))
boxplot(-1*pd_mpa~plot, tmp, col=col
        , xlab="Elevation", ylab="Predawn water potential\n(soil dryness)"
        , boxwex=.6, at=c(1,2,3)
        , staplewex=0, notch=F, border="white"
        , whisklty=1, whisklwd=3, whiskcol=col, medlwd=3, outcol=col, outcex=.5, outlwd=1, bty="")

palette(coldark)
points(-1*pd_mpa~jitter(as.numeric(factor(plot)),factor = .5), tmp, pch=16, col=factor(plot))
legend("topleft", c("Low","Mid","High"), fill = col,border=col, bg=col, bty = "n", title = "Elevation")









#__________________________________________
#+++++++++ Boxplots for Piper (Shedd vs Chamise soil) ++++++++####

# subset the full data to just the 'Chamise' site, only the oaks, and only week 19 and 29
tmp <- wp_ind[which(wp_ind$site=="Chamise" & wp_ind$species!= "ARCA" & wp_ind$week != 11),]
par(mar=c(9,4,1,1))
boxplot(pd_mpa~plot+species+week, tmp, las=2, col=c("lightblue","darkblue","red2","darkred"), xlab="")
abline(v=4.5)

# quick and dirty anova to see whether 'plot' matters
summary(aov(pd_mpa~species+plot+week, tmp))

### blue oaks, pd vs midday on SvC through time
boxplot(mpa~ time+plot + week, wp_ind_long[which(wp_ind_long$species=="blue oak" & wp_ind_long$site=="Chamise" & wp_ind_long$week>15),], col=c("red","blue"), las=2, xlab="")
abline(v=c(4.5,8.5))





######## OLD DATA LOADING/CLEANING #########################
# # depricated and replaced by md_pd_processing.R as of 7.19.22

# 
# # Water potential version ###
# # note: this currently assumes that everything is stored in Data_05042022/ with versioning in the file name
# wpversion <- "SHIFT data collection 2022_22.06.27.xlsx"
# 
# ###### Sedgwick DEM
# 
# sdem <- raster::raster("Data_05042022/geospatial/DEM_sedgwick_3m.tif")
# #sdemll <- st_transform(sdem, crs= 4326)
# #sdemll <- raster::projectRaster(sdem)
# # make a df for plotting with ggplot
# sdem_gg <- sdem %>% 
#   rasterToPoints() %>% 
#   data.frame()
# 
# #### Tree Locations
# latlon <- read.csv("Data_05042022/Trees_LatLon_v2.csv")
# colnames(latlon)[1:2] <- c("Longitude","Latitude")
# # pull out 'B' and 'L' at start of tree names
# latlon$Tree <- latlon$Name
# latlon$Tree <- str_remove(pattern="B", string=latlon$Tree)
# latlon$Tree <- str_remove(pattern="L", string=latlon$Tree)
# 
# latlon$Species <- NA
# latlon$Species[grep("B",latlon$Name )] <- "B"
# latlon$Species[grep("L",latlon$Name )] <- "L"
# 
# # make a latlon and utm spatial data version 
# latlon.ll <- SpatialPointsDataFrame(coords = data.frame(latlon$Longitude, latlon$Latitude), proj4string = CRS("+proj=longlat +datum=WGS84"), data=latlon)
# latlon.utm <- spTransform(latlon.ll, CRSobj = crs(sdem))
# 
# 
# 
# ## table for linking tree numbers with sites
# treesite <- read_excel(paste0("Data_05042022/WP_WC/",wpversion),sheet="Tree_Sites", skip=0, na = "NA", col_types = "text")
# treesite$Tag <- str_replace(treesite$Tag, "\\.0","")
# 
# ####### Water Potentials 
# #Date: 228 WP + LWC
# wpwc228 <- read_excel(paste0("Data_05042022/WP_WC/",wpversion),sheet="228 WP + LWC", skip=5, na = "NA")
# 
# #Date: 38-311 WP + LWC
# wpwc38 <- read_excel(paste0("Data_05042022/WP_WC/",wpversion),sheet="38-311 WP + LWC", skip=5, na = "NA")
# # remove mislabeled tree 2567 which was actually 2367 
# wpwc38 <- wpwc38[-which(wpwc38$Tag=="2567"),]
# 
# #Date: 315 WP + LWC
# wpwc315 <- read_excel(paste0("Data_05042022/WP_WC/",wpversion),sheet="315 WP + LWC", skip=5, na = "NA")
# 
# #Date: 330 WP + LWC (core)
# wpwc330 <- read_excel(paste0("Data_05042022/WP_WC/",wpversion),sheet="330 WP + LWC", skip=5, na = "NA")
# 
# #Date: 411-412 WP + LWC (satellite)
# wpwc44 <- read_excel(paste0("Data_05042022/WP_WC/",wpversion),sheet="44 WP + LWC", skip=5, na = "NA")
# 
# #Date: 411-412 WP + LWC
# wpwc411 <- read_excel(paste0("Data_05042022/WP_WC/",wpversion),sheet="411-412 WP + LWC", skip=5, na = "NA")
# 
# 
# 
# 
# 
# 
# ######### . Data cleaning ####################
# 
# # take wide form and hella messy data and start making in long
# # note, loading {raster} caused a problem with select() from dplyr. just defensively coded it.
# 
# # pull out predawns and make them wide form
# ####### ++ 2.28.22 #########
# wp228pd <- wpwc228 %>%
#   filter(!is.na(Tag)) %>%
#   dplyr::select(!matches("_g_")) %>% 
#   dplyr::select(1:3,6,matches("PD")) %>% 
#   dplyr::select(-PD_bulk_wet, -PD_bulk_dry, -PD_avg) %>% 
#   pivot_longer(cols=matches("PD[1-9]") 
#                , names_to="PD"
#                , values_to="MPa"
#                , values_drop_na=TRUE)
# # add in sampling date
# # assign 2/28 date to predawns appropriately
# wp228pd$Date <- as_date("2022-02-28")
# 
# 
# wp228md <- wpwc228 %>%
#   filter(!is.na(Tag)) %>%
#   dplyr::select(!matches("_g_")) %>% 
#   dplyr::select(1:3,6,matches("MD")) %>% 
#   dplyr::select(-MD_bulk_wet, -MD_bulk_dry, -MD_avg) %>% 
#   pivot_longer(cols=matches("MD[1-9]") 
#                , names_to="MD"
#                , values_to="MPa"
#                , values_drop_na=TRUE)
# # add in sampling date
# # assign 3/8 date to middays appropriately
# wp228md$Date <- as_date("2022-02-28")
# 
# 
# 
# #average to tree
# wp228pdind <- wp228pd %>% group_by(Tag) %>% summarise(Date.PD=unique(Date),PD_MPa = mean(MPa))
# wp228pdind$WOY <- week(wp228pdind$Date.PD)
# wp228mdind <- wp228md %>% group_by(Tag) %>% summarise(Date.MD=unique(Date),MD_MPa = mean(MPa))
# wp228mdind$Date.MD[which(is.na(wp228mdind$Date.MD))] <- as_date("2022-03-08") # missing date for one value
# wp228mdind$WOY <- week(wp228mdind$Date.MD)
# 
# 
# #Merge PD and MD
# wp228all <- full_join(wp228pdind, wp228mdind, by=c("Tag", "WOY"))
# 
# # add in lat lon
# #wp228 <- left_join(wp228all, latlon, by= c("Tag"="Tree"))
# #wp228$Edrop <- wp228$MD_MPa - wp228$PD_MPa
# 
# 
# 
# 
# ####### ++ 3.08.22 & 3.11.22 #########
# wp38pd <- wpwc38 %>%
#   filter(!is.na(Tag)) %>%
#   dplyr::select(!matches("_g_")) %>% 
#   dplyr::select(1:3,6,matches("PD")) %>% 
#   dplyr::select(-PD_bulk_wet, -PD_bulk_dry, -PD_avg) %>% 
#   pivot_longer(cols=matches("PD[1-9]") 
#                , names_to="PD"
#                , values_to="MPa"
#                , values_drop_na=TRUE)
# # add in sampling date
# # assign 3/11 date to predawns appropriately
# wp38pd$Date[which(wp38pd$Date=="3/8 (MD); 3/11 (PD)")] <- "3/11/2022"
# wp38pd$Date <- as_date(wp38pd$Date, format="%m/%d/%Y")
# 
# 
# wp38md <- wpwc38 %>%
#   filter(!is.na(Tag)) %>%
#   dplyr::select(!matches("_g_")) %>% 
#   dplyr::select(1:3,6,matches("MD")) %>% 
#   dplyr::select(-MD_bulk_wet, -MD_bulk_dry, -MD_avg) %>% 
#   pivot_longer(cols=matches("MD[1-9]") 
#                , names_to="MD"
#                , values_to="MPa"
#                , values_drop_na=TRUE)
# # add in sampling date
# # assign 3/8 date to middays appropriately
# wp38md$Date[which(wp38md$Date=="3/8 (MD); 3/11 (PD)")] <- "3/8/2022"
# wp38md$Date <- as_date(wp38md$Date, format="%m/%d/%Y")
# 
# 
# #average to tree
# wp38pdind <- wp38pd %>% group_by(Tag) %>% summarise(Date.PD=unique(Date),PD_MPa = mean(MPa))
# wp38pdind$WOY <- week(wp38pdind$Date.PD)
# wp38mdind <- wp38md %>% group_by(Tag) %>% summarise(Date.MD=unique(Date),MD_MPa = mean(MPa))
# wp38mdind$Date.MD[which(is.na(wp38mdind$Date.MD))] <- as_date("2022-03-08") # missing date for one value
# wp38mdind$WOY <- week(wp38mdind$Date.MD)
# 
# 
# #Merge PD and MD
# wp38all <- full_join(wp38pdind, wp38mdind, by=c("Tag", "WOY"))
# 
# 
# #wp38 <- left_join(wp38all, latlon, by= c("Tag"="Tree"))
# #wp38$Edrop <- wp38$MD_MPa - wp38$PD_MPa
# 
# 
# ####### ++ 3.15.22 #########
# wp315pd <- wpwc315 %>%
#   filter(!is.na(Tag)) %>%
#   dplyr::select(!matches("_g_")) %>% 
#   dplyr::select(1:3,6,matches("PD")) %>% 
#   dplyr::select(-PD_bulk_wet, -PD_bulk_dry, -PD_avg) %>% 
#   pivot_longer(cols=matches("PD[1-9]") 
#                , names_to="PD"
#                , values_to="MPa"
#                , values_drop_na=TRUE)
# # add in sampling date
# # assign 3/11 date to predawns appropriately
# wp315pd$Date <- as_date("2022-03-15")
# 
# 
# wp315md <- wpwc315 %>%
#   filter(!is.na(Tag)) %>%
#   dplyr::select(!matches("_g_")) %>% 
#   dplyr::select(1:3,6,matches("MD")) %>% 
#   dplyr::select(-MD_bulk_wet, -MD_bulk_dry, -MD_avg) %>% 
#   pivot_longer(cols=matches("MD[1-9]") 
#                , names_to="MD"
#                , values_to="MPa"
#                , values_drop_na=TRUE)
# # add in sampling date
# # assign 3/15 date to middays appropriately
# wp315md$Date <- as_date("2022-03-15")
# 
# 
# #average to tree
# wp315pdind <- wp315pd %>% group_by(Tag) %>% summarise(Date.PD=unique(Date),PD_MPa = mean(MPa))
# wp315pdind$WOY <- week(wp315pdind$Date.PD)
# wp315mdind <- wp315md %>% group_by(Tag) %>% summarise(Date.MD=unique(Date),MD_MPa = mean(MPa))
# wp315mdind$Date.MD[which(is.na(wp315mdind$Date.MD))] <- as_date("2022-03-08") # missing date for one value
# wp315mdind$WOY <- week(wp315mdind$Date.MD)
# 
# 
# #add in lat lon
# wp315all <- full_join(wp315pdind, wp315mdind, by=c("Tag", "WOY"))
# 
# 
# #wp315 <- left_join(wp315all, latlon, by= c("Tag"="Tree"))
# #wp315$Edrop <- wp315$MD_MPa - wp315$PD_MPa
# 
# 
# 
# 
# ####### ++ 3.30.22 #########
# # currently just pd from core
# wp330pd <- wpwc330 %>%
#   filter(!is.na(Tag)) %>%
#   dplyr::select(!matches("_g_")) %>% 
#   dplyr::select(1:3,6,matches("PD")) %>% 
#   dplyr::select(-PD_bulk_wet, -PD_bulk_dry, -PD_avg) %>% 
#   pivot_longer(cols=matches("PD[1-9]") 
#                , names_to="PD"
#                , values_to="MPa"
#                , values_drop_na=TRUE)
# # add in sampling date
# wp330pd$Date <- as_date("2022-03-30")
# 
# # currently no MD values
# # wp330md <- wpwc330 %>%
# #   filter(!is.na(Tag)) %>%
# #   dplyr::select(!matches("_g_")) %>% 
# #   dplyr::select(1:3,6,matches("MP")) %>% 
# #   dplyr::select(-MP_bulk_wet, -MP_bulk_dry, -MP_avg) %>% 
# #   pivot_longer(cols=matches("MP[1-9]") 
# #                , names_to="MP"
# #                , values_to="MPa"
# #                , values_drop_na=TRUE)
# # # add in sampling date
# # wp330md$Date <- as_date("2022-04-11")
# 
# 
# 
# #average to tree
# wp330pdind <- wp330pd %>% group_by(Tag) %>% summarise(Date.PD=unique(Date),PD_MPa = mean(MPa))
# wp330pdind$WOY <- week(wp330pdind$Date.PD)
# wp330pdind$Date.MD <- NA
# wp330pdind$MD_MPa <- NA
# 
# 
# #wp330mdind <- wp330md %>% group_by(Tag) %>% summarise(MD_MPa = mean(MPa))
# 
# 
# 
# #add in lat lon
# #wp330 <- left_join(wp330pdind, latlon, by= c("Tag"="Tree"))
# #wp330all <- full_join(wp330pdind, wp330mdind)
# #wp330 <- full_join(wp330all, latlon, by= c("Tag"="Tree"))
# #wp330$Edrop <- wp330$MD_MPa - wp330$PD_MPa
# 
# # apply(wp330, FUN=function(x){ length(which(is.na(x)))}, MARGIN =2)
#   #looks good
# 
# 
# 
# 
# 
# ####### ++ 4.04.22 #########
# wp44pd <- wpwc44 %>%
#   filter(!is.na(Tag)) %>%
#   dplyr::select(!matches("_g_")) %>% 
#   dplyr::select(1:3,6,matches("PD")) %>% 
#   dplyr::select(-PD_bulk_wet, -PD_bulk_dry, -PD_avg) %>% 
#   pivot_longer(cols=matches("PD[1-9]") 
#                , names_to="PD"
#                , values_to="MPa"
#                , values_drop_na=TRUE)
# # add in sampling date
# wp44pd$Date <- as_date("2022-04-04")
# 
# 
# wp44md <- wpwc44 %>%
#   filter(!is.na(Tag)) %>%
#   dplyr::select(!matches("_g_")) %>% 
#   dplyr::select(1:3,6,matches("MD")) %>% 
#   dplyr::select(-MD_bulk_wet, -MD_bulk_dry, -MD_avg) %>% 
#   pivot_longer(cols=matches("MD[1-9]") 
#                , names_to="MD"
#                , values_to="MPa"
#                , values_drop_na=TRUE)
# # add in sampling date
# wp44md$Date <- as_date("2022-04-04")
# 
# 
# #average to tree
# wp44pdind <- wp44pd %>% group_by(Tag) %>% summarise(Date.PD=unique(Date),PD_MPa = mean(MPa))
# wp44pdind$WOY <- week(wp44pdind$Date.PD)
# wp44mdind <- wp44md %>% group_by(Tag) %>% summarise(Date.MD=unique(Date),MD_MPa = mean(MPa))
# wp44mdind$WOY <- week(wp44mdind$Date.MD)
# 
# 
# #add in lat lon
# wp44all <- full_join(wp44pdind, wp44mdind, by=c("Tag", "WOY"))
# 
# 
# #wp44 <- left_join(wp44all, latlon, by= c("Tag"="Tree"))
# #wp44$Edrop <- wp44$MD_MPa - wp44$PD_MPa
# 
# # apply(wp44, FUN=function(x){ length(which(is.na(x)))}, MARGIN =2)
#   # 2013 and 2085 are missing lat/lons
# 
# 
# 
# ####### ++ 4.11.22 #########
# wp411pd <- wpwc411 %>%
#   filter(!is.na(Tag)) %>%
#   dplyr::select(!matches("_g_")) %>% 
#   dplyr::select(1:3,6,matches("PD")) %>% 
#   dplyr::select(-PD_bulk_wet, -PD_bulk_dry, -PD_avg) %>% 
#   pivot_longer(cols=matches("PD[1-9]") 
#                , names_to="PD"
#                , values_to="MPa"
#                , values_drop_na=TRUE)
# # add in sampling date
# wp411pd$Date <- as_date("2022-04-11")
# 
# # currently no MD values
# # wp411md <- wpwc411 %>%
# #   filter(!is.na(Tag)) %>%
# #   dplyr::select(!matches("_g_")) %>% 
# #   dplyr::select(1:3,6,matches("MP")) %>% 
# #   dplyr::select(-MP_bulk_wet, -MP_bulk_dry, -MP_avg) %>% 
# #   pivot_longer(cols=matches("MP[1-9]") 
# #                , names_to="MP"
# #                , values_to="MPa"
# #                , values_drop_na=TRUE)
# # # add in sampling date
# # wp411md$Date <- as_date("2022-04-11")
# 
# 
# 
# #average to tree
# wp411pdind <- wp411pd %>% group_by(Tag) %>% summarise(Date.PD=unique(Date),PD_MPa = mean(MPa))
# wp411pdind$WOY <- week(wp411pdind$Date.PD)
# wp411pdind$Date.MD <- NA
# wp411pdind$MD_MPa <- NA
# 
# 
# #wp411mdind <- wp411md %>% group_by(Tag) %>% summarise(MD_MPa = mean(MPa))
# 
# 
# 
# #add in lat lon
# #wp411 <- left_join(wp411pdind, latlon, by= c("Tag"="Tree"))
# #wp411all <- full_join(wp411pdind, wp411mdind)
# #wp411 <- full_join(wp411all, latlon, by= c("Tag"="Tree"))
# #wp411$Edrop <- wp411$MD_MPa - wp411$PD_MPa



################# Combine Ind average WPs together ###########

# wp.ind <- rbind(wp228all,wp38all, wp315all, wp330pdind, wp44all, wp411pdind)
# wp.ind$Tag <- as.character(as.numeric(wp.ind$Tag))
# wp.ind <- left_join(wp.ind, latlon, by=c("Tag"="Tree"))
# wp.ind <- wp.ind[!is.na(wp.ind$Latitude),] 
# wp.ind$Site <- treesite$Site[match(wp.ind$Tag, treesite$Tag)]
# wp.ind$Plot <- treesite$Plot[match(wp.ind$Tag, treesite$Tag)]
