#____________________________________________________________
######## ENVIRONMENTAL SENSOR PROCESSIONG ###################
#____________________________________________________________

# script for quickly ingesting
# - sapflow
# - soil moisture
# and eventually Decagon temp/RH + PAR and HOBO Temp/rH data

# started 04.05.22 by LDLA
# last updated: 
#     05.31.22 by IB: wrote sapflow to .csv for accessing in other scripts
#     06.02.22 by IB: read in new downloads and added to final sapflow.csv

# version notes:
# - initial raw data files have shitty file names. Almost certainly need to rename them.
# - critical metadata may not always be associated with file name...
#   because edaphic sucks and stores everything in a rough data format

library(tidyverse) # all the tidyverse data handling functions
library(lubridate) #Dealing with dates. 
library(janitor) # cleaning up column names, etc.
require(ggplot2)
library(here) #for easier/less buggy file organization

#source("/Users/leeanderegg/Desktop/R functions, general/ggplot_helpers.R") 

#NOTE: put the above in the 'scripts' folder of the repo? 
##i.e.: 
##       source(here("scripts", "ggplot_helpers.R"))

#______________________________________________________________
############### Begin: SAPFLOW #######################################
#______________________________________________________________

sapflow.dir <- "Data_05042022/sapflow/"
sapfiles <- dir(sapflow.dir)


#### RT sapflow  ###########
start.date <- as_datetime("2022-03-24 17:24:33") # ED04 deployment date
RTtree.names <- data.frame(Sensor=c("SAP.FLOW.TOTAL","SAP.FLOW.TOTAL.1","SAP.FLOW.TOTAL.2","SAP.FLOW.TOTAL.3"), Tree=c("2381","2384","2382","2010"))

RT_maydownload <- read.csv("Data_05042022/sapflow/20220331-101209 Log Download_ED04.csv", header=T, na.strings = "-1004600",skip = 3) #older version
RT_junedownload <- read.csv("Data_05042022/sapflow/20220503-111350 Log Download_ED04.csv", header=T, na.strings = "-1004600",skip = 3)

RT <- rbind(RT_maydownload, RT_junedownload)

RT <- RT[-1,]
colnames(RT)[1] <- "Date.Time"
RT$Date.Time  <- lubridate::as_datetime(RT$Date.Time,format="%d/%m/%y %H:%M:%S")
RT <- RT[which(RT$Date.Time>start.date),]
  # label the dendrometer output of Tree 1
colnames(RT)[ncol(RT)] <- "S1Radius"

RT$SAP.FLOW.TOTAL <- as.numeric(RT$SAP.FLOW.TOTAL)
RT$SAP.FLOW.TOTAL.1 <- as.numeric(RT$SAP.FLOW.TOTAL.1)
RT$SAP.FLOW.TOTAL.2 <- as.numeric(RT$SAP.FLOW.TOTAL.2)
RT$SAP.FLOW.TOTAL.3 <- as.numeric(RT$SAP.FLOW.TOTAL.3)
RT$S1Radius <- as.numeric(RT$S1Radius)

# quick switch from wide to long form, including only SAP.FLOW.TOTAL columns
RT.wide <- RT %>% select(Date.Time, SAP.FLOW.TOTAL,SAP.FLOW.TOTAL.1,SAP.FLOW.TOTAL.2,SAP.FLOW.TOTAL.3, S1Radius)
RT.long <- pivot_longer(RT.wide, names_to="Sensor",col=-1, values_to = "Sapflow")

# name the trees
RT.long$Tree <- RTtree.names$Tree[match(RT.long$Sensor, RTtree.names$Sensor)]

# tree name for dendrometer
RT.long$Tree[RT.long$Sensor=="S1Radius"] <- "2381"
RT.dendro <- RT.long[which(RT.long$Sensor=="S1Radius"),]
colnames(RT.dendro)[3] <- "rawRadius"
RT.dendro$Growth_um <- RT.dendro$rawRadius - min(RT.dendro$rawRadius, na.rm=T)
  # create a 0-1 scaled version for plotting over sapflow
RT.dendro$Growth_sc <- RT.dendro$Growth_um/max(RT.dendro$Growth_um,na.rm=T)  
#kill the dendrometer readings
RT.long <- RT.long[which(RT.long$Sensor!="S1Radius"),]
# label RT
RT.long$Loc <- "RT"

# data from broke ED03
start.date <- as_date("03/03/22 11:29:54", format="%d/%m/%y %H:%M:%S") # deployment date of ED03
RTtree.names <- data.frame(Sensor=c("SAP.FLOW.TOTAL","SAP.FLOW.TOTAL.1","SAP.FLOW.TOTAL.2","SAP.FLOW.TOTAL.3"), Tree=c("2381","2384","2382","2010"))
RT.old <- read.csv("Data_05042022/sapflow/20220326-173619 Log Download_ED03_RT.csv", header=T, na.strings = "-1004600",skip = 3)

RT.old <- RT.old[-1,]
colnames(RT.old)[1] <- "Date.Time"
RT.old$Date.Time  <- lubridate::as_datetime(RT.old$Date.Time,format="%d/%m/%y %H:%M:%S")
RT.old <- RT.old[which(RT.old$Date.Time>start.date),]

RT.old$SAP.FLOW.TOTAL <- as.numeric(RT.old$SAP.FLOW.TOTAL)
RT.old$SAP.FLOW.TOTAL.1 <- as.numeric(RT.old$SAP.FLOW.TOTAL.1)
RT.old$SAP.FLOW.TOTAL.2 <- as.numeric(RT.old$SAP.FLOW.TOTAL.2)
#RT.old$SAP.FLOW.TOTAL.3 <- as.numeric(RT.old$SAP.FLOW.TOTAL.3)
#RT.old$S1Radius <- as.numeric(RT.old$S1Radius)

# quick switch from wide to long form, including only SAP.FLOW.TOTAL columns
RT.old.wide <- RT.old %>% select(Date.Time, SAP.FLOW.TOTAL,SAP.FLOW.TOTAL.1,SAP.FLOW.TOTAL.2)
RT.old.long <- pivot_longer(RT.old.wide, names_to="Sensor",col=-1, values_to = "Sapflow")

# name the trees
RT.old.long$Tree <- RTtree.names$Tree[match(RT.old.long$Sensor, RTtree.names$Sensor)]
# label RT
RT.old.long$Loc <- "RT"



### TS sapflow ##############
start.date <- as_date("2022-02-18") # ED01 deployment date
reprog.date <- as_datetime("17/03/22 11:32:28",format="%d/%m/%y %H:%M:%S") # date.time of reprogramming to add sensor 4
sensor.switch.date <- reprog.date # also switched sensor 3 from one tree to another on this date
TStree.names <- data.frame(Sensor=c("SAP.FLOW.TOTAL","SAP.FLOW.TOTAL.1","SAP.FLOW.TOTAL.2","SAP.FLOW.TOTAL.3"), Tree=c("2347","2346","2349","2343"))

TS_maydownload <- read.csv("Data_05042022/sapflow/20220331-130401 Log Download_ED01.csv", header=T, na.strings = c("-1004600","-99.000"),skip = 3)

TS_junedownload <- read.csv("Data_05042022/sapflow/20220530-123409 Log Download_ED01.csv", header=T, na.strings = c("-1004600","-99.000"),skip = 3)

TS <- rbind(TS_maydownload, TS_junedownload)

TS <- TS[-1,]
colnames(TS)[1] <- "Date.Time"
TS$Date.Time  <- lubridate::as_datetime(TS$Date.Time,format="%d/%m/%y %H:%M:%S")
TS <- TS[which(TS$Date.Time>start.date),]
TS <- TS[-grep("Download", TS$Charge.Status),] # kill empty rows when data was downloaded
# move the logger notes to a new column
TS$lognotes <- NA
# grab notes from before the reprogram date
TS$lognotes[which(TS$Date.Time<reprog.date)] <- paste(TS$SAP.FLOW.TOTAL.3[which(TS$Date.Time<reprog.date)],TS$HEAT.VELOCITY.OUTER.3[which(TS$Date.Time<reprog.date)] )
# kill all the info so we can make it numeric
TS[which(TS$Date.Time<reprog.date), grep(".3", colnames(TS))] <- NA

TS$SAP.FLOW.TOTAL <- as.numeric(TS$SAP.FLOW.TOTAL)
TS$SAP.FLOW.TOTAL.1 <- as.numeric(TS$SAP.FLOW.TOTAL.1)
TS$SAP.FLOW.TOTAL.2 <- as.numeric(TS$SAP.FLOW.TOTAL.2)
TS$SAP.FLOW.TOTAL.3 <- as.numeric(TS$SAP.FLOW.TOTAL.3)

# quick switch from wide to long form, including only SAP.FLOW.TOTAL columns
TS.wide <- TS %>% select(Date.Time, SAP.FLOW.TOTAL,SAP.FLOW.TOTAL.1,SAP.FLOW.TOTAL.2,SAP.FLOW.TOTAL.3)
TS.long <- pivot_longer(TS.wide, names_to="Sensor",col=-1, values_to = "Sapflow")

# name the trees
TS.long$Tree <- TStree.names$Tree[match(TS.long$Sensor, TStree.names$Sensor)]
  # switch sensor 3 on reprog.date
TS.long$Tree[which(TS.long$Sensor=="SAP.FLOW.TOTAL.2" & TS.long$Date.Time>sensor.switch.date)] <- "2009"
# label TS
TS.long$Loc <- "TS"



### MS sapflow ###########
start.date <- as_date("03/03/22 11:29:54", format="%d/%m/%y %H:%M:%S") # ED01 deployment date
reprog.date <- as_datetime("26/03/22 17:23:10",format="%d/%m/%y %H:%M:%S") # date.time of reprogramming to add sensor 4
MStree.names <- data.frame(Sensor=c("SAP.FLOW.TOTAL","SAP.FLOW.TOTAL.1","SAP.FLOW.TOTAL.2","SAP.FLOW.TOTAL.3"), Tree=c("2369","2367","2365","2360"))

MS_maydownload <- read.csv("Data_05042022/sapflow/20220331-111140 Log Download_ED02.csv", header=T, na.strings = c("-1004600","-99.000"),skip = 3)

MS_junedownload <- read.csv("Data_05042022/sapflow/20220530-103346 Log Download_ED02.csv", header=T, na.strings = c("-1004600","-99.000"),skip = 3)

MS <- rbind(MS_maydownload, MS_junedownload)

MS <- MS[-1,]
colnames(MS)[1] <- "Date.Time"
MS$Date.Time  <- lubridate::as_datetime(MS$Date.Time,format="%d/%m/%y %H:%M:%S")
MS <- MS[which(MS$Date.Time>start.date),]
MS <- MS[-grep("Download", MS$Charge.Status),] # kill the times when a row is blank because data was downloaded
# move the logger notes to a new column
MS$lognotes <- NA
# grab notes from before the reprogram date
MS$lognotes[which(MS$Date.Time<reprog.date)] <- paste(MS$SAP.FLOW.TOTAL.3[which(MS$Date.Time<reprog.date)],MS$HEAT.VELOCITY.OUTER.3[which(MS$Date.Time<reprog.date)] )
# kill all the info so we can make it numeric
MS[which(MS$Date.Time<reprog.date), grep(".3", colnames(MS))] <- NA

MS$SAP.FLOW.TOTAL <- as.numeric(MS$SAP.FLOW.TOTAL)
MS$SAP.FLOW.TOTAL.1 <- as.numeric(MS$SAP.FLOW.TOTAL.1)
MS$SAP.FLOW.TOTAL.2 <- as.numeric(MS$SAP.FLOW.TOTAL.2)
MS$SAP.FLOW.TOTAL.3 <- as.numeric(MS$SAP.FLOW.TOTAL.3)

# quick switch from wide to long form, including only SAP.FLOW.TOTAL columns
MS.wide <- MS %>% select(Date.Time, SAP.FLOW.TOTAL,SAP.FLOW.TOTAL.1,SAP.FLOW.TOTAL.2,SAP.FLOW.TOTAL.3)
MS.long <- pivot_longer(MS.wide, names_to="Sensor",col=-1, values_to = "Sapflow")

# name the trees
MS.long$Tree <- MStree.names$Tree[match(MS.long$Sensor, MStree.names$Sensor)]
# label MS
MS.long$Loc <- "MS"



######### Combine all Data ########
sap <- rbind(RT.long, MS.long, TS.long, RT.old.long)
sapmax <- sap %>% group_by(Tree) %>% summarize(maxSapflow = max(Sapflow, na.rm=T))
sap$Sapflow.st <- sap$Sapflow/sapmax$maxSapflow[match(sap$Tree, sapmax$Tree)]
sap$Loc <- factor(sap$Loc)


###New dataset: 
write_csv(sap, here("processed-data", "sapflow.csv"))


##### PLOTS #####


plot(Sapflow.st~Date.Time, sap, col=factor(Loc), pch=".", ylim=c(-.5,1))
for(i in unique(sap$Tree)){
  lines(Sapflow.st~Date.Time, sap[which(sap$Tree==i),], col=Loc)
}
abline(h=0)

quartz(width=6, height=3)
ggplot(sap[which(sap$Tree==2009),], aes(x=Date.Time, y=Sapflow, col=Tree)) + geom_line() +geom_hline(yintercept = 0)+ facet_wrap(~Loc)  +
  geom_line(data=RT.dendro, aes(y=Growth_sc)) # add dendrometer


plot(Sapflow~Date.Time, MS.long, col=factor(Tree), pch=".")
abline(h=0)
plot(Sapflow~Date.Time, TS.long, col=factor(Tree), pch=".", ylim=c(-.2,1))
abline(h=0)
abline(v=as_datetime("2022-03-28 12:00:00"), col="blue") # rain event

plot(Sapflow~Date.Time, RT.long, col=factor(Tree), pch=".")
abline(h=0)
lines(Growth_sc~Date.Time, RT.dendro, col="red")



ggplot(temp, aes(x=Date.Time, y=SAP.FLOW.TOTAL.3)) + geom_line() + 
  geom_line(aes(y=SAP.FLOW.TOTAL.2, col="red")) + 
  geom_line(aes(y=SAP.FLOW.TOTAL.1, col="blue")) + 
  geom_hline(yintercept = 0)
