##################################################
#      Reading .csv file output from imageJ
######### 1/26/2018 LDL Anderegg ##################
# this is edited from the original file to read AU leaf areas


require(stringr)
require(dplyr)
# set working directory to be location of imageJ output files
#require(tcltk) # don't know why I originally loaded this package, but 

############# Change to directory of imageJ text files #########
#setwd("/Users/leeanderegg/Desktop/transplant")
setwd("~/Dropbox/WD project/LeafImages/")
      #wa_la_results/")
      #Trait_data/tassie_la/results/")


all <- dir()
files_all <- all[grep(".txt", all)]
files <- files_all[-grep("\\(", files_all)] # remove duplicates



# # create function called fn_readfiles() that reads in your files of leaf area 
# # given a vector of .csv names
# 
# fn_readfiles <- function (filenames = files, threshold= 0.005) {
#   require(stringr) # get the str.split function ready
#   tags <- c() # make vector to store sample name
#   Species <- c() # splitting apart tags into species-elev-plot-treenum
#   Site <- c()
#   Plot <- c()
#   Tree <- c()
#   Sample <- c()
#   misc <- c()   # make another column for random identifiers such as 'vcurve' or 'year 1'
#   tot_area <- c() # total leaf area
#   leaves <- c() # number of leaves
#   avg_area <- c() # mean needle area
#   sd_area <- c() # sd of neadle area
#   min_area <- c() # min area, to check if we're picking up dust flecks
#   max_area <- c() # max area, just a safety check to make sure leaves are reasonable
#   median_area <- c()
#   avg_perim <- c()
#   sd_perim <- c()
#   avg_width <- c()
#   sd_width <- c()
#   avg_height <- c()
#   sd_height <- c()
#   avg_major <- c()
#   avg_minor <- c()
#   avg_angle <- c()
#   avg_circ <- c()
#   avg_Feret <- c() # ignored FeretX,Y,angle
#   avg_AR <- c()
#   avg_round <- c()
#   avg_solidity <- c()
#   for (i in 1:length(files)){
#     j <- files[i]
#     tmp <- read.csv(j, header=T)
#     tmpclean <- tmp[which(tmp$Area>threshold),]
#     tags[i] <- str_split(as.character(j), ".csv")[[1]][1]
#     Species[i] <- str_split(tags[i], "-")[[1]][1]
#     Site[i] <- str_split(tags[i], "-")[[1]][2]
#     Plot[i] <- str_split(tags[i], "-")[[1]][3]
#     Tree[i] <- str_split(tags[i], "-")[[1]][4]
#     Sample[i] <- str_split(tags[i], "-")[[1]][5]
#     misc[i] <- str_split(tags[i], "-")[[1]][6]
#     tot_area[i] <- sum(tmpclean$Area)
#     leaves[i] <- length(tmpclean$Area)
#     avg_area[i] <- mean(tmpclean$Area)
#     sd_area[i] <- sd(tmpclean$Area)
#     min_area[i] <- min(tmpclean$Area)
#     max_area[i] <- max(tmpclean$Area)
#     median_area[i] <- median(tmpclean$Area)
#     avg_perim[i] <- mean(tmpclean$Perim.)
#     sd_perim[i] <- sd(tmpclean$Perim.)
#     avg_width[i] <- mean(tmpclean$Width)
#     sd_width[i] <- sd(tmpclean$Width)
#     avg_height[i] <- mean(tmpclean$Height)
#     sd_height[i] <- sd(tmpclean$Height)
#     avg_major[i] <- mean(tmpclean$Major)
#     avg_minor[i] <- mean(tmpclean$Minor)
#     avg_angle[i] <- mean(tmpclean$Angle)
#     avg_circ[i] <- mean(tmpclean$Circ.)
#     avg_Feret[i] <- mean(tmpclean$Feret) # ignored FeretX,Y,angle
#     avg_AR[i] <- mean(tmpclean$AR)
#     avg_round[i] <- mean(tmpclean$Round)
#     avg_solidity[i] <- mean(tmpclean$Solidity)
#   }
#   # combine all the info into one data frame with seedlings as rows
#   dataz <- data.frame(tags, Species, Site, Plot, Tree, Sample,misc, leaves, tot_area, avg_area, sd_area, min_area, max_area, median_area,
#                       avg_perim ,sd_perim ,avg_width ,sd_width ,avg_height ,sd_height ,
#                       avg_major ,avg_minor ,avg_angle ,avg_circ ,avg_Feret , # ignored FeretX,Y,angle
#                       avg_AR ,avg_round ,avg_solidity)
#   
#   return(dataz) # return your DATAZZZ
# }
# 
# Tas <- fn_readfiles(filenames=files, threshold=0.009)
# head(Tas)
# Tas1 <- arrange(Tas,Species,Site,Plot,Tree,Sample)
# write.csv(Tas1, "/Users/leeanderegg/Dropbox/Trade-offs project/LeafAreaResults/Tas_LeafArea_Results_all_032917.csv")
# 
# Tas <- Tas1
# 
# ###### Cleaning Notes: (did these in excel?)
# # AMYG-GRA-A-1-breal -> b, and old b -> b-bad in bad folder (both scan and .csv file)
# # OBLI-MER-C-1-b had scan2, so added '-scan1' to other b
# # OBLI-MER-C-2-abetter -> a, and moved a to bad 
# # OVAT-MER-A-1 a-c has conflicting copies. not sure if 'conflict1' (the first round), or "conflict2" the second round originally named "a 1" is correct
# # the second round had a sample b and breal, which might mean I checked the labels?
# # conflict1 copies were scanned in place that should be VIMI-MER-A-5a-c (which are missing), but they don't appear to be VIMI
#   # for the time being i'm removing the mislabled VIMI (non a1,b1,c1)
# # VIMI-TMP-B-1-c 1 was actually 2 -a that was rescanned.
# 
# Tas$scans <- str_split(Tas$Sample,"_", simplify = T)[,2]
# Tasclean <- Tas[-which(Tas$tags=='AMYG-GRA-A-1-b'),]
# Tasclean[which(Tasclean$tags=="OBLI-MER-C-1-b"),"Sample"] <- "b_scan1"
# Tasclean <- Tasclean[-which(Tasclean$tags=="OBLI-MER-C-2-a"),]
# Tasclean <- Tasclean[-which(Tasclean$tags=="VIMI-TMP-B-1-c 1"),]
# # get rid of all the _scanX s
# Tasclean$Sample <- str_split(Tasclean$Sample, "_", simplify=T)[,1]
# Tasclean$Sample[grep("real", Tasclean$Sample)] <- "b"
# Tasclean$Sample[grep("better", Tasclean$Sample)] <- "a"
# # the strange, replicated tree... OVAT-MER-A-1-a-c. Remove the non'1' samples
# Tasclean <- Tasclean[-which(Tasclean$tags %in% c("OVAT-MER-A-1-a", "OVAT-MER-A-1-b","OVAT-MER-A-1-c")),]
# Tasclean$Sample[grep("1", Tasclean$Sample)]<- c("a","b","c")
# 
# 
# 
# 
# # missing:
# # 1 Tree3 sample at VIMI-MER
# # 1 Tree4 sample @ VIMI-GRA and VIMI-TMP
# # 1 Tree5 @ OVAT-CMP
# # 3 Tree5 @ VIMI-MER-A-5
# 
# 
# ###### combining multiple scans ################
#   # note: median_area isn't combined correctly. but I don't think it's important
#   # 12 samples had more than 1 scan. This is maybe unfortunate, because I think median leaf size is better than mean
# TasLA <- Tasclean %>% group_by(Species,Site,Plot,Tree,Sample) %>% summarise(nleaves=sum(leaves), c_tot_area=sum(tot_area), c_min_area=min(min_area)
#                                                                             , c_max_area=max(max_area), c_median_area=mean(median_area), nscans = n(), c_sd_area = mean(sd_area), c_avg_circ= mean(avg_circ), c_avg_Feret = mean(avg_Feret), c_avg_AR = mean(avg_AR), c_avg_round= mean(avg_round), c_avg_major= mean(avg_major), c_avg_minor=mean(avg_minor) )
# 
# colnames(TasLA) <- gsub("c_", "", colnames(TasLA))
# TasLA <- TasLA %>% mutate(Tag=paste(Species,Site,Plot,Tree,Sample), mean_area=tot_area/nleaves)
# 
# # calculating actual median area. If there are important shape changes, should also add these in
#   # note: this would have been much easier to just do in the initial data creation step. But I didn't, and now I'm lazy.
#   # so instead of rewriting the code above, I'm just throwing in a cludge for the one measure I think is important...
# reps <- TasLA$Tag[which(TasLA$nscans>1)]
# reps <- gsub(pattern = " ", replacement = "-", reps)
# repsamps <- c()
# for(i in reps){
#   tmp <- files[grep(i, files)]
#   repsamps <- c(repsamps, tmp)
# }
# allareas <- data.frame()
# threshold <- 0.009
# for(j in repsamps){
#   tmp <- read.csv(j)
#   tmpclean <- tmp[which(tmp$Area>threshold),]
#   tmpclean$tags <- str_split(as.character(j), ".csv")[[1]][1]
#   tmpclean$Species <- str_split(tmpclean$tags[1], "-")[[1]][1]
#   tmpclean$Site <- str_split(tmpclean$tags[1], "-")[[1]][2]
#   tmpclean$Plot <- str_split(tmpclean$tags[1], "-")[[1]][3]
#   tmpclean$Tree <- str_split(tmpclean$tags[1], "-")[[1]][4]
#   tmpclean$Sample <- str_split(tmpclean$tags[1], "-")[[1]][5]
#   allareas <- rbind(allareas, tmpclean)
# }
# allareas$Sampleclean <- str_split(allareas$Sample, "_", simplify=T)[,1]
# medianareas <- allareas %>% group_by(Species,Site,Plot,Tree,Sampleclean) %>% summarise(med_area = median(Area))  
# medianareas$Tag <- with(medianareas, paste(Species, Site, Plot, Tree, Sampleclean, sep=" "))
# 
# # replace the estimated median area with the actual median area
# TasLA$median_area[which(TasLA$Tag %in% medianareas$Tag)] <- medianareas$med_area[match(TasLA$Tag[which(TasLA$Tag %in% medianareas$Tag)], medianareas$Tag)]
# 
# # v 032917 - original attempt
# # v 110117 - replaced median area with actual median area for twelve samples with multiple scans
# # write.csv(TasLA,  "/Users/leeanderegg/Dropbox/Trade-offs project/LeafAreaResults/Tas_LeafArea_Results_clean_032917.csv")
# write.csv(TasLA,  "/Users/leeanderegg/Dropbox/Trade-offs project/LeafAreaResults/Tas_LeafArea_Results_clean_032917.csv")









############# New updated approach 10.31.17 ######
  # the old approach of creating summary statistics for every results file (i.e. every scan)
  # sucked ballz for samples with multiple scans. I screwed around with a work around, but realized
  # I just had to grow up and rewrite this shit.

all <- dir()
files <- all[grep(".csv", all)]


# create function called fn_readfiles() that reads in your files of leaf area 
# given a vector of .csv names

fn_combinefiles <- function (filenames = files, threshold= 0.005) {
  require(stringr) # get the str.split function ready
  dataz <- data.frame() # start the dataframe to add all dataz to
  for (i in 1:length(files)){
    j <- files[i]
    tmp <- read.table(j, header=T)
    tmpclean <- tmp[which(tmp$Area>threshold),]
    tmpclean$Tag <- str_split(as.character(j), ".txt")[[1]][1]
    tmpclean$Species <- str_split(tmpclean$Tag[1], "-")[[1]][1]
    tmpclean$Individual <- str_split(tmpclean$Tag[1], "-")[[1]][2]
    tmpclean$Branch <- str_split(tmpclean$Tag[1], "-")[[1]][3]
    tmpclean$Scan <- str_split(tmpclean$Tag[1], "-")[[1]][4]
    dataz <- rbind(dataz, tmpclean)
  }
  return(dataz)
}

WDareas_all <- fn_combinefiles()  # appends all measurements from 4163 leaves
WDareas_branch <- data.frame(WDareas_all %>% group_by(Species, Individual, Branch) %>% summarise(tot_Area = sum(Area), mean_Area=mean(Area), max_Area=max(Area), min_Area=min(Area), sd_Area=sd(Area)))

write.csv(WDareas_branch, "LeafAreas_BranchTotals_20180126.csv")


###### Cleaning Notes: (did these in excel?)
# AMYG-GRA-A-1-breal -> b, and old b -> b-bad in bad folder (both scan and .csv file)
# OBLI-MER-C-1-b had scan2, so added '-scan1' to other b
# OBLI-MER-C-2-abetter -> a, and moved a to bad 
# OVAT-MER-A-1 a-c has conflicting copies. not sure if 'conflict1' (the first round), or "conflict2" the second round originally named "a 1" is correct
# the second round had a sample b and breal, which might mean I checked the labels?
# conflict1 copies were scanned in place that should be VIMI-MER-A-5a-c (which are missing), but they don't appear to be VIMI
# for the time being i'm removing the mislabled VIMI (non a1,b1,c1)
# VIMI-TMP-B-1-c 1 was actually 2 -a that was rescanned.

TasLA_all$scans <- str_split(TasLA_all$Sample,"_", simplify = T)[,2]
TasLA_allclean <- TasLA_all[-which(TasLA_all$Tag=='AMYG-GRA-A-1-b'),]
TasLA_allclean[which(TasLA_allclean$Tag=="OBLI-MER-C-1-b"),"Sample"] <- "b_scan1"
TasLA_allclean <- TasLA_allclean[-which(TasLA_allclean$Tag=="OBLI-MER-C-2-a"),]
TasLA_allclean <- TasLA_allclean[-which(TasLA_allclean$Tag=="VIMI-TMP-B-1-c 1"),]
# get rid of all the _scanX s
TasLA_allclean$Sample <- str_split(TasLA_allclean$Sample, "_", simplify=T)[,1]
TasLA_allclean$Sample[grep("real", TasLA_allclean$Sample)] <- "b"
TasLA_allclean$Sample[grep("better", TasLA_allclean$Sample)] <- "a"
# the strange, replicated tree... OVAT-MER-A-1-a-c. Remove the non'1' samples
TasLA_allclean <- TasLA_allclean[-which(TasLA_allclean$Tag %in% c("OVAT-MER-A-1-a", "OVAT-MER-A-1-b","OVAT-MER-A-1-c")),]
# rename the OVAT-Mer-A-1-a1-c1 samples
TasLA_allclean$Sample[grep("1", TasLA_allclean$Sample)]<- c(rep("a", length(grep("a 1", TasLA_allclean$Sample))),rep("b", length(grep("b1", TasLA_allclean$Sample))),rep("c", length(grep("c 1", TasLA_allclean$Sample))) )


# summarize everything, appropriately this time
TasLA <- TasLA_allclean %>% group_by (Species, Site, Plot, Tree, Sample) %>% summarise (nleaves = n(), tot_area = sum(Area), avg_area = mean(Area), median_area = median(Area), max_area = max(Area), min_area=min(Area)
                                                                                  , avg_width = mean(Width), avg_height = mean(Height), avg_major = mean(Major), avg_minor=mean(Minor)
                                                                                  , avg_circ = mean(Circ.), avg_Feret=mean(Feret), avg_FeretAngle = mean(FeretAngle), avg_AR = mean(AR)
                                                                                  , avg_Round = mean(Round), avg_Solidity = mean(Solidity), nscans = length(unique(scans)))


### write results to .csv

# all results
# v 032917.csv = old version that's uncleaned summaries of all scans
#   103117.csv = new version of all leaves, cleaned
# write.csv(Tas1, "/Users/leeanderegg/Dropbox/Trade-offs project/LeafAreaResults/Tas_LeafArea_Results_all_032917.csv")
write.csv(TasLA_allclean, "/Users/leeanderegg/Dropbox/Trade-offs project/LeafAreaResults/Tas_LeafArea_Results_all_103117.csv")

# sample summaries
# v clean_032917.csv = old version summaries from combining scan summaries
#   clean_103117.csv = new version summaries from all leaves cleaned before summarizing
# write.csv(TasLA,  "/Users/leeanderegg/Dropbox/Trade-offs project/LeafAreaResults/Tas_LeafArea_Results_clean_032917.csv")
write.csv(TasLA,  "/Users/leeanderegg/Dropbox/Trade-offs project/LeafAreaResults/Tas_LeafArea_Results_clean_103117.csv")




#____________________________________________________
######## Preliminary check before combining scans of the same branch to see if I need to keep other leaf traits #######
#____________________________________________________
## really doesn't seem to be super strong trends in much (measures of leaf are decrease w/ MD)
#   AMYG get thinner w/ MD, OBLI get thicker
#   everything gets ?less round? (smaller 'round' score) w/ MD
#   AR of everything increases with MD
#   Feret (whatever that measn) decreases w/ MD for everythin except OVAT
#   circ goes down in OVAT and 
#LAtas <- Tasclean
Taslocs <- read.csv("/Users/leeanderegg/Dropbox/Trade-offs project/Tassie Planning/Tassie data/Taslocs_TamsaniaSiteClimates_032917.csv")

TasLA$Branchtag <- paste(TasLA$Species, TasLA$Site, TasLA$Plot, TasLA$Tree, TasLA$Sample, sep="-")
TasLA$Treetag <- paste(TasLA$Species, TasLA$Site, TasLA$Plot, TasLA$Tree, sep="-")
TasLA$Plottag <- paste(TasLA$Species, TasLA$Site, TasLA$Plot, sep="-")
TasLA$Sitetag <- paste(TasLA$Species, TasLA$Site, sep="-")
TasLA$pAI <- Taslocs$pAI[match(TasLA$Treetag, Taslocs$Treetag)] #Plot level AIs
TasLA$AI <- Taslocs$AI[match(TasLA$Treetag, Taslocs$Treetag)] #Site level AIs
TasLA$pAIscaled <- scale(TasLA$pAI)
TasLA$pPPT <- Taslocs$pPPT[match(TasLA$Treetag, Taslocs$Treetag)] 
TasLA$PPT <- Taslocs$PPT[match(TasLA$Treetag, Taslocs$Treetag)]
TasLA$pPPTscaled <- scale(TasLA$pPPT)
TasLA$pPET <- Taslocs$pPET[match(TasLA$Treetag, Taslocs$Treetag)] #Site level AIs
TasLA$PET <- Taslocs$PET[match(TasLA$Treetag, Taslocs$Treetag)]
TasLA$pPETscaled <- scale(TasLA$pPET)
TasLA$pMD <- Taslocs$pMD[match(TasLA$Treetag, Taslocs$Treetag)] #Site level AIs
TasLA$MD <- Taslocs$MD[match(TasLA$Treetag, Taslocs$Treetag)]
TasLA$pMDscaled <- scale(TasLA$pMD)

TasLA <- data.frame(TasLA)

for (i in c(6:21)){
  plot(TasLA[,i]~TasLA$PPT, col=factor(TasLA$Species), ylab=colnames(TasLA)[i])
  abline(lm(TasLA[which(TasLA$Species=="AMYG"),i]~TasLA$PPT[which(TasLA$Species=="AMYG")]), col=1)
  abline(lm(TasLA[which(TasLA$Species=="OBLI"),i]~TasLA$PPT[which(TasLA$Species=="OBLI")]), col=2)
  abline(lm(TasLA[which(TasLA$Species=="OVAT"),i]~TasLA$PPT[which(TasLA$Species=="OVAT")]), col=3)
  abline(lm(TasLA[which(TasLA$Species=="VIMI"),i]~TasLA$PPT[which(TasLA$Species=="VIMI")]), col=4)
  
}
for (i in c(6:21)){
  plot(TasLA[,i]~TasLA$MD, col=factor(TasLA$Species), ylab=colnames(TasLA)[i])
  abline(lm(TasLA[which(TasLA$Species=="AMYG"),i]~TasLA$MD[which(TasLA$Species=="AMYG")]), col=1)
  abline(lm(TasLA[which(TasLA$Species=="OBLI"),i]~TasLA$MD[which(TasLA$Species=="OBLI")]), col=2)
  abline(lm(TasLA[which(TasLA$Species=="OVAT"),i]~TasLA$MD[which(TasLA$Species=="OVAT")]), col=3)
  abline(lm(TasLA[which(TasLA$Species=="VIMI"),i]~TasLA$MD[which(TasLA$Species=="VIMI")]), col=4)
  
}






########## WA leaf area processing ##########################


############# Change to directory of imageJ text files #########
#setwd("/Users/leeanderegg/Desktop/transplant")
setwd("~/Dropbox/Trade-offs project/LeafAreaResults/wa_la_results/")
#wa_la_results/")
#Trait_data/tassie_la/results/")



files <- dir()[grep("results", dir())]

acacLA <- read.csv(files[1])[,-1]
cocaLA <- read.csv(files[2])[,-1]
emarLA <- read.csv(files[3])[,-1]
esalLA <- read.csv(files[4])[,-1]

WALA_all <- rbind(acacLA, cocaLA, emarLA, esalLA)
WALA_all$Tag <- str_split(as.character(WALA_all$Label), ".jpg", simplify=T)[,1]
WALA_all$Species <- str_split(WALA_all$Tag, "-", simplify=T)[,1]
WALA_all$Species <- factor(WALA_all$Species)
  # rename some misnamed species for consistency
levels(WALA_all$Species) <- list(ACAC=c("Acac","ACAC"), COCA = c("COCA","Coca"), EMARG=c("Emar","EMARG"), ESAL =c("Esal","ESAL"))
WALA_all$Site <- str_split(WALA_all$Tag, "-", simplify=T)[,2]
WALA_all$Site <- factor(WALA_all$Site)
  # rename sites for consistency with WD dataframe
levels(WALA_all$Site) <- list(BEN="Ben", GNO="GNO", GRIM = "GRIM", LP="LP", MIDG="Midg", NAR="NAR", PER="Per", STIR="STIR", WAL="WAL")
WALA_all$Plot <- str_split(WALA_all$Tag, "-", simplify=T)[,3]
WALA_all$Tree <- str_split(WALA_all$Tag, "-", simplify=T)[,4]
WALA_all$Sample <- str_split(WALA_all$Tag, "-", simplify=T)[,5]
WALA_all$Flag_Area <- 0
  # Flags: 2= to be removed before combining
  #        1= issue to be aware of but kept
  #        0= no issues

###### Cleaning Notes: 
# lots of trouble with duplicates and label uncertainty

# remove all but the branch number for a clean Sample and Tag
WALA_all$SampleClean <- substring(WALA_all$Samp, 1,2)
WALA_all$TagClean <- with(WALA_all, paste(Species, Site, Plot, Tree, SampleClean, sep="-"))

# removing note in ACAC-NAR-C-T4-B2, COCA-STIR-C-T5-B2
WALA_all$Sample[which(WALA_all$Tag=="ACAC-NAR-C-T4-B2 (NO B3)")] <- "B2"
WALA_all$Sample[which(WALA_all$Tag=="COCA-STIR-C-T5-B2 (NO B3)")] <- "B2"

# getting rid of 'better' and any old scans they replaced
# WALA_all$Tag[which(WALA_all$TagClean %in% unique(WALA_all$TagClean[grep("better",WALA_all$Tag)]))]
# looks like the only duplicate is Emar-Midg-C-T2-B3
WALA_all$Flag_Area[which(WALA_all$TagClean=="EMARG-MIDG-C-T2-B3" & nchar(WALA_all$Sample)==2)] <- 2
  # remove the 'betters' from Sample so I can focus on other issues
WALA_all$Sample[grep("better", WALA_all$Sample)] <- WALA_all$SampleClean[grep("better", WALA_all$Sample)] 


# getting rid of 'coulds be's
# WALA_all$Tag[which(WALA_all$TagClean %in% unique(WALA_all$TagClean[grep("could",WALA_all$Sample)]))]
  # COCA-GRIM-A-T4-B2 could be B3 - also has 5 0001s and 4 normals
    # 0001 = B3 (has herbivory and leaf areas and masses match up)
  # ESAL-BEN-C-T1-B3 could be B2

# COCA-GRIM-A-T4, rename B2 0001 -> B3
WALA_all$Sample[which(WALA_all$Tag=="COCA-GRIM-A-T4-B2_0001_could be B3")] <- "B3"
WALA_all$SampleClean[which(WALA_all$Tag=="COCA-GRIM-A-T4-B2_0001_could be B3")] <- "B3"
WALA_all$TagClean[which(WALA_all$Tag=="COCA-GRIM-A-T4-B2_0001_could be B3")] <- "COCA-GRIM-A-T4-B3"
# clean B2 the real one
WALA_all$Sample[which(WALA_all$Tag=="COCA-GRIM-A-T4-B2_could be B3")] <- "B2"

#ESAL-BEN-C-T1 -> B3 is actually B3
WALA_all$Sample[which(WALA_all$Tag=="Esal-Ben-C-T1-B3_couldbeB2")] <- "B3"



# getting rid of 'either' samples
# WALA_all$Tag[which(WALA_all$TagClean %in% unique(WALA_all$TagClean[grep("either",WALA_all$Sample)]))]
  # ESAL-BEN-B-T2-B1,2,3 -> have a "_0001" version and an "_eitherT2orT5" version
  # the _0001 version appears to be T5 (based on leaf areas vs leaf wet mass)
  # however, T5 isn't in the LDMC dataset, so I'll flag them both and remove T5

# killing _0001
WALA_all$Flag_Area[which(WALA_all$Tag=="Esal-Ben-B-T2-B1_0001")] <- 2
WALA_all$Flag_Area[which(WALA_all$Tag=="Esal-Ben-B-T2-B2_0001")] <- 2
WALA_all$Flag_Area[which(WALA_all$Tag=="Esal-Ben-B-T2-B3_0001")] <- 2
# flagging other version
WALA_all$Flag_Area[which(WALA_all$Tag=="Esal-Ben-B-T2-B1_eitherT2orT5")] <- 1
WALA_all$Flag_Area[which(WALA_all$Tag=="Esal-Ben-B-T2-B2_eitherT2orT5")] <- 1
WALA_all$Flag_Area[which(WALA_all$Tag=="Esal-Ben-B-T2-B3_eitherT2orT5")] <- 1

WALA_all$Sample[which(WALA_all$Tag=="Esal-Ben-B-T2-B1_eitherT2orT5")] <- "B1"
WALA_all$Sample[which(WALA_all$Tag=="Esal-Ben-B-T2-B2_eitherT2orT5")] <- "B2"
WALA_all$Sample[which(WALA_all$Tag=="Esal-Ben-B-T2-B3_eitherT2orT5")] <- "B3"

# getting rid of "mislabeled double"
# WALA_all$Tag[which(WALA_all$TagClean %in% unique(WALA_all$TagClean[grep("mislabeled",WALA_all$Sample)]))]
  # EMARG-MIDG-A-T4-B3, two scans, each with two leaves. one bigger new leaves, other ('mislabeld double'), smaller leaves
    # the 'mislabeled double' is definitely wrong, based on the reported stem length in LDMC.ind
# THIS COULD BE A MISSING BRANCH ELSEWHERE?

# killing '_mislabeld double'
WALA_all$Flag_Area[which(WALA_all$Tag=="Emar-Midg-A-T4-B3_mislabeled double")] <- 2


# Delete all the bad samples
WALA_allclean <- WALA_all[which(WALA_all$Flag_Area <2),]





# summarize everything, appropriately this time
WALA <- WALA_allclean %>% group_by (Species, Site, Plot, Tree, SampleClean) %>% summarise (nleaves = n(), tot_area = sum(Area), avg_area = mean(Area), median_area = median(Area), max_area = max(Area), min_area=min(Area)
                                                                                        , avg_width = mean(Width), avg_height = mean(Height), avg_major = mean(Major), avg_minor=mean(Minor)
                                                                                         , avg_circ = mean(Circ.), avg_Feret=mean(Feret), avg_FeretAngle = mean(FeretAngle), avg_AR = mean(AR)
                                                                                         , avg_Round = mean(Round), avg_Solidity = mean(Solidity), nscans = length(unique(Sample)), Flag_Area = unique(Flag_Area))


### write results to .csv

# all results
#   103117.csv = new version of all leaves, cleaned
write.csv(WALA_allclean, "/Users/leeanderegg/Dropbox/Trade-offs project/LeafAreaResults/WA_LeafArea_Results_all_103117.csv")

# sample summaries
#   clean_103117.csv = new version summaries from all leaves cleaned before summarizing
write.csv(WALA,  "/Users/leeanderegg/Dropbox/Trade-offs project/LeafAreaResults/WA_LeafArea_Results_clean_103117.csv")


