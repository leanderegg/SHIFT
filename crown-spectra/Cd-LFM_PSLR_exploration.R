#_________________________________________________________
########## LFM prediction ################################
#________________________________________________________


# initiated 2.16.2024
# goal: break PSLR models for LFM in interesting ways
#       in order to test whether things break down spatially or temporally faster

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
################### Load Packages ###################
#install.packages("pls")
library(pls)
library(tidyverse)
library(here)
require(lubridate)

# install dependencies for the spectratrait github package
# req.packages <- c("devtools","remotes","readr","RCurl","httr","pls","dplyr","reshape2","here",
#                   "plotrix","scales","ggplot2","gridExtra")
# new.packages <- req.packages[!(req.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages, dependencies=TRUE)
devtools::install_github(repo = "TESTgroup-BNL/spectratrait", dependencies=TRUE, build_vignettes = TRUE)

library(spectratrait)
vignette(package="spectratrait")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++


#_______________________________________________________
############# 1) Load Data #############################
#_______________________________________________________

#++++ current version: 20240216 +++++
# extracted by Piper on Feb 16 2024
version <- "shiftcrownspectra_20240216"
#++++++++++++++++++++++++++++++++++++

# load in tree info: THIS VERSION COULD CHANGE
info0 <- read.csv(here("processed-data", "FullTreeList_fromwps20230220.csv")) %>% select(-X,-n_pds,-n_mds)
# missing tree 2085 in LL
tree2085 <- data.frame(tree=2085, site="PineNeedle",plot="Low",species='blue oak')
info <-rbind(info0, tree2085)

# load in the tree lookup table to match IDs to tree #s
ids <- read.csv(here("crown-spectra", version,"SHIFT_LL_tree_IDs.csv"), header=T)
ids$SHIFT_ID <- str_replace(ids$SHIFT_ID,"-","_")

# load band-to-wavelength conversion lookup table
wavelengths <- read.csv(here("crown-spectra", version,"bands_wavlength_conversion.csv"), header=T)

# load in all of the spectra
  # these are in 1 csv per overflight/sampling period
spectra.files <- dir(here("crown-spectra",version))[grep("spectra", dir(here("crown-spectra",version)))]

spectra.raw <- data.frame()
for(i in spectra.files){
  dataz <- read.csv(here("crown-spectra",version, i))
  spectra.raw <- rbind(spectra.raw, dataz)
}

# associate with our tree names
spectra.raw$id <- ids$LA_Tree_ID[match(spectra.raw$SHIFT_ID,ids$SHIFT_ID)]



# bring in tree info and drop water bands
spectra <- right_join(info,spectra.raw, by=c("tree"="id")) %>% select( -c(sample_1:sample_6),-c(sample_195:sample_209), -c(sample_285:sample_314))
  # remove 1363.90564 (sample_198) to 1403.975708 (sample 206), plus three bands each way (some overflights look good in this buffer, but some don't)
  # and 1829.705688 (sample_291) to 1919.865601 (sample_309)
  # and the first band (377, sample_1 - sample_6) looks weird on some dates.
#from spectra-trait canopy example: dropwaves <- c(1350:1440, 1826:1946)



# turn date into week for matching with LFM
spectra$date <- ymd(spectra$date)
spectra$week <- week(spectra$date)

# reorder columns
spectra <- spectra %>% select (tree:notes, week, starts_with("sample"))
# rename columns actual wavelengths
colnames(spectra)[grep("sample", colnames(spectra))] <- round(wavelengths$nm[match(colnames(spectra)[grep("sample", colnames(spectra))], wavelengths$wavelength)])


# removing some weird canopy spectra where there are multiple retrievals for the same tree for the same week. what's going on?
# 2373 wk 15,16,19 (SRC_061)
# 2379 wk 19 (SRL_022)
# 2027 - two SHIFT_IDs - SRL_027 and SRC_027
# 2015 - SRL_018, SRC_018
# 1478 - SRL_015, SRC_015
#####********* Problem to be solved: THESE NEED TO BE CHECKED 
#####* Also need to remove some outlier spectra and outlier LFM values.


spectra <- spectra[-which(spectra$SHIFT_ID %in% c("SRC_015", "SRC_018", "SRC_027")),]

# make a long form dataframe for plotting spectra
spectra.long <- pivot_longer(spectra, cols=-c(tree, site, plot, species, SHIFT_ID, date, notes, week), names_to = "wavelength", values_to = "reflect")
spectra.long$wavelength <- as.numeric(spectra.long$wavelength)

### Load field LFM data
lfm <- read.csv(here("processed-data","lfm_alldates_20230724.csv")) # just LFM data
lfmplus <- read.csv(here("processed-data","lfm_rwc_alas_wp_wc_df20230724.csv")) # LFM and Everything data
  # this has everything, but has a shitton of duplicats (0 NAs but only 881 unique values)

lfm$type[which(lfm$type=="both")] <- "bulk" # clear up nomenclature
  # trouble lfms
  # pd 2369 in wk 37, total wet weight is MASSIVE
  # md 2322 in wk 9, wet weight is real high
# turns out there are lots of stem lfms with CRAZY high LFM. prob because of measurement error?
# will just remove the lfm with wet weight >30
lfm <- lfm[which(lfm$lfm_wet_wt_g<30),]
lfm <- lfm[-which(lfm$tree==2322 & lfm$week==9),]
lfm$time[which(is.na(lfm$time))] <- "md" # it looks like these lfms missing a time are probably mds
# some 33 trees with bad numbers (6 shrub numbers, but 61 trees in info and 100 trees in lfm)
probtrees <- lfm$tree[-which(lfm$tree %in% info$tree)]
probtrees <-probtrees[which(probtrees>100)]
  # 2013 == 2014?
  # 3477?
  # 3480?
  # 2389 == 2089? 2379/69?
  # 2323 == 2023 definitely (missing week 11 leaf yr1)
lfm$tree[which(lfm$tree==2013)] <- 2014 # probably?
lfm$tree[which(lfm$tree==2323)] <- 2023
lfm$tree[which(lfm$tree==2385)] <- 2382 # maybe? unclear
lfm$tree[which(lfm$tree==2127)] <- 2027
lfm$tree[which(lfm$tree==2032)] <- 2031 # missing week 17, but 2030 isn't
lfm$tree[which(lfm$tree==1472)] <- 1478 #definitely
lfm$tree[which(lfm$tree==9382)] <- 2382 #definitely

# outlier detection
lfm[which(lfm$lfm_wet_wt_g<lfm$lfm_dry_wt_g),] # shit tone of values with wet weight lower than dry
spp <- info$species[match(lfm$tree, info$tree)]
ggplot(lfm, aes(x=lfm_dry_wt_g, y=lfm_wet_wt_g, col=spp)) + geom_point() + geom_abline(intercept=0, slope=1) + facet_wrap(~type)

lfm$lfm_percent[which(lfm$lfm_percent<20)] <- NA # none of these are late season, so unreasonable even for stems


# quick and dirty adding everything into bulk.
lfm.bulk <- lfm %>% group_by(time, tree, date_lfm, week) %>% summarise (tot_wet_g = sum(lfm_wet_wt_g, na.rm=T)
                                                                        , tot_dry_g = sum(lfm_dry_wt_g, na.rm=T)
                                                                        , nyears = length(unique(year))
                                                                        , ntypes = length(unique(type))
                                                                        , tot_samps = n()) %>%
  mutate(lfm_percent = (tot_wet_g-tot_dry_g)/tot_dry_g*100) %>%
  filter(lfm_percent >20)


  # so week 17 lfm -> week 16 or 18 spectra. two days closer to 18
  # week 21 lfm -> week 19 spectra
lfm.bulk$week[which(lfm.bulk$week==17)] <- 18
lfm.bulk$week[which(lfm.bulk$week==21)] <- 19

# lfm dataset for lfm exploration (rather than matching with spectra)
lfm.bulk.test <- left_join(lfm.bulk, info) # get rid of shrubs

# lfm dataset for matching with spectra
lfm.bulk.clean <- lfm.bulk[which(lfm.bulk$tree %in% unique(spectra$tree) & lfm.bulk$week %in% unique(spectra$week)),]
lfm.wide <- pivot_wider(lfm.bulk.clean, id_cols = c(tree, date_lfm, week), id_expand=F, names_from = time, values_from=c(5:10))

# first pass = sub in pds where mds are missing
lfm.wide$lfm_percent <- lfm.wide$lfm_percent_md
lfm.wide$lfm_percent[which(is.na(lfm.wide$lfm_percent_md))] <- lfm.wide$lfm_percent_pd[which(is.na(lfm.wide$lfm_percent_md))]
lfm.wide$nyears <- lfm.wide$nyears_md
lfm.wide$nyears[which(is.na(lfm.wide$nyears_md))] <- lfm.wide$nyears_pd[which(is.na(lfm.wide$nyears_md))]
lfm.wide$tot_samps <- lfm.wide$tot_samps_md
lfm.wide$tot_samps[which(is.na(lfm.wide$tot_samps_md))] <- lfm.wide$tot_samps_pd[which(is.na(lfm.wide$tot_samps_md))]
  #215 unique LFM values to train and test with. woof.

# just grab columns we care about
lfm.bc <- lfm.wide %>% select(tree, date_lfm, week, lfm_percent, nyears, tot_samps) %>% group_by(tree, week) %>% summarise(lfm_percent=mean(lfm_percent), nyears=max(nyears), tot_samps = sum(tot_samps))
  # this has 13 duplicate tree+week rows...hmmm. seems like they maybe have different lfm dates?
  # only 204 datapoints once you average out


###___________ Read in LWC to augment LFM ________________

# unprocessed LWC data
# datver <- "20230724"
# dataversion <- paste0("Data_", datver)
# wc_df <- read_csv(here("processed-data", paste0("wc_alldates_",datver,".csv")), show_col_types = FALSE) %>% #contains LWC data
#   mutate(date = lubridate::ymd(date))%>% 
#   select(-...1) %>% 
#   mutate(date_wc = date) %>% 
#   select(-date) %>% 
#   mutate(time  = case_when(
#     time %in% c("PD") ~ "pd", 
#     time %in% c("MD") ~ "md", 
#     TRUE ~"NA"
#   ), 
#   tree = case_when(
#     tree == 2127 ~ 2327, #weird trees
#     tree == 2309 ~ 2379, 
#     TRUE ~ tree
#   )) %>% 
#   distinct() %>% 
#   #select(tree, date_wc, time, lwc_bulk, lwc_leaf, rep) %>% 
#   ungroup()
# 
# wcmd <- wc_df %>% filter(time=="md") %>% group_by(tree, plot, site, species, week, date_wc) %>% summarise(lwc = mean(lwc_bulk, na.rm=T),
#                                                                                                           dm_tot_g= sum(dm_bulk_g, na.rm=T),
#                                                                                                           wm_tot_g= sum(wm_bulk_g, na.rm=T),
#                                                                                                           )

# all trait data, theoretically averaged to individual (but not really)
alldat <- read.csv("processed-data/analysis_bothspp20231128.csv")

allmd <- alldat %>% filter(time=="md")
#allmd$week[which(allmd$week==18)] <- 18.1
allmd$week[which(allmd$week==17)] <- 18 # make them all wk 18 for meshing with spectra

allmd.mean <- allmd %>% group_by(tree, week, plot, site, species) %>% summarise(wp = mean(water_potential, na.rm=T),
                                                                                lwc = mean(lwc_mean, na.rm=T),
                                                                                lma = mean(lma_g_cm2, na.rm=T),
                                                                                ldmc = mean(ldmc, na.rm=T),
                                                                                alas = mean(alas_cm2_per_mm2, na.rm=T),
                                                                                lwa = mean(lwa_g_cm2, na.rm=T),
                                                                                leaf_count = mean(leaf_count, na.rm=T),
                                                                                swc = mean(swc_per_dry_g_mean, na.rm=T),
                                                                                time_season=unique(time_season))


allmd.mean <- allmd.mean %>% filter(week %in% spectra$week, tree %in% spectra$tree)
  # only 211 overlapping samples...
xtabs(~species+week, allmd.mean)
xtabs(~species+week, lfm.spectra)

allmd.mean$tree.week <- with(allmd.mean, paste(tree, week, sep="."))
lfm.bc$tree.week <- with(lfm.bc, paste(tree, week,sep="."))

length(which(allmd.mean$tree.week %in% lfm.bc$tree.week))
  # 163 overlaps
allmd.mean$lfm <- lfm.bc$lfm_percent[match(allmd.mean$tree.week, lfm.bc$tree.week)]


plot(lfm~I(lwc*100), allmd.mean, col=factor(species))
abline(a=0,b=1)
  # fuck. that correlations is total SHIT.

length(which(!allmd.mean$tree.week %in% lfm.bc$tree.week))
  # 48 lwc not in LFM
length(which(!lfm.bc$tree.week %in% allmd.mean$tree.week))
  # 41 LFM not in lwc


##### Final LFM dataset merging #########
# for quick and dirty, let's just throw the missing 48 lwc into the lfm dataset.

allmd.tomerge <- allmd.mean %>% ungroup() %>% select(tree, week, lwc,wp,lma, swc, leaf_count) # ldmc, lwa, alas,

lfm.bc.merge <- full_join(lfm.bc, allmd.tomerge) # added in missing values
apply(lfm.bc.merge,MARGIN=2, FUN=function(x){length(which(is.na(x)))})
  # oh, well, the only traits that actually have any replication are lwc and wp
lfm.bc.merge$lfm_percent[which(is.na(lfm.bc.merge$lfm_percent))] <- lfm.bc.merge$lwc[which(is.na(lfm.bc.merge$lfm_percent))]*100

# get rid of missing lfm row
lfm.bc.merge <- lfm.bc.merge %>% filter(lfm_percent>0)

# add in tree info
lfm.bc.merge.info <- left_join(lfm.bc.merge,info)


lfm.spectra <- left_join(lfm.bc.merge.info, spectra)
lfm.spectra <- lfm.spectra[which(lfm.spectra$`2501`>0 & lfm.spectra$lfm_percent>37 & lfm.spectra$lfm_percent<190),] # kill three NAs that crept in somewhere...
  # week 18, a few blues that have very small lfm <37
  # a few large lfms <190, blues are defo outliers, lives probs as well though the models capture them better

#______________________________________________________________________
####### Fit PSLRS #####################################################
#______________________________________________________________________


# first attempt: just see how well it fits to everything

#make this example reproducible
set.seed(1)

#fit PCR model
model <- plsr(lfm_percent~. -tree - week -nyears - tot_samps - tree.week - lwc - wp -lma -swc- leaf_count- site - plot - species - SHIFT_ID - date - notes, data=lfm.spectra,ncomp=50, scale=TRUE, validation="CV" )

summary(model)
  # looks like 6 components is best RMSE
  # which explains ~48% of the training variance

#visualize cross-validation plots
validationplot(model)
validationplot(model, val.type="MSEP")
validationplot(model, val.type="R2")


# make a plot of predicted versus observed
ncomps <- 4
full_preds <- predict(model, lfm.spectra, ncomp=ncomps)

quartz(width=3.5, height=3.8)
par(mar=c(4,4,3,1), mgp=c(2.5,1,0))
plot(lfm.spectra$lfm_percent~full_preds, col=factor(lfm.spectra$species), ylim=c(20,230), xlim=c(20,230),
     xlab="Predicted LFM", ylab="Observed LFM",
     main="Full Dataset")
abline(a=0,b=1)
mtext(paste("RMSE=",round(sqrt(mean((full_preds - lfm.spectra$lfm_percent)^2, na.rm=T)),1)), side = 3, line=-1)
#mtext(paste("R2=", round(cor(full_preds, lfm.spectra$lfm_percent)^2, 2)), side=3, line=-2)
mtext(paste("R2=", round(1-sum((full_preds - lfm.spectra$lfm_percent)^2)/sum((lfm.spectra$lfm_percent-mean(lfm.spectra$lfm_percent))^2), 2)), side=3, line=-2)
legend("bottom",legend=c("blue","live"), pch=1, ncol=2, col=1:2, cex=.8)
quartz.save(here("crown-spectra","figures","FullDataset_4comps_v1.pdf"), type="pdf")

#### color by week, pch by spp
quartz(width=3.5, height=3.8)
par(mar=c(4,4,3,1), mgp=c(2.5,1,0))
plot(lfm.spectra$lfm_percent~full_preds, col=factor(lfm.spectra$week), pch=(as.numeric(as.factor(lfm.spectra$species))-1)*13+3,, ylim=c(20,230), xlim=c(20,230),
     xlab="Predicted LFM", ylab="Observed LFM",
     main="Full Dataset")
abline(a=0,b=1)
mtext(paste("RMSE=",round(sqrt(mean((full_preds - lfm.spectra$lfm_percent)^2, na.rm=T)),1)), side = 3, line=-1)
#mtext(paste("R2=", round(cor(full_preds, lfm.spectra$lfm_percent)^2, 2)), side=3, line=-2)
mtext(paste("R2=", round(1-sum((full_preds - lfm.spectra$lfm_percent)^2)/sum((lfm.spectra$lfm_percent-mean(lfm.spectra$lfm_percent))^2), 2)), side=3, line=-2)
legend("bottom",legend=c("blue","live"), pch=c(3,16), ncol=2, col=1, cex=.8)
quartz.save(here("crown-spectra","figures","FullDataset_4comps_colbyweek_v1.pdf"), type="pdf")


#### LMA & WP #########


#fit PCR model
lma.mod <- plsr(lma~. -tree - week -nyears - tot_samps - tree.week - lwc - wp -lma -swc- leaf_count- site - plot - species - SHIFT_ID - date - notes, data=lfm.spectra,ncomp=50, scale=TRUE, validation="CV" )

summary(lma.mod)
# looks like 4 components is best RMSE, 48%

# make a plot of predicted versus observed
ncomps <- 4
lma_preds <- predict(lma.mod, lfm.spectra, ncomp=ncomps)

#### color by week, pch by spp
quartz(width=3.5, height=3.8)
par(mar=c(4,4,3,1), mgp=c(2.5,1,0))
plot(lfm.spectra$lma~lma_preds, col=factor(lfm.spectra$week), pch=(as.numeric(as.factor(lfm.spectra$species))-1)*13+3,
     xlab="Predicted LMA", ylab="Observed LMA",
     main="Full Dataset", ylim=c(0.007,0.022), xlim=c(0.007,0.022),)
abline(a=0,b=1)
mtext(paste("RMSE=",round(sqrt(mean((lma_preds - lfm.spectra$lma)^2, na.rm=T)),3)), side = 3, line=-1)
#mtext(paste("R2=", round(cor(lma_preds, lfm.spectra$lma)^2, 2)), side=3, line=-2)
mtext(paste("R2=", round(1-sum((lma_preds - lfm.spectra$lma)^2, na.rm=T)/sum((lfm.spectra$lma-mean(lfm.spectra$lma, na.rm=T))^2,na.rm=T), 2)), side=3, line=-2)
#legend("bottom",legend=c("blue","live"), pch=c(3,16), ncol=2, col=1, cex=.8)
quartz.save(here("crown-spectra","figures","lmaDataset_4comps_colbyweek_v1.pdf"), type="pdf")


#fit PCR model
wp.mod <- plsr(wp~. -tree - week -nyears - tot_samps - tree.week - lwc - wp -lma -swc- leaf_count- site - plot - species - SHIFT_ID - date - notes, data=lfm.spectra,ncomp=50, scale=TRUE, validation="CV" )

summary(wp.mod)
# looks like 6 components is best RMSE, 70%

# make a plot of predicted versus observed
ncomps <- 6
wp_preds <- predict(wp.mod, lfm.spectra, ncomp=ncomps)

#### color by week, pch by spp
quartz(width=3.5, height=3.8)
par(mar=c(4,4,3,1), mgp=c(2.5,1,0))
plot(lfm.spectra$wp~wp_preds, col=factor(lfm.spectra$week), pch=(as.numeric(as.factor(lfm.spectra$species))-1)*13+3,
     xlab="Predicted WP", ylab="Observed WP",
     main="Full Dataset", ylim=c(-4,0), xlim=c(-4,0))
abline(a=0,b=1)
mtext(paste("RMSE=",round(sqrt(mean((wp_preds - lfm.spectra$wp)^2, na.rm=T)),3)), side = 3, line=-1)
#mtext(paste("R2=", round(cor(wp_preds, lfm.spectra$wp)^2, 2)), side=3, line=-2)
mtext(paste("R2=", round(1-sum((wp_preds - lfm.spectra$wp)^2, na.rm=T)/sum((lfm.spectra$wp-mean(lfm.spectra$wp, na.rm=T))^2,na.rm=T), 2)), side=3, line=-2)
#legend("bottom",legend=c("blue","live"), pch=c(3,16), ncol=2, col=1, cex=.8)
quartz.save(here("crown-spectra","figures","wpDataset_4comps_colbyweek_v1.pdf"), type="pdf")



######## ______ blue to predict live: #######
blue <- lfm.spectra[which(lfm.spectra$species=="blue oak"),] %>% select( -nyears, - tot_samps, - lwc, - wp, -lma, -swc,- leaf_count, - plot, - species, - SHIFT_ID, - date, - notes, - tree.week)
live <- lfm.spectra[which(lfm.spectra$species=="live oak"),] %>% select( -nyears, - tot_samps, - lwc, - wp, -lma, -swc,- leaf_count, - plot, - species, - SHIFT_ID, - date, - notes, - tree.week)


#### All Blues model
blue_mod <- plsr(lfm_percent~. -tree -week - site , data=blue ,ncomp=15, scale=TRUE, validation="CV" )
#visualize cross-validation plots
summary(blue_mod)
validationplot(blue_mod)
validationplot(blue_mod, val.type="MSEP")
validationplot(blue_mod, val.type="R2")
  # 4 comps - 40% variance explained

blue_preds <- predict(blue_mod, blue, ncomp=4)
quartz(width=3.5, height=3.8)
par(mar=c(4,4,3,1), mgp=c(2.5,1,0))
plot(blue$lfm_percent~blue_preds, col=factor(blue$week), ylim=c(20,230), xlim=c(20,230),
     xlab="Predicted LFM", ylab="Observed LFM",
     main="Blues Only")
abline(a=0,b=1)
mtext(paste("RMSE=",round(sqrt(mean((blue_preds - blue$lfm_percent)^2, na.rm=T)),1)), side = 3, line=-1)
#mtext(paste("R2=", round(cor(full_preds, lfm.spectra$lfm_percent)^2, 2)), side=3, line=-2)
mtext(paste("R2=", round(1-sum((blue_preds - blue$lfm_percent)^2)/sum((blue$lfm_percent-mean(blue$lfm_percent))^2), 2)), side=3, line=-2)
legend("bottomright",legend=levels(factor(blue$week)), pch=1, ncol=1, col=1:7, cex=.8,title = "Week")
quartz.save(here("crown-spectra","figures","BluesAll_v1.pdf"), type="pdf")



#### All Lives model
live_mod <- plsr(lfm_percent~. -tree -week - site , data=live ,ncomp=15, scale=TRUE, validation="CV" )
summary(live_mod)
validationplot(live_mod)
validationplot(live_mod, val.type="MSEP")
validationplot(live_mod, val.type="R2")
  # 5 comps, 42%  variance explained

live_preds <- predict(live_mod, live, ncomp=5)
quartz(width=3.5, height=3.8)
par(mar=c(4,4,3,1), mgp=c(2.5,1,0))
plot(live$lfm_percent~live_preds, col=factor(live$week), ylim=c(20,230), xlim=c(20,230),
     xlab="Predicted LFM", ylab="Observed LFM",
     main="Lives Only")
abline(a=0,b=1)
mtext(paste("RMSE=",round(sqrt(mean((live_preds - live$lfm_percent)^2, na.rm=T)),1)), side = 3, line=-1)
#mtext(paste("R2=", round(cor(full_preds, lfm.spectra$lfm_percent)^2, 2)), side=3, line=-2)
mtext(paste("R2=", round(1-sum((live_preds - live$lfm_percent)^2)/sum((live$lfm_percent-mean(live$lfm_percent))^2), 2)), side=3, line=-2)
#legend("bottom",legend=c("live","live"), pch=1, ncol=2, col=1:2, cex=.8)
quartz.save(here("crown-spectra","figures","livesAll_v1.pdf"), type="pdf")



#### try predicting lives with the blue model _______
bluemod_live_preds  <- predict(blue_mod, live, ncomp=4)
quartz(width=3.5, height=3.8)
par(mar=c(4,4,3,1), mgp=c(2.5,1,0))
plot(live$lfm_percent~bluemod_live_preds, col=factor(live$week), ylim=c(20,230), xlim=c(20,230),
     xlab="Predicted LFM", ylab="Observed LFM",
     main="Lives with Blue Model")
abline(a=0,b=1)
mtext(paste("RMSE=",round(sqrt(mean((bluemod_live_preds - live$lfm_percent)^2, na.rm=T)),1)), side = 3, line=-1)
mtext(paste("R2=", round(1-sum((bluemod_live_preds - live$lfm_percent)^2,na.rm=T)/sum((live$lfm_percent-mean(live$lfm_percent,na.rm=T))^2, na.rm=T), 2)), side=3, line=-2)
quartz.save(here("crown-spectra","figures","Lives-w-bluemod_v1.pdf"), type="pdf")



# try predicting blues with the live model
livemod_blue_preds  <- predict(live_mod, blue, ncomp=5)
quartz(width=3.5, height=3.8)
par(mar=c(4,4,3,1), mgp=c(2.5,1,0))
plot(blue$lfm_percent~livemod_blue_preds, col=factor(blue$week), ylim=c(20,230), xlim=c(20,230),
     xlab="Predicted LFM", ylab="Observed LFM",
     main="Blues with Live Model")
abline(a=0,b=1)
mtext(paste("RMSE=",round(sqrt(mean((livemod_blue_preds - blue$lfm_percent)^2, na.rm=T)),1)), side = 3, line=-1)
mtext(paste("R2=", round(1-sum((livemod_blue_preds - blue$lfm_percent)^2,na.rm=T)/sum((blue$lfm_percent-mean(blue$lfm_percent,na.rm=T))^2, na.rm=T), 2)), side=3, line=-2)
quartz.save(here("crown-spectra","figures","Blues-w-livemod_v1.pdf"), type="pdf")


######_____ quick space vs time: ____ ######

### LFM values through time
lfm.week <- lfm.spectra %>% group_by(species, week) %>% summarise(mean_lfm = mean(lfm_percent, na.rm=T))

quartz(width=3.5, height=3.2)
ggplot() + 
  geom_point(data=lfm.spectra[which(lfm.spectra$species=="blue oak"),], aes(x=jitter(week), y=lfm_percent), color="lightgray") +
  geom_point(data=lfm.week[which(lfm.week$species=="blue oak" & lfm.week$week>9),], aes(x=week, y=mean_lfm, size=2)) +
  geom_smooth(data=lfm.week[which(lfm.week$species=="blue oak" & lfm.week$week>9),], aes(x=week, y=mean_lfm), color="black", se=F, method="lm") +
  theme_classic() + ylab("LFM") + xlab("Week") 
quartz.save(here("crown-spectra","figures","LFMthroughtime_v2.pdf"), type="pdf")




  # if I do weeks 1,10,11,14,15 = 96 samples, if I do 15,18,19 = 107 samples
blue.early <- blue %>% filter(week<=15)
blue.late0 <- blue %>% filter(week>=15)
blue.late <- blue.late0[sample(1:nrow(blue.late0),size =nrow(blue.early)),] # randomly sample down to same size

####. early model
early_mod <- plsr(lfm_percent~. -tree -week - site , data=blue.early ,ncomp=15, scale=TRUE, validation="CV" )
summary(early_mod)
validationplot(early_mod)
validationplot(early_mod, val.type="MSEP")
validationplot(early_mod, val.type="R2")
# 3 comps or 6, 35% or 54% variance explained

# predict late:
earlymod_late_preds  <- predict(early_mod, blue.late, ncomp=6)
plot(earlymod_late_preds~blue.late$lfm_percent, col=factor(blue.late$week), ylim=c(40,200), xlim=c(40,200))
quartz(width=3.5, height=3.8)
par(mar=c(4,4,3,1), mgp=c(2.5,1,0))
plot(earlymod_late_preds~blue.late$lfm_percent, col=factor(blue.late$week), ylim=c(20,230), xlim=c(20,230),
     xlab="Predicted LFM", ylab="Observed LFM",
     main="Late with Early model")

abline(a=0,b=1)
mtext(paste("RMSE=",round(sqrt(mean((earlymod_late_preds - blue.late$lfm_percent)^2, na.rm=T)),1)), side = 3, line=-1)
mtext(paste("R2=", round(1-sum((earlymod_late_preds - blue.late$lfm_percent)^2,na.rm=T)/sum((blue.late$lfm_percent-mean(blue.late$lfm_percent,na.rm=T))^2, na.rm=T), 2)), side=3, line=-2)
quartz.save(here("crown-spectra","figures","Late-w-Earlymod_v1.pdf"), type="pdf")


#####. late model
late_mod <- plsr(lfm_percent~. -tree -week - site , data=blue.late ,ncomp=15, scale=TRUE, validation="CV" )
summary(late_mod)
validationplot(late_mod)
validationplot(late_mod, val.type="MSEP")
validationplot(late_mod, val.type="R2")
# 7 comps 45% variance explained

# predict late:
latemod_early_preds  <- predict(late_mod, blue.early, ncomp=7)

quartz(width=3.5, height=3.8)
par(mar=c(4,4,3,1), mgp=c(2.5,1,0))
plot(latemod_early_preds~blue.early$lfm_percent, col=factor(blue.early$week), ylim=c(20,230), xlim=c(20,230),
     xlab="Predicted LFM", ylab="Observed LFM",
     main="Early with late model")

abline(a=0,b=1)
mtext(paste("RMSE=",round(sqrt(mean((latemod_early_preds - blue.late$lfm_percent)^2, na.rm=T)),1)), side = 3, line=-1)
mtext(paste("R2=", round(1-sum((latemod_early_preds - blue.late$lfm_percent)^2,na.rm=T)/sum((blue.late$lfm_percent-mean(blue.late$lfm_percent,na.rm=T))^2, na.rm=T), 2)), side=3, line=-2)
quartz.save(here("crown-spectra","figures","Early-w-latemod_v1.pdf"), type="pdf")



######_____ predict new space ____ ######
# need to select an equal number of trees that are well vs poorly represented for training and testing
  # or can actually just split Cucu Mesa + Chamise vs LL.
  # Pine Needle can either throw out or split 2085/2087 vs 2088/2090 (throw out 2086)
blue.space1 <- blue %>% filter(site=="LL" | tree %in% c(2085, 2087))
blue.space2 <- blue %>% filter(site %in% c("Chamise","Cucu Mesa") | tree %in% c(2088, 2090))

blue.space1 <- blue %>% filter(tree %in% names(sort(xtabs(~tree, blue)))[seq(2,38, by=2)])
  # drop 2008 for symmetry
blue.space2 <- blue %>% filter(tree %in% names(sort(xtabs(~tree, blue)))[seq(3,39, by=2)])

#####. Train on Lisque
space1_mod <- plsr(lfm_percent~. -tree -week - site , data=blue.space1 ,ncomp=15, scale=TRUE, validation="CV" )
summary(space1_mod)
validationplot(space1_mod)
validationplot(space1_mod, val.type="MSEP")
validationplot(space1_mod, val.type="R2")
# 1/25% or 3/33% variance explained
  # or 3/45%

space2_mod <- plsr(lfm_percent~. -tree -week - site , data=blue.space2 ,ncomp=15, scale=TRUE, validation="CV" )
summary(space2_mod)
validationplot(space2_mod)
validationplot(space2_mod, val.type="MSEP")
# 6 comps, 73% variation!
# 4/40%

# predict late:
space1mod_space2_preds  <- predict(space1_mod, blue.space2, ncomp=3)
space2mod_space1_preds  <- predict(space2_mod, blue.space1, ncomp=5)

quartz(width=3.5, height=3.8)
par(mar=c(4,4,3,1), mgp=c(2.5,1,0))
plot(space1mod_space2_preds~blue.space2$lfm_percent, col=factor(blue.space2$week), ylim=c(20,230), xlim=c(20,230),
     xlab="Predicted LFM", ylab="Observed LFM",
     main="Timecourse of new trees", pch=16)
points(space2mod_space1_preds~blue.space1$lfm_percent, col=factor(blue.space1$week), pch=3)
abline(a=0,b=1)
mtext(paste("avg RMSE=",round(mean(sqrt(mean((space1mod_space2_preds - blue.space2$lfm_percent)^2, na.rm=T)),sqrt(mean((space2mod_space1_preds - blue.space1$lfm_percent)^2, na.rm=T))),1)), side = 3, line=-1)
mtext(paste("avg R2=", round(mean(1-sum((space1mod_space2_preds - blue.space2$lfm_percent)^2,na.rm=T)/sum((blue.space2$lfm_percent-mean(blue.space2$lfm_percent,na.rm=T))^2, na.rm=T),1-sum((space2mod_space1_preds - blue.space1$lfm_percent)^2,na.rm=T)/sum((blue.space1$lfm_percent-mean(blue.space1$lfm_percent,na.rm=T))^2, na.rm=T)), 2)), side=3, line=-2)
#mtext(paste("RMSE=",round(sqrt(mean((space2mod_space1_preds - blue.space1$lfm_percent)^2, na.rm=T)),1)), side = 3, line=-3)
#mtext(paste("R2=", round(1-sum((space2mod_space1_preds - blue.space1$lfm_percent)^2,na.rm=T)/sum((blue.space1$lfm_percent-mean(blue.space1$lfm_percent,na.rm=T))^2, na.rm=T), 2)), side=3, line=-4)


quartz.save(here("crown-spectra","figures","HalfTrees_mod_v1.pdf"), type="pdf")



#____________________________________________________________________





xtabs(~species + week, lfm.spectra)

ggplot(lfm.bulk, aes(x=week, y=lfm_percent, col=factor(tree))) + geom_line()



# ##********* Problem to be solved: why do we have SOOO many lfm replicates per tree in many trees???
# hist(lfm.bulk$tot_samps)
# hist(lfm.bulk$ntypes*lfm.bulk$nyears)


# Question: can we sub in pd lfms for md lfms? Or are they actually usually higher?
ggplot(lfm.wide, aes(x=lfm_percent_pd, y=lfm_percent_md, col=tot_wet_g_md, size=tot_wet_g_pd)) + geom_point() + geom_abline(slope=1, intercept=0)
  # well, it's not obvious that the smallest samples are the ones with the worst correlation.
  # pd IS slightly higher on average. but not immensely.

# length(which(is.na(lfm.wide$lfm_percent_md) & lfm.wide$lfm_percent_pd>0))
#   # and there are 62 samples that have pds but no middays.
# length(which(lfm.wide$lfm_percent_md>0))
#   # which is a decent number given there are only 153 mds + 3 NAs
# length(which(lfm.wide$lfm_percent_NA>0 & is.na(lfm.wide$lfm_percent_pd)))
#   # all three of the NA times for lfm are times where we have pd but no midday.
# plot(lfm_percent_NA~lfm_percent_pd, lfm.wide)
# abline(a=0,b=1)
#   # but they look like MDs

# question: do our spectra look reasonable (and what are the water bands to eliminate?)
spectra.long$species <- factor(spectra.long$species)
levels(spectra.long$species) <- list(live = "live oak", blue="blue oak")
ggplot(spectra.long[which(spectra.long$week>8 & !is.na(spectra.long$species)),], aes(x=wavelength, y=reflect, col=species, group=tree)) + geom_line() + facet_wrap(~week) + ylab("Reflectance") + theme_classic()
  # should remove 1363.90564 (sample_198) to 1403.975708 (sample 206), out two bands each way
  # and 1829.705688 (sample_291) to 1919.865601 (sample_309)
# wow, the blues look like shit even by 5/11

# try out the spectra.trait plotting function:
f.plot.spec(Z=spectra[which(spectra$species=="blue oak"),-c(1:8)], wv=as.numeric(colnames(spectra)[-c(1:8)]))
f.plot.spec(Z=spectra[which(spectra$species=="live oak"),-c(1:8)], wv=as.numeric(colnames(spectra)[-c(1:8)]))


# question: how to live oak and blue oak LFM differ?
ggplot(lfm.bulk.test[which(lfm.bulk.test$week==18),], aes(x=species, y=lfm_percent, col=species)) + geom_boxplot() + facet_wrap(~week)
  
  # blues usually have higher lfm than lives. shrubs are weird.

# only post week 29 when lfm gets real compressed
ggplot(lfm.bulk.test[which(lfm.bulk.test$week>28),], aes(x=species, y=lfm_percent, col=species)) + geom_boxplot() + facet_wrap(~week)
  # same's true, blues mostly larger than lives. shrubs are weird.

