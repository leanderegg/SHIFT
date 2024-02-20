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
info <- read.csv(here("processed-data", "FullTreeList_fromwps20230220.csv")) %>% select(-X,-n_pds,-n_mds)


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
spectra <- right_join(info,spectra.raw, by=c("tree"="id")) %>% select(-sample_1,-c(sample_196:sample_208), -c(sample_289:sample_313))
  # and the first band (377) looks weird on some dates.

# rename columns actual wavelengths
colnames(spectra)[grep("sample", colnames(spectra))] <- wavelengths$nm[match(colnames(spectra)[grep("sample", colnames(spectra))], wavelengths$wavelength)]

# turn date into week for matching with LFM
spectra$date <- ymd(spectra$date)
spectra$week <- week(spectra$date)

# removing some weird canopy spectra where there are multiple retrievals for the same tree for the same week. what's going on?
# 2373 wk 15,16,19 (SRC_061)
# 2379 wk 19 (SRL_022)
# 2027 - two SHIFT_IDs - SRL_027 and SRC_027
# 2015 - SRL_018, SRC_018
# 1478 - SRL_015, SRC_015
#####********* Problem to be solved: THESE NEED TO BE CHECKED 

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




# quick and dirty adding everything into bulk.
lfm.bulk <- lfm %>% group_by(time, tree, date_lfm, week) %>% summarise (tot_wet_g = sum(lfm_wet_wt_g)
                                                                        , tot_dry_g = sum(lfm_dry_wt_g)
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
lfm.bc <- lfm.wide %>% select(tree, date_lfm, week, lfm_percent, nyears, tot_samps)
lfm.spectra <- left_join(lfm.bc,spectra)




#______________________________________________________________________
####### Fit PSLRS #####################################################
#______________________________________________________________________


# first attempt: just see how well it fits to everything

#make this example reproducible
set.seed(1)

#fit PCR model
model <- plsr(lfm_percent~. -tree - date_lfm - week - nyears - tot_samps - site - plot - species - SHIFT_ID - date - notes, data=lfm.spectra,ncomp=50, scale=TRUE, validation="CV", )

summary(model)
  # looks like 8 components is best RMSE
  # which explains ~49% of the training variance

#visualize cross-validation plots
validationplot(model)
validationplot(model, val.type="MSEP")
validationplot(model, val.type="R2")


# make a plot of predicted versus observed
full_preds <- predict(model, lfm.spectra, ncomp=8)

plot(full_preds~lfm.spectra$lfm_percent, col=factor(lfm.spectra$species), ylim=c(40,200), xlim=c(40,200))
abline(a=0,b=1)
mtext(paste("RMSE=",round(sqrt(mean((full_preds - lfm.spectra$lfm_percent)^2, na.rm=T)),1)), side = 3, line=-1)

# now make a model with blue oak and predict live:
blue <- lfm.spectra[which(lfm.spectra$species=="blue oak"),]
live <- lfm.spectra[which(lfm.spectra$species=="live oak"),]

blue_mod <- plsr(lfm_percent~. -tree - date_lfm - week - nyears - tot_samps - site - plot - species - SHIFT_ID - date - notes, data=blue ,ncomp=50, scale=TRUE, validation="CV", )





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
ggplot(spectra.long, aes(x=wavelength, y=reflect, col=species, group=tree)) + geom_line() + facet_wrap(~date)
  # should remove 1363.90564 (sample_198) to 1403.975708 (sample 206), out two bands each way
  # and 1829.705688 (sample_291) to 1919.865601 (sample_309)
# wow, the blues look like shit even by 5/11


# question: how to live oak and blue oak LFM differ?
ggplot(lfm.bulk.test, aes(x=species, y=lfm_percent, col=species)) + geom_boxplot() + facet_wrap(~week)
  # blues usually have higher lfm than lives. shrubs are weird.

# only post week 29 when lfm gets real compressed
ggplot(lfm.bulk.test[which(lfm.bulk.test$week>28),], aes(x=species, y=lfm_percent, col=species)) + geom_boxplot() + facet_wrap(~week)
  # same's true, blues mostly larger than lives. shrubs are weird.

