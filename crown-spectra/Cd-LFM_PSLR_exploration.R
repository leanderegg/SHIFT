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
spectra <- right_join(info,spectra.raw, by=c("tree"="id")) %>% select(-c(sample_196:sample_208), -c(sample_289:sample_313))

# rename columns actual wavelengths
colnames(spectra)[grep("sample", colnames(spectra))] <- wavelengths$nm[match(colnames(spectra)[grep("sample", colnames(spectra))], wavelengths$wavelength)]


# make a long form dataframe for plotting spectra
spectra.long <- pivot_longer(spectra, cols=-c(1:7), names_to = "wavelenth", values_to = "reflect")
spectra.long$wavelenth <- as.numeric(spectra.long$wavelenth)

### Load field LFM data
lfm <- read.csv(here("processed-data","lfm_alldates_20230724.csv"))
lfmplus <- read.csv(here("processed-data","lfm_rwc_alas_wp_wc_df20230724.csv"))





ggplot(spectra.long, aes(x=wavelenth, y=reflect, col=species, group=tree)) + geom_line() + facet_wrap(~date)
  # should remove 1363.90564 (sample_198) to 1403.975708 (sample 206), out two bands each way
  # and 1829.705688 (sample_291) to 1919.865601 (sample_309)
# wow, the blues look like shit even by 5/11


