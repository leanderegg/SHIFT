### Exploring temporal correlations between water potentials.


source("/Users/leeanderegg/Desktop/random science/ZuurMixedModelling/AllRCode/HighstatLibV6.R")
require(tidyverse)
require(here)
require(lubridate)
# 2022
pd <- data_og %>% filter(time=="pd" & week %in% c(10,14,15,17,19,21,29,33, 37)) %>% select(tree, week, water_potential, site)

pd_wide <- pivot_wider(pd, id_cols = c(tree, site),names_from = week, values_from = water_potential,names_prefix = "PD_")

pd_wide <- pd_wide %>% select(tree, site, PD_10, PD_14, PD_15, PD_17, PD_19, PD_21, PD_29, PD_33, PD_37)
Mypairs.color(pd_wide[,grep("PD",colnames(pd_wide))], col=pd_wide$site)
Mypairs(pd_wide[,grep("PD",colnames(pd_wide))])


md <- data_og %>% filter(time=="md" & week %in% c(10,14,15,17,19,21,29,33, 37)) %>% select(tree, week, water_potential, site)
md_wide <- pivot_wider(md, id_cols = c(tree, site),names_from = week, values_from = water_potential,names_prefix = "md_")
md_wide <- md_wide %>% select(tree, site, md_10, md_14, md_15, md_17, md_19, md_21, md_29, md_33, md_37)
Mypairs.color(md_wide[,grep("md",colnames(md_wide))], color.var = md_wide$site)

# ### 2022 after cleaning
# wp22 <- read.csv(here("processed-data","wp_wide_cleanedup_20240516.csv"))
# pd22_wide <- pivot_wider(wp22, id_cols=c(tree, site), names_from=week, values_from=pd, names_prefix="pd_")
# Mypairs.color(pd22_wide[,grep("pd",colnames(pd22_wide))], color.var = factor(pd22_wide$site))


# or less averaged version that still keeps species
wp22.all <- read.csv(here("processed-data","wp_cleanedup_20240516.csv"))
wp22 <- wp22.all %>% group_by(tree, site, plot, species, week, time) %>% summarise(mMPa = mean(mpa, na.rm=T), sdMPa=sd(mpa, na.rm=T)) %>% mutate(year=2022)

wp22$mMPa.clean <- wp22$mMPa
wp22$mMPa.clean[which(wp22$sdMPa>0.5)] <- NA
wp22$mMPa.clean[which(wp22$week==19 & wp22$time=="pd" & wp22$tree==2381)] <- NA # this value is pretty clearly a dry outlier based on all forward and backward correlations


pd22_wide <- pivot_wider(wp22[which(wp22$time=="pd"),], id_cols=c(tree, site, plot, species, year), names_from=week, values_from=mMPa.clean, names_prefix="pd_")

pd22_wide <- pd22_wide %>% select(tree, site,plot, species,year, pd_10, pd_14, pd_17, pd_19, pd_21, pd_29, pd_33, pd_37)

Mypairs.color(pd22_wide[which(pd22_wide$species=="blue oak"),grep("pd",colnames(pd22_wide))], color.var = factor(pd22_wide$site))

Mypairs.color(pd22_wide[,grep("pd",colnames(pd22_wide))], color.var = factor(pd22_wide$species))



## 2023
wp23.all <- read.csv(here("processed-data","2023_wp.csv"))
wp23.all$ymd <- ymd(wp23.all$date)
wp23.all$week <- week(wp23.all$ymd)
wp23 <- wp23.all %>% group_by(tag, site, plot, ymd, week, species, year, pd_md) %>% summarise(mMPa = mean(water_potential_mpa, na.rm=T), sdMPa=sd(water_potential_mpa)) 

pd23_wide <- pivot_wider(wp23[which(wp23$pd_md=="pd"),], id_cols=c(tag, site, plot, species, year), names_from = week, values_from = mMPa, names_prefix = "pd_")
pd23_widem <- pivot_wider(wp23[which(wp23$pd_md=="pd"),], id_cols=c(tag, site, plot, species, year), names_from = week, values_from = mMPa, names_prefix = "pd23_")


Mypairs.color(pd23_wide[,grep("pd",colnames(pd23_wide))], color.var = factor(pd23_wide$species))

Mypairs.color(pd23_wide[which(pd23_wide$species=="blue oak"),grep("pd",colnames(pd23_wide))], color.var = factor(pd23_wide$site[which(pd23_wide$species=="blue oak")]))


plot(-1*pd_39~I(-1*pd_15), pd23_wide, pch=16, ylab=expression(paste("Sept ", Psi," (MPa)")), xlab=expression(paste("April ", Psi," (MPa)")))
abline(lm(-1*pd_39~ I(-1*pd_15), pd23_wide))
#abline(a=0,b=1, lty=2, col="gray")


logpds <- apply(pds %>% select(-), MARGIN=2, FUN=function(x){log(x, base=10)})

## joining 2022 and 2023
pds <- full_join(pd22_wide, pd23_widem, by=c("tree"="tag","plot","site", "species"))

plot(pd23_30~pd_29, pds, col=factor(species))
plot(pd23_39~pd_37, pds, col=factor(species))
ggplot(pds[which(pds$species=="blue oak"),], aes(x=pd23_39, y=pd_37, col=site)) + geom_point()


ggplot(pd23, aes(x=jitter(week), y=mPD, col=plot, shape=species)) + geom_point()+
  geom_line(data=pd23.plot, aes(x=week, y=PD, col=plot)) + facet_wrap(~species) 
ggplot(wp22[which(wp22$time=="pd" & wp22$species %in% c("blue oak","live oak")),], aes(x=jitter(week), y=mMPa, col=plot, shape=species)) + #geom_point() +
  geom_line(data=pd22.plot, aes(x=week, y=PD, col=plot)) +
  geom_line(data=pd23.plot, aes(x=week, y=PD, col=plot))+ facet_wrap(~species)


pd23.plot <- pd23 %>% group_by(site, species, week, plot) %>% summarise(PD = mean(mPD, na.rm=T), sdPD=sd(mPD, na.rm=T))
pd22.plot <- wp22 %>% filter(time=="pd" & species %in% c("blue oak","live oak")) %>% group_by(site, species, week, plot) %>% summarise(PD = mean(mMPa.clean, na.rm=T), sdPD=sd(mMPa.clean, na.rm=T))

# looking at plots through time
ggplot(wp22[which(wp22$time=="pd" & wp22$species %in% c("blue oak")),], aes(x=jitter(week), y=mMPa, col=plot, shape=species)) + #geom_point() +
  geom_line(data=pd22.plot[which(pd22.plot$species=="blue oak"),], aes(x=week, y=PD, col=plot)) +
  geom_line(data=pd23.plot[which(pd23.plot$species=="blue oak"),], aes(x=week, y=PD, col=plot), linetype=2)+ facet_wrap(~site)

# CHAMISE - shedd soil ALWAYS dryer by mid gs
# Cucu - Bottom ALWAYS drier by mid gs
# LL - Weathertop way drier in 23, but not in 22. Hobbiton consistently driest in 22.


wp22_pdmd <- wp22 %>% pivot_wider(id_cols = c("tree","site","plot","week","species"),names_from=time, values_from=mMPa)
wp23_pdmd <- wp23 %>% pivot_wider(id_cols = c("tag","site","plot","week","species"),names_from=pd_md, values_from=mMPa)



### hydroscape figure
library("viridis")
ggplot(wp22_pdmd[which(wp22_pdmd$species %in% c("live oak", "blue oak") ),], aes(x=-1*pd, y=-1*md, col=week)) + geom_point() + 
   geom_point(data=wp23_pdmd[which(wp23_pdmd$species %in% c("live oak","blue oak")),], shape=2)+ geom_abline(intercept=0, slope=1) +
  scale_color_viridis()+ facet_wrap(~factor(species))
# is there more seperation between weeks for blue oak, and more between trees (i.e. overlaping smear across weeks) for lives?

ggplot(wp22_pdmd[which(wp22_pdmd$species %in% c("live oak", "blue oak") & wp22_pdmd$week>15 ),], aes(x=-1*pd, y=-1*md, col=plot)) + geom_line(aes(group=tree)) + facet_wrap(~factor(species))

ggplot(wp22_pdmd[which(wp22_pdmd$species %in% c("live oak", "blue oak") & wp22_pdmd$week>15 ),], aes(x=-1*pd, y=-1*md, col=plot)) + geom_point(aes(col="white"))+ geom_smooth(method="lm", se=F, aes(group=tree)) + facet_wrap(~factor(species))
# there's a slight trend where higher delta_psi trees at hydrated pds end up with lower delta psi at drier pds?