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

plot(PD_14~PD_17, pd_wide, pch=16, col=factor(site))
plot(PD_21~PD_29, pd_wide)


ggplot(pd_wide, aes(x=PD_17, y=PD_21, col=factor(site))) + geom_point()


md <- data_og %>% filter(time=="md" & week %in% c(10,14,15,17,19,21,29,33, 37)) %>% select(tree, week, water_potential, site)

md_wide <- pivot_wider(md, id_cols = c(tree, site),names_from = week, values_from = water_potential,names_prefix = "md_")

md_wide <- md_wide %>% select(tree, site, md_10, md_14, md_15, md_17, md_19, md_21, md_29, md_33, md_37)
Mypairs.color(md_wide[,grep("md",colnames(md_wide))], color.var = md_wide$site)



ggplot(pd_wide, aes(x=PD_17, y=PD_29, col=factor(site))) + geom_point() + geom_abline(intercept=0, slope=1)
ggplot(pd_wide, aes(x=PD_21, y=PD_29, col=factor(site))) + geom_point() + geom_abline(intercept=0, slope=1)


## 2023
wp23 <- read.csv(here("processed-data","2023_wp.csv"))
wp23$ymd <- ymd(wp23$date)
wp23$week <- week(wp23$ymd)
pd23 <- wp23 %>% filter(pd_md=="pd") %>% group_by(tag, site, plot, ymd, week, species, year) %>% summarise(mPD = mean(water_potential_mpa, na.rm=T), sdPD=sd(water_potential_mpa))

pd23_wide <- pivot_wider(pd23, id_cols=c(tag, site, plot, species, year), names_from = week, values_from = mPD, names_prefix = "pd_")

Mypairs.color(pd23_wide[,grep("pd",colnames(pd23_wide))], color.var = factor(pd23_wide$species))
