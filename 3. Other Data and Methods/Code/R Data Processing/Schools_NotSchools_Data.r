## GHSL data distribution

library(sf)
library(ggplot2)

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_wGHSL.RData")
load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_neg_samp_wGHSL.RData")


png(file = "/home/azureuser/cloudfiles/code/Users/ariley/R-Code/Plots/School_NotSchool_Covars.png",
width = 1200, height = 1800)

par(mfrow = c(6,3))

hist(schools.sf$land)
hist(schools.sf$pop)
hist(schools.sf$built_s)
hist(schools.sf$built_v)
hist(schools.sf$smod)
hist(schools.sf$DEGURBA_L2_adm2)
hist(schools.sf$DEGURBA_L1_adm1)
hist(schools.sf$Tot_Pop_adm2)
hist(schools.sf$Tot_Pop_adm1)


hist(neg_samp.sf$land)
hist(neg_samp.sf$pop)
hist(neg_samp.sf$built_s)
hist(neg_samp.sf$built_v)
hist(neg_samp.sf$smod)
hist(neg_samp.sf$DEGURBA_L2_adm2)
hist(neg_samp.sf$DEGURBA_L1_adm1)
hist(neg_samp.sf$Tot_Pop_adm2)
hist(neg_samp.sf$Tot_Pop_adm1)

dev.off()

#ggarrange these
ggplot(schools.sf, mapping = aes(smod, smod)) +
    geom_bar(stat = "identity", width = 1)

ggplot(schools.sf, mapping = aes(DEGURBA_L2_adm2, DEGURBA_L2_adm2)) +
    geom_bar(stat = "identity", width = 1)

ggplot(schools.sf, mapping = aes(DEGURBA_L1_adm1, DEGURBA_L1_adm1)) +
    geom_bar(stat = "identity", width = 1)

ggplot(neg_samp.sf, mapping = aes(smod, smod)) +
    geom_bar(stat = "identity", width = 1)

ggplot(neg_samp.sf, mapping = aes(DEGURBA_L2_adm2, DEGURBA_L2_adm2)) +
    geom_bar(stat = "identity", width = 1)

ggplot(neg_samp.sf, mapping = aes(DEGURBA_L1_adm1, DEGURBA_L1_adm1)) +
    geom_bar(stat = "identity", width = 1)
