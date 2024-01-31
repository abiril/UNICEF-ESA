## Load packages

library(ggplot2)
library(ggpubr)
#library(mapview)
library(readr)
library(dplyr)
library(tidygeocoder)
library(tidyr)
#library(sp)
library(Rcpp)
library(sf)
library(stars)
library(terra)
library(tidyterra)
library(viridis)
library(grDevices)
library(unikn)
library(ggpubr)
library(plyr)

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wLandPopBSVSMOD.RData")

head(grid.all)

grid.all$x.10 <- round_any(grid.all$x, 1000, f = floor)
grid.all$y.10 <- round_any(grid.all$y, 1000, f = floor)

grid.all$grid.id <- as.numeric(factor(paste0(grid.all$x.10, grid.all$y.10)))
save(grid.all, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_10km_wLandPopBSVSMOD_wID.RData")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_10km_wLandPopBSVSMOD_wID.RData")

grid.10 <- distinct(grid.all[,c("grid.id", "x.10", "y.10")])

save(grid.10, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_10km.RData")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_10km.RData")

land.10 <- aggregate(grid.all$land, by = list(grid.all$grid.id), FUN = mean) # 0/1 indicator to % lang
names(land.10) <- c("grid.id", "land")
grid.10 <- left_join(grid.10, land.10, by = "grid.id")
save(grid.10, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_10km_wLand.RData")

pop.10 <- aggregate(grid.all$pop, by = list(grid.all$grid.id), FUN = sum) # sum of population
names(pop.10) <- c("grid.id", "pop")
grid.10 <- left_join(grid.10, pop.10, by = "grid.id")
save(grid.10, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_10km_wLandPop.RData")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_10km_wLandPop.RData")

built_s.10 <- aggregate(grid.all$built_s, by = list(grid.all$grid.id), FUN = mean) # avergae %
names(built_s.10) <- c("grid.id", "built_s")
grid.10 <- left_join(grid.10, built_s.10, by = "grid.id")
save(grid.10, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_10km_wLandPopBS.RData")

built_v.10 <- aggregate(grid.all$built_v, by = list(grid.all$grid.id), FUN = mean) # average %
names(built_v.10) <- c("grid.id", "built_v")
grid.10 <- left_join(grid.10, built_v.10, by = "grid.id")

save(grid.10, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_10km_wLandPopBSV.RData")
load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_10km_wLandPopBSV.RData")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_10km_wLandPopBSVSMOD_wID.RData")



smod <- as.data.frame(cbind(c(10,11,12,13,21,22,23,30), c("water", "very low density rural", "low density rural", "rural cluster", "suburban or per-urban", "semi-dense urban cluster", "dense urban cluster", "urban centre")))
names(smod) <- c("smod", "smod_cat")
smod$smod <- as.integer(smod$smod)
unique(smod$smod)
unique(grid.all$smod)
smod$smod %in% grid.all$smod

grid.all2 <- merge(grid.all, smod, by = "smod")

save(grid.all file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_10km_all.RData")
head(grid.all)

for (i in 1:1){
    grid.cell <- subset(grid.all, grid.10 == i)
    smod.10 <- aggregate(grid.cell$smod, by = list(grid.cell$grid.id), FUN = count)

    grid.data <- c(grid.all[1,c("x.10", "y.10")])

}
head(grid.all$grid.10)


write.csv(grid.all, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wLandPopBSVSMOD.csv")

Brazil <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/Brazil_Admin_0_to_2/bra_admbnda_adm2_ibge_2020.shp", crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
Brazil <- st_transform(Brazil, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs")

NE_muni <- c("Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia")

NE_Brazil <- subset(Brazil, ADM1_PT %in% NE_muni)
st_bbox(NE_Brazil)


### school counts
schools <- read.csv("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_school_density_10km.csv")
head(schools)

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_10km_wLandPopBS.RData")
head(grid.10)

school_counts <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/ne_brazil_school_density_10km.gpkg")
school_counts <- school_counts[,-c(2,3,4,5)]
school_counts$centroid <- st_centroid(school_counts)
school_counts$x <- st_coordinates(school_counts$centroid)[,1]
school_counts$y <- st_coordinates(school_counts$centroid)[,2]
names(school_counts)
school_df <- st_drop_geometry(school_counts)[,c("id", "NUMPOINTS", "x", "y")]

names(grid.10) <- c("grid.id", "x", "y", "land", "pop", "built_s")
grid_df <- grid.10
grid_sf <- st_as_sf(grid_df, coords = c("x", "y"), crs = st_crs(school_counts))
names(grid_sf)
grid_sf <- cbind(as.data.frame(grid_sf, xy = TRUE), st_coordinates(grid_sf))
names(grid_sf)[c(6,7)] <- c("x", "y")
#grid_df <- st_drop_geometry(grid_df)[,-5]
#names(grid_df)[c(5,6)] <- c("x", "y")

grid_sf <- st_drop_geometry(grid_sf)[,c("x", "y", "land", "pop", "built_s")]
grid_raster <- rast(grid_sf, type = "xyz")


school_sf <- st_as_sf(school_df, coords = c("x", "y"), crs = st_crs(school_counts))

school_data <- cbind(school_sf, extract(grid_raster, school_sf))

school_data$x <- st_coordinates(school_data)[,1]
school_data$y <- st_coordinates(school_data)[,2]
head(school_data)

save(school_data, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_school_count_data.RData")

school_df <- st_drop_geometry(school_data)
head(school_df)

write.csv(school_df, "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/ne_brazil_school_density_10km_wLandPopBS.csv")
