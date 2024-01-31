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


### GHSL

Brazil <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/BR_UF_2021/BR_UF_2021.shp")
Brazil <- st_transform(Brazil, crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")

GHSL <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/GHSL_data_54009_shapefile/GHSL2_0_MWD_L1_tile_schema_land.shp")


Brazil <- st_transform(Brazil, crs = st_crs(GHSL))

#could mosaic, OR couple subset data by ghsl grid square and extract from that
schools <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_new_schools.csv")

schools$x.coord <- schools$x
schools$y.coord <- schools$y

schools.sf <- st_as_sf(schools, coords = c("x.coord","y.coord"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
schools.sf <- st_transform(schools.sf, crs = st_crs(GHSL))


both <- st_intersection(GHSL, Brazil)
tiles <- unique(both$tile_id)

tiles[1]

grid_schools <- list()
grid_ghsl <- list()
for (i in 1:length(tiles)){
    tile <- subset(both, tile_id == tiles[[i]])
    grid_schools[[i]] <- st_crop(schools.sf, tile)
    
    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_LAND_E2018_GLOBE_R2022A_54009_100_V1_0_", tile.id, ".tif", sep = ""))

    grid_schools_vect <- vect(grid_schools[[i]])
    grid_schools[[i]]$land <- as.data.frame(extract(grid_ghsl[[i]], grid_schools_vect))[,2]

}

schools <- as.data.frame(do.call(rbind, grid_schools))
names(schools)

schools$x.coord <- schools$x
schools$y.coord <- schools$y

schools.sf <- st_as_sf(schools, coords = c("x.coord","y.coord"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
schools.sf <- st_transform(schools.sf, crs = st_crs(GHSL))

plot(schools.sf$geometry, col = schools.sf$land)


save(schools.sf, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_wLand.RData")
write.csv(schools, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_schools_wLand.csv")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_wLand.RData")
schools <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_schools_wLand.csv")



rast1 <- rast("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R10_C13.tif")
hist(rast1)


grid_schools <- list()
grid_ghsl <- list()
for (i in 1:length(tiles)){
    tile <- subset(both, tile_id == tiles[[i]])
    grid_schools[[i]] <- st_crop(schools.sf, tile)
    
    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_", tile.id, ".tif", sep = ""))

    grid_schools_vect <- vect(grid_schools[[i]])
    grid_schools[[i]]$pop <- as.data.frame(extract(grid_ghsl[[i]], grid_schools_vect))[,2]

}

hist(grid_schools[[1]]$pop)

schools <- as.data.frame(do.call(rbind, grid_schools))

schools$x.coord <- schools$x
schools$y.coord <- schools$y

schools.sf <- st_as_sf(schools, coords = c("x.coord","y.coord"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
schools.sf <- st_transform(schools.sf, crs = st_crs(GHSL))

save(schools.sf, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_wLandPop.RData")
write.csv(schools, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_schools_wLandPop.csv")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_wLandPop.RData")
schools <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_schools_wLandPop.csv")


grid_schools <- list()
grid_ghsl <- list()
for (i in 1:length(tiles)){
    tile <- subset(both, tile_id == tiles[[i]])
    grid_schools[[i]] <- st_crop(schools.sf, tile)
    
    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_BUILT_S_E2025_GLOBE_R2023A_54009_100_V1_0_", tile.id, ".tif", sep = ""))

    grid_schools_vect <- vect(grid_schools[[i]])
    grid_schools[[i]]$built_s <- as.data.frame(extract(grid_ghsl[[i]], grid_schools_vect))[,2]

}

schools <- as.data.frame(do.call(rbind, grid_schools))
names(schools)

schools$x.coord <- schools$x
schools$y.coord <- schools$y

schools.sf <- st_as_sf(schools, coords = c("x.coord","y.coord"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
schools.sf <- st_transform(schools.sf, crs = st_crs(GHSL))

plot(schools.sf$geometry, col = schools.sf$built_s)

hist(schools$built_s)

save(schools.sf, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_wLandPopBS.RData")
write.csv(schools, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_schools_wLandPopBS.csv")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_wLandPopBS.RData")
schools <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_schools_wLandPopBS.csv")


grid_schools <- list()
grid_ghsl <- list()
for (i in 1:length(tiles)){
    tile <- subset(both, tile_id == tiles[[i]])
    grid_schools[[i]] <- st_crop(schools.sf, tile)
    
    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_BUILT_V_E2025_GLOBE_R2023A_54009_100_V1_0_", tile.id, ".tif", sep = ""))

    grid_schools_vect <- vect(grid_schools[[i]])
    grid_schools[[i]]$built_v <- as.data.frame(extract(grid_ghsl[[i]], grid_schools_vect))[,2]

}

schools <- as.data.frame(do.call(rbind, grid_schools))
names(schools)

schools$x.coord <- schools$x
schools$y.coord <- schools$y

schools.sf <- st_as_sf(schools, coords = c("x.coord","y.coord"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
schools.sf <- st_transform(schools.sf, crs = st_crs(GHSL))

plot(schools.sf$geometry, col = schools.sf$built_v)

hist(schools$built_s)

save(schools.sf, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_wLandPopBSV.RData")
write.csv(schools, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_schools_wLandPopBSV.csv")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_wLandPopBSV.RData")
schools <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_schools_wLandPopBSV.csv")


grid_schools <- list()
grid_ghsl <- list()
for (i in 1:length(tiles)){
    tile <- subset(both, tile_id == tiles[[i]])
    grid_schools[[i]] <- st_crop(schools.sf, tile)
    
    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_SMOD_E2025_GLOBE_R2023A_54009_1000_V1_0_", tile.id, ".tif", sep = ""))

    grid_schools_vect <- vect(grid_schools[[i]])
    grid_schools[[i]]$smod<- as.data.frame(extract(grid_ghsl[[i]], grid_schools_vect))[,2]

}

schools <- as.data.frame(do.call(rbind, grid_schools))
names(schools)

schools$x.coord <- schools$x
schools$y.coord <- schools$y

schools.sf <- st_as_sf(schools, coords = c("x.coord","y.coord"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
schools.sf <- st_transform(schools.sf, crs = st_crs(GHSL))

plot(schools.sf$geometry, col = schools.sf$built_v)

hist(schools$smod)

save(schools.sf, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_wLandPopBSVSMOD.RData")
write.csv(schools, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_schools_wLandPopBSVSMOD.csv")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_wLandPopBSVSMOD.RData")
schools <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_schools_wLandPopBSVSMOD.csv")


### DUC
DUC0.shp <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/GHSL_DUC_shapefile/gadm41_BRA_0.shp")
DUC0 <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_DUC_GLOBE_R2023A_V1_0_GADM41_2025_level0.csv")

names(DUC0)
names(DUC0.shp)

DUC1.shp <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/GHSL_DUC_shapefile/gadm41_BRA_1.shp")
DUC1 <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_DUC_GLOBE_R2023A_V1_0_GADM41_2025_level1.csv")

head(DUC1)

DUC2.shp <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/GHSL_DUC_shapefile/gadm41_BRA_2.shp")
DUC2 <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_DUC_GLOBE_R2023A_V1_0_GADM41_2025_level2.csv")


DUC0 <- subset(DUC0, GID_0GHSL == "BRA")
DUC1 <- subset(DUC1, GID_0GHSL == "BRA")
DUC2 <- subset(DUC2, GID_0GHSL == "BRA")

Brazil_DUC0 <- merge(DUC0.shp, DUC0, by = "GID_0")
Brazil_DUC1 <- merge(DUC1.shp, DUC1, by = "GID_1")
Brazil_DUC2 <- merge(DUC2.shp, DUC2, by = "GID_2")

names(Brazil_DUC0)
names(Brazil_DUC1)
names(Brazil_DUC2)

DUC0.df <- as.data.frame(Brazil_DUC0)
DUC1.df <- as.data.frame(Brazil_DUC1)
DUC2.df <- as.data.frame(Brazil_DUC2)


names(DUC0.df)
names(DUC1.df)
names(DUC2.df)

DUC12.df <- merge(DUC2.df, DUC1.df, by = "GID_1", all.x = TRUE, all.y = FALSE, suffixes = c("_adm2", "_adm1"))
DUC012.df <- merge(DUC12.df, DUC0.df, by.x = "GID_0_adm1", by.y = "GID_0", all.x = TRUE, all.y = FALSE, suffixes = c("", "_adm0"))

DUC.df <- DUC012.df
#write.csv(DUC.df, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHSL_DUC.csv")
save(DUC.df, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHSL_DUC.RData")

names(DUC.df)
names(DUC2.shp)

DUC.df <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHSL_DUC.csv")
load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHSL_DUC.RData")

both <- names(DUC2.shp)[!names(DUC2.shp) %in% names(DUC.df)]
both

DUC2.shp <- DUC2.shp[,c("GID_2", "geometry")]
names(DUC.df)

DUC.df <- st_drop_geometry(DUC.df)[,-c(38, 71, 96)]

DUC <- left_join(DUC2.shp, DUC.df, by = "GID_2")

names(DUC)
names(DUC)[order(names(DUC))]
#DUC <- DUC[,c("GID_1", "GID_2", )]

ggplot(DUC) +
    geom_sf(mapping = aes(fill = DEGURBA_L2)) + coord_sf()

### give named classifications
codes <- c(30,23,22,21,13,12,11,10)
smod_L2 <- c("Urban Centre", "Dense Urban Cluster", "Semi-Dense Urban Cluster", "Sururban or Per-Urban", "Rural Cluster", 
            "Low Density Rural", "Very Low Density", "Water")
smod_L2_color <- c(rgb(255,0,0,maxColorValue = 255), rgb(115,38,0,maxColorValue = 255) ,rgb(168,112,0,maxColorValue = 255) ,
                   rgb(255,255,0,maxColorValue = 255) ,rgb(55,86,35,maxColorValue = 255) ,rgb(171,205,102,maxColorValue = 255) ,
                   rgb(205,245,122,maxColorValue = 255) ,rgb(122,182,245, maxColorValue = 255))
smod_L2_color <- newpal(col = smod_L2_color, names = smod_L2)
smod_L2 <- as.data.frame(cbind(codes, smod_L2))
smod_L2$codes <- as.numeric(smod_L2$codes)


codes <- c(3,2,1)
smod_L1 <- c("Urban Centre", "Urban Cluster", "Rural")
smod_L1_color <- c(rgb(255,0,0,maxColorValue = 255) ,rgb(255,170,0,maxColorValue = 255) ,rgb(115,178,115, maxColorValue = 255))
smod_L1_color <- newpal(col = smod_L1_color, names = smod_L1)
smod_L1 <- as.data.frame(cbind(codes, smod_L1))
smod_L1$codes <- as.numeric(smod_L1$codes) 



codes <- c(30,23,22,21,13,12,11)
duc_L2 <- c("Urban Centre", "Dense Urban Cluster", "Semi-Dense Urban Cluster", "Sururban", "Rural Cluster", 
             "Low Density Rural", "Very Low Density")
breaks2 <- duc_L2
duc_L2_color <- c(rgb(255,0,0,maxColorValue = 255) ,rgb(115, 38, 0,maxColorValue = 255) ,rgb(168,112,0,maxColorValue = 255),
                  rgb(255,255,0,maxColorValue = 255) ,rgb(55,86,35,maxColorValue = 255) ,rgb(171,205,102,maxColorValue = 255) ,
                  rgb(205,245,122,maxColorValue = 255))
duc_L2_color <- newpal(col = duc_L2_color, names = duc_L2)
duc_L2 <- as.data.frame(cbind(codes, duc_L2))
duc_L2$codes <- as.numeric(duc_L2$codes)


codes <- c(3,2,1)
duc_L1 <- c("City", "Town", "Rural")
breaks1 <- duc_L1
duc_L1_color <- c(rgb(255,0,0,maxColorValue = 255) ,rgb(255,170,0,maxColorValue = 255) ,rgb(115,178,115, maxColorValue = 255))
duc_L1_color <- newpal(col = duc_L1_color, names = duc_L1)
duc_L1 <- as.data.frame(cbind(codes, duc_L1))
duc_L1$codes <- as.numeric(duc_L1$codes)

names(duc_L1)[1] <- "DEGURBA_L1_adm1"
DUC <- merge(DUC, duc_L1, by = "DEGURBA_L1_adm1", x.all = TRUE, y.all= FALSE)
names(duc_L2)[1] <- "DEGURBA_L2_adm2"
DUC <- left_join(DUC, duc_L2, by = "DEGURBA_L2_adm2")

save(DUC, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/DUC.RData")
load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/DUC.RData")

DUC$area.adm2 <- st_area(DUC)

duc_L1_color
DUC$duc_L1

duc1 <- ggplot(DUC) +
  geom_sf(mapping = aes(fill = duc_L1, color = duc_L1)) + coord_sf() +
  scale_color_manual(values = duc_L1_color, breaks = breaks1) +
  scale_fill_manual(values = duc_L1_color, breaks = breaks1) 

duc2 <- ggplot(DUC) +
  geom_sf(mapping = aes(fill = duc_L2, color = duc_L2)) + coord_sf() +
  scale_color_manual(values = duc_L2_color, breaks = breaks2) +
  scale_fill_manual(values = duc_L2_color, breaks = breaks2) 
duc2

ggarrange(duc1, duc2, nrow = 1)

### extract at schools
load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_wLandPopBSVSMOD.RData")

schools.sf <- st_as_sf(schools.sf, coords = c("x.coord","y.coord"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
schools.sf <- st_transform(schools.sf, crs = st_crs(GHSL))

DUC.sf <- st_as_sf(DUC)
st_extract(DUC.sf, schools.sf)

schools <- as.data.frame(schools.sf)

DUC_plot <- ggplot(DUC) +
    geom_sf(mapping = aes(fill = duc_L2, color = duc_L2)) + coord_sf() +
    scale_color_manual(values = duc_L2_color, breaks = breaks2) +
    scale_fill_manual(values = duc_L2_color, breaks = breaks2) #+
    geom_point(schools, mapping = aes(x = x, y = y, alpha = 0.5))

ggsave(DUC_plot, file = "/home/azureuser/cloudfiles/code/Users/ariley/R-Code/Plots/DUC_L2_Brazil.png")

DUC_plot2 <- ggplot(DUC) +
    geom_sf(mapping = aes(fill = duc_L2)) + coord_sf() +
    scale_fill_manual(values = duc_L2_color, breaks = breaks2) #+
    geom_point(schools, mapping = aes(x = x, y = y, alpha = 0.5))

ggsave(DUC_plot2, file = "/home/azureuser/cloudfiles/code/Users/ariley/R-Code/Plots/DUC_L2_Brazil2.png")


DUC.r <- st_rasterize(DUC)
DUC.r <- st_transform(DUC.r, st_crs(schools.sf))
st_crs(DUC.r)
st_crs(schools.sf)


names(DUC)
head(DUC)
head(schools.sf)

names(DUC.df)

Brazil <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/Brazil_Admin_0_to_2/bra_admbnda_adm2_ibge_2020.shp")
st_crs(Brazil) <- "+proj=longlat +ellps=GRS80 +no_defs +type=crs" ##from source
Brazil <- st_transform(Brazil, crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
#st_crs(Brazil) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"
Brazil <- st_transform(Brazil, crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
muni <- Brazil[,c("ADM2_PT")]
muni <- cbind(muni, 1:nrow(muni))
names(muni)[2] <- "ID.muni"
muni.df <- st_drop_geometry(muni)

schools.sf <- left_join(schools.sf, muni.df, by = "ID.muni")
names(schools.sf)

DUC.df <- as.data.frame(st_drop_geometry(DUC))

schools.sf <- merge(schools.sf, DUC.df, x.by = "ADM2_PT", y.by = "NAME_2", x.all = TRUE) #, relationship = "many-to-many")
hist(schools.sf$area.adm2)

ggplot(schools.sf) +
    geom_sf(mapping = aes(fill = duc_L2, color = duc_L2)) + coord_sf() +
    scale_color_manual(values = duc_L2_color, breaks = breaks2) +
    scale_fill_manual(values = duc_L2_color, breaks = breaks2) 

save(schools.sf, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_wGHSL.RData")
load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_wGHSL.RData")
head(schools.sf)
names(schools.sf)

st_crs(schools.sf)
schools.sf$lon <- st_coordinates(schools.sf)[,1]
schools.sf$lat <- st_coordinates(schools.sf)[,2]
schools.df <- st_drop_geometry(schools.sf)
head(schools.df)

names(schools.sf)
unique(names(schools.sf))

st_write(schools.sf, dsn = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_wGHSL.gpkg", driver = "gpkg")

library(csv)
write.csv(schools.df, "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_wGHSL.csv")
schools.df <- read.csv("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_wGHSL.csv")
names(schools.df)

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_wGHSL.RData")

head(schools.sf)
schools.sf$lon <- st_coordinates(schools.sf)[,1]
schools.sf$lat <- st_coordinates(schools.sf)[,2]
schools.df <- st_drop_geometry(schools.sf)
locations <- schools.df[,c("name", "lon", "lat", "coord_source")]
write.csv(locations, "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_locations.csv")


head(schools.sf)
head(Brazil)
unique(schools.sf$NAME_1_adm1)
NE_Brazil <- subset(Brazil, NM_REGIAO == "Nordeste")
NE_schools <- st_crop(schools.sf, NE_Brazil)

#schools.df <- st_drop_geometry(schools.sf)

#school_points <- st_as_sf(as.data.frame(st_coordinates(schools.sf)), coords = c("X", "Y"), crs = st_crs(GHSL))
#NE_points <- st_intersection(school_points, NE_Brazil)
#NE_schools <- left_join(NE_points, schools.df)

NE_schools$lon <- st_coordinates(NE_schools)[,1]
NE_schools$lat <- st_coordinates(NE_schools)[,2]
NE_schools <- st_drop_geometry(NE_schools)

write.csv(NE_schools, "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/NE_schools_wGHSL.csv")

all.schools <- read.csv("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/NE_schools_wGHSL.csv")


range(NE_schools$lon)
range(NE_schools$lat)


L1 <- unique(schools.sf$NAME_1_adm1)
p <- list()
for (i in 1:27){
    DUC_L1 <- subset(DUC, NAME_1_adm1 == L1[i])
    schools_L1 <- subset(schools.sf, NAME_1_adm1 == L1[i])
    p[[i]] <- ggplot(DUC_L1) +
                geom_sf(mapping = aes(fill = duc_L2, color = duc_L2)) + coord_sf() +
                scale_color_manual(values = duc_L2_color, breaks = breaks2) +
                scale_fill_manual(values = duc_L2_color, breaks = breaks2) #+
                #theme_void()

}

all_regions <- ggarrange(plotlist = p, nrow = 9, ncol = 3, common.legend = TRUE)
ggsave(all_regions, file = "/home/azureuser/cloudfiles/code/Users/ariley/R-Code/Plots/DUC_L2.png")

names(DUC)
#Distrito Federal (25) has no municipalities
L1[26]

DUC_L1 <- subset(DUC, NAME_1_adm1 == L1[26])
head(DUC_L1)
unique(DUC_L1$NAME_2)
Pernambuco

schools_L1 <- subset(schools.sf, NAME_1_adm1 == L1[26])
ggplot(DUC_L1) +
    geom_sf(mapping = aes(fill = NAME_2, color = NAME_2)) + coord_sf() #+
    xlim(c(-21, -20)) + ylim(c(-30,-28))

st_bbox(DUC_L1)
View(DUC_L1)

locs <- cbind(as.data.frame(schools_L1$NAME_2), st_coordinates(schools_L1))
View(locs)


Viana
Anchieta
Afonso Cláudio
Linhares
Boa Esperança

BE <- subset(DUC, NAME_2 == "Boa Esperança")

ggplot(BE) +
    geom_sf(mapping = aes(fill = duc_L2, color = duc_L2)) + coord_sf()

No_BE_DUC <- subset(DUC, NAME_2 != "Boa Esperança")
No_BE_schools.sf <- subset(schools.sf, ADM2_PT != "Boa Esperança")

L1 <- unique(No_BE_schools.sf$NAME_1_adm1)
p <- list()
for (i in 1:27){
    DUC_L1 <- subset(No_BE_DUC, NAME_1_adm1 == L1[i])
    schools_L1 <- subset(No_BE_schools.sf, NAME_1_adm1 == L1[i])
    p[[i]] <- ggplot(DUC_L1) +
                geom_sf(mapping = aes(fill = duc_L2, color = duc_L2)) + coord_sf() +
                scale_color_manual(values = duc_L2_color, breaks = breaks2) +
                scale_fill_manual(values = duc_L2_color, breaks = breaks2) #+
                theme_void()

}

all_regions <- ggarrange(plotlist = p, nrow = 9, ncol = 3, common.legend = TRUE)
all_regions
