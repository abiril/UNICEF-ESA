## Load packages

library(ggplot2)
library(sf)
library(dplyr)
library(rquery)
library(terra)
library(tidyterra)
library(stars)
library(unikn)


load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_schools_wGHSL.RData")

schools <- schools.sf
not_schools <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Samples/south-america_sampled_not-schools.csv")
nature <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Samples/south-america_sampled_nature.csv")

head(not_schools)
head(nature)


Brazil <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/BR_UF_2021/BR_UF_2021.shp")
Brazil <- st_transform(Brazil, crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")

not_schools$x <- not_schools$lon
not_schools$y <- not_schools$lat

nature$x <- nature$lon
nature$y <- nature$lat

not_schools.sf <- st_as_sf(not_schools, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
nature.sf <- st_as_sf(nature, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")

not_schools <- st_intersection(not_schools.sf, Brazil)
nature <- st_intersection(nature.sf, Brazil)

## 10000 not schools - 3829 in brazil
## 5000 nature - 1043 in brazil

nrow(not_schools)
nrow(nature)

names(schools)

ggplot(Brazil) +
    geom_sf() + coord_sf() +
    geom_point(schools, mapping = aes(x = x, y = y), col = "red") +
    geom_point(not_schools, mapping = aes(x = x, y = y), col = "blue") +
    geom_point(nature, mapping = aes(x = x, y = y), col = "green")


names(not_schools)
names(nature)

not_schools$not_schools <- 1
not_schools$nature <- 0
nature$not_schools <- 1
nature$nature <- 1

names_left_nature <- names(nature)[!(names(nature) %in% names(not_schools))]
names_left_not_schools <- names(not_schools)[!(names(not_schools) %in% names(nature))]

not_schools[,names_left_nature] <- NA
nature[,names_left_not_schools] <- NA

nature <- nature[, names(not_schools)]

neg_samp <- rbind(not_schools, nature)
neg_samp.sf <- st_as_sf(neg_samp)

head(neg_samp)

write.csv(neg_samp, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Negative_Sample.csv")

neg_samp <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Negative_Sample.csv")

### let's get some data here
GHSL <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/GHSL_data_54009_shapefile/GHSL2_0_MWD_L1_tile_schema_land.shp")
Brazil <- st_transform(Brazil, crs = st_crs(GHSL))

neg_samp.sf <- st_transform(neg_samp.sf, crs = st_crs(GHSL))

both <- st_intersection(GHSL, Brazil)
tiles <- unique(both$tile_id)

grid_neg_samp <- list()
grid_ghsl <- list()
for (i in 1:20){
    tile <- subset(both, tile_id == tiles[[i]])
    grid_neg_samp[[i]] <- st_crop(neg_samp.sf, tile)
    
    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_LAND_E2018_GLOBE_R2022A_54009_100_V1_0_", tile.id, ".tif", sep = ""))

    grid_neg_samp_vect <- vect(grid_neg_samp[[i]])
    grid_neg_samp[[i]]$land <- as.data.frame(extract(grid_ghsl[[i]], grid_neg_samp_vect))[,2]

}

neg_samp <- as.data.frame(do.call(rbind, grid_neg_samp))
names(neg_samp)

neg_samp$x.coord <- neg_samp$x
neg_samp$y.coord <- neg_samp$y

neg_samp.sf <- st_as_sf(neg_samp, coords = c("x.coord","y.coord"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
neg_samp.sf <- st_transform(neg_samp.sf, crs = st_crs(GHSL))

plot(neg_samp.sf$geometry, col = neg_samp.sf$land)


save(neg_samp.sf, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_neg_samp_wLand.RData")
write.csv(neg_samp, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_neg_samp_wLand.csv")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_neg_samp_wLand.RData")
neg_samp <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_neg_samp_wLand.csv")



rast1 <- rast("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R10_C13.tif")
hist(rast1)


grid_neg_samp <- list()
grid_ghsl <- list()
for (i in 1:20){
    tile <- subset(both, tile_id == tiles[[i]])
    grid_neg_samp[[i]] <- st_crop(neg_samp.sf, tile)
    
    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_", tile.id, ".tif", sep = ""))

    grid_neg_samp_vect <- vect(grid_neg_samp[[i]])
    grid_neg_samp[[i]]$pop <- as.data.frame(extract(grid_ghsl[[i]], grid_neg_samp_vect))[,2]

}

hist(grid_neg_samp[[1]]$pop)

neg_samp <- as.data.frame(do.call(rbind, grid_neg_samp))

neg_samp$x.coord <- neg_samp$x
neg_samp$y.coord <- neg_samp$y

neg_samp.sf <- st_as_sf(neg_samp, coords = c("x.coord","y.coord"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
neg_samp.sf <- st_transform(neg_samp.sf, crs = st_crs(GHSL))

save(neg_samp.sf, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_neg_samp_wLandPop.RData")
write.csv(neg_samp, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_neg_samp_wLandPop.csv")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_neg_samp_wLandPop.RData")
neg_samp <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_neg_samp_wLandPop.csv")


grid_neg_samp <- list()
grid_ghsl <- list()
for (i in 1:20){
    tile <- subset(both, tile_id == tiles[[i]])
    grid_neg_samp[[i]] <- st_crop(neg_samp.sf, tile)
    
    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_BUILT_S_E2025_GLOBE_R2023A_54009_100_V1_0_", tile.id, ".tif", sep = ""))

    grid_neg_samp_vect <- vect(grid_neg_samp[[i]])
    grid_neg_samp[[i]]$built_s <- as.data.frame(extract(grid_ghsl[[i]], grid_neg_samp_vect))[,2]

}

neg_samp <- as.data.frame(do.call(rbind, grid_neg_samp))
names(neg_samp)

neg_samp$x.coord <- neg_samp$x
neg_samp$y.coord <- neg_samp$y

neg_samp.sf <- st_as_sf(neg_samp, coords = c("x.coord","y.coord"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
neg_samp.sf <- st_transform(neg_samp.sf, crs = st_crs(GHSL))

plot(neg_samp.sf$geometry, col = neg_samp.sf$built_s)

hist(neg_samp$built_s)

save(neg_samp.sf, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_neg_samp_wLandPopBS.RData")
write.csv(neg_samp, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_neg_samp_wLandPopBS.csv")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_neg_samp_wLandPopBS.RData")
neg_samp <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_neg_samp_wLandPopBS.csv")


grid_neg_samp <- list()
grid_ghsl <- list()
for (i in 1:20){
    tile <- subset(both, tile_id == tiles[[i]])
    grid_neg_samp[[i]] <- st_crop(neg_samp.sf, tile)
    
    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_BUILT_V_E2025_GLOBE_R2023A_54009_100_V1_0_", tile.id, ".tif", sep = ""))

    grid_neg_samp_vect <- vect(grid_neg_samp[[i]])
    grid_neg_samp[[i]]$built_v <- as.data.frame(extract(grid_ghsl[[i]], grid_neg_samp_vect))[,2]

}

neg_samp <- as.data.frame(do.call(rbind, grid_neg_samp))
names(neg_samp)

neg_samp$x.coord <- neg_samp$x
neg_samp$y.coord <- neg_samp$y

neg_samp.sf <- st_as_sf(neg_samp, coords = c("x.coord","y.coord"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
neg_samp.sf <- st_transform(neg_samp.sf, crs = st_crs(GHSL))

plot(neg_samp.sf$geometry, col = neg_samp.sf$built_v)

hist(neg_samp$built_s)

save(neg_samp.sf, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_neg_samp_wLandPopBSV.RData")
write.csv(neg_samp, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_neg_samp_wLandPopBSV.csv")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_neg_samp_wLandPopBSV.RData")
neg_samp <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_neg_samp_wLandPopBSV.csv")


grid_neg_samp <- list()
grid_ghsl <- list()
for (i in 1:20){
    tile <- subset(both, tile_id == tiles[[i]])
    grid_neg_samp[[i]] <- st_crop(neg_samp.sf, tile)
    
    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_SMOD_E2025_GLOBE_R2023A_54009_1000_V1_0_", tile.id, ".tif", sep = ""))

    grid_neg_samp_vect <- vect(grid_neg_samp[[i]])
    grid_neg_samp[[i]]$smod<- as.data.frame(extract(grid_ghsl[[i]], grid_neg_samp_vect))[,2]

}

neg_samp <- as.data.frame(do.call(rbind, grid_neg_samp))
names(neg_samp)

neg_samp$x.coord <- neg_samp$x
neg_samp$y.coord <- neg_samp$y

neg_samp.sf <- st_as_sf(neg_samp, coords = c("x.coord","y.coord"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
neg_samp.sf <- st_transform(neg_samp.sf, crs = st_crs(GHSL))

plot(neg_samp.sf$geometry, col = neg_samp.sf$built_v)

hist(neg_samp$smod)

save(neg_samp.sf, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_neg_samp_wLandPopBSVSMOD.RData")
write.csv(neg_samp, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_neg_samp_wLandPopBSVSMOD.csv")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_neg_samp_wLandPopBSVSMOD.RData")
neg_samp <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_neg_samp_wLandPopBSVSMOD.csv")

### extract at neg_samp
load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_neg_samp_wLandPopBSVSMOD.RData")

neg_samp.sf <- st_as_sf(neg_samp.sf, coords = c("x.coord","y.coord"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
neg_samp.sf <- st_transform(neg_samp.sf, crs = st_crs(GHSL))

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/DUC.RData")

DUC.sf <- st_as_sf(DUC)

neg_samp <- as.data.frame(neg_samp.sf)

DUC.r <- st_rasterize(DUC)
DUC.r <- st_transform(DUC.r, st_crs(neg_samp.sf))

names(neg_samp)
head(neg_samp)
unique(neg_samp$CD_UF)

Brazil <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/Brazil_Admin_0_to_2/bra_admbnda_adm2_ibge_2020.shp")
st_crs(Brazil) <- "+proj=longlat +ellps=GRS80 +no_defs +type=crs" ##from source
Brazil <- st_transform(Brazil, crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
muni <- Brazil[,c("ADM2_PT")]
muni <- cbind(muni, 1:nrow(muni))
names(muni)[2] <- "ID.muni"
muni.df <- st_drop_geometry(muni)
muni.r <- st_rasterize(muni)

neg_samp.sf <- st_transform(neg_samp.sf, crs = st_crs(muni.r))
extract <- st_extract(muni.r, neg_samp.sf)

neg_samp.sf <- cbind(neg_samp.sf, st_extract(muni.r, neg_samp.sf)[1])

DUC.df <- as.data.frame(st_drop_geometry(DUC))

neg_samp.sf <- merge(neg_samp.sf, DUC.df, x.by = "ADM2_PT", y.by = "NAME_2", x.all = TRUE) 

save(neg_samp.sf, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_neg_samp_wGHSL.RData")
load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_neg_samp_wGHSL.RData")

