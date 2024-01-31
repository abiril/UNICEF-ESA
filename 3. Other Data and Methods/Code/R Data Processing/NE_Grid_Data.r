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
NE_Brazil <- subset(Brazil, NM_REGIAO == "Nordeste")

names(NE_Brazil)
NE_munis <- unique(NE_Brazil$NM_UF)
NE_regions <- unique(NE_Brazil$SIGLA)

both <- st_intersection(GHSL, NE_Brazil)
tiles <- unique(both$tile_id)

tiles[1]

grid_ghsl <- list()
grid_df <- list()
for (i in 1:length(tiles)){
    tile <- subset(both, tile_id == tiles[[i]])
    
    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_LAND_E2018_GLOBE_R2022A_54009_100_V1_0_", tile.id, ".tif", sep = ""))

    grid_df[[i]] <- as.data.frame(grid_ghsl[[i]], xy = TRUE)
    names(grid_df[[i]])[3] <- "land"
}

write.csv(grid_df, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_wLand.csv")

grid_all <-  as.data.frame(do.call(rbind, grid_df))
write.csv(grid_all, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_wLand.csv")


grid_ghsl <- list()
for (i in 1:length(tiles)){
    tile <- subset(both, tile_id == tiles[[i]])
    
    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_", tile.id, ".tif", sep = ""))

    pop_df <- as.data.frame(grid_ghsl[[i]], xy = TRUE)
    names(pop_df)[3] <- "pop"

    grid_df[[i]] <- left_join(grid_df[[i]], pop_df, by = c("x", "y"))
}

save(grid_df, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_wLandPop.RData")

grid_all <-  as.data.frame(do.call(rbind, grid_df))
write.csv(grid_all, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_wLandPop.csv")

head(grid_all)
hist(grid_all$land)
hist(grid_all$pop)

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_wLandPop.RData")
grid_all <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_wLandPop.csv")

grid_ghsl <- list()
for (i in 1:length(tiles)){
    tile <- subset(both, tile_id == tiles[[i]])

    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_BUILT_S_E2025_GLOBE_R2023A_54009_100_V1_0_", tile.id, ".tif", sep = ""))

    built_s_df <- as.data.frame(grid_ghsl[[i]], xy = TRUE)
    names(built_s_df)[3] <- "built_s"

    grid_df[[i]] <- left_join(grid_df[[i]], built_s_df, by = c("x", "y"))

}

save(grid_df, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_wLandPopBS.RData")

grid_all <-  as.data.frame(do.call(rbind, grid_df))
write.csv(grid_all, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_wLandPopBS.csv")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_wLandPopBS.RData")
grid_all <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_wLandPopBS.csv")

grid_ghsl <- list()
for (i in 1:length(tiles)){
    tile <- subset(both, tile_id == tiles[[i]])
    
    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_BUILT_V_E2025_GLOBE_R2023A_54009_100_V1_0_", tile.id, ".tif", sep = ""))

    built_v_df <- as.data.frame(grid_ghsl[[i]], xy = TRUE)
    names(built_v_df)[3] <- "built_v"

    grid_df[[i]] <- left_join(grid_df[[i]], built_v_df, by = c("x", "y"))

}

save(grid_df, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_wLandPopBSV.RData")

grid_all <-  as.data.frame(do.call(rbind, grid_df))
write.csv(grid_all, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_wLandPopBSV.csv")


grid_ghsl <- list()
for (i in 1:length(tiles)){
    tile <- subset(both, tile_id == tiles[[i]])
    
    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_SMOD_E2025_GLOBE_R2023A_54009_1000_V1_0_", tile.id, ".tif", sep = ""))

    smod_df <- as.data.frame(grid_ghsl[[i]], xy = TRUE)
    names(smod_df)[3] <- "smod"

    grid_df[[i]] <- left_join(grid_df[[i]], smod_df, by = c("x", "y"))

}

save(grid_df, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_wLandPopBSVSMOD.RData")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_wLandPopBSVSMOD.RData")

grid_all <-  as.data.frame(do.call(rbind, grid_df))
write.csv(grid_all, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_wLandPopBSVSMOD.csv")


