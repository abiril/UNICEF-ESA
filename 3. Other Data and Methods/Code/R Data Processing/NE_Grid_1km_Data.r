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
tiles
tiles[1]

grid_ghsl <- list()
grid_df <- list()
for (i in 1:length(tiles)){
    tile <- subset(both, tile_id == tiles[[i]])
    
    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL_1km/GHS_LAND_E2018_GLOBE_R2022A_54009_1000_V1_0_", tile.id, ".tif", sep = ""))

    grid_df[[i]] <- as.data.frame(grid_ghsl[[i]], xy = TRUE)
    names(grid_df[[i]])[3] <- "land"
}

write.csv(grid_df, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wLand.csv")

grid_all <-  as.data.frame(do.call(rbind, grid_df))
write.csv(grid_all, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wLand.csv")


grid_ghsl <- list()
for (i in 1:length(tiles)){
    tile <- subset(both, tile_id == tiles[[i]])
    
    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL_1km/GHS_POP_E2025_GLOBE_R2023A_54009_1000_V1_0_", tile.id, ".tif", sep = ""))

    pop_df <- as.data.frame(grid_ghsl[[i]], xy = TRUE)
    names(pop_df)[3] <- "pop"

    grid_df[[i]] <- left_join(grid_df[[i]], pop_df, by = c("x", "y"))
}

save(grid_df, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wLandPop.RData")

grid_all <-  as.data.frame(do.call(rbind, grid_df))
write.csv(grid_all, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wLandPop.csv")

head(grid_all)
hist(grid_all$land)
hist(grid_all$pop)

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wLandPop.RData")
grid_all <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wLandPop.csv")

grid_ghsl <- list()
for (i in 1:length(tiles)){
    tile <- subset(both, tile_id == tiles[[i]])

    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL_1km/GHS_BUILT_S_E2025_GLOBE_R2023A_54009_1000_V1_0_", tile.id, ".tif", sep = ""))

    built_s_df <- as.data.frame(grid_ghsl[[i]], xy = TRUE)
    names(built_s_df)[3] <- "built_s"

    grid_df[[i]] <- left_join(grid_df[[i]], built_s_df, by = c("x", "y"))

}
save(grid_df, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wLandPopBS.RData")

grid_all <-  as.data.frame(do.call(rbind, grid_df))
write.csv(grid_all, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_wLandPopBS.csv")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_wLandPopBS.RData")
grid_all <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_wLandPopBS.csv")

head(grid_df[[1]])
head(grid_all)

grid_ghsl <- list()
for (i in 1:length(tiles)){
    tile <- subset(both, tile_id == tiles[[i]])
    
    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL_1km/GHS_BUILT_V_E2025_GLOBE_R2023A_54009_1000_V1_0_", tile.id, ".tif", sep = ""))

    built_v_df <- as.data.frame(grid_ghsl[[i]], xy = TRUE)
    names(built_v_df)[3] <- "built_v"

    #grid_df[[i]] <- left_join(grid_df[[i]], built_v_df, by = c("x", "y"))
    grid_df[[i]] <- merge(grid_df[[i]], built_v_df, by = c("x", "y"), all = TRUE)

}
head(grid_df[[1]])

save(grid_df, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wLandPopBSV.RData")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wLandPopBSV.RData")


grid_all <-  as.data.frame(do.call(rbind, grid_df))
write.csv(grid_all, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wLandPopBSV.csv")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wLandPopBSV.RData")
grid_all <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wLandPopBSV.csv")


grid_ghsl <- list()
for (i in 1:length(tiles)){
    tile <- subset(both, tile_id == tiles[[i]])
    
    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL_1km/GHS_SMOD_E2025_GLOBE_R2023A_54009_1000_V1_0_", tile.id, ".tif", sep = ""))

    smod_df <- as.data.frame(grid_ghsl[[i]], xy = TRUE)
    names(smod_df)[3] <- "smod"

    grid_df[[i]] <- left_join(grid_df[[i]], smod_df, by = c("x", "y"))

}

save(grid_df, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wLandPopBSVSMOD.RData")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wLandPopBSVSMOD.RData")

head(grid_df[[1]])

names(grid_df[[1]])
names(grid_df[[2]])

for(i in 1:3) {
    grid_df[[i]] <- grid_df[[i]][,c(1,2,3,4,5,6,8)]
    names(grid_df[[i]]) <- c("x", "y", "land", "pop", "built_s", "built_v", "smod")
}

for(i in 4:5) {
    grid_df[[i]] <- grid_df[[i]][,c(1:7)]
    names(grid_df[[i]]) <- c("x", "y", "land", "pop", "built_s", "built_v", "smod")
}

save(grid_df, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wLandPopBSVSMOD.RData")


grid_all <-  as.data.frame(do.call(rbind, grid_df))

save(grid_all, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wLandPopBSVSMOD.RData")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wLandPopBSVSMOD.RData")
head(grid.all)

names(grid.all)

Brazil <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/Brazil_Admin_0_to_2/bra_admbnda_adm2_ibge_2020.shp", crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
Brazil <- st_transform(Brazil, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs")

NE_muni <- c("Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia")

NE_Brazil <- subset(Brazil, ADM1_PT %in% NE_muni)
st_bbox(NE_Brazil)

grid <- grid.all[,c("x", "y")]
ne_grid <- subset(grid, x >= -4873234.0 & y >= -2253248.8 & x <= -3240817.1, y <= -129762.4)

grid.sf <- st_as_sf(grid[,c("x","y")], coords = c("x", "y"), crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs")

save(grid.sf, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL_1km/NE_grid_1km_sf.RData")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL_1km/NE_grid_1km_sf.RData")


grid.rast <- rast(grid.sf)

#grid.rast <- rast(grid.all, type = 'xyz')

x <- writeRaster(grid.rast, "~/downloads/NE_grid_1km_GHSL.tif", overwrite=TRUE)

grid_df <- read.csv("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL_1km/GHS_ALL_NE_1km.csv")
head(grid_df)

library(fastDummies)

grid.df <- dummy_cols(grid_df, select_columns = "smod")
head(grid.df)

write.csv(grid.df, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wData.csv")

school_counts <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/ne_brazil_school_density2_1km.gpkg")
head(school_counts)


grid.all <- merge(grid.df, school_counts, by = c("x", "y"))

write.csv(grid.all, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_Grid_Counts_1km_wData.csv")