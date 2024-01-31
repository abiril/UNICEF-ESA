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
library(osmextract)
library(terra)
library(tidyterra)
library(viridis)


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
for (i in 1:20){
    tile <- subset(both, tile_id == tiles[[i]])
    grid_schools[[i]] <- st_crop(schools.sf, tile)
    
    tile.id <- tiles[i]
    grid_ghsl[[i]] <- rast(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL/GHS_LAND_E2018_GLOBE_R2022A_54009_100_V1_0_", tile.id, ".tif", sep = ""))

    grid_schools_vect <- vect(grid_schools[[i]])
    grid_schools[[i]]$land <- as.data.frame(extract(grid_ghsl[[i]], grid_schools_vect))[,2]

}

schools <- as.data.frame(do.call(rbind, grid_schools))
names(schools)
schools.sf <- st_as_sf(schools, coords = c("x", "y"), crs = st_crs(GHSL))
plot(schools.sf$geometry, col = schools.sf$land)

ggplot(schools.sf$geometry) +
    geom_sf(mapping = aes(color = schools.sf$land)) + coord_sf() +
    scale_color_viridis_c()



hist(schools.sf$land)


#grid.schools.df <- as.data.frame(st_drop_geometry(grid_schools[[1]], xy = TRUE))
grid.schools.df <- as.data.frame(cbind(st_drop_geometry(grid_schools[[1]]), st_coordinates(grid_schools[[1]])))
names(grid.schools.df)[c(98,99)] <- c("Easting", "Northing")
 
my_breaks = c(0, 1,  100, 1000, 5000, 7000, 9999, 10000)
hist(log(grid_ghsl[[1]]))


ggplot() + 
    geom_spatraster(grid_ghsl[[1]], mapping = aes(fill = GHS_LAND_E2018_GLOBE_R2022A_54009_100_V1_0_R10_C12)) + 
    scale_fill_viridis_c(option = "B", trans = "log10") + # , breaks = my_breaks) +
    geom_point(grid.schools.df, mapping = aes(x = Easting, y = Northing, color = land)) +
    scale_color_viridis_c(option = "B", trans = "log10") # , breaks = my_breaks) +

no_land_schools <- subset(grid.schools.df, land == 0)
range(no_land_schools$Easting)
range(no_land_schools$Northing)

no_land_school <- no_land_schools[1,]
no_land_school$Easting
no_land_school$Northing

ggplot() + 
    geom_spatraster(grid_ghsl[[1]], mapping = aes(fill = GHS_LAND_E2018_GLOBE_R2022A_54009_100_V1_0_R10_C12)) + 
    scale_fill_viridis_c(option = "B", trans = "log10") + # , breaks = my_breaks) +
    geom_point(no_land_schools, mapping = aes(x = Easting, y = Northing), color = "red") + #, color = land)) +
    #scale_color_viridis_c(option = "B", trans = "log10") + # , breaks = my_breaks) +
    xlim(c(-6166000, -6156600)) + ylim(c(-465400, -458700))



hist(grid_schools[[1]]$land)
