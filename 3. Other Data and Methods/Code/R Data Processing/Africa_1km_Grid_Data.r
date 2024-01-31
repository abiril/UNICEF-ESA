library(sf)
library(csv)
library(dplyr)

schools <- list()
for (i in 1:14){
    schools[[i]] <- read.csv(file = paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/all_schools", i , ".csv", sep = ""))
}

BEN_schools <- schools[[i]]
head(BEN_schools)

Africa <- st_read(dsn = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/Africa_Shapefile/Africa_Boundaries.shp")
Africa <- st_transform(Africa, crs = "ESRI:54009")
BEN <- subset(Africa, ISO == "BEN")

BEN_grid <- st_make_grid(BEN, cellsize = 10000, crs = "ESRI:54009")
plot(BEN_grid)
BEN_grid_sf <- st_as_sf(BEN_grid)
BEN_grid_sf$id_polygons <- 1:length(BEN_grid_sf)
plot(BEN_grid_sf)

BEN_points <- st_as_sf(BEN_schools, coords = c("X", "Y"), crs = "ESRI:54009")

intersection <- st_intersection(BEN_grid_sf, BEN_points)

int_result <- intersection %>% group_by(id_polygons) %>% count()

BEN_counts <- as.data.frame(int_result)
head(BEN_counts)
