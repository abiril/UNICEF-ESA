
grid.df <- load.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_grid_1km_wData.csv")

school_counts <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/ne_brazil_school_density2_1km.gpkg")
head(school_counts)

school_counts <- school_counts[,-c(2,3,4,5)]
school_counts$centroid <- st_centroid(school_counts)
school_counts$x <- st_coordinates(school_counts$centroid)[,1]
school_counts$y <- st_coordinates(school_counts$centroid)[,2]
names(school_counts)
school_df <- st_drop_geometry(school_counts)[,c("id", "NUMPOINTS", "x", "y")]

grid_df <- grid.df[,-1]
grid_sf <- st_as_sf(grid_df, coords = c("x", "y"), crs = st_crs(school_counts))
names(grid_sf)
grid_sf <- cbind(as.data.frame(grid_sf, xy = TRUE), st_coordinates(grid_sf))
names(grid_sf)
names(grid_sf)[c(16,17)] <- c("x", "y")
#grid_df <- st_drop_geometry(grid_df)[,-5]
#names(grid_df)[c(5,6)] <- c("x", "y")

grid_sf <- st_drop_geometry(grid_sf)[,c("x", "y", "land", "pop", "smod","built_s","built_v","smod_0", "smod_10", "smod_11", "smod_12", "smod_13", "smod_21", "smod_22", "smod_23", "smod_30")]
grid_raster <- rast(grid_sf, type = "xyz")

school_sf <- st_as_sf(school_df, coords = c("x", "y"), crs = st_crs(school_counts))

school_data <- cbind(school_sf, extract(grid_raster, school_sf))

school_data$x <- st_coordinates(school_data)[,1]
school_data$y <- st_coordinates(school_data)[,2]
head(school_data)

NE_1km_data <- as.data.frame(st_drop_geometry(school_data, xy = TRUE))
head(NE_1km_data)

write.csv(NE_1km_data, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Grid/NE_Grid_Counts_1km_wData.csv")
