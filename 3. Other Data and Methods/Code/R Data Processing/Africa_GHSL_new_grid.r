#Downloading GHSL from URL???

#First, get shapefile of area.

#Subset GHSL tiles by shapefile

#Download those tiles for each variable


library(sf)
library(terra)
library(dplyr)

setwd("/home/azureuser/cloudfiles/code/Users/ariley/")

#1. Download GHSL tiles
temp <- tempfile()
download.file("https://ghsl.jrc.ec.europa.eu/download/GHSL_data_54009_shapefile.zip", temp)
GHSL <- st_read(unzip(temp)[1])
st_crs(GHSL)
rm(temp)

#2. Download AOI shapefile, e.g. AfricaN UNICef COUNTRIES
Africa <- st_read(dsn = "Data/Shapefiles/Africa_Shapefile/Africa_Boundaries.shp")
Africa <- st_transform(Africa, crs = st_crs(GHSL))
countries <- c("BEN", "BWA", "GHA", "GIN", "KEN", "MWI", "NAM", "NER", "NGA", "RWA", "SLE", "SSD", "ZAF", "ZWE")
AOI <- subset(Africa, ISO %in% countries)

#3. Get intersection to get relevant tile IDs
both <- st_intersection(GHSL, AOI)
tiles <- unique(both$tile_id)
#tiles <- subset(tiles, tiles != "R15_C22" & tiles != "R11_C21" & tiles != "R14_C21" & tiles != "R14_C20") ### why are some tiles not WORKING???? AHHHHHH
tiles

library(fastDummies)
library(terra)

setwd("/home/azureuser/cloudfiles/code/Users/ariley/")

### for all countries
grid_df <- read.csv(file = "Data/Schools/Africa/ghsl_africa.csv")
grid_df <- dummy_cols(grid_df, select_columns = "smod")
head(grid_df)
grid_df <- grid_df[,-1]
grid_sf <- st_as_sf(grid_df, coords = c("x", "y"), crs = "ESRI:54009")

africa_1km_data <- list()
for (i in 1:length(countries)) {
    school_counts <- st_read(paste("Data/Schools/Africa/School_Counts/Gridded_1km_Counts/africa_school_counts_1km_ghsl", i, ".gpkg", sep = ""), crs = "ESRI:54009")
    #school_counts <- st_transform(school_counts, st_crs(GHSL))
    head(school_counts)
    #school_counts <- school_counts[,-c(2,3,4,5)]
    #school_counts$centroid <- st_centroid(school_counts)
    #school_counts$x <- st_coordinates(school_counts$centroid)[,1]
    #school_counts$y <- st_coordinates(school_counts$centroid)[,2]
    #school_df <- st_drop_geometry(school_counts)[,c("id", "NUMPOINTS", "x", "y")]

    school_counts <- st_drop_geometry(school_counts)
    school_counts <- school_counts[,c("left", "bottom", "NUMPOINTS")]

    school_counts$x <- school_counts$left + 500
    school_counts$y <- school_counts$bottom + 500
    school_counts <- school_counts[,c("x", "y", "NUMPOINTS")]   

    unique(school_counts$NUMPOINTS)

    school_ghsl <- left_join(school_counts, grid_df, by = c("x", "y"))
    
    school_sf <- st_as_sf(school_ghsl, coords = c("x", "y"), crs = "ESRI:54009")

    grid_country <- st_crop(grid_sf, school_sf)

    grid_country <- cbind(as.data.frame(grid_country, xy = TRUE), st_coordinates(grid_country))
    grid_country <- dummy_cols(grid_country, select_columns = "smod")
    names(grid_country)
    names(grid_country)[c(1,16,17)] <- c("ID","x", "y")
    
    grid <- st_drop_geometry(grid_country[,c(16,17,1:15)])[,-17]
    grid_raster <- rast(grid, type = "xyz", crs = "ESRI:54009")

    school_sf <- st_as_sf(school_ghsl, coords = c("x", "y"), crs = "ESRI:54009")
    #school_data <- cbind(school_sf, terra::extract(grid_raster, school_sf))

    school_sf$x <- st_coordinates(school_sf)[,1]
    school_sf$y <- st_coordinates(school_sf)[,2]

    africa_1km_data[[i]] <- as.data.frame(st_drop_geometry(school_sf, xy = TRUE))

    write.csv(africa_1km_data[[i]], file = paste("Data/Schools/Africa/School_Counts/Gridded_1km_Data/By_Country_ghsl/Africa_Grid_Counts_1km_wData_ghsl_", i, ".csv", sep = ""))
}

head(africa_1km_data[[6]])


setwd("/home/azureuser/cloudfiles/code/Users/ariley/")
for (i in 1:length(countries)) {
    africa_1km_data[[i]] <- read.csv(file = paste("Data/Schools/Africa/School_Counts/Gridded_1km_Data/By_Country_ghsl/Africa_Grid_Counts_1km_wData_ghsl_", i, ".csv", sep = ""))

}
unique(africa_1km_data[[12]]$NUMPOINTS)



africa_grid_data <- do.call(rbind, africa_1km_data)
head(africa_grid_data)

setwd("/home/azureuser/cloudfiles/code/Users/ariley/")

write.csv(africa_grid_data, file = "Data/Schools/Africa/School_Counts/Gridded_1km_Data/Africa_Grid_Counts_1km_wData_ghsl.csv")

africa_grid_data <- read.csv(file = "Data/Schools/Africa/School_Counts/Gridded_1km_Data/By_Country_ghsl/Africa_Grid_Counts_1km_wData_ghsl_5.csv")
unique(africa_grid_data$NUMPOINTS)
