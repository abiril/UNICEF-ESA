#Downloading GHSL from URL???

#First, get shapefile of area.

#Subset GHSL tiles by shapefile

#Download those tiles for each variable


library(sf)
library(terra)

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

#4. Download those tiles for each variable layer
root <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/"

GHSL_labels <- c("GHS_LAND_GLOBE_R2022A/GHS_LAND_E2018_GLOBE_R2022A_54009_1000/V1-0/tiles/GHS_LAND_E2018_GLOBE_R2022A_54009_1000_V1_0_",
            "GHS_POP_GLOBE_R2023A/GHS_POP_E2025_GLOBE_R2023A_54009_1000/V1-0/tiles/GHS_POP_E2025_GLOBE_R2023A_54009_1000_V1_0_",
            "GHS_BUILT_S_GLOBE_R2023A/GHS_BUILT_S_E2025_GLOBE_R2023A_54009_1000/V1-0/tiles/GHS_BUILT_S_E2025_GLOBE_R2023A_54009_1000_V1_0_",
            "GHS_BUILT_V_GLOBE_R2023A/GHS_BUILT_V_E2025_GLOBE_R2023A_54009_1000/V1-0/tiles/GHS_BUILT_V_E2025_GLOBE_R2023A_54009_1000_V1_0_",
            "GHS_SMOD_GLOBE_R2023A/GHS_SMOD_E2025_GLOBE_R2023A_54009_1000/V1-0/tiles/GHS_SMOD_E2025_GLOBE_R2023A_54009_1000_V1_0_")

GHSL_files <- c("GHS_LAND_E2018_GLOBE_R2022A_54009_1000_V1_0_", "GHS_POP_E2025_GLOBE_R2023A_54009_1000_V1_0_",
                    "GHS_BUILT_S_E2025_GLOBE_R2023A_54009_1000_V1_0_", "GHS_BUILT_V_E2025_GLOBE_R2023A_54009_1000_V1_0_",
                    "GHS_SMOD_E2025_GLOBE_R2023A_54009_1000_V1_0_")

GHSL_names <- c("GHSL_land", "GHSL_pop", "GHSL_built_s", "GHSL_built_v", "GHSL_smod")

which.file <- c(1,1,1,1,2)

options(timeout=50000)
grid_cell_ghsl <- list()
grid_df <- list()
grid_ghsl <- list()
for(j in 1:length(GHSL_labels)){
    for (i in 1:length(tiles)){
        tile.id <- tiles[i]
        url <- paste(root, GHSL_labels[j], tile.id, ".zip", sep = "")
        temp <- tempfile()
        download.file(url, temp)
        unzipped <- unzip(temp)[which.file[j]]
        grid_cell_ghsl[[i]] <- rast(unzipped)
        file.remove(temp)
    }
    grid_ghsl[[j]] <- do.call(mosaic, grid_cell_ghsl)
    grid_df[[j]] <- as.data.frame(grid_ghsl[[j]], xy = TRUE)
    lapply(paste(GHSL_files[[j]], tile.id, ".tif", sep = ""), file.remove)
    lapply(paste(GHSL_files[[j]], tile.id, ".clr", sep = ""), file.remove)

    write.csv(grid_df[[j]], paste("Data/Schools/Africa/ghsl_africa/", GHSL_names[[j]], ".csv", sep = ""))
}

head(grid_df[[1]])

for (i in 1:5){
    grid_df[[i]] <- read.csv(paste("Data/Schools/Africa/ghsl_africa/", GHSL_names[[i]], ".csv", sep = ""))
    print(head(grid_df[[i]]))
}

ghsl <- grid_df[[1]][,c(2:4)]
for (i in 2:length(GHSL_labels)){
    ghsl <- merge(ghsl, grid_df[[i]][,c(2:4)], by = c("x", "y"))
    write.csv(ghsl, file = "Data/Schools/Africa/ghsl_africa.csv")
}

ghsl <- read.csv(file = "Data/Schools/Africa/ghsl_africa.csv")
head(ghsl)
names(ghsl) <- c("X", "x", "y", "land", "pop", "built_s", "built_v", "smod")

write.csv(ghsl, file = "Data/Schools/Africa/ghsl_africa.csv")

library(ggplot2)
library(tidyterra)

hist(ghsl$land)
hist(ghsl$pop)
hist(ghsl$built_s)
hist(ghsl$built_v)
hist(ghsl$smod)

ghsl$smod <- as.factor(ghsl$smod)

ghsl_raster <- rast(ghsl[,-1], type = 'xyz')


ggplot() + 
    geom_spatraster(ghsl_raster, mapping = aes(fill = smod)) #+  
    scale_fill_viridis_d(option = "B", trans = "log10")


#### for one country
library(fastDummies)
library(terra)

setwd("/home/azureuser/cloudfiles/code/Users/ariley/")

grid_df <- read.csv(file = "Data/Schools/Africa/ghsl_africa.csv")
grid_df <- dummy_cols(grid_df, select_columns = "smod")
school_counts <- st_read("Data/Schools/Africa/School_Counts/Gridded_1km_Counts/africa_school_counts1.gpkg")
school_counts <- st_transform(school_counts, st_crs(GHSL))
names(school_counts)

school_counts <- school_counts[,-c(2,3,4,5)]
school_counts$centroid <- st_centroid(school_counts)
school_counts$x <- st_coordinates(school_counts$centroid)[,1]
school_counts$y <- st_coordinates(school_counts$centroid)[,2]
names(school_counts)
school_df <- st_drop_geometry(school_counts)[,c("id", "NUMPOINTS", "x", "y")]

grid_df <- grid_df[,-1]
grid_sf <- st_as_sf(grid_df, coords = c("x", "y"), crs = st_crs(school_counts))
grid_sf <- st_crop(grid_sf, st_bbox(school_counts))
names(grid_sf)
grid_sf <- cbind(as.data.frame(grid_sf, xy = TRUE), st_coordinates(grid_sf))
names(grid_sf)
names(grid_sf)[c(1,16,17)] <- c("ID","x", "y")
#grid_df <- st_drop_geometry(grid_df)[,-5]
#names(grid_df)[c(5,6)] <- c("x", "y")

head(grid_sf)
grid_df <- st_drop_geometry(grid_sf[,c(16,17,1:15)])[,-17]
head(grid_df)
grid_raster <- rast(grid_df, type = "xyz")
plot(grid_raster)

school_sf <- st_as_sf(school_df, coords = c("x", "y"), crs = st_crs(school_counts))

school_data <- cbind(school_sf, extract(grid_raster, school_sf))

school_data$x <- st_coordinates(school_data)[,1]
school_data$y <- st_coordinates(school_data)[,2]
head(school_data)
View(school_data)

BEN_1km_data <- as.data.frame(st_drop_geometry(school_data, xy = TRUE))
head(BEN_1km_data)

write.csv(BEN_1km_data, file = "Data/Schools/Africa/School_Counts/Gridded_1km_Data/BEN_Grid_Counts_1km_wData.csv")



### for all countries
grid_df <- read.csv(file = "Data/Schools/Africa/ghsl_africa.csv")
grid_df <- dummy_cols(grid_df, select_columns = "smod")
head(grid_df)
grid_df <- grid_df[,-1]
grid_sf <- st_as_sf(grid_df, coords = c("x", "y"), crs = st_crs(GHSL))
st_crs(grid_sf)

africa_1km_data <- list()
for (i in 1:14) {
    school_counts <- st_read(paste("Data/Schools/Africa/School_Counts/Gridded_1km_Counts/africa_school_counts", i, ".gpkg", sep = ""))
    school_counts <- st_transform(school_counts, st_crs(GHSL))
    head(school_counts)
    school_counts <- school_counts[,-c(2,3,4,5)]
    school_counts$centroid <- st_centroid(school_counts)
    school_counts$x <- st_coordinates(school_counts$centroid)[,1]
    school_counts$y <- st_coordinates(school_counts$centroid)[,2]
    school_df <- st_drop_geometry(school_counts)[,c("id", "NUMPOINTS", "x", "y")]

    xmin <- as.numeric(st_bbox(school_counts)[1] - 10000)
    ymin <- as.numeric(st_bbox(school_counts)[2] - 10000)
    xmax <- as.numeric(st_bbox(school_counts)[3] + 10000)
    ymax <- as.numeric(st_bbox(school_counts)[4] + 10000)

    grid_country <- st_crop(grid_sf, xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)


    grid_country <- cbind(as.data.frame(grid_country, xy = TRUE), st_coordinates(grid_country))
    grid_country <- dummy_cols(grid_country, select_columns = "smod")
    names(grid_country)
    names(grid_country)[c(1,16,17)] <- c("ID","x", "y")
    
    grid_df <- st_drop_geometry(grid_country[,c(16,17,1:15)])[,-17]
    grid_raster <- rast(grid_df, type = "xyz", crs = "ESRI:54009")

    school_sf <- st_as_sf(school_df, coords = c("x", "y"), crs = st_crs(school_counts))
    school_data <- cbind(school_sf, extract(grid_raster, school_sf))

    school_data$x <- st_coordinates(school_data)[,1]
    school_data$y <- st_coordinates(school_data)[,2]

    africa_1km_data[[i]] <- as.data.frame(st_drop_geometry(school_data, xy = TRUE))

    write.csv(africa_1km_data[[i]], file = paste("Data/Schools/Africa/School_Counts/Gridded_1km_Data/By_Country/Africa_Grid_Counts_1km_wData_", i, ".csv", sep = ""))
}

plot(grid_raster)

africa_grid_data <- do.call(rbind, africa_1km_data)
head(africa_grid_data)

write.csv(africa_grid_data, file = "Data/Schools/Africa/School_Counts/Gridded_1km_Data/Africa_Grid_Counts_1km_wData.csv")

