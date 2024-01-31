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
library(corrplot)
library(fastDummies)

setwd("/")

###### Now for all african countries
list_csv_files <- list.files(path = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Gridded_1km_Data/By_Country_wBuffer/")
list_csv_files
ordered_list <- list_csv_file[c(1,7:14,2:6)]
grid.df <- lapply(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Gridded_1km_Data/By_Country_wBuffer/", list_csv_files, sep = ""), function(x) read.csv(x, stringsAsFactors = FALSE))
names(grid.df[[1]])

grid <- do.call(rbind, grid.df)
plot(grid$x, grid$y)


Africa <- st_read(dsn = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/Africa_Shapefile/Africa_Boundaries.shp")
Africa <- st_transform(Africa, crs = "ESRI:54009")
countries <- c("BEN", "BWA", "GHA", "GIN", "KEN", "MWI", "NAM", "NER", "NGA", "RWA", "SLE", "SSD", "ZAF", "ZWE")
AOI <- subset(Africa, ISO %in% countries)

'ghana <- subset(Africa, ISO == "GHA")
grid.ghana <- grid.df[[3]]

plot(ghana$geometry) 
points(grid.ghana$x, grid.ghana$y)

ghana.sf <- st_as_sf(grid.ghana, coords = c("x", "y"), crs = "ESRI:54009")
ggplot(ghana) + geom_sf() + coord_sf() +
    geom_sf(ghana.sf, mapping = aes(fill = "red"))

school_counts <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Gridded_1km_Counts/africa_school_counts_wBuffer3.gpkg", crs = "ESRI:54009")
ggplot(ghana) + geom_sf() + coord_sf() +
    geom_sf(school_counts, mapping = aes(fill = "red"))
head(st_coordinates(school_counts))

inter <- st_intersection(school_counts, ghana)
st_intersects(school_counts, ghana, sparse = FALSE) 

plot(africa2$geometry) 
points(grid$x, grid$y)'

countries <- c("BEN", "BWA", "GHA", "GIN", "KEN", "MWI", "NAM", "NER", "NGA", "RWA", "SLE", "SSD", "ZAF", "ZWE")

grid.country <- list()
for (i in 1:length(countries)) {
    grid.data <- grid.df[[i]]
    grid.data <- dummy_cols(grid.data, select_columns = "smod")

    names(grid.data)
    nrow(grid.data)

    grid.land.builts.builtv <- rast(grid.data[,c(19,20,6,8,9)], type = 'xyz', crs = "ESRI:54009")
    grid.means <- aggregate(grid.land.builts.builtv, fact = 10, fun = "mean")

    grid.numpoints.pop.smod <- rast(grid.data[,c(19,20,3,7,10:18)], type = 'xyz', crs = "ESRI:54009")
    grid.sum <- aggregate(grid.numpoints.pop.smod, fact = 10, fun = "sum", na.rm = TRUE)

    head(grid.means)
    head(grid.sum)

    means.df <- as.data.frame(grid.means, xy = TRUE)
    sums.df <- as.data.frame(grid.sum, xy = TRUE)

    head(means.df)
    head(sums.df)

    grid <- left_join(means.df, sums.df, by = c("x", "y"))
    grid$id <- 1:nrow(grid)
    grid$country <- countries[i] #### I THINK THIS IS WRONG!!!

    grid.country[[i]] <- grid

    write.csv(grid, file = paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Gridded_10km_Data/", countries[i], "_Grid_Data_10km_wBuffer.csv", sep = ""))
}


grid_data <- do.call(rbind, grid.country)
head(grid_data)
write.csv(grid_data, "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Gridded_10km_Data/Africa_Grid_Data_10km_wBuffer.csv")



head(grid.country[[4]])
