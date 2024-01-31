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
countries <- c("BEN", "BWA", "GHA", "GIN", "KEN", "MWI", "NAM", "NER", "NGA", "RWA", "SLE", "SSD", "ZAF", "ZWE")


grid.df <- list()
for (i in 1:length(countries)){
    grid.df[[i]] <- read.csv(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Gridded_1km_Data/By_Country_ghsl/Africa_Grid_Counts_1km_wData_ghsl_", i, ".csv", sep = ""))
}

unique(grid.df[[13]]$NUMPOINTS)

grid <- do.call(rbind, grid.df)
plot(grid$x, grid$y)


Africa <- st_read(dsn = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/Africa_Shapefile/Africa_Boundaries.shp")
Africa <- st_transform(Africa, crs = "ESRI:54009")
countries <- c("BEN", "BWA", "GHA", "GIN", "KEN", "MWI", "NAM", "NER", "NGA", "RWA", "SLE", "SSD", "ZAF", "ZWE")
AOI <- subset(Africa, ISO %in% countries)


countries <- c("BEN", "BWA", "GHA", "GIN", "KEN", "MWI", "NAM", "NER", "NGA", "RWA", "SLE", "SSD", "ZAF", "ZWE")

grid.country <- list()
for (i in 1:length(countries)) {
    grid.data <- grid.df[[i]]
    names(grid.data)
    plot(grid.data$x, grid.data$y)
    grid.data <- dummy_cols(grid.data, select_columns = "smod")

    head(grid.data)
    names(grid.data)
    nrow(grid.data)

    unique(grid.data$NUMPOINTS)

    grid.land.builts.builtv <- rast(grid.data[,c(17,18,4,6,7)], type = 'xyz', crs = "ESRI:54009")
    grid.means <- aggregate(grid.land.builts.builtv, fact = 10, fun = "mean")

    grid.numpoints.pop.smod <- rast(grid.data[,c(17,18,2,5,9:16)], type = 'xyz', crs = "ESRI:54009")
    grid.sum <- aggregate(grid.numpoints.pop.smod, fact = 10, fun = "sum", na.rm = TRUE)

    head(grid.means)
    head(grid.sum)

    means.df <- as.data.frame(grid.means, xy = TRUE)
    sums.df <- as.data.frame(grid.sum, xy = TRUE)

    head(means.df)
    head(sums.df)

    grid <- left_join(means.df, sums.df, by = c("x", "y"))
    grid$id <- 1:nrow(grid)
    grid$country <- i

    grid.country[[i]] <- grid

    write.csv(grid, file = paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Gridded_10km_Data/", countries[i], "_Grid_Data_10km_ghsl.csv", sep = ""))
}

unique(grid.country[[12]]$NUMPOINTS)


grid_data <- do.call(rbind, grid.country)
head(grid_data)
write.csv(grid_data, "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Gridded_10km_Data/Africa_Grid_Data_10km_ghsl.csv")



head(grid.country[[6]])
