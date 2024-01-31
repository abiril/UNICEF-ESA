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

grid.df <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Gridded_1km_Data/BEN_Grid_Counts_1km_wData.csv")
names(grid.df)

grid.land.builts.builtv <- rast(grid.df[,c(19,20,6,8,9)], type = 'xyz')
grid.means <- aggregate(grid.land.builts.builtv, fact = 10, fun = "mean")
head(grid.means)

grid.numpoints.pop.smod <- rast(grid.df[,c(19,20,3,7,10:18)], type = 'xyz')
grid.sum <- aggregate(grid.numpoints.pop.smod, fact = 10, fun = "sum")
head(grid.sum)

means.df <- as.data.frame(grid.means, xy = TRUE)
sums.df <- as.data.frame(grid.sum, xy = TRUE)


grid <- left_join(means.df, sums.df)
grid$id <- 1:nrow(grid)
head(grid)

write.csv(grid, "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Gridded_10km_Data/BEN_Grid_Data_10km.csv")



###### Now for all african countries
list_csv_files <- list.files(path = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Gridded_1km_Data/By_Country/")
list_csv_files
grid.df <- lapply(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Gridded_1km_Data/By_Country/", list_csv_files, sep = ""), function(x) read.csv(x, stringsAsFactors = FALSE))
names(grid.df[[1]])

grid <- do.call(rbind, grid.df)
plot(grid$x, grid$y)

countries <- c("BEN", "BWA", "GHA", "GIN", "KEN", "MWI", "NAM", "NER", "NGA", "RWA", "SLE", "SSD", "ZAF", "ZWE")

grid.country <- list()
for (i in 1:length(countries)) {
    grid.data <- grid.df[[i]]
    grid.data <- dummy_cols(grid.data, select_columns = "smod")

    names(grid.data)

    grid.land.builts.builtv <- rast(grid.data[,c(11,12,2,6,8,9)], type = 'xyz', crs = "ESRI:54009")
    grid.means <- aggregate(grid.land.builts.builtv, fact = 10, fun = "mean")

    grid.numpoints.pop.smod <- rast(grid.data[,c(11,12,3,7,10,13:20)], type = 'xyz', crs = "ESRI:54009")
    grid.sum <- aggregate(grid.numpoints.pop.smod, fact = 10, fun = "sum", na.rm = TRUE)

    head(grid.means)
    head(grid.sum)

    means.df <- as.data.frame(grid.means, xy = TRUE)
    sums.df <- as.data.frame(grid.sum, xy = TRUE)

    head(means.df)
    head(sums.df)

    grid <- left_join(means.df, sums.df, by = c("x", "y"))
    grid$id <- 1:nrow(grid)
    grid$country <- countries[i]

    grid.country[[i]] <- grid

    write.csv(grid, file = paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Gridded_10km_Data/", countries[i], "_Grid_Data_10km.csv", sep = ""))
}

grid_data <- do.call(rbind, grid.country)
head(grid_data)
write.csv(grid_data, "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/School_Counts/Gridded_10km_Data/Africa_Grid_Data_10km.csv")

