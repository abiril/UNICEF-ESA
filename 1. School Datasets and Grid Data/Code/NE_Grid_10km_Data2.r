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


grid.land <- rast("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL_1km/GHS_LAND_NE_1km.tif")
land.df <- as.data.frame(grid.land, xy = TRUE)
names(land.df)[3] <- "land"
grid_df <- land.df

head(grid_df)
hist(grid_df$land)

grid.pop <- rast("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL_1km/GHS_POP_NE_1km.tif")
pop.df <- as.data.frame(grid.pop, xy = TRUE)
names(pop.df)[3] <- "pop"
grid_df <- left_join(grid_df, pop.df, by = c("x", "y"))

grid.smod <- rast("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL_1km/GHS_SMOD_NE_1km.tif")
smod.df <- as.data.frame(grid.smod, xy = TRUE)
names(smod.df)[3] <- "smod"
grid_df <- left_join(grid_df, smod.df, by = c("x", "y"))

grid.built_s <- rast("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL_1km/GHS_BUILT_S_NE_1km.tif")
built_s.df <- as.data.frame(grid.built_s, xy = TRUE)
names(built_s.df)[3] <- "built_s"
grid_df <- left_join(grid_df, built_s.df, by = c("x", "y"))

grid.built_v <- rast("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL_1km/GHS_BUILT_V_NE_1km.tif")
built_v.df <- as.data.frame(grid.built_v, xy = TRUE)
names(built_v.df)[3] <- "built_v"
grid_df <- left_join(grid_df, built_v.df, by = c("x", "y"))


head(grid_df)
write.csv(grid_df, "/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL_1km/GHS_ALL_NE_1km.csv")

#pairs(grid_df[,c("land", "pop", "smod", "built_s", "built_v")])
corrplot(cor(grid_df[,c("land", "pop", "smod", "built_s", "built_v")]))


land <- aggregate(grid.land, fact = 10, fun = "mean")
land.df <- as.data.frame(land, xy = TRUE)
names(land.df)[3] <- "land"
grid_df <- land.df

pop <- aggregate(grid.pop, fact = 10, fun = "sum")
pop.df <- as.data.frame(pop, xy = TRUE)
names(pop.df)[3] <- "pop"
grid_df <- left_join(grid_df, pop.df, by = c("x", "y"))

built_s <- aggregate(grid.built_s, fact = 10, fun = "mean")
built_s.df <- as.data.frame(built_s, xy = TRUE)
names(built_s.df)[3] <- "built_s"
grid_df <- left_join(grid_df, built_s.df, by = c("x", "y"))

built_v <- aggregate(grid.built_v, fact = 10, fun = "mean")
built_v.df <- as.data.frame(built_v, xy = TRUE)
names(built_v.df)[3] <- "built_v"
grid_df <- left_join(grid_df, built_v.df, by = c("x", "y"))

smod.df <- as.data.frame(grid.smod, xy = TRUE)
names(smod.df)[3] <- "smod"
smod.df <- dummy_cols(smod.df, select_columns = "smod")
smod.df <- smod.df[,c(1,2,5:12)]
head(smod.df)

head(grid_df)

#grid_df <- grid_df[,c(1:6)]

smod.cat <- c(10,11,12,13,21,22,23,30)
smod.list <- list()
grid.smod <- list()
smod.10.df <- list()
smod.10 <- list()
for (i in 1:8){
    smod.list[[i]] <- cbind(smod.df[,c("x", "y")], smod.df[,i + 2])
    grid.smod[[i]] <- rast(smod.list[[i]], type = "xyz")
    smod.10[[i]] <- aggregate(grid.smod[[i]], fact = 10, fun = "sum")
    smod.10.df[[i]] <- as.data.frame(smod.10[[i]], xy = TRUE)
    names(smod.10.df[[i]])[3] <- paste("smod.", smod.cat[i])
    grid_df <- left_join(grid_df, smod.10.df[[i]], by = c("x", "y"))
}

head(grid_df)

write.csv(grid_df, "/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL_1km/GHS_ALL_NE_10km.csv")
grid_df <- read.csv("/home/azureuser/cloudfiles/code/Users/ariley/Data/GHSL_1km/GHS_ALL_NE_10km.csv")
head(grid_df)

school_counts <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/ne_brazil_school_density_10km.gpkg")
school_counts <- school_counts[,-c(2,3,4,5)]
school_counts$centroid <- st_centroid(school_counts)
school_counts$x <- st_coordinates(school_counts$centroid)[,1]
school_counts$y <- st_coordinates(school_counts$centroid)[,2]
names(school_counts)
school_df <- st_drop_geometry(school_counts)[,c("id", "NUMPOINTS", "x", "y")]
head(school_df)

grid_sf <- st_as_sf(grid_df, coords = c("x", "y"), crs = st_crs(school_counts))
head(grid_sf)
grid_sf <- cbind(as.data.frame(grid_sf, xy = TRUE), st_coordinates(grid_sf))[,-1]
head(grid_sf)
names(grid_sf)[c(14,15)] <- c("x", "y")
#grid_df <- st_drop_geometry(grid_df)[,-5]
#names(grid_df)[c(5,6)] <- c("x", "y")

head(grid_sf)
grid_sf <- st_drop_geometry(grid_sf)[,c(14,15,1:12)]
grid_raster <- rast(grid_sf, type = "xyz")

school_sf <- st_as_sf(school_df, coords = c("x", "y"), crs = st_crs(school_counts))

school_data <- cbind(school_sf, extract(grid_raster, school_sf))

school_data$x <- st_coordinates(school_data)[,1]
school_data$y <- st_coordinates(school_data)[,2]
head(school_data)

names(school_data)[8:15] <- c("smod.10", "smod.11", "smod.12", "smod.13", "smod.21", "smod.22", "smod.23", "smod.30")

save(school_data, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_school_count_wData.RData")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_school_count_wData.RData")
names(school_data)

school_data.df <- st_drop_geometry(school_data)


write.csv(school_data.df, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_school_count_wData.csv")
