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
library(utils)


unzip(zipfile = "/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/centro-oeste-latest-free.shp.zip",
                exdir = "/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/centro-oeste")

unzip(zipfile = "/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/nordeste-latest-free.shp.zip",
                exdir = "/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/nordeste")

unzip(zipfile = "/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/norte-latest-free.shp.zip",
                exdir = "/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/norte")

unzip(zipfile = "/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/sudeste-latest-free.shp.zip",
                exdir = "/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/sudeste")

unzip(zipfile = "/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/sul-latest-free.shp.zip",
                exdir = "/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/sul")

### schools in buildings a, pois a, pois
schools <- c("schools", "university", "kindergarten", "college")

co_buildings <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/centro-oeste/gis_osm_buildings_a_free_1.shp")
co_schools1 <- subset(co_buildings, type %in% schools)

co_pois <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/centro-oeste/gis_osm_pois_free_1.shp")
co_schools2 <- subset(co_pois, fclass %in% schools)

co_pois_a <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/centro-oeste/gis_osm_pois_a_free_1.shp")
co_schools3 <- subset(co_pois_a, fclass %in% schools)

names(co_schools1)
names(co_schools2)
names(co_schools3)

co_schools1 <- co_schools1[,-3]
names(co_schools1)[4] <- "fclass"

co_schools2 <- co_schools2[,names(co_schools1)]
co_schools3 <- co_schools3[,names(co_schools1)]

co_schools <- rbind(co_schools1, co_schools2, co_schools3) #1519


##nordeste

ne_buildings <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/nordeste/gis_osm_buildings_a_free_1.shp")
ne_schools1 <- subset(ne_buildings, type %in% schools)

ne_pois <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/nordeste/gis_osm_pois_free_1.shp")
ne_schools2 <- subset(ne_pois, fclass %in% schools)

ne_pois_a <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/nordeste/gis_osm_pois_a_free_1.shp")
ne_schools3 <- subset(ne_pois_a, fclass %in% schools)

names(ne_schools1)
names(ne_schools2)
names(ne_schools3)

ne_schools1 <- ne_schools1[,-3]
names(ne_schools1)[4] <- "fclass"

ne_schools2 <- ne_schools2[,names(ne_schools1)]
ne_schools3 <- ne_schools3[,names(ne_schools1)]

ne_schools <- rbind(ne_schools1, ne_schools2, ne_schools3) #4448


##norte

n_buildings <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/norte/gis_osm_buildings_a_free_1.shp")
n_schools1 <- subset(n_buildings, type %in% schools)

n_pois <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/norte/gis_osm_pois_free_1.shp")
n_schools2 <- subset(n_pois, fclass %in% schools)

n_pois_a <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/norte/gis_osm_pois_a_free_1.shp")
n_schools3 <- subset(n_pois_a, fclass %in% schools)

names(n_schools1)
names(n_schools2)
names(n_schools3)

n_schools1 <- n_schools1[,-3]
names(n_schools1)[4] <- "fclass"

n_schools2 <- n_schools2[,names(n_schools1)]
n_schools3 <- n_schools3[,names(n_schools1)]

n_schools <- rbind(n_schools1, n_schools2, n_schools3) #1358


##sudeste

se_buildings <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/sudeste/gis_osm_buildings_a_free_1.shp")
se_schools1 <- subset(se_buildings, type %in% schools)

se_pois <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/sudeste/gis_osm_pois_free_1.shp")
se_schools2 <- subset(se_pois, fclass %in% schools)

se_pois_a <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/sudeste/gis_osm_pois_a_free_1.shp")
se_schools3 <- subset(se_pois_a, fclass %in% schools)

names(se_schools1)
names(se_schools2)
names(se_schools3)

se_schools1 <- se_schools1[,-3]
names(se_schools1)[4] <- "fclass"

se_schools2 <- se_schools2[,names(se_schools1)]
se_schools3 <- se_schools3[,names(se_schools1)]

se_schools <- rbind(se_schools1, se_schools2, se_schools3) #6768


##sul

s_buildings <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/sul/gis_osm_buildings_a_free_1.shp")
s_schools1 <- subset(s_buildings, type %in% schools)

s_pois <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/sul/gis_osm_pois_free_1.shp")
s_schools2 <- subset(s_pois, fclass %in% schools)

s_pois_a <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/sul/gis_osm_pois_a_free_1.shp")
s_schools3 <- subset(s_pois_a, fclass %in% schools)

names(s_schools1)
names(s_schools2)
names(s_schools3)

s_schools1 <- s_schools1[,-3]
names(s_schools1)[4] <- "fclass"

s_schools2 <- s_schools2[,names(s_schools1)]
s_schools3 <- s_schools3[,names(s_schools1)]

s_schools <- rbind(s_schools1, s_schools2, s_schools3) #5440

schools <- rbind(co_schools, ne_schools, n_schools, se_schools, s_schools)
osm_schools <- schools

save(osm_schools, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/osm_schools.RData")













