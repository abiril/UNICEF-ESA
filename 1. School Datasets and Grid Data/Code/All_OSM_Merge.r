### Getting Data and Merging Schools and OSM

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
library(stringdist)

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/correct_all_schools2.RData")

unique(is.na(all_schools[,c("X.chosen", "Y.chosen")]))

#OSM Points from Casper
osm_schools <- read.csv(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/OpenStreetMap/south-america_schools.csv")
names(osm_schools)

library(stringr)
osm_schools$x <- osm_schools$lon
osm_schools$y <- osm_schools$lat
osm_schools <- osm_schools[str_detect(osm_schools$adm02, "BRA"),]
unique(osm_schools$adm02)

Brazil <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/BR_UF_2021/BR_UF_2021.shp")
Brazil <- st_transform(Brazil, crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")

ggplot(Brazil) +
  geom_sf() + coord_sf() +
  geom_point(all_schools, mapping = aes(x = X.chosen, y = Y.chosen), color = "green") +
  geom_point(osm_schools, mapping = aes(x = lon, y = lat), color = "red")


all_schools$x <- all_schools$X.chosen
all_schools$y <- all_schools$Y.chosen

all.sf <- st_as_sf(all_schools, coords = c("X.chosen", "Y.chosen"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
osm.sf <- st_as_sf(osm_schools, coords = c("lon", "lat"),  crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")

###get municipality for osm schools
Brazil <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/Brazil_Admin_0_to_2/bra_admbnda_adm2_ibge_2020.shp")
st_crs(Brazil) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"
Brazil <- st_transform(Brazil, crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")

osm_schools$osm_id <- 1:nrow(osm_schools)

schools <- st_as_sf(osm_schools, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
Brazil <- st_transform(Brazil, crs = st_crs(schools))
muni <- Brazil[,c("ADM2_PT", "ADM1_PT")]
muni <- cbind(muni, 1:nrow(muni))

head(muni)
names(muni)[3] <- "ID.muni"
muni.df <- st_drop_geometry(muni)
head(muni.df)

muni.r <- st_rasterize(muni)
head(schools)
schools <- cbind(schools, st_extract(muni.r, schools))
names(schools)
schools <- schools[,-11]

##### trying to get all munis and regions
schools_missing <- subset(schools, is.na(ID.region))#[,-c(8,9)]
schools_full <- subset(schools, !is.na(ID.region))#[,-c(8,9)]

sf_use_s2(FALSE)

missing_municipality <- cbind(schools_missing, muni[st_nearest_feature(schools_missing, muni), c("ID.muni", "ADM2_PT", "ID.region", "ADM1_PT")])
unique(is.na(missing_municipality$ID.region))

names(missing_municipality)
names(schools_full)

missing_municipality <- missing_municipality[,c(1:7, 10, 12, 14)]
names(missing_municipality) <- names(schools_full)
osm_schools <- rbind(missing_municipality, schools_full)

unique(is.na(osm_schools$ID.region))

#do the same for all schools
library(rquery)
library(rqdatatable)
unique(is.na(all_schools$Municipality))
unique(is.na(all_schools$ID.muni))

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/correct_all_schools2.RData")

all_schools <- all_schools[,-83]
all_schools$x <- all_schools$X.chosen
all_schools$y <- all_schools$Y.chosen

all_schools <- st_drop_geometry(all_schools)
all_schools <- st_as_sf(all_schools, coords = c("X.chosen", "Y.chosen"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")

names(muni.df)[1] <- "Municipality"
unique(muni.df)

names(all_schools)
all_schools <- all_schools[,-60]

class(all_schools)

sf_use_s2(FALSE)

all_schools <- cbind(all_schools, st_extract(muni.r, all_schools))
#all_schools <- left_join(all_schools, muni.df, by = "Municipality", relationship = "many-to-many")
names(all_schools)

all_schools <- all_schools[,-88]

all_schools_missing <- subset(all_schools, is.na(ID.region))#[,-c(8,9)]
all_schools_full <- subset(all_schools, !is.na(ID.region))#[,-c(8,9)]

st_crs(all_schools_missing)

all_missing_municipality <- cbind(all_schools_missing, muni[st_nearest_feature(all_schools_missing, muni), c("ID.muni", "ADM2_PT", "ID.region", "ADM1_PT")])
unique(is.na(all_missing_municipality$ID.muni))

names(all_missing_municipality)

all_missing_municipality <- all_missing_municipality[,c(1:84, 87, 88, 89, 90, 92)]
names(all_missing_municipality)[c(85,87,89)] <- c("ID.muni", "ID.region",  "geometry")
all_missing_municipality <- all_missing_municipality[,names(all_schools_full)]

all_schools <- rbind(all_schools_full, all_missing_municipality)
unique(is.na(all_schools$ID.muni))

save(all_schools, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_schools_wMuni.RData")
save(osm_schools, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/osm_schools_wMuni.RData")

###Merge

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_schools_wMuni.RData")
load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/osm_schools_wMuni.RData")

osm_schools <- cbind(st_drop_geometry(osm_schools), st_coordinates(osm_schools))
names(osm_schools)
names(all_schools)

all.points <- st_as_sf(all_schools[,c("name","ID","ID.muni","x","y")], coords = c("x","y"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
osm.points <- st_as_sf(osm_schools[,c("name","osm_id","ID.muni", "x","y")], coords = c("x","y"), crs = st_crs(all.points))

names(all_schools)
names(osm_schools)

more_schools <- merge(all_schools, osm_schools, by = c("ID.muni", "ID.region", "X", "Y", "name", "x", "y"), all = TRUE)

unique(is.na(more_schools$x))
unique(is.na(more_schools$y))
unique(is.na(more_schools$ID.muni))
unique(is.na(more_schools$name))

unique(more_schools$coord_source)
more_schools$coord_source[is.na(more_schools$coord_source)] <- "osm"

names(more_schools)
unique(is.na(more_schools$unicef))
unique(more_schools$unicef)

more_schools$gov[is.na(more_schools$gov)] <-  "No"
more_schools$geocoded[is.na(more_schools$geocoded)] <- "No"
more_schools$unicef <- ifelse(!is.na(more_schools$unicef), "Yes", "No")
more_schools$osm <- ifelse(more_schools$coord_source == "osm", "Yes", "No")

unique(st_drop_geometry(more_schools[,c("gov", "geocoded", "unicef", "osm")]))
more_schools$school_id <- 1:nrow(more_schools)

save(more_schools, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_and_osm_schools.RData")


unique(more_schools$osm)
more_schools$name.caps <- str_to_upper(more_schools$name)
unique(is.na(more_schools$ID.region))
unique(more_schools$ID.region)

ggplot() +
    geom_point(more_schools, mapping = aes(x = x, y = y, color = factor(ID.region)))

ggplot() +
    geom_point(all_schools, mapping = aes(x = x, y = y, color = factor(ID.region)))

nrow(unique(st_drop_geometry(all_schools[,c("ID.region", "ID.muni")])))


ggplot() +
    geom_point(osm_schools, mapping = aes(x = x, y = y, color = factor(ID.region)))


area_schools <- list()
all <- list()
osm <- list()
for (i in 1:27) {
    area_schools[[i]] <- subset(more_schools, ID.region == i)
    area_schools[[i]] <- area_schools[[i]][,-92]

    all[[i]] <- subset(area_schools[[i]], osm == "No")
    osm[[i]] <- subset(area_schools[[i]], osm == "Yes")

    all[[i]] <- st_drop_geometry(all[[i]])[,-88]
    osm[[i]] <- st_drop_geometry(osm[[i]])[,-88]

    write.csv(all[[i]], file = paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_schools", i , ".csv", sep = ""), row.names = TRUE)
    write.csv(osm[[i]], file = paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/osm_schools", i , ".csv", sep = ""), row.names = TRUE)

}

head(all[[1]])
head(area_schools[[1]])


#### Now import to QGIS, find nearest "all" school to each OSM school, if within 100m? 250m? with similar name, then same school
all <- list()
osm <- list()
merged <- list()
osm_merged <- list()
all_osm <- list()
all_osm_new <- list()
for(i in 1:27){
    all[[i]] <- read.csv(file = paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_schools", i , ".csv", sep = ""))[,-1]
    osm[[i]] <- read.csv(file = paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/osm_schools", i , ".csv", sep = ""))[,-1]

    merged[[i]] <- st_read(dsn = paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_osm_schools", i, ".gpkg", sep = ""))

    merged[[i]] <- merged[[i]][!duplicated(merged[[i]][,c("InputID")]), ]

    if(i == 25){
        merged[[i]] <- merged[[25]][-1731, ]
    }

    
    osm_merged[[i]] <- subset(cbind(osm[[i]][,c("name", "x", "y", "geocoded", "gov", "coord_source", "fid", "amenity", "osm", "school_id")], 
                            merged[[i]][,-4]), Distance < 250)

    all_osm[[i]] <- merge(all[[i]], osm_merged[[i]], by.x = "school_id", by.y = "TargetID", all.x = TRUE, all.y = FALSE)

    names(all_osm[[i]])[c(6, 7, 8, 85, 86, 87, 88, 89, 91, 92, 93, 94)] <- c("name", "x","y", "geocode", "gov", "coord_source", "fid", "amenity", "osm", "osm.name", "osm.x", "osm.y")
    all_osm[[i]] <- all_osm[[i]][,c(1:94, 102, 103, 104)]

    all_osm[[i]]$dist <- stringdist(all_osm[[i]]$name, all_osm[[i]]$osm.name, method = "lv")


    similar_schools <- subset(all_osm[[i]], is.na(dist) | dist <= 25)
    similar_names <- subset(similar_schools, !is.na(dist) & dist <= 25)[,c("name", "osm.name")]

    osm_left <- subset(osm[[i]], !(school_id %in% similar_schools$InputID))

    osm_left[,c(names(similar_schools[,!(names(similar_schools) %in% names(osm_left))]))] <- NA
    similar_schools[,c(names(osm_left[,!(names(osm_left) %in% names(similar_schools))]))] <- NA

    osm_left <- osm_left[,names(similar_schools)]

    all_osm_new[[i]] <- rbind(similar_schools, osm_left)

    all_osm_new[[i]]$osm <- ifelse(!is.na(all_osm_new[[i]]$osm.x), "Yes", "No")
    all_osm_new[[i]]$country <- "Brazil"
    
    all_osm_new[[i]] <- st_drop_geometry(all_osm_new[[i]])[,-97]
    write.csv(all_osm_new[[i]], file = paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/merged_schools", i , ".csv", sep = ""))[,-1]


}

write.csv(all_osm_new, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/merged_schools.csv")[,-1]

unique(is.na(all_osm_new[[1]]$ID.region))
class(all_osm_new[[1]])

### This looks good!
### for ID.region = 1
### 1772 all schools
nrow(all[[1]])
### 93 osm schools
nrow(osm[[1]])
### 74 close schools (250m)
nrow(osm_merged[[1]])
### 36 close and similar name schools, merge
nrow(similar_names)
### 57 osm schools left
nrow(osm_left)
### 1798 all schools
nrow(all_osm_new[[1]])

hist(dist)

all_new <- bind_rows(all_osm_new, .id = "ID.region")
write.csv(all_new, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/OSM_All_Merge/all_new_schools.csv")
