
## Load packages

library(sf)
library(ggplot2)
library(stars)
library(ggpubr)
library(mapview)
library(readr)
library(dplyr)
#library(sp)


## Load school data 

schools_gov <- st_read("UNICEF-ESA/Data/Brazil/Grid_Square/schools_brgov_cropped.gpkg")
schools_unicef <- st_read("UNICEF-ESA/Data/Brazil/Grid_Square/schools_unicef_cropped.gpkg")

#schools_gov <- read_csv("UNICEF-ESA/Data/Brazil/Grid_Square/brgov_schools_cropped.csv")
#schools_unicef <- read_csv("UNICEF-ESA/Data/Brazil/Grid_Square/unicef_schools_cropped.csv")

names(schools_gov)
names(schools_unicef)

schools_gov <- schools_gov[,c("Escola", "Localização", "Dependência.Administrativa", "Latitude", "Longitude", "geom")]
schools_gov$database <- "gov"
schools_gov$gov.id <- 1:nrow(schools_gov)
schools_gov$unicef.id <- NA

schools_unicef <- schools_unicef[,c("name", "school_region", "school_type", "lat", "lon", "geom")]
schools_unicef$database <- "unicef"
schools_unicef$gov.id <- NA
schools_unicef$unicef.id <- 1:nrow(schools_unicef)

names(schools_gov) <- names(schools_unicef)

schools_gov <- subset(schools_gov, !is.na(lat) & !is.na(lon))
schools_unicef <- subset(schools_unicef, !is.na(lat) & !is.na(lon))

schools <- rbind(schools_gov, schools_unicef)
schools <- st_as_sf(schools, coords = c("lat", "lon"))

ggplot() +
  geom_sf(schools, mapping = aes(color = database)) +
  #geom_sf_label(schools, mapping = aes(label = name)) +
  coord_sf()

## Get distances between the datasets and find close schools (< 100m)

all_dists <- st_distance(schools)

sep_dists <- st_distance(schools_gov, schools_unicef)
matrix_dists <-  matrix(sep_dists, nrow = 173, ncol = 118)
dists <- as.vector(sep_dists)
hist(dists, breaks = c(0,5,10,15,20,25,30,35,40,45,50,100,200,1000,12000), xlim = c(0,100))

min(na.omit(dists))

close_schools <- which(matrix_dists < 20, arr.ind = TRUE) 
names(close_schools) <- c("gov.id", "unicef.id")

close_dists <- subset(dists, dists < 20)
hist(close_dists)

## get potentially duplicated schools

dup_schools <- subset(schools, gov.id %in% close_schools[,1] | unicef.id %in% close_schools[,2])

ggplot() +
  geom_sf(schools, mapping = aes(color = database)) +
  #geom_sf_label(schools, mapping = aes(label = gov.id), color = "red") +
  #geom_sf_label(schools, mapping = aes(label = unicef.id), color = "blue") +
  coord_sf()

ggplot() +
  geom_sf(schools, mapping = aes(color = database)) 
#geom_sf_label(schools, mapping = aes(label = name), color = "red") +
#geom_sf_label(schools, mapping = aes(label = gov.id), color = "red") +
#geom_sf_label(schools, mapping = aes(label = unicef.id), color = "blue")

mapview(schools, zcol = "database")

matrix_dists[81,] < 12


## get nearest point
schools <- rbind(schools_gov, schools_unicef)
schools <- st_as_sf(schools, coords = c("lat", "lon"))

schools$id <- 1:nrow(schools)

nearest <- st_nearest_feature(schools)
schools <- cbind(schools, nearest, st_distance(schools, schools[nearest,], by_element = TRUE))
names(schools)[11] <- "distance"
schools$distance <- as.numeric(schools$distance)

close_schools <- subset(schools, distance < 50)

st_crs(close_schools)

mapview(close_schools, zcol = "database")

## merge 2

schools_gov <- st_read("UNICEF-ESA/Data/Brazil/Grid_Square/schools_brgov_cropped.gpkg")
schools_unicef <- st_read("UNICEF-ESA/Data/Brazil/Grid_Square/schools_unicef_cropped.gpkg")

schools_gov$database <- "gov"
schools_gov$gov.id <- 1:nrow(schools_gov)
schools_gov$unicef.id <- NA

schools_unicef$database <- "unicef"
schools_unicef$gov.id <- NA
schools_unicef$unicef.id <- 1:nrow(schools_unicef)

names(schools_gov) <- c("attendence_registration", "name", "gov_school_id", "uf", 
                        "muncipality", "school_region", "different_location",
                        "pub_priv", "address", "telephone", "school_type",
                        "priv_cat", "power_agree", "gov_regulate", "school_size",
                        "school_level",  "other_edu", "lat", "lon", "geom", "database", 
                        "gov.id", "unicef.id")
sch_gov.df <- as.data.frame(schools_gov)
sch_unicef.df <- as.data.frame(schools_unicef)

###merge on repeated perfectly columns
all_schools <- merge(sch_unicef.df, sch_gov.df, by = c("name", "school_type"), all = TRUE)
#all_schools$school_type.x == all_schools$school_type.y

all_schools$database <- as.numeric(as.factor(all_schools$database.x)) + 0.5*as.numeric(as.factor(all_schools$database.y))
all_schools$database <- as.factor(all_schools$database)

### prioritise gov info
all_schools$lat <- coalesce(all_schools$lat.y, all_schools$lat.x)
all_schools$lon <- coalesce(all_schools$lon.y, all_schools$lon.x)
all_schools$x <- all_schools$lon
all_schools$y <- all_schools$lat


all_schools$school_region <- coalesce(all_schools$school_region.y, all_schools$school_region.x)
all_schools$gov.id <- coalesce(all_schools$gov.id.y, all_schools$gov.id.x)
all_schools$unicef.id <- coalesce(all_schools$unicef.id.x, all_schools$unicef.id.y)


all_schools <- all_schools[,c(1:8, 11:19, 21:30, 35:38, 40:49, 56:61)]
all_schools.sf <- st_as_sf(all_schools, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
#st_crs(all_schools.sf) <- st_crs(close_schools)


###need to sort coords
mapview(all_schools.sf, zcol = "school_type")

ggplot() +
  geom_sf(all_schools.sf, mapping = aes(color = school_type)) 

all_schools <- all_schools.sf
save(all_schools, file = "UNICEF-ESA/Data/Brazil/Grid_Square/schools_cropped.gpkg")

load(file = "UNICEF-ESA/Data/Brazil/Grid_Square/schools_cropped.gpkg")

## Sentinel-2
s2 <- read_stars("UNICEF-ESA/Data/Brazil/Grid_Square/s2_cropped.tif")
s2.rgb <- st_rgb(s2[,,,c(4,3,2)], 3, probs = c(0.05, 0.95), stretch = "percent")
plot(s2.rgb)
