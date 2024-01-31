
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

schools_gov <- st_read("UNICEF-ESA/Data/Brazil/Schools/schools_brgov_54009.gpkg")
schools_unicef <- st_read("UNICEF-ESA/Data/Brazil/Schools/schools_unicef_54009.gpkg")

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

schools_gov <- st_read("UNICEF-ESA/Data/Brazil/Schools/schools_brgov_54009.gpkg")
schools_unicef <- st_read("UNICEF-ESA/Data/Brazil/Schools/schools_unicef_54009.gpkg")

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
all_schools <- subset(all_schools, !is.na(lon & lat))
all_schools.sf <- st_as_sf(all_schools, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
#st_crs(all_schools.sf) <- st_crs(close_schools)


###need to sort coords
mapview(all_schools.sf, zcol = "school_type")

ggplot() +
  geom_sf(all_schools.sf, mapping = aes(color = school_type)) 

all_schools <- all_schools.sf

save(all_schools, file = "UNICEF-ESA/Data/Brazil/Grid_Square/schools_all.RData")

## Sentinel-2

clipped_centroids <- c(277,228,238,241)
clipped_centroids <- clipped_centroids - 1

length(clipped_centroids)

s2 <- list()
s2[[1]] <- read_stars("UNICEF-ESA/Data/Brazil/Images/brazil_schools_276.tif")
s2_stars <- read_stars("UNICEF-ESA/Data/Brazil/Images/brazil_schools_276.tif")
st_crs(s2[[1]])

for (i in 2:4) {
  j = clipped_centroids[i]
  s2[[i]] <- read_stars(paste("UNICEF-ESA/Data/Brazil/Images/brazil_schools_", j, ".tif", sep = ""))
  s2[[i]] <- st_transform(s2[[i]], crs = st_crs(s2[[1]]))
  s2_stars <- st_join(s2_stars, s2[[i]])
}

ggplot() +
  geom_stars(data = s2_stars) 

built_s100 <- st_crop(built_s100, s2_stars)
s <- st_crop(s, s2_stars)

ggplot() +
  geom_stars(data = s2_stars) +
  geom_point(s, mapping = aes(x = lon, y = lat, color = dataset)) +
  coord_equal()
