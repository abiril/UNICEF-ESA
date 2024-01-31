library(csv)
library(dplyr)
library(sf)

vars <- c("giga_id_school", "school_id", "name", "lat", "lon", "education_level", 
                            "education_level_regional", "school_type", "school_type_govt", "admin1", "admin2",
                            "admin3", "admin4", "school_region", "num_students", "address")

ben.schools <- read.csv("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/UNICEF_master/BEN_school_geolocation_coverage_master.csv")
names(ben.schools)
ben.schools <- ben.schools[,vars]
head(ben.schools)

list_csv_files <- list.files(path = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/UNICEF_master/")
list_csv_files
schools <- lapply(paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/UNICEF_master/", list_csv_files, sep = ""), function(x) read.csv(x, stringsAsFactors = FALSE))
head(schools[[1]])

countries <- c("BEN", "BWA", "GHA", "GIN", "KEN", "MWI", "NAM", "NER", "NGA", "RWA", "SLE", "SSD", "ZAF", "ZWE")

for (i in 1:length(schools)) {
    schools[[i]] <- schools[[i]][,vars]
    schools[[i]]$country <- countries[i]
}

all_schools <- do.call(rbind, schools)

head(all_schools)
nrow(all_schools)

names(all_schools)[1] <- "unicef.id"
all_schools$unicef <- 1

write.csv(all_schools, "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/UNICEF_schools.csv")

all_schools$x <- all_schools$lat
all_schools$y <- all_schools$lon

all.sf <- st_as_sf(all_schools, coords = c("lat", "lon"), crs = "EPSG:3857")

plot(all.sf$geometry, col = as.factor(all.sf$country))

###get a shapefile
Africa <- st_read(dsn = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/Africa_Shapefile/Africa_Boundaries.shp")
head(Africa)

africa_countries <- subset(Africa, ISO %in% countries)
#plot(africa_countries)


osm_schools <- read.csv("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/africa_schools.csv")
head(osm_schools)

osm_schools$country <- substr(osm_schools$adm02, 1, 3)  
unique(osm_schools$country)

osm_schools <- subset(osm_schools, country %in% countries)
osm_schools$osm_id <- 1:nrow(osm_schools)
osm_schools$osm <- 1

osm_schools$x <- osm_schools$lat
osm_schools$y <- osm_schools$lon

head(osm_schools)
osm.sf <- st_as_sf(osm_schools, coords = c("lon", "lat"),  crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")

head(all_schools)
head(osm_schools)

all_schools <- all_schools[,-c(4,5)]
osm_schools <- osm_schools[,-c(6,7)]

more_schools <- merge(all_schools, osm_schools, by = c("country", "x", "y"), all = TRUE)
head(more_schools)

more_schools$ID.country <- as.numeric(as.factor(more_schools$country))

length(unique(more_schools$ID.country))

more_schools$lat <- more_schools$x
more_schools$lon <- more_schools$y
schools.sf <- st_as_sf(more_schools, coords = c("lat", "lon"), crs = "+proj=longlat +ellps=GRS80 +no_defs +type=crs")


#more_schools <- st_transform(schools.sf, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs")

library(ggplot2)

Africa <- st_transform(Africa, crs = "EPSG:3857")
st_crs(Africa)
ggplot(Africa) + geom_sf() + coord_sf() +
    geom_point(more_schools, mapping = aes(x = lat, y = lon, color = country))

more_schools <- cbind(st_drop_geometry(more_schools), st_coordinates(more_schools))
head(more_schools)

area_schools <- list()
all <- list()
osm <- list()
for (i in 1:14) {
    area_schools[[i]] <- subset(more_schools, ID.country == i)

    all[[i]] <- subset(area_schools[[i]], unicef == 1)
    osm[[i]] <- subset(area_schools[[i]], osm == 1)

    all[[i]] <- st_drop_geometry(all[[i]])
    osm[[i]] <- st_drop_geometry(osm[[i]])

    write.csv(all[[i]], file = paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/all_schools", i , ".csv", sep = ""), row.names = TRUE)
    write.csv(osm[[i]], file = paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/osm_schools", i , ".csv", sep = ""), row.names = TRUE)

}
