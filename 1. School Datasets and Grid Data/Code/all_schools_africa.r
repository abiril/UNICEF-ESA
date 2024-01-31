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

#write.csv(all_schools, "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/UNICEF_schools.csv")
save(all_schools, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/UNICEF_schools.RData")

load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/UNICEF_schools.RData")


###PLOTS
schools_country <- all_schools %>%
    count(country, sort = FALSE) 
head(schools_country)

#https://en.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations)
population <- c(13712828, 2675353, 34121985, 14190612, 55100587, 20931751, 2604172, 27202843, 223804632, 14094683, 8791092, 11088796, 60414495, 16665409)
#https://data.worldbank.org/indicator/SP.POP.TOTL
population <- c(13352860, 2630300, 33475870, 13859340, 54027490, 20405320, 2567010, 26207980, 218541210, 13776700, 8605720, 10913160, 59893890, 16320540)

pop_under_15 <- c(5665760, 857560, 12365110, 5757400, 20428570, 8691170, 930300, 12801440, 94070210, 5299330, 3354130, 4787080, 17100180, 6631690)

plot(population, pop_under_15)

country.population <- as.data.frame(cbind(countries, population, pop_under_15))
names(country.population)[1] <- "country"
schools_country <- merge(schools_country, country.population, by = "country")

schools_country$schools_per_10000 <- (as.numeric(schools_country$n) / as.numeric(schools_country$population)) * 10000
schools_country$schools_per_10000_children <- (as.numeric(schools_country$n) / as.numeric(schools_country$pop_under_15)) * 10000
schools_country$pop_under_15 <- as.numeric(schools_country$pop_under_15)
schools_country$population <- as.numeric(schools_country$population)

View(schools_country)

library(ggplot2)
library(reshape2)

head(schools_country)
names(schools_country)[2] <- "num_schools"
country_counts <- melt(schools_country[,c(1,2,3,5)], id.vars = c("country", "population"))
country_counts


ggplot(schools_country) +
    geom_bar(aes(x = reorder(country, -num_schools), y = num_schools), stat="identity")
ggsave("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/UNICEF_school_counts.pdf")



scl <- max(schools_country$num_schools) / max(schools_country$schools_per_10000)
country_counts$scaled.value <- ifelse(country_counts$variable == "num_schools", country_counts$value, country_counts$value*scl)
country_counts

ggplot(country_counts) +
    geom_col(aes(x = reorder(country, -value), y = scaled.value, fill = variable, group = variable), position="dodge") +
    scale_y_continuous(name = "Number of Schools",
        sec.axis = sec_axis(trans = ~./scl, name="Schools per 10,000")) 

        
ggsave("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/UNICEF_school_counts_and_density.pdf")

pop_counts <- melt(schools_country[,c(1,3,4)], id.vars = c("country"))
pop_counts

ggplot(pop_counts) +
    geom_col(aes(x = reorder(country, -value), y = value, fill = variable, group = variable), position="dodge") 

####
load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/UNICEF_schools.RData")

all_schools$x <- all_schools$lat
all_schools$y <- all_schools$lon

all.sf <- st_as_sf(all_schools, coords = c("lat", "lon"), crs = "EPSG:3857")

plot(all.sf$geometry, col = as.factor(all.sf$country))

###get a shapefile
Africa <- st_read(dsn = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/Africa_Shapefile/Africa_Boundaries.shp")
head(Africa)

africa_countries <- subset(Africa, ISO %in% countries)
#plot(africa_countries)


### let's just do unicef schools for now
all_schools$ID.country <- as.numeric(as.factor(all_schools$country))

area_schools <- list()
all <- list()
for (i in 1:14) {
    area_schools[[i]] <- subset(all_schools, ID.country == i)

    all[[i]] <- subset(area_schools[[i]], unicef == 1)

    all[[i]] <- st_drop_geometry(all[[i]])

    write.csv(all[[i]], file = paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Africa/all_schools_2_", i , ".csv", sep = ""), row.names = TRUE)
}

all.sf <- st_as_sf(all_schools, coords = c("lat", "lon"), crs = st_crs(GHSL))
all_schools <- cbind(st_drop_geometry(all.sf), st_coordinates(all.sf))

plot(ghsl_raster$land)
points(all_schools$x, all_schools$y)

##osm

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

schools.sf <- st_transform(schools.sf, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs")
schools.df <- cbind(st_drop_geometry(schools.sf), st_coordinates(schools.sf))

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
