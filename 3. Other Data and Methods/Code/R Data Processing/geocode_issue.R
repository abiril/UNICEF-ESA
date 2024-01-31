# OSM Schools

## Load packages

library(sf)
library(ggplot2)
library(stars)
library(ggpubr)
library(mapview)
library(readr)
library(dplyr)
library(tidygeocoder)
#library(sp)

## School point data

setwd("~/")

load(file = "UNICEF-ESA/Data/Brazil/OpenStreetMap/norte-schools.RData") ## all_schools
load(file = "UNICEF-ESA/Data/Brazil/OpenStreetMap/norte-schools-pois-a.RData") ## all_school_pois_a
load(file = "UNICEF-ESA/Data/Brazil/OpenStreetMap/norte-schools-pois.RData") ## all_school_pois

load(file = "UNICEF-ESA/Data/Brazil/Schools/complete_all_schools.RData") ## all_schools

school1 <- subset(all_schools, Endereço == "AVENIDA EDESIO DE CARVALHO, 200 VILA MARCELA. 56350-000 Petrolina - PE.")
school1$address <- paste(school1$name, ", ", school1$Municipality, ", ", school1$ADM1_PT, ", Brazil", sep = "")

geocode1 <- geocode(school1, Endereço, method = "arcgis", lat = Latitude.new , long = Longitude.new)

geocode2 <- geocode(school1, address, method = 'arcgis', lat = Latitude.new , long = Longitude.new)

school1$Latitude.new <- NA
school1$Longitude.new <- NA

school1 <- rbind(school1, geocode1, geocode2)

plot(school1$Latitude.new, school1$Longitude.new)


### more random
schools <- all_schools[sample(nrow(all_schools), 100), ]
schools$address <- paste(schools$name, ", ", schools$Municipality, ", ", schools$ADM1_PT, ", Brazil", sep = "")


geocode1 <- geocode(schools, Endereço, method = "arcgis", lat = Latitude.new , long = Longitude.new)

geocode2 <- geocode(schools, address, method = 'arcgis', lat = Latitude.new , long = Longitude.new)

schools$Latitude.new <- NA
schools$Longitude.new <- NA

schools <- rbind(schools, geocode1, geocode2)

plot(schools$Latitude.new, schools$Longitude.new)

