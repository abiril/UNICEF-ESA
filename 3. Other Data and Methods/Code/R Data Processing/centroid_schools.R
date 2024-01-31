
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


#load(file = "UNICEF-ESA/Data/Brazil/Schools/all_merged_schools.RData")
load(file = "UNICEF-ESA/Data/Brazil/Schools/unicef_schools_municipality.RData")


Brazil <- st_read("UNICEF-ESA/Data/Brazil/Shapefiles/Brazil_Admin_0_to_2/bra_admbnda_adm2_ibge_2020.shp")
st_crs(Brazil) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"
Brazil <- st_transform(Brazil, crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")

sf_use_s2(FALSE)
muni_centroids <- st_centroid(Brazil, of_largest_polygon = TRUE)

centroids <- cbind(st_drop_geometry(muni_centroids), st_coordinates(muni_centroids))


ggplot(Brazil$geometry) +
  geom_sf() + coord_sf() +
  geom_point(centroids, mapping = aes(x = X, y = Y), color = "red")

centroids <- centroids[,c("ADM2_PT", "X", "Y")]

#all_schools <- merge(all_schools, centroids, by.x = "Municipality", by.y = "ADM2_PT", all.x = TRUE, all.y = FALSE)
schools <- cbind(st_drop_geometry(schools), st_coordinates(schools$geom))
schools.sf <- st_as_sf(schools)
schools.sf <- st_transform(schools.sf, crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
schools <- cbind(st_drop_geometry(schools.sf), st_coordinates(schools.sf))
schools <- schools[,-c(34,35)]

schools <- merge(schools, centroids, by.x = "Municipality", by.y = "ADM2_PT", all.x = TRUE, all.y = FALSE)
#schools <- schools[,-c(34,35)]

#all_schools$x.diff <- all_schools$Latitude - all_schools$X
#all_schools$y.diff <- all_schools$Longitude - all_schools$Y

schools$x.diff <- abs(schools$X.x - schools$X.y)
schools$y.diff <- abs(schools$Y.x - schools$Y.y)

hist(schools$x.diff)
hist(schools$y.diff)

quantile(schools$x.diff, probs = seq(.1, .9, by = .1))
quantile(schools$y.diff, probs = seq(.1, .9, by = .1))

centroid_schools <- subset(schools, x.diff < 0.0005 & y.diff < 0.0005)
hist(centroid_schools$x.diff)
hist(centroid_schools$y.diff)

schools$centroid <- ifelse(schools$x.diff < 0.0005 & schools$y.diff < 0.0005, 1, 0)

hist(schools$centroid)

Abadia_schools <- subset(schools, Municipality == "Abadia de Goiás")
Abadia_centroid <- subset(centroids, ADM2_PT == "Abadia de Goiás")
Abadia <- subset(Brazil, ADM2_PT == "Abadia de Goiás")
st_bbox(Abadia)

ggplot(Abadia) +
  geom_sf() + coord_sf() +
  geom_point(Abadia_centroid, mapping = aes(x = X, y = Y), color = "red") +
  geom_point(Abadia_schools, mapping = aes(x = X.x, y = Y.x), color = "blue")


####
Brazil <- st_read("UNICEF-ESA/Data/Brazil/Shapefiles/Brazil_Admin_0_to_2/bra_admbnda_adm2_ibge_2020.shp")
st_crs(Brazil) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"

adm1 <- st_drop_geometry(Brazil)[,c("ADM1_PT", "ADM2_PT")]
schools <- merge(schools, adm1, by.x = "Municipality", by.y = "ADM2_PT")

ggplot(Brazil) +
  geom_sf() + coord_sf() +
  geom_point(schools, mapping = aes(x = X.x, y = Y.x, color = ADM1_PT)) 


Tocantins_schools <- subset(schools, ADM1_PT == "Tocantins")
Tocantins <- subset(Brazil, ADM1_PT == "Tocantins")

ggplot(Brazil) +
  geom_sf() + coord_sf() +
  geom_point(centroids, mapping = aes(x = X, y = Y), color = "red") +
  geom_point(Tocantins_schools, mapping = aes(x = X.x, y = Y.x), color = "blue") +
  xlim(c(-52,-45)) + ylim(c(-4,-16))

st_crs(Brazil)
st_crs(schools.sf)
st_bbox(Tocantins)

ggplot(Tocantins) +
  geom_sf() + coord_sf() +
  geom_point(centroids, mapping = aes(x = X, y = Y), color = "red") +
  geom_point(schools, mapping = aes(x = X.x, y = Y.x), color = "blue") +
  xlim(c(-51,-45)) + ylim(c(-5,-14))


save(schools, file =  "UNICEF-ESA/Data/Brazil/Schools/schools_w_centroid.RData")
