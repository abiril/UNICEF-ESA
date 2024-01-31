
## Load packages

library(sf)
library(ggplot2)
library(stars)
library(ggpubr)
library(mapview)
library(readr)
library(dplyr)
#library(sp)

setwd("~/")

## Load school data 
schools_gov <- st_read("UNICEF-ESA/Data/Brazil/Schools/schools_brgov_54009.gpkg")
schools_unicef <- st_read("UNICEF-ESA/Data/Brazil/Schools/schools_unicef_54009.gpkg")


save(schools_gov, file = "UNICEF-ESA/Data/Brazil/Schools/schools_brgov_54009.RData")


Brazil <- st_read("UNICEF-ESA/Data/Brazil/Shapefiles/Brazil_Admin_0_to_2/bra_admbnda_adm2_ibge_2020.shp")
st_crs(Brazil) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"

ggplot(Brazil) + 
  geom_sf() + coord_sf() +
  geom_point(schools_gov, mapping = aes(x = Longitude, y = Latitude), size = 1) +
  theme(legend.position = "none")

ggplot(Brazil) + 
  geom_sf() + coord_sf() +
  geom_point(schools_gov, mapping = aes(x = Longitude, y = Latitude, color = UF), size = 1) +
  theme(legend.position = "none")


load(file = "UNICEF-ESA/Data/Brazil/Grid_Square/schools_all.RData")

mapview(all_schools)

### schools w missing coordinate
gov_missing <- subset(schools_gov, is.na(Latitude) | is.na(Longitude))
unicef_missing <- subset(schools_unicef, is.na(lat) | is.na(lon))

missing_names <- unique(gov_missing$Escola)
dup_names <- schools_gov[which(duplicated(schools_gov$Escola)),"Escola"]
unique_dup_names <- unique(dup_names)

dup_names <- gov_missing[which(duplicated(gov_missing[,c("Escola", "Município")])),"Escola"]
unique_dup_names <- unique(dup_names)

### dup name and UF
name_region <- st_drop_geometry(schools_gov[,c("Escola", "UF")])
unique_name_region <- unique(name_region)

duplicate_name_region <- name_region[which(duplicated(name_region)),]

dupliate_gov_schools <- subset(schools_gov, Escola %in% duplicate_name_region$Escola & UF %in% duplicate_name_region$UF)

## dup name and municipalty
schools_gov <- st_read("UNICEF-ESA/Data/Brazil/Schools/schools_brgov_54009.gpkg")

name_municipality <- st_drop_geometry(schools_gov[,c("Escola", "Município")])
unique_name_municipality <- unique(name_municipality)

duplicate_name_municipality <- name_municipality[which(duplicated(name_municipality)),]
duplicate_gov_schools <- subset(schools_gov, Escola %in% duplicate_name_municipality$Escola & Município %in% duplicate_name_municipality$Município)

dup_set <- unique(st_drop_geometry(duplicate_gov_schools[,c("Escola","Município")]))

dists <- list()
for (i in 1:nrow(dup_set)){
  school <- subset(duplicate_gov_schools, Escola == dup_set[i,1] & Município == dup_set[i,2])
  dists[[i]] <- cbind(school, st_distance(school))
  
}

dist_all <- bind_rows(dists, .id = "column_label")
full_missing <- subset(dist_all, is.na(X2))
dist_coords <- subset(dist_all, !is.na(X2))


ggplot(Brazil) + 
  geom_sf() + coord_sf() +
  geom_point(dist_coords, mapping = aes(x = Longitude, y = Latitude, color = as.numeric(X1)), size = 1) 



### dups with at least 1 missing

dup_gov_missing <- subset(duplicate_gov_schools, is.na(Latitude) | is.na(Longitude))

dup_missing <- duplicate_gov_schools[which(is.na(dupliate_gov_schools$Latitude) | is.na(dupliate_gov_schools$Longitude)),]
dup_missing <- subset(schools_gov, Escola %in% dup_missing$Escola & Município %in% dup_missing$Município)

dup_schools <- dup_missing[,c("Escola", "Município", "Longitude", "Latitude", "UF")]


Brazil <- st_read("UNICEF-ESA/Data/Brazil/Shapefiles/Brazil_Admin_0_to_2/bra_admbnda_adm2_ibge_2020.shp")
st_crs(Brazil) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"

ggplot(Brazil) + 
  geom_sf() + coord_sf() +
  geom_point(dup_schools, mapping = aes(x = Longitude, y = Latitude, color = Escola)) +
  theme_dark() + theme(legend.position = "none")

dup_schools <- st_drop_geometry(dup_schools)
dup_schools$school_id <- as.numeric(as.factor(dup_schools$Escola))

unique_schools <- unique(dup_schools[,c("Escola", "school_id")])

  



###

CETAM <- subset(schools_gov, Escola == "CENTRO DE EDUCACAO TECNOLOGICA DO AMAZONAS - CETAM")
max(na.omit(CETAM$Longitude))

CETAM$size <- ifelse(CETAM$Porte.da.Escola == "Até 50 matrículas de escolarização", "Small",
                     ifelse(CETAM$Porte.da.Escola == "Entre 51 e 200 matrículas de escolarização", "Medium",
                     ifelse(CETAM$Porte.da.Escola == "Entre 201 e 500 matrículas de escolarização", "Large", NA)))

ggplot(Brazil) + 
  geom_sf() + coord_sf() +
  geom_point(CETAM, mapping = aes(x = Longitude, y = Latitude, color = factor(size)), size = 3) +
  theme(legend.position = "bottom") +
  xlim(c(-73, -56)) + ylim(c(-9,0))


###
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

#schools_gov <- subset(schools_gov, !is.na(lat) & !is.na(lon))
#schools_unicef <- subset(schools_unicef, !is.na(lat) & !is.na(lon))

schools <- rbind(schools_gov, schools_unicef)


#### shapefiles to assign area

Brazil <- st_read("UNICEF-ESA/Data/Brazil/Shapefiles/Brazil_Admin_0_to_2/bra_admbnda_adm2_ibge_2020.shp")
st_crs(Brazil) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"


Brazil <- st_read("UNICEF-ESA/Data/Brazil/Shapefiles/BR_UF_2021/BR_UF_2021.shp")
Brazil <- st_transform(Brazil, crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")


ggplot(Brazil) + 
  geom_sf() + coord_sf() +
  geom_point(schools_gov, mapping = aes(x = Longitude, y = Latitude), size = 1) +
  theme(legend.position = "none")


schools_gov <- st_read("UNICEF-ESA/Data/Brazil/Schools/schools_brgov_54009.gpkg")

schools <- schools_gov
schools <- st_as_sf(schools, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
Brazil <- st_transform(Brazil, crs = st_crs(schools))

full_schools <- subset(schools, !is.na(geom))
intersected <- st_intersection(Brazil, full_schools)
save(intersected, file = "UNICEF-ESA/Data/Brazil/Schools/region_matching_schools.RData")

unique(st_drop_geometry(intersected[,c("UF", "SIGLA")]))

## unique broken pairs, * aren't neighbours
## RO-MT*, AM-PA, PA-AM, CE-RN, PB-CE, PB-PE, PE-CE, PE-PB, PE-AL, AL-PE, BA-MG, 
## BA-ES, MG-BA, PR-SC, PR-SP, SC-PR

diff_UF <- subset(intersected, UF != SIGLA)
Brazil <- st_transform(Brazil, crs = st_crs(schools))

ggplot(Brazil) + 
  geom_sf() + coord_sf() +
  geom_sf(diff_UF, mapping = aes(color = UF), size = 3) +
  theme(legend.position = "bottom")

### these are all close enough