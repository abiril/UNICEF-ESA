#install.packages(c("sf", "ggplot2", "geobr", "stars"))

library(sf)
library(ggplot2)
library(geobr)
library(stars)
#library(raster)

setwd("C:/Users/abi.riley/OneDrive - ESA/Documents/UNICEF-ESA")

schools_brazil <- st_read("Data/Brazil/Schools/schools_brazil.gpkg")

crs_all <- st_crs(schools_brazil)

brazil_region <- read_region(year = 2020)
brazil_region <- st_transform(brazil_region, crs = crs_all)

brazil_municipalities <- read_municipality(year = 2020)
brazil_municipalities <- st_transform(brazil_municipalities, crs = crs_all)

ggplot(brazil_municipalities) +
  geom_sf() +
  geom_point(schools_brazil, mapping = aes(x = lon, y = lat, size = pl_student_count, col = pl_school_type)) +
  coord_sf()

amazon <- read_amazon()
biomes <- read_biomes()
indigenous <- read_indigenous_land()
schools <- read_schools()
urban <- read_urban_area()

ggplot(pop_arrangements) +
  geom_sf() + coord_sf()

ggplot(biomes) +
  geom_sf(aes(fill = code_biome)) + coord_sf()

ggplot(brazil_region) +
  geom_sf() + coord_sf() +
  geom_sf(data = amazon, fill = "darkgreen") +
  geom_sf(data = indigenous, fill = "red" +
  geom_sf(data = urban, fill = "grey") +
  geom_sf(data = schools, col = "blue", size = 0.5)


ggplot(brazil_region) +
  geom_sf() + coord_sf() +
  geom_sf(data = schools, col = "blue")


schools_brazil$pl_municipality_code <- as.numeric(substring(schools_brazil$pl_district_code, first = 1, last = 7))
unique(schools_brazil$pl_municipality_code)
unique(brazil_municipalities$code_muni)

municipalities_data <- as.data.frame(brazil_municipalities)
schools_data <- as.data.frame(schools_brazil)

schools_brazil <- merge(schools_data, municipalities_data, by.x = "pl_municipality_code", by.y = "code_muni", all.x = TRUE)

biomes <- st_transform(biomes, crs = crs_all)
biome_stars <- st_rasterize(biomes, st_as_stars(st_bbox(biomes), dx = 0.1, dy = 0.1), crs = crs_all)

schools_sf <- st_as_sf(schools_brazil, crs = crs_all)
schools_brazil$biome <- st_extract(biome_stars, schools_sf)
schools_brazil$biome$code_biome <- as.factor(schools_brazil$biome$code_biome)
schools_sf <- st_as_sf(schools_brazil, crs = crs_all)

ggplot(brazil_region) +
  geom_sf() +
  geom_point(schools_sf, mapping = aes(x = lon, y = lat, col = name_muni)) +
  coord_sf()


#By region
unique(schools_sf$name_region)
unique(brazil_region$name_region)

norte_schools <- subset(schools_sf, name_region == "Norte")
norteste_schools <- subset(schools_sf, name_region == "Norteste")
sudeste_schools <- subset(schools_sf, name_region == "Sudeste")
sul_schools <- subset(schools_sf, name_region == "Sul")
centro_schools <- subset(schools_sf, name_region == "Centro Oeste")

norte <- subset(brazil_region, name_region == "Norte")
norteste <- subset(brazil_region, name_region == "Norteste")
sudeste <- subset(brazil_region, name_region == "Sudeste")
sul <- subset(brazil_region, name_region == "Sul")
centro <- subset(brazil_region, name_region == "Centro Oeste")

norte_muni <- subset(brazil_municipalities, name_region == "Norte")
norteste_muni <- subset(brazil_municipalities, name_region == "Norteste")
sudeste_muni <- subset(brazil_municipalities, name_region == "Sudeste")
sul_muni <- subset(brazil_municipalities, name_region == "Sul")
centro_muni <- subset(brazil_municipalities, name_region == "Centro Oeste")

###
pop_dens <- read.csv("Data/Brazil/bra_pd_2020_1km_UNadj_ASCII_XYZ.csv")
pop_sf <- st_as_sf(pop_dens, coords = c("X","Y"))
pop_rast <- rasterFromXYZ(pop_dens)
pop_stars <- st_as_stars(pop_rast)

#pop_dens$Z.norm <- scale(pop_dens$Z)
#pop_dens$Z.norm.sqrt <- sqrt(pop_dens$Z.norm + 0.06446785)
#min(pop_dens$Z.norm)

#min(pop_dens$Z)
#pop_dens$Z.log10 <- log10(pop_dens$Z + 0.000001)
#hist(pop_dens$Z.log10)

#pop_dens$logZ <- log(pop_dens$Z)
#pop_sf <- st_as_sf(pop_dens, coords = c("X","Y"))

#pop_rast <- rasterFromXYZ(pop_dens[,c("X","Y","Z.log10")])
#pop_stars <- st_as_stars(pop_rast)

st_crs(pop_stars) <- "+proj=longlat +datum=WGS84 +no_defs"

ggplot() +
  geom_stars(data = pop_stars) + scale_fill_continuous(na.value = NA) +
  coord_sf()

pop_stars <- st_transform(pop_stars, crs = crs_all)

schools_sf <- st_as_sf(schools_brazil, crs = crs_all)
schools_sf$pop_dens <- st_extract(pop_stars, schools_sf)
