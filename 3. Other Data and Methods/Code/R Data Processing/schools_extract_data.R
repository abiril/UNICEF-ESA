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
setwd("~")
load(file = "UNICEF-ESA/Data/Brazil/Grid_Square/schools_cropped.gpkg")


### Load in other data, crop and extract at school locations
ghs_built_c_10 <- read_stars("UNICEF-ESA/Data/Brazil/GHSL/GHS_BUILT_C_MSZ_E2018_GLOBE_R2023A_54009_10_V1_0_R10_C13.tif")
ghs_built_s_10 <- read_stars("UNICEF-ESA/Data/Brazil/GHSL/GHS_BUILT_S_E2018_GLOBE_R2023A_54009_10_V1_0_R10_C13.tif")
ghs_built_s_100 <- read_stars("UNICEF-ESA/Data/Brazil/GHSL/GHS_BUILT_S_E2025_GLOBE_R2023A_54009_100_V1_0_R10_C13.tif")
ghs_built_v_100 <- read_stars("UNICEF-ESA/Data/Brazil/GHSL/GHS_BUILT_V_E2025_GLOBE_R2023A_54009_100_V1_0_R10_C13.tif")
ghs_land_10 <- read_stars("UNICEF-ESA/Data/Brazil/GHSL/GHS_LAND_E2018_GLOBE_R2022A_54009_10_V1_0_R10_C13.tif")
ghs_land_100 <- read_stars("UNICEF-ESA/Data/Brazil/GHSL/GHS_LAND_E2018_GLOBE_R2022A_54009_100_V1_0_R10_C13.tif")
ghs_pop_100 <- read_stars("UNICEF-ESA/Data/Brazil/GHSL/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R10_C13.tif")

# crop
all_schools <- st_transform(all_schools, st_crs(ghs_built_c_10))
bbox <- st_bbox(all_schools)

built_c_10 <- st_crop(ghs_built_c_10, bbox)
built_s_10 <- st_crop(ghs_built_s_10, bbox)
built_s_100 <- st_crop(ghs_built_s_100, bbox)
built_v_100 <- st_crop(ghs_built_v_100, bbox)
land_10 <- st_crop(ghs_land_10, bbox)
land_100 <- st_crop(ghs_land_100, bbox)
pop_100 <- st_crop(ghs_pop_100, bbox)


# school locations
schools <- as.data.frame(all_schools[,c("name","x","y")])

all_schools <- cbind(all_schools, st_extract(built_c_10, st_coordinates(all_schools)))
all_schools <- cbind(all_schools, st_extract(built_s_10, st_coordinates(all_schools)))
all_schools <- cbind(all_schools, st_extract(built_s_100, st_coordinates(all_schools)))
all_schools <- cbind(all_schools, st_extract(built_v_100, st_coordinates(all_schools)))
all_schools <- cbind(all_schools, st_extract(land_10, st_coordinates(all_schools)))
all_schools <- cbind(all_schools, st_extract(land_100, st_coordinates(all_schools)))
all_schools <- cbind(all_schools, st_extract(pop_100, st_coordinates(all_schools)))

names(all_schools)[46:52] <- c("built_c_10", "built_s_10", "built_s_100", "built_v_100", 
                               "land_10", "land_100", "pop_100")

# nightlights
nightlights_2021 <- read_stars("UNICEF-ESA/Data/Brazil/Nightlights/VNL_v21_npp_2021_global_vcmslcfg_c202205302300.median_masked.dat.tif")
plot(nightlights_2021)

all_schools <- st_transform(all_schools, st_crs(nightlights_2021))
bbox <- st_bbox(all_schools)

nl_2021 <- st_crop(nightlights_2021, bbox)

schools <- as.data.frame(all_schools[,c("name","x","y")])
all_schools <- cbind(all_schools, st_extract(nl_2021, st_coordinates(all_schools)))
names(all_schools)[53] <- "nightlights"

# formatting

unique(all_schools$built_c_10) # 13  1 12  2  0 23  5 11 21
unique(all_schools$built_s_10) # 0-100
unique(all_schools$built_s_100) # ??
unique(all_schools$built_v_100) # ??
unique(all_schools$land_10) # 100 only
unique(all_schools$land_100) # 10000 only
unique(all_schools$pop_100) # continuous

plot(built_s_10)
plot(built_v_100)
plot(land_10, breaks = "equal")
plot(land_100, breaks = "equal")
plot(pop_100)


ggplot() +
  geom_stars(data = built_c_10) +
  scale_colour_manual(values = built_c_colours)

built_c_10.sf <- st_as_sf(built_c_10, as_points = TRUE)
names(built_c_10.sf)[1] <- "built_c"
built_c_10.sf$built_c <- as.integer(built_c_10.sf$built_c)

built_c_10.sf$x <- st_coordinates(built_c_10.sf)[,1]
built_c_10.sf$y <- st_coordinates(built_c_10.sf)[,2]

built_c_colours <- c("0" = "black", "1" = "#275e2e", "2" = "#1e9c2e", "3" = "#81f08f",
                     "4" = "#6cd5f0", "5" = "#ffdf3d", "11" ="#a86800", "12" = "#e36e00",
                     "13" = "#ff1900",  "14" = "#780000", "15" = "#360000", "21" = "#ff91ed",
                     "22" = "#eb3dce", "23" = "#870487", "24" = "#41015c", "25" = "#030054")
unique(built_c_10.sf$built_c)

ggplot() +
  geom_raster(data = built_c_10.sf, mapping = aes(x = x, y = y, fill = factor(built_c))) +
  scale_fill_manual(values = built_c_colours) +
  coord_sf()

###
all_schools$built_c_10 <- as.factor(all_schools$built_c_10)

ggplot() + geom_bar(data = all_schools, mapping = aes(x = built_c_10))
ggplot() + geom_histogram(data = all_schools, mapping = aes(x = built_s_10))
ggplot() + geom_histogram(data = all_schools, mapping = aes(x = built_s_100))
ggplot() + geom_histogram(data = all_schools, mapping = aes(x = built_v_100))
ggplot() + geom_histogram(data = all_schools, mapping = aes(x = pop_100))


### now look at all grid squares with schools as 0-1 indicator or counts

built_s_100.sf <- st_as_sf(built_s_100, as_points = TRUE)
names(built_s_100.sf)[1] <- "built_s"
built_s_100.sf$built_c <- as.integer(built_s_100.sf$built_s)

built_s_100.sf$x <- st_coordinates(built_s_100.sf)[,1]
built_s_100.sf$y <- st_coordinates(built_s_100.sf)[,2]

ggplot() +
  geom_raster(data = built_s_100.sf, mapping = aes(x = x, y = y, fill = built_s)) +
  geom_sf(data = all_schools) +
  coord_sf()

library(starsExtra)
grid <- make_grid(built_s_100.sf, res = 1)
plot(grid, breaks = "equal")

inter.count <- st_intersects(built_s_100, all_schools)
grid_count <- lengths(st_intersects(built_s_100, all_schools))
hist(grid_count)

save(all_schools, file = "UNICEF-ESA/Data/Brazil/Grid_Square/schools_cropped.gpkg")

### s2 

load(file = "UNICEF-ESA/Data/Brazil/Grid_Square/schools_cropped.gpkg")

s2 <- read_stars("UNICEF-ESA/Data/Brazil/Grid_Square/s2_cropped.tif")
s2.rgb <- st_rgb(s2[,,,c(4,3,2)], 3, probs = c(0.05, 0.95), stretch = "percent")
plot(s2.rgb)

all_schools <- st_transform(all_schools, st_crs(s2))

plot(s2.rgb)
points(all_schools$x, all_schools$y, col = "red")

mapview(s2) + all_schools

all_schools <- cbind(all_schools, st_extract(s2, st_coordinates(all_schools)))

names(all_schools)[c(54:64)] <-c("scl01", "blue02", "green03", "red04", "rededge106", "rededge207", 
                                 "rededge308", "nir05", "nnir09", "swir110", "swir211")
all_schools$scl01 <- as.factor(all_schools$scl01)


save(all_schools, file = "UNICEF-ESA/Data/Brazil/Grid_Square/schools_cropped_data.gpkg")


##classifications from s2
category <- c("No Data", "Saturated or defective", "Dark features or Shadows",
                "Cloud shadows", "Vegetation", "Not Vegetated", "Water", "Unclassified",
                "Cloud medium probability", "Cloud high probability", "Thin cirrus", "Snow or ice")
code <- c(0:11)
colour <- c("black", "red", "darkgrey", "grey", "darkgreen", "#aa6116", 
             "blue", "darkred", "lightgrey", "white", "lightblue", "#E3F2FD")

s2_class <- as.data.frame(cbind(category, code, colour))
s2_class$code <- as.factor(as.numeric(s2_class$code))

unique(all_schools$scl01)

all_schools$scl <- as.factor(ifelse(all_schools$scl01 == "4", "Vegetation",
                             ifelse(all_schools$scl01 == "5", "Not Vegetated",
                             ifelse(all_schools$scl01 == "9", "Cloud high probability", "Other"))))

save(all_schools, file = "UNICEF-ESA/Data/Brazil/Grid_Square/schools_cropped_data.gpkg")

##small

category <- c("Vegetation", "Not Vegetated", "Cloud high probability")
code <- c(4,5,9)
s2_colour <- c("darkgreen", "#aa6116", "white")

s2_class <- as.data.frame(cbind(category, code, colour))
s2_class$code <- as.factor(as.numeric(s2_class$code))

s2_colours <- mapviewColors(x = all_schools, zcol = "scl01",
                            colors = s2_colour, at = code) 

mapview(all_schools, zcol = "scl01", col = s2_colours)

