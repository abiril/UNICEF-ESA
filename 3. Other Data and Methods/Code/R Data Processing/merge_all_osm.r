library(ggplot2)
library(ggpubr)
library(mapview)
library(readr)
library(dplyr)
library(tidygeocoder)
library(tidyr)
#library(sp)
library(Rcpp)
library(sf)
library(stars)
library(stringdist)
library(stringr)


load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_and_osm_schools.RData")

head(more_schools)

more_schools$name[1:100]
more_schools$name.caps <- str_to_upper(more_schools$name)
more_schools$name.caps[1:100]

more_schools$coord_source[is.na(more_schools$coord_source)] <- "osm"

unique(is.na(more_schools$ID.muni))
unique(more_schools$coord_source)
names(more_schools)

one_muni <- subset(more_schools, ID.muni == 1)
names <- one_muni[,c("name.caps", "coord_source")]

'ggplot(one_muni) +
    geom_point(mapping = aes(x = x, y = y, color = name.caps)) +
    theme(legend.position = "bottom") +
    xlim(c(-62.0125, -61.975)) + ylim(c(-11.94, -11.91)) +
    geom_label(aes(label = name.caps, x = x, y = y, size = NULL), nudge_y = 0.002, jitter = TRUE)

all <- subset(more_schools, coord_source != "osm")
osm <- subset(more_schools, coord_source == "osm")

length(unique(osm$name))
nrow(osm)

lev <- stringdistmatrix(all$name.caps, osm$name.caps, useBytes = TRUE)


###need two columns in same dataset

stringdist("COLÉGIO TIRADENTES -  ESCOLA MILITAR", "COLEGIO TIRADENTES DA POLICIA MILITAR - CTPM XI")
stringdist("COLÉGIO TIRADENTES -  ESCOLA MILITAR", "EMEF NOVA VIDA" )
stringdist("COLÉGIO TIRADENTES -  ESCOLA MILITAR", "TIRADENTES OLÉGIO" )
'

####
names(more_schools)
all <- subset(more_schools, is.na(osm_id))
osm <- subset(more_schools, !is.na(adm02))

all.sf <- st_as_sf(all, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
osm.sf <- st_as_sf(osm, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")

plot(all.sf$geometry)
plot(osm.sf$geometry, col = "red", add = TRUE)

ggplot(all.sf$geometry) +
    geom_sf() + coord_sf() +
    geom_sf(osm.sf$geometry, mapping = aes(), color = "red") +
    xlim(c(-62.0125, -61.975)) + ylim(c(-11.94, -11.91)) 

range(st_coordinates(all.sf)[1,])

buffer_schools <- list()
for(i in 1:10){
    all.muni <- subset(all.sf, ID.muni == i)
    osm.muni <- subset(osm.sf, ID.muni == i)[,c("ID.muni", "name", "Municipality","fid", "osm_id", "geometry")]
    buffer <- st_buffer(all.muni, 1)
    buffer_schools[[i]] <- st_intersects(buffer, osm.muni)
}

names(buffer)
st_crs(buffer)

all.muni

mapview(buffer)

ggplot(buffer$geometry) +
    geom_sf(col = "green", size = 1) + coord_sf() +
    geom_sf(all.muni$geometry, mapping = aes(), color = "blue") +
    geom_sf(osm.muni$geometry, mapping = aes(), color = "red")

plot(buffer$geometry)

ggplot(all.muni$geometry) +
    geom_sf(col = "green", size = 10) + coord_sf() +
    geom_sf(all.muni$geometry, mapping = aes(), color = "blue") +
    geom_sf(osm.muni$geometry, mapping = aes(), color = "red")


near[[1]]
plot(buffer)

st_intersection(buffer, osm.muni)

mapview(all.muni)
mapview(buffer)




library(data.table)
library(rdist)

all.dt <- as.data.table(subset(all, ID.muni == 1))
osm.dt <- as.data.table(subset(osm, ID.muni == 1))

cbind(all.dt, osm.dt[apply(cdist(all.dt, osm.dt), 1, which.min),])

more <- st_as_sf(more_schools, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
more.muni <- subset(more, ID.muni == 1)

more.muni <- st_transform(more.muni, crs = 29101)

more_neigbours <- st_buffer(more.muni, 1) %>% 
  st_join(more.muni %>% select(geometry)) %>% 
  st_drop_geometry() #%>% 
  #group_by(id, name, area) %>% 
  #summarise(no_neigbours = sum(n()))

neighbours <-  st_buffer(more.muni, 1)



library(fuzzyjoin)

all.muni <- subset(all, ID.muni == 1 | ID.muni == 2)
osm.muni <- subset(osm, ID.muni == 1 | ID.muni == 2)
more.muni <- subset(more_schools, ID.muni == 1)


joined <- fuzzy_join(all.muni, osm.muni, exact = "ID.muni", fuzzy = c("name.caps", "x", "y"))


write.csv(more_schools, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_and_osm_schools.csv")

names(more_schools)
unique(more_schools$coord_source)
more_schools <- st_drop_geometry(more_schools)

more.sf <- st_as_sf(more_schools, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
st_write(more.sf, "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_and_osm_schools.shp", driver = "ESRI Shapefile", delete_layer = TRUE)

library(openxlsx)
more.dt <- as.data.table(more_schools)

openxlsx::write.xlsx(more_schools, "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_and_osm_schools.xlsx")

more_schools <- more_schools[,-83]
more_schools$id <- 1:nrow(more_schools)
write.csv(more_schools, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_and_osm_schools.csv", row.names = TRUE)

all <- subset(more_schools, is.na(osm_id))
osm <- subset(more_schools, !is.na(adm02))

write.csv(all, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_schools.csv", row.names = TRUE)
write.csv(osm, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/osm_schools.csv", row.names = TRUE)

all.muni <- subset(all, ID.muni %in% c(1:100))
osm.muni <- subset(osm, ID.muni %in% c(1:100))

write.csv(all.muni, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_schools_100.csv", row.names = TRUE)
write.csv(osm.muni, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/osm_schools_100.csv", row.names = TRUE)

length(unique(all$ID.muni))
length(unique(osm$ID.muni))

Brazil <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Shapefiles/Brazil_Admin_0_to_2/bra_admbnda_adm2_ibge_2020.shp")
st_crs(Brazil) <- "+proj=longlat +ellps=GRS80 +no_defs +type=crs" ##from source
Brazil <- st_transform(Brazil, crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
muni <- Brazil[,c("ADM2_PT")]
muni <- cbind(muni, 1:nrow(muni))
names(muni)[2] <- "ID.muni"

plot(muni)

region <- Brazil[,c("ADM1_PT")]
region <- cbind(region, 1:nrow(region))
names(region)[2] <- "ID.region"
plot(region)

names(Brazil)
all.munis <- subset(muni, !(ID.muni %in% all$ID.muni))
osm.munis <- subset(muni, !(ID.muni %in% osm$ID.muni))

plot(all.munis$geometry)
plot(osm.munis$geometry)

muni$has_schools <- ifelse((muni$ID.muni %in% all$ID.muni) & (muni$ID.muni %in% osm$ID.muni), "both",
                        ifelse((muni$ID.muni %in% all$ID.muni) & !(muni$ID.muni %in% osm$ID.muni), "all",
                        ifelse(!(muni$ID.muni %in% all$ID.muni) & (muni$ID.muni %in% osm$ID.muni), "osm", "none")))
unique(muni$has_schools)

plot(muni$has_schools)
ggplot(muni) + 
    coord_sf() + geom_sf(mapping = aes(fill = factor(has_schools)))


### split into regions????
names(more_schools)
names(muni)
names(Brazil)

regions <- Brazil[,c("ADM2_PT", "ADM1_PT")]
regions <- cbind(regions, 1:nrow(regions))
names(regions)[3] <- "ID.muni"

#adm1 <- Brazil[,c("ADM0_PT", "ADM1_PT", "ADM1_PCODE", "ADM2_PT")]
#adm1$ID.region <- as.numeric(as.factor(adm1$ADM1_PT))
#adm1 <- adm1[,c("ADM1_PT", "ID.region", "ADM2_PT")]
adm1 <- unique(regions)
more_schools2 <- merge(more_schools, adm1, by = "ID.muni", x.all = TRUE, y.all = FALSE)
more_schools2 <- unique(more_schools2)

View(more_schools2[351000:352029,])
unique(is.na(more_schools$ADM2_PT))

names(more_schools)
head(more_schools2)
unique(more_schools$ADM1_PT)
unique(more_schools$ID.region)
unique(more_schools$coord_source)

area_schools <- list()
all <- list()
osm <- list()
for (i in 1:27) {
    area_schools[[i]] <- subset(more_schools, ID.region == i)
    #all[[i]] <- subset(area_schools[[i]], is.na(osm_id))
    osm[[i]] <- subset(area_schools[[i]], !is.na(adm02))

    #all[[i]] <- st_drop_geometry(all[[i]])
    osm[[i]] <- st_drop_geometry(osm[[i]])

    #all[[i]] <- all[[i]][,-97]
    osm[[i]] <- osm[[i]][,-97]

    #write.csv(all[[i]], file = paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/all_schools", i , ".csv", sep = ""), row.names = TRUE)
    write.csv(osm[[i]], file = paste("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/osm_schools", i , ".csv", sep = ""), row.names = TRUE)

}
head(area_schools[[1]])

unique(more_schools$coord_source)

osm[[i]] <- subset(area_schools[[i]], !is.na(osm_id))

head(osm[[10]])

names(area_schools[[1]])
