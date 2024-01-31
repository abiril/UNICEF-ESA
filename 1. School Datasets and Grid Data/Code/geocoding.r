library(readr)
library(dplyr)
library(tidygeocoder)
library(tidyr)



all_schools <- st_read("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/brazil_all_schools.gpkg")
names(all_schools)


duplicates <- all_schools[duplicated(all_schools[,c("X.chosen", "Y.chosen")]), ]

unique(duplicates$coord_source)
geocode2 <- subset(duplicates, coord_source == "1st_geocode")

filled_missing <- geocode2
names(filled_missing)

filled_missing$address <- paste(filled_missing$name, ", ", filled_missing$Municipality , ", ", filled_missing$ADM1_PT, ", Brazil", sep = "")
filled_missing$address

geocoded_filled_missing <- geocode(filled_missing, address, method = 'arcgis', lat = Latitude.new2 , long = Longitude.new2)


geocoded_filled_missing

geocoded_filled_missing$Latitude.new2
geocoded_filled_missing$Longitude.new2

nrow(geocoded_filled_missing) #33,434
unique(geocoded_filled_missing[,c("Latitude.new2", "Longitude.new2")]) #12,364

save(geocoded_filled_missing, file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/geocoded_filled.RData")
load(file = "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/Other_Schools/geocoded_filled.RData")