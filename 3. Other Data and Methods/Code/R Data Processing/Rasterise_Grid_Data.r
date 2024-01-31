library(readr)
library(terra)

grid_data <- read.csv("/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_school_count_wData.csv")
head(grid_data)

grid_data <- grid_data[c(17,18,1:16)]

raster_data <- rast(grid_data, type = 'xyz', crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", digits = 6, extent = NULL)

writeRaster(raster_data, "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_school_count_raster.tif", filetype = "GTiff", overwrite = TRUE)


plot(raster_data)


head(raster_data[["NUMPOINTS"]])

writeRaster(raster_data[["NUMPOINTS"]], "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_NUMPOINTS_raster.tif", filetype = "GTiff", overwrite = TRUE)
writeRaster(raster_data[["ID"]], "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_ID_raster.tif", filetype = "GTiff", overwrite = TRUE)
writeRaster(raster_data[["land"]], "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_land_raster.tif", filetype = "GTiff", overwrite = TRUE)
writeRaster(raster_data[["pop"]], "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_pop_raster.tif", filetype = "GTiff", overwrite = TRUE)
writeRaster(raster_data[["built_s"]], "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_built_s_raster.tif", filetype = "GTiff", overwrite = TRUE)
writeRaster(raster_data[["built_v"]], "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_built_v_raster.tif", filetype = "GTiff", overwrite = TRUE)
writeRaster(raster_data[["smod.10"]], "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_smod_10_raster.tif", filetype = "GTiff", overwrite = TRUE)
writeRaster(raster_data[["smod.11"]], "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_smod_11_raster.tif", filetype = "GTiff", overwrite = TRUE)
writeRaster(raster_data[["smod.12"]], "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_smod_12_raster.tif", filetype = "GTiff", overwrite = TRUE)
writeRaster(raster_data[["smod.13"]], "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_smod_13_raster.tif", filetype = "GTiff", overwrite = TRUE)
writeRaster(raster_data[["smod.21"]], "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_smod_21_raster.tif", filetype = "GTiff", overwrite = TRUE)
writeRaster(raster_data[["smod.22"]], "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_smod_22_raster.tif", filetype = "GTiff", overwrite = TRUE)
writeRaster(raster_data[["smod.23"]], "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_smod_23_raster.tif", filetype = "GTiff", overwrite = TRUE)
writeRaster(raster_data[["smod.30"]], "/home/azureuser/cloudfiles/code/Users/ariley/Data/Schools/School_Counts/NE_10km_smod_30_raster.tif", filetype = "GTiff", overwrite = TRUE)
