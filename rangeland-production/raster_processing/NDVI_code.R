library(raster)
library(sf)
library(sp)
library(tidyverse)
library(rgeos)


##### CONSTANTS ##### 

setwd("/home/csparks/soilc-ca-rangelands/rangeland-production/raster_processing")
#dir.create('/home/csparks/soilc-ca-rangelands/rangeland-production/raster_processing')

## Inputs path and file names

path_input <- "/home/shares/soilcarbon"

ca_ndvi_filename <- "/NDVI_grassland/Raster_Grasslands/CA_Landsat8_maxNDVI_20161101_20170701.tif"
rangeland_filename <- "/NDVI_grassland/Raster_Grasslands/RMZ/RMZ_crop.tif" # A cropped version to use to test script
#rangeland_filename <- "/soilc-california/rangeland-production/data/RMZ/RMZ.tif" # Use for full CA dataset (time consuming)

ca_ndvi <- file.path(path_input, ca_ndvi_filename)
rangeland_in <- file.path(path_input, rangeland_filename)  

## Define the projection to use (ESPG 3310: NAD 83 California Albers)

newproj <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 
newproj <- "+proj=aea +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 

## Output filename

output_name <- "RMZ_NDVI_Mean_Ratio"


##### MAIN ##### 

## Load raster data
NDVI <- raster(ca_ndvi)
rangelands <- raster(rangeland_in)

## Load shapefiles
state <- file.path(path_input,"/soilc-california/rangeland-production/data/shapefiles/Admin/CA_State_TIGER2016.shp") %>%
  rgdal::readOGR()
county <- file.path(path_input,"/soilc-california/rangeland-production/data/shapefiles/Admin/CA_Counties_TIGER2016.shp") %>%
  rgdal::readOGR()

## Reproject and resample data
proj <- projectExtent(rangelands, crs = newproj)  # Set a new reprojection (blank raster)
range_proj <- projectRaster(rangelands, proj, method= "ngb", alignOnly = FALSE) # Apply new projection to rangelands data
NDVI_proj <- projectRaster(NDVI, range_proj, method = "ngb", alignOnly = FALSE) # Project, align, and crop the NDVI layer to the projection

## Plot data
plot(NDVI_proj)
plot(county, add=T)

## Create raster of mean values
mean_NDVI <- zonal(NDVI_proj, range_proj, 'mean', na.rm = TRUE) # This creates a matrix of mean values for each RMZ (median only works for smaller rasters)
mean_NDVI_df <- as.data.frame(mean_NDVI) # Converts into dataframe, col 1 shows RMZ value (1-6), col 2 shows corresponding mean value
mean_raster <- subs(range_proj, mean_NDVI_df, by=1, which=2, subsWithNA = TRUE) # Uses dataframe to reclassify raster (replace values in column 1 with matching values in column 2)

## Create raster of ratio of NDVI to mean value (NDVI/mean; only shows cells that fall within an RMZ)

ratio_raster <- NDVI_proj/mean_raster # Values less than one demonstrate that the NDVI value at that cell is less than the average for that RMZ type

## Write final raster to file

writeRaster(ratio_raster, filename = output_name, format = "GTiff", overwrite = TRUE, datatype = "FLT4S")
