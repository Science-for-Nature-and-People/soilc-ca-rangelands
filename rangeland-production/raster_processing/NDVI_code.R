library(raster)
library(sf)
library(sp)
library(tidyverse)
library(rgeos)
library(rgdal)


#### CONSTANTS ####
#setwd("/home/csparks/soilc-ca-rangelands/rangeland-production/raster_processing")
#dir.create('/home/csparks/soilc-ca-rangelands/rangeland-production/raster_processing')

#### Inputs path and file names ####
path_input <- "/home/shares/soilcarbon"
ca_ndvi_filename <- "/NDVI_grassland/Raster_Grasslands/CA_Landsat8_maxNDVI_20161101_20170701.tif"
rangeland_filename <- "/NDVI_grassland/Raster_Grasslands/RMZ/CApam10nosoil_clip.tif" # A cropped version to use to test script
#rangeland_filename <- "/soilc-california/rangeland-production/data/RMZ/CApam10nosoil.tif" # Use for full CA dataset (time consuming)
vegetation_filename <- "NDVI_grassland/Raster_Grasslands/Vegetation/veg_grassland1-002.tif"
state_shape <- "soilc-california/shapefiles/Admin/CA_State_TIGER2016.shp"
county_shape <- "soilc-california/shapefiles/Admin/CA_Counties_TIGER2016.shp"

ca_ndvi <- file.path(path_input, ca_ndvi_filename)
rangeland_in <- file.path(path_input, rangeland_filename)  
vegetation_data <-file.path(path_input, vegetation_filename)
state_data <- file.path(path_input, state_shape)
county_data <- file.path(path_input, county_shape)

#### Define the projection to use (ESPG 3310: NAD 83 California Albers) ####
newproj <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 

#### Output filename ####
output_name <- "RMZ_NDVI_Mean_Ratio"


##### MAIN ##### 

#### Load raster data ####
NDVI <- raster(ca_ndvi)
rangelands <- raster(rangeland_in)
vegetation <- raster(vegetation_data)

#### Load shapefile data ####
county <- raster::shapefile(county_data)

#### Reproject and resample data ####
proj <- projectExtent(rangelands, crs = newproj)  # Set a new reprojection (blank raster)
range_proj <- projectRaster(rangelands, proj, method= "ngb", alignOnly = FALSE) # Apply new projection to rangelands data
NDVI_proj <- projectRaster(NDVI, range_proj, method = "ngb", alignOnly = FALSE) # Project, align, and crop the NDVI layer to the projection
veg_proj <- projectRaster(vegetation, range_proj, method = "ngb", alignOnly = FALSE) # Do the same for the annual grasslands layer
county_proj <- spTransform(county,CRS(newproj))

## Crop data to only annual grasslands
range_crop <- overlay(range_proj, veg_proj, fun = function(x, y) {
  x[is.na(y[])] <- NA
  return(x)
})

## Create raster of mean values
mean_NDVI <- zonal(NDVI_proj, range_crop, 'mean', na.rm = TRUE) # This creates a matrix of mean values for each RMZ (median only works for smaller rasters)
mean_NDVI_df <- as.data.frame(mean_NDVI) # Converts into dataframe, col 1 shows RMZ value (1-10), col 2 shows corresponding mean value
mean_raster <- subs(range_crop, mean_NDVI_df, by=1, which=2, subsWithNA = TRUE) # Uses dataframe to reclassify raster (replace values in column 1 with matching values in column 2)

## Create raster of ratio of NDVI to mean value (NDVI/mean; only shows cells that fall within an RMZ)
ratio_raster <- NDVI_proj/mean_raster # Values less than one demonstrate that the NDVI value at that cell is less than the average for that RMZ type

## Write final raster to file
# writeRaster(ratio_raster, filename = output_name, format = "GTiff", overwrite = TRUE, datatype = "FLT4S")


## READ IN FULL RASTER
ratio_raster <- raster("/home/shares/soilcarbon/Cristina_Outputs/RMZ_NDVI_Mean_Ratio.tif")

## PLOT FULL
r.spdf <- as(ratio_raster, "SpatialPixelsDataFrame")
r.df <- as.data.frame(r.spdf)
c.spdf <- fortify(county_proj,region="GEOID")
c.spdf$color <- 'NA'

ggplot() +
  geom_polygon(data = c.spdf, aes(long,lat,group=group,fill=as.numeric(color)),color="grey70") +
  geom_tile(data=r.df, aes(fill=RMZ_NDVI_Mean_Ratio,x=x,y=y)) +
  scale_fill_gradient2(low="#a50026",midpoint=1,mid="#ffffbf",high="#006837",na.value="white") +
  theme_bw() +
  coord_sf()


## PLOT SUBSET
c.sub <- crop(county_proj, extent(ratio_raster))

plot(ratio_raster, 
     box = FALSE, 
     axes = FALSE
)
plot(c.sub,
     add = T)

r.spdf <- as(ratio_raster, "SpatialPixelsDataFrame")
r.df <- as.data.frame(r.spdf)
c.spdf <- fortify(c.sub,region="GEOID")
c.spdf$color <- 'NA'
  
ggplot() +
  geom_polygon(data = c.spdf, aes(long,lat,group=group,fill=as.numeric(color)),color="black") +
  geom_tile(data=r.df, aes(fill=layer,x=x,y=y)) +
  scale_fill_gradient2(low="#a50026",midpoint=1,mid="#ffffbf",high="#006837",na.value="white") +
  theme_bw() +
  coord_sf()
