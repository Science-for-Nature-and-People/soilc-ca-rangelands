# Plot maps and NDVI, EVI, NIRv relationships for 4 CA rangeland test sites 

setwd("C:/Users/eordway/Desktop")

# Load libraries
library(raster); library(rgdal); library(sp)
library(reshape2); library(dplyr); library(tidyr); library(rgeos)
library(GISTools); library(sf); library(SDMTools)
library(ggplot2); library(rasterVis); library(viridis)

colr = inferno(100, direction=-1)

#----- Load covariates & TCC ----------------------------------------------------
## load CA shapefile
CA = readOGR(dsn="CA_state_boundary", layer="CA_State_TIGER2016"); plot(CA)

## load 5 test site shapfiles
bak = readOGR(dsn="Test_Sites", layer="Bakersfield"); plot(bak)
las = readOGR(dsn="Test_Sites", layer="Lassen"); plot(las)
mrc = readOGR(dsn="Test_Sites", layer="Merced"); plot(mrc)
mh = readOGR(dsn="Test_Sites", layer="Mt_Hamilton"); plot(mh)

## load GLCF Tree Canopy Cover product (2000)
## also 2005 & 2010
tcc <- raster("D:/DEM_Precip_Soil_TCC/TCC_CA_2000.tif"); plot(tcc)# 2000

## load & stack veg indices for all years
current.list <- list.files(path="Temp_Veg_Indices/EVI/",pattern =".tif$", full.names=TRUE)
#current.list <- list.files(path="Temp_Veg_Indices/NDVI/",pattern =".tif$", full.names=TRUE)
#current.list <- list.files(path="Temp_Veg_Indices/NIRv/",pattern =".tif$", full.names=TRUE)
veg.stack <- stack(current.list); dim(veg.stack) # should have 31 bands
plot(veg.stack, zlim = c(0,1), col=colr)# zlim = c(0,1),

## add TCC to stack
dat.stack <- stack(tcc,veg.stack); dim(dat.stack) # should have 32 bands
plot(dat.stack, col=colr)# zlim = c(0,1),

#-----------------------------------------------------------------------------
## crop stacked vegetation indices to test sites
out_rastB <- crop(dat.stack, bak, snap="out"); plot(trim(out_rastB), zlim = c(0,1), col=colr)
out_B <- mask(out_rastB, bak); plot(out_B, zlim = c(0,1), col=colr)

out_rastL <- crop(dat.stack, las, snap="out"); plot(trim(out_rastL), zlim = c(0,1), col=colr)
out_L <- mask(out_rastL, las); plot(out_L, zlim = c(0,1), col=colr)

out_rastM <- crop(dat.stack, mrc, snap="out"); plot(trim(out_rastM), zlim = c(0,1), col=colr)
out_M <- mask(out_rastM, mrc); plot(out_M, zlim = c(0,1), col=colr)

out_rastMH <- crop(dat.stack, mh, snap="out"); plot(trim(out_rastMH), zlim = c(0,1), col=colr)
out_MH <- mask(out_rastMH, mh); plot(out_MH, zlim = c(0,1), col=colr)

# write rasters 
string_out <- "EVI_stack" # EVI_stack | NDVI_stack | NIRv_stack
writeRaster(out_B, format="GTiff", paste('D:/Test_Site_Ouput/',string_out,'_Bakersfield.tif',sep=""), overwrite=T)
writeRaster(out_L, format="GTiff", paste('D:/Test_Site_Ouput/',string_out,'_Lassen.tif',sep=""), overwrite=T)
writeRaster(out_M, format="GTiff", paste('D:/Test_Site_Ouput/',string_out,'_Merced.tif',sep=""), overwrite=T)
writeRaster(out_MH, format="GTiff", paste('D:/Test_Site_Ouput/',string_out,'_Mt_Hamilton.tif',sep=""), overwrite=T)