install.packages(dismo)

library(dismo)
library(rgdal)
library(raster)
library(sf)

#load x as your basemap raster
x <- raster("~/Desktop/RMZ-raster/RMZ.tif", package="raster")
x
#load df as your csv with lat long columns
df <- read.csv("~/Snapp/soilc-ca-rangelands/food-web-metaanalysis/data/sites.csv")
#project csv coords in the same datum as your basemap
sites <- st_as_sf(df,
                  coords = c('x', 'y'),
                  crs = "+proj=longlat +datum=WGS84 +no_defs")
sites_sp <- as(sites, "Spatial")

#projection(x) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
#buffer in this instance sets the radius that searches for values in your basemap if you want to see values over which your csv alings
sites.mrc <- spTransform(sites_sp, CRS(projection(x)))
data <- extract(x, sites.mrc, method="simple", buffer=1000, fun=median)
sites$rmz <- data
data

#plot
plot(x)
plot(sites.mrc,add=TRUE, col="seagreen2", pch = 18, cex = 2)


df$RMZ <- data
