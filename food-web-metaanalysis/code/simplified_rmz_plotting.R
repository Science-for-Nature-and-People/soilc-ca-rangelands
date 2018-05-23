install.packages(dismo)

library(dismo)
library(rgdal)
library(raster)
library(sf)

x <- raster("~/Desktop/RMZ-raster/RMZ.tif", package="raster")
x
df <- read.csv("~/Snapp/soilc-ca-rangelands/food-web-metaanalysis/data/sites.csv")
sites <- st_as_sf(df,
                  coords = c('x', 'y'),
                  crs = "+proj=longlat +datum=WGS84 +no_defs")
sites_sp <- as(sites, "Spatial")

#projection(x) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
sites.mrc <- spTransform(sites_sp, CRS(projection(x)))
data <- extract(x, sites.mrc, method="simple", buffer=1000, fun=median)
sites$rmz <- data
data


plot(x)
plot(sites.mrc,add=TRUE, col="seagreen2", pch = 18, cex = 2)


df$RMZ <- data
