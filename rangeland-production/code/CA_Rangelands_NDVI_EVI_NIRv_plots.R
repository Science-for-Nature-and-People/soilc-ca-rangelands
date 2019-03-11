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

#---------------------------------------------------------------------------------------------
## load vegetation indices @ test sites
Bevi = brick("D:/Test_Site_Ouput/EVI_stack_Bakersfield.tif") #Google Drive
Levi = brick("D:/Test_Site_Ouput/EVI_stack_Lassen.tif") #Google Drive
Mevi = brick("D:/Test_Site_Ouput/EVI_stack_Merced.tif") #Google Drive
MHevi = brick("D:/Test_Site_Ouput/EVI_stack_Mt_Hamilton.tif") #Google Drive

Bndvi = brick("D:/Test_Site_Ouput/NDVI_stack_Bakersfield.tif") #Google Drive
Lndvi = brick("D:/Test_Site_Ouput/NDVI_stack_Lassen.tif") #Google Drive
Mndvi = brick("D:/Test_Site_Ouput/NDVI_stack_Merced.tif") #Google Drive
MHndvi = brick("D:/Test_Site_Ouput/NDVI_stack_Mt_Hamilton.tif") #Google Drive

Bnirv = brick("D:/Test_Site_Ouput/NIRv_stack_Bakersfield.tif") #Google Drive
Lnirv = brick("D:/Test_Site_Ouput/NIRv_stack_Lassen.tif") #Google Drive
Mnirv = brick("D:/Test_Site_Ouput/NIRv_stack_Merced.tif") #Google Drive
MHnirv = brick("D:/Test_Site_Ouput/NIRv_stack_Mt_Hamilton.tif") #Google Drive

#---------------------------------------------------------------------------------------------
# convert to data frames & plot trends

## stack all sites per veg index
Bevi <- as.data.frame(Bevi,xy=T,na.rm=T) # na.rm=T/F
Levi <- as.data.frame(Levi,xy=T,na.rm=T) # na.rm=T/F
Mevi <- as.data.frame(Mevi,xy=T,na.rm=T) # na.rm=T/F
MHevi <- as.data.frame(MHevi,xy=T,na.rm=T) # na.rm=T/F

summary(Bevi); dim(Bevi)

# add site label column, rename columns & combine into single data frame
Bevi$site <- rep("Bakersfield", length(Bevi$x))
Levi$site <- rep("Lassen", length(Levi$x))
Mevi$site <- rep("Merced", length(Mevi$x))
MHevi$site <- rep("Mt_Hamilton", length(MHevi$x))

yrs <- seq(1987,2018,1); yrs <- as.character(yrs)

colnames(Bevi) <- c("TCC",yrs,"x","y","site")
colnames(Levi) <- c("TCC",yrs,"x","y","site")
colnames(Mevi) <- c("TCC",yrs,"x","y","site")
colnames(MHevi) <- c("TCC",yrs,"x","y","site")

dat <- bind_rows(Bevi, Levi, Mevi, MHevi)
summary(dat); dim(dat)

# exclude all pixels >= 25% tree cover
dat.df <- dat %>% filter(TCC < 25)
summary(dat.df); dim(dat.df)
# get rid of TCC column
dat.df <- dplyr::select(dat.df, -TCC, -x, -y)

test <- dat.df %>% 
  gather("variable", "value", -site) %>% 
  group_by(variable, site) %>% 
  summarize(mean_val = mean(value), 
            sd_val = sd(value), 
            p05 = quantile(value, 0.05),
            p95 = quantile(value, 0.95))

# create year column
test$year <- rep(seq(1987,2018,1),each=4)

# plot 
ggplot(test, aes(x=year, y=mean_val, col=site)) + geom_line(size=1.2) +
  labs(x="Year", y="Peak EVI") +
  scale_colour_brewer(type = "div") +
  #scale_color_manual(values=c()) +
  ylim(0.2,0.7) + 
  theme_classic()

#---------------------------------------------------------------------------------------------
#------------- Load 3 veg indices for 2018 and compare -------------------------------------
# Bakersfield | Lassen | Merced | Mt_Hamilton
evi <- brick("D:/Test_Site_Ouput/EVI_stack_Mt_Hamilton.tif"); plot(evi)# 2000
ndvi <- brick("D:/Test_Site_Ouput/NDVI_stack_Mt_Hamilton.tif"); plot(ndvi)# 2000
nirv <- brick("D:/Test_Site_Ouput/NIRv_stack_Mt_Hamilton.tif"); plot(nirv)# 2000

tcc <- evi[[1]]
evi18 <- evi[[33]]
ndvi18 <- ndvi[[33]]
nirv18 <- nirv[[33]]

plot(tcc, col=colr)

all <- stack(tcc, evi18, ndvi18, nirv18)

levelplot(all[[2:4]], col.regions = colr, #colorkey = list(space = "bottom"),
          at=seq(0,1, length.out=10),
          names.attr=c("EVI","NDVI","NIRv"), xlab="",ylab="") +
  layer(sp.polygons(bak))

all.df <- as.data.frame(all,xy=T,na.rm=T)
summary(all.df); dim(all.df)
colnames(all.df) <- c("x","y","tcc","evi","ndvi","nirv")
all.df <- na.omit(all.df)

# exclude all pixels >= 25% tree cover
all.df <- all.df %>% filter(tcc < 25)
summary(all.df); dim(all.df)

# sample from all.df to plot
samp <- all.df[sample(nrow(all.df), 100000),]

ggplot(samp, aes(ndvi,evi)) + geom_point(col="black",alpha=0.08) + 
  xlim(0,1) + ylim(0,1) + 
  xlab("peak NDVI") + ylab("peak EVI") +
  geom_abline(intercept = 0, slope = 1, lwd=1, lty="dashed") +
  theme_classic()

ggplot(samp, aes(ndvi,nirv)) + geom_point(col="black",alpha=0.08) + 
  xlim(0,1) + ylim(0,1) + 
  xlab("peak NDVI") + ylab("peak NIRv") +
  geom_abline(intercept = 0, slope = 1, lwd=1, lty="dashed") +
  theme_classic()
