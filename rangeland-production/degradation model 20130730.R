############################################################################
# RMRS         ... BIOMASS MODEL
# RESEARCHERS  ... MATT REEVES
# CODE AUTHOR  ... SCOTT BAGGETT
# FILE DATE    ... 2013-07-30
############################################################################
## DETAILS
# Degradation analysis of Bob's field evaluating both mean and trend.
############################################################################
library(nlme)
library(fields)
library(reshape)
setwd("C:/MCR/Degradation/")
source("C:/MCR/Degradation/fdr.R")
d1 <- read.table("Bobs_Field_CODE_Test.csv", header = TRUE, sep = ",")
nyears <- 11

year = c(1:nyears)
z1 <- data.frame(matrix(ncol = 17, nrow = nrow(d1))) 
for(i in 1:nrow(d1)) 
{
  d1a <- d1[-i,]
  p1 <- cbind(melt(d1a, id=c("FieldNum","X","Y")),as.matrix((rep(1:nyears,each=nrow(d1a)))))
  colnames(p1) <- c("FieldNum","X","Y","description","y","x")
  p1.mean <- lm(y~1,data=p1)
  p1.trend <- lm(y~x,data=p1)


  y <- as.vector(t(d1[i,4:(nyears+3)]))
  d1.mean <- gls(y ~ 1, correlation=corARMA(p=1), method="ML")
  d1.trend <- gls(y ~ year, correlation=corARMA(p=1), method="ML")
  degf.trend <- d1.trend$dims$N - d1.trend$dims$p
  degf.mean <- d1.mean$dims$N - d1.mean$dims$p

  z1[i,1] <- d1[i,1]                                             # FieldNum
  z1[i,2] <- d1[i,2]                                             # X
  z1[i,3] <- d1[i,3]                                             # Y

  z1[i,4] <- mean(y)                                             # mean (field)
  z1[i,5] <- mean(p1$y)                                          # mean (population)
  z1[i,6] <- z1[i,4] - z1[i,5]                                   # mean difference
  z1[i,7] <- sqrt(diag(d1.mean$varBeta))[1]                      # standard error of mean
  z1[i,8] <- z1[i,6]/z1[i,7]                                     # t-statistic of difference in means
  z1[i,9] <- 2*pt(abs(z1[i,8]),df=degf.mean,lower.tail=FALSE)    # p-value of difference in means
  z1[i,10] <- NA                                                 # reserved for adjusted mean difference p-value

  z1[i,11] <- d1.trend$coefficients[2]                           # slope (field)
  z1[i,12] <- p1.trend$coefficients[2]                           # slope (population)
  z1[i,13] <- z1[i,11] - z1[i,12]                                # slope difference
  z1[i,14] <- sqrt(diag(d1.trend$varBeta))[2]                    # standard error of slope coefficient
  z1[i,15] <- z1[i,13]/z1[i,14]                                  # t-statistic of difference in slope coefficients
  z1[i,16] <- 2*pt(abs(z1[i,15]),df=degf.trend,lower.tail=FALSE) # p-value of difference in slope coefficients
  z1[i,17] <- NA                                                 # reserved for adjusted slope difference p-value
}

# z1[,10] <- p.adjust(z1[,9], method = "BH")
# z1[,17] <- p.adjust(z1[,16], method = "BH")

par(mfrow=c(2,2))

a1 <- fdr(z1[,9], method="general", adjustment.method = "mean", qlevel=0.2)
b1 = rep(0,length(z1[,9]))
b1[a1] = 1

a2 <- fdr(z1[,16], method="general", adjustment.method = "mean", qlevel=0.2)
b2 = rep(0,length(z1[,16]))
b2[a2] = 1

z1[,10] <- b1
z1[,17] <- b2


colnames(z1) <- c("FieldNum","X","Y","field(mean)","pop(mean)","diff(mean)","SE(mean)","t(mean)","p(|t|<T)(mean)","Adj. Sig. (mean)",
                  "field(slope)","pop(slope)","diff(slope)","SE(slope)","t(slope)","p(|t|<T)(slope)","Adj. Sig. (slope)")

par(mfrow=c(2,2))

quilt.plot(x=z1[,2], y=z1[,3], z=z1[,8], ylab="Northing", nx=50, ny=50,
           xlab="Easting", main="mean difference\nt-statistic",
           legend.lab="")

quilt.plot(x=z1[,2], y=z1[,3], z=z1[,10], ylab="Northing", nx=50, ny=50,
           xlab="Easting", main="mean difference\nsignificance",
           legend.lab="",zlim=c(0,1),col=c("grey80","red"),
           axis.args=list(at=c(0.25,0.75),labels=c("NS","S")))

quilt.plot(x=z1[,2], y=z1[,3], z=z1[,15], ylab="Northing", nx=50, ny=50,
           xlab="Easting", main="slope difference\nt-statistic",
           legend.lab="")

quilt.plot(x=z1[,2], y=z1[,3], z=z1[,17], ylab="Northing", nx=50, ny=50,
           xlab="Easting", main="slope difference\nsignificance",
           legend.lab="",zlim=c(0,1),col=c("grey80","red"),
           axis.args=list(at=c(0.25,0.75),labels=c("NS","S")))


write.table(z1,file="K:/Consulting/Reeves, Matt 2011 08 - Degradation Analysis/R/comparison analysis.csv",sep=",",row.names=F)


