#######################
# Code for APTK in 2D #
#######################

#Steps
#Simulate dataset with know structure
#Sample from it - point and blocks
#Estimate variogram with REML

#1. Simulate synthetic dataset
#from http://santiago.begueria.es/2010/10/generating-spatially-correlated-random-fields-with-r/
library(gstat)
library(sp)
library(raster)

xy <- expand.grid(1:100, 1:100)
names(xy) <- c('x','y')
g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1, model=vgm(psill=0.025, range=5, model='Exp'), nmax=20)
yy <- predict(g.dummy, newdata=xy, nsim=4)

#show one realization
gridded(yy) = ~x+y
spplot(yy[1])

#show all four simulations:
spplot(yy)

#2. Sample from synthetic dataset

#sample points randomly
points<-spsample(yy,25,type="random")

#Issue takes random points - need to sample grid points
#Could take 5 random points and extract focal mean of area to get block support
pointsize<-25

point_sup <- yy[sample(1:length(yy),pointsize),]

#create grids

gridsize<-5

yy5 <- focal(raster(yy[1]), w=matrix(1/gridsize^2,nrow=5,ncol=5))

yy5 <- as.data.frame(yy5,xy=T)

yy5p <- SpatialPixelsDataFrame(points = yy5[c("x", "y")], data = yy5)

spplot(yy5p[3])
grid_sup <- yy5p[sample(1:length(yy5p),gridsize),]

#Two datasets - convert to data frames

sup_pt<-as.data.frame(point_sup[1])
sup_gr<-as.data.frame(grid_sup[3])

#Create discretised dataset

xg<-seq(-0.5*gridsize, 0.5*gridsize, length.out = 3)
yg<-seq(-0.5*gridsize, 0.5*gridsize, length.out = 3)
gridd<-expand.grid(x=xg,y=yg)

#Next use gridd to loop through and fill out discretised grid



