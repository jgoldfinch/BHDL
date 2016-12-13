#                                                                              
# BHDL 2016 Pilot Study Design 
# Jessie Golding
# November 6, 2016

# Load packages
library(raster)
library(rgdal)
library(tmvtnorm)


# read in raster and shapefiles
# fisher habitat raster Olson et al. 2014
fhab <-raster("C:/Users/jgolding/Documents/fisher.tif")
fgrid <-shapefile("C:/Users/jgolding/Documents/USFS_R1_Carnivore_Monitoring/GIS/USFS/BHDL_fisher_grid_UTM.shp")
fgridint <-shapefile("C:/Users/jgolding/Documents/USFS_R1_Carnivore_Monitoring/GIS/USFS/BHDL_fisher_grid_UTM_intersect.shp")
bhdl <-shapefile("C:/Users/jgolding/Documents/USFS_R1_Carnivore_Monitoring/GIS/USFS/r1_beaverheaddeerlodge_nf_UTM.shp")



# select cells with 10 or more % in BHDL
# fgridint <-fgridint[fgridint$Percent >= 10,]
# keepid <-fgridint$ID

# subset fgrid to only those with 10% or more
# fgrid <-fgrid[fgrid$ID %in% keepid,]

# quantify amount of fisher habitat within each of the selected cells
# crop habitat raster for quicker calculation
fhabcrop <-crop(fhab, bhdl)
# a<-extract(fhabcrop, fgrid, fun=mean)
# a2<-extract(fhabcrop, fgrid, fun=max, cellnumbers=TRUE)

#a2[is.na(a2),] <-0

# the above action goes really slowly - try rasterizing fgrid
fgridr <-rasterize(fgrid, fhabcrop)


#########################################################################################
# Method from Josh Nowak to create grid with raster package
dsn <-"C:/Users/jgolding/Documents/USFS_R1_Carnivore_Monitoring/GIS/USFS"
dsn2 <-"C:/Users/jgolding/Documents/USFS_R1_Carnivore_Monitoring/GIS/RMRS"

shp <-readOGR(dsn=dsn,layer="r1_beaverheaddeerlodge_nf_UTM")
shp2 <-readOGR(dsn=dsn2,layer="Fisher_5_mile_Grid_UTM_Habit_UTM_83Z11")

# Create extent for "reference" raster that will evently fit 5x5 mi (8046.72 x 8046.72 m) grid cells
# Extent calculated from shp UTM coordinates 
ext<-extent(shp)
extb <-c(ext@xmin - (((33*8046.72) - (ext@xmax-ext@xmin)/2)),ext@xmax + (((33*8046.72) - (ext@xmax-ext@xmin)/2)),
         ext@ymin - (((33*8046.72) - (ext@ymax-ext@ymin)/2)),ext@ymax + (((33*8046.72) - (ext@ymax-ext@ymin)/2)))
extb <-matrix(NA,2,2)
extb[1,1] <-ext@xmin - (((33*8046.72) - (ext@xmax-ext@xmin)/2))
extb[1,2] <-ext@xmax + (((33*8046.72) - (ext@xmax-ext@xmin)/2))
extb[2,1] <-ext@ymin - (((33*8046.72) - (ext@ymax-ext@ymin)/2))
extb[2,2] <-ext@ymax + (((33*8046.72) - (ext@ymax-ext@ymin)/2))

# Define resolution for "reference" raster
res <-c(8046.72,8046.72)

# Create "reference" raster - r 
r <- raster(extent(extb), res=res, crs=crs(shp))
# add cell values for plotting purposes only!
r[] <-1:ncell(r) 
# plot to visualize (sunset palette from coolors color palette)
plot(r, col=c(colorRampPalette(c("#DD4D46","#ED9349","#4FAFAF","#43AA8B"))(ncell(r))))
    
# rasterize Beaverhead-Deerlodge shapefile using "reference" raster r
# in the raster file bhdlr values are 1 for raster cells within BHDL 
bhdlr <-rasterize(shp,r)

# rasterize sampling grid file (fgrid)
fgridr2 <- rasterize(fgrid,r)
writeRaster(fgridr2, filename="USFS_R1_Carnivore_Monitoring/GIS/bhdlfisher5migrid.tif", overwrite=TRUE)

# you can also create a raster of Beaverhead-Deerlodge cells using the mask function
# in the raster file r2 values correspond to cell number (like in r raster)
r2 <-mask(r,bhdlr)

# project fisher habitat raster with the same extent and coordinate system as "reference" raster
fhab2 <-projectRaster(fhab, r)

# visualize
plot(fhab2, col=c(colorRampPalette(c("#DD4D46","#ED9349","#4FAFAF","#43AA8B"))(length(unique(values(fhab2))))))

# check to see that all coordinates are identical
# fhab2 - habitat information
# r - reference raster (values = cell #s for display purposes only)
# bhdlr - Beaverhead Deerlodge as a raster, (values = 1 in the forest, NA everywhere else)
# r2 - Beaverhead Deerlodge as a raster, (values = cell# in the forest, NA everywhere else)
identical(coordinates(fhab2), coordinates(r), coordinates(r2), coordinates(bhdlr))


# Trying to extract fhab2 raster values from bhdl cells
# This method does not work because it's taking values of fhab based on the shapefile, not the bhdlr raster
# a3 <-extract(fhab2, shp, method=bilinear, cellnumbers=TRUE)
# a3 <-as.data.frame(a3[[1]][,1:2])
# a3[is.na(a3$value), ] <-0
# a3 <-a3[a3$value!=0,]
# hist(a3$value)
# quantile(a3$value, .5)
# a3 <-a3[a3$value>=quantile(a3$value, .75),]
# a3<- a3[order(-a3$value),] 
# a3$priority <-1:nrow(a3)
# cellstosample <-a3$cell

# plot(bhdlr)
# r3 <- bhdlr
# r3[setdiff(seq_len(ncell(r3)), cellstosample)] <- NA
# r3[!is.na(r3)] <- 1
# plot(rasterToPolygons(r3, dissolve=TRUE), add=TRUE, border='red', lwd=2)

s <-stack(fhab2,bhdlr)
cellstosample <-which(values(bhdlr==1))
mat <- as.data.frame(extract(s,cellstosample))
mat$cells <-cellstosample
mat <-as.data.frame(na.omit(mat))
#mat <-mat[mat$fisher>=quantile(mat$fisher, .75),]
mat <- mat[order(-mat$fisher),] 
mat$priority <-1:nrow(mat)

cellstosamplefisher <-sort(mat$cells)
cellstosamplefisher2 <-mat$cells[mat$priority<13]


r2[values(r2)%in%cellstosamplefisher2,]<-10000
plot(r2)

# write sampling raster to file
writeRaster(r2, filename="USFS_R1_Carnivore_Monitoring/GIS/bhdlfishersample.tif", overwrite=TRUE)

# write habitat raster to file
writeRaster(fhab2, filename="USFS_R1_Carnivore_Monitoring/GIS/bhdlfisherhabraster.tif", overwrite=TRUE)
#############################################################################################
# Lynx habitat
lynx <-raster("C:/Users/jgolding/Documents/USFS_R1_Carnivore_Monitoring/GIS/USFS/tdrive/lynx/lynx_habitat_nrla_2005_100k/lynxhab2005.tif")
plot(lynx)

# project lynx habitat raster with the same extent and coordinate system as "reference" raster
lhab <-projectRaster(lynx, r)
plot(lhab, col=c(colorRampPalette(c("#DD4D46","#ED9349","#4FAFAF","#43AA8B"))(length(unique(values(lhab))))))
identical(coordinates(fhab2), coordinates(r), coordinates(r2), coordinates(bhdlr), coordinates(lhab))


s2 <-stack(lhab,bhdlr)
cellstosamplelynx <-which(values(bhdlr==1))
mat2 <- as.data.frame(extract(s2,cellstosamplelynx))



#########################################################################################

# Fisher power anaylsis
# Starting assumption - 3 fisher
f <-3

# Sampling grid - r2 (where values = 10000)

# Distribute them non-randomly on the landscape using the "animalsnonrandomhab" function
# function takes the following argument

# n = # of animals
# aoi = area of interest
# hab = habitat raster
# spmin = habitat cutoff for species
source("USFS_R1_Carnivore_Monitoring/data/simulation/animalsnonrandomhab.R")
xy <-animalsnonrandomhab(f,bhdl,fhab,0.05)

# Simulate locations at each temporal replicate by simulating 
# home range data using a multivariate normal

# create matrix to hold data
# total number of observations
n <-3
reps <-2
n.obs <- n*reps
nindv <-as.numeric(rep(1:n,reps))

############## This only needs to be created for the non-closure assumption ##############
hrc <-matrix(NA,nrow=n.obs,ncol=3)

# Specify covariance matrix
sigma <-matrix(c(1,0,0,1),nrow = 2, ncol = 2)


for (i in 1:n.obs){
  hrc[i,1] <-nindv[i]
  #Truncated mulitvariate normal using the tmvtnorm package
  hrc[i,2:3]<-rtmvnorm(1,as.numeric(xy[hrc[i,1],]),sigma,lower=c(ext@xmin,ext@ymin), upper= c(ext@xmax, ext@ymax)) 
}

hrc <-as.data.frame(hrc)
hrc$reps <-rep(1:reps,each=n)
colnames(hrc)<-c("indiv","x","y","reps")

###########################################################################################
# Create yhat (true occupancy)
test<-as.data.frame(rasterToPoints(r2,cellnumbers=TRUE))
test$cell <-rep(1:ncell(r2))
sampledcells<-test$cell[test$layer==10000]


# fill in yhat - are the cells occupied at the time of sampling
scells <-matrix(NA,length(sampledcells),reps)
scells <-as.data.frame(cbind(scells[,1],scells[,2],scells[,3],scells[,4],sampledcells))

# scells[i,j] <-as.numeric(extract(rsgrid,hrc[hrc$reps==j & hrc$indiv==i,2:3]))

tmp <-matrix(NA,reps,n)
index <- 1:length(sampledcells)

for(i in 1:length(index)){
  for (j in 1:reps){
    for (k in 1:n){ 
      tmp[j,k]<-as.data.frame(extract(rsgrid,hrc[hrc$reps==j & hrc$indiv==k,2:3],cellnumbers=TRUE))[,1]
    }
    scells[i,j] <-ifelse(scells$sampledcells[i] %in% tmp[j,]==TRUE,1,0)
  }
}


# Now create matrix for yhat
yhat <-as.data.frame(cbind(scells[,1],scells[,2],scells[,3],scells[,4]))


#Now create y which is actual deteciton

# set a detection probabilty (p)
p <-0.5


# generate matrix to hold data
y <-matrix(NA,length(sampledcells),reps)


# simulate observation data - observation process

for (j in 1:reps){ 
  y[,j] <-rbinom(n=length(sampledcells), size=1, prob = (yhat*p)[,j])
}

y<-as.data.frame(y)

#Plots
plot(brnf.r)
points(xy)
points(hrc[hrc$reps==1,2],hrc[hrc$reps==1,3], col="red", pch=20)
points(hrc[hrc$reps==2,2],hrc[hrc$reps==2,3], col="blue", pch=20)
points(hrc[hrc$reps==3,2],hrc[hrc$reps==3,3], col="green", pch=20)
points(hrc[hrc$reps==4,2],hrc[hrc$reps==4,3], col="orange", pch=20)

