#                                                                              
# BHDL 2016 Pilot Study Design 
# Jessie Golding
# November 6, 2016

# Load packages
library(raster)


# read in raster and shapefiles
# fisher habitat raster Olson et al. 2014
fhab <-raster("C:/Users/jgolding/Documents/fisher.tif")
fgrid <-shapefile("C:/Users/jgolding/Documents/USFS_R1_Carnivore_Monitoring/GIS/USFS/BHDL_fisher_grid_UTM.shp")
fgridint <-shapefile("C:/Users/jgolding/Documents/USFS_R1_Carnivore_Monitoring/GIS/USFS/BHDL_fisher_grid_UTM_intersect.shp")
bhdl <-shapefile("C:/Users/jgolding/Documents/USFS_R1_Carnivore_Monitoring/GIS/USFS/r1_beaverheaddeerlodge_nf_UTM.shp")



# select cells with 10 or more % in BHDL
fgridint <-fgridint[fgridint$Percent >= 10,]
keepid <-fgridint$ID

#subset fgrid to only those with 10% or more
fgrid <-fgrid[fgrid$ID %in% keepid,]

# quantify amount of fisher habitat within each of the selected cells
# crop habitat raster for quicker calculation
fhabcrop <-crop(fhab, bhdl)
a<-extract(fhabcrop, fgrid, fun=mean)
a2<-extract(fhabcrop, fgrid, fun=max, cellnumbers=TRUE)

a2[is.na(a2),] <-0

# the above action goes really slowly - try rasterizing fgrid
fgridr <-rasterize(fgrid, fhabcrop)

