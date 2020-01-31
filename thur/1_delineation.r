library(data.table)
library(rgrass7)
library(sp)
library(WatershedTools)
library(raster)
library(rgdal)



tifInt <- c("COMPRESS=LZW", "PREDICTOR=2")
tifFloat <- c("COMPRESS=LZW", "PREDICTOR=3")

# setwd("~/work/projects/FLEE_catchments")

# use this if drainAccum fails with an error about stars
tryCatch(rgrass7::use_sp(), error = function(e) warning(e))

metadat <- fread("catchment_list.csv")[catchment == "thur"]
dir <- file.path(metadat$dir, metadat$catchment, metadat$version)
shpdir <- file.path(dir, "shape")
shareDir <- metadat$dir

dir.create(dir, recursive=TRUE, showWarnings = FALSE)
dir.create(shpdir, showWarnings = FALSE)


#############
# Grass setup
# 	it is essential that the DEM be projected
#	also do a bit of cropping because the original DEM is huge
#############

gisbase <- WatershedTools:::getGISBase()
# used this older DEM of unknown origin, because the copernicus DEM wasn't working for some reason
dem <- raster(file.path(shareDir, "dem", "thur_25m", "thur_dem_3035.tif"))
plot(dem)
outletPoint_approx <- SpatialPoints(matrix(c(4218000, 2720800), nrow=1), 
	proj4string = CRS(proj4string(dem)))
plot(outletPoint_approx, add=TRUE)
gs <- GrassSession(dem, gisbase, layerName = 'dem')



#############
# Fill the DEM
#############
gs <- fillDEM("dem", gs = gs, filledDEM = 'filledDEM', probs = 'probs')
demFilled <- GSGetRaster('filledDEM', gs)
demFilled <- writeRaster(demFilled, file.path(dir, "filled_dem.tif"), 
	options = tifFloat)

#############
# Compute drainage direction/flow accumulation
#############
gs <- drainageAccumulation('filledDEM', gs = gs, accumulation = 'accum', drainage = 'drainage')
drain <- GSGetRaster('drainage', gs, file = file.path(dir, "drainage.tif")) 
accum <- GSGetRaster('accum', gs, file = file.path(dir, "accumulation.tif"))

plot(drain, col=rainbow(12), xaxt='n', yaxt='n')
plot(log(accum), xaxt='n', yaxt='n')



#############
# Find streams; threshold must be determined empirically, so we output, look at it in QGIS
# then adjust
#############

writeOGR(outletPoint_approx, file.path(dir, "tmp"), "outlet")
thresh <- 0.995
streamChannel <- extractStream(dem = 'filledDEM', gs = gs, accumulation = 'accum', 
	qthresh = thresh, type='both')
writeOGR(streamChannel$vector, file.path(dir, "tmp"), paste0("stream", thresh*100), 
	"ESRI Shapefile")

#############
# Find an outlet point (by eye) and delineate catchment
#############
outletPointSnap <- snapToStream(outletPoint_approx, streamChannel$raster, buff= 400)
catchment <- catchment(outletPointSnap, drainage = 'drainage', gs = gs, areas=FALSE)
catchment <- writeRaster(catchment, file=file.path(dir, "catchment.tif"), options = tifInt)

















#############
# Find an outlet point (by eye-in QGIS) and delineate catchment
#############
writeOGR(streamChannel$vector, file.path(dir, "tmp"), paste0("stream", thresh*100), 
	"ESRI Shapefile")
# the original ybbs one was fine
yb <- coordinates(outlets)[outlets$name == 'ybbs']
ka <- c(4749353, 2825060)
outlets <- data.frame(x = c(yb[1], ka[1]), y = c(yb[2], ka[2]), name = c('ybbs', 'kamp'))
coordinates(outlets) <- c(1,2)
proj4string(outlets) <- proj4string(streamChannel$vector)
outletsSnap <- snapToStream(outlets, streamChannel$raster, buff= 400)
catchment <- catchment(outletsSnap, drainage = 'drainage', gs = gs, areas=FALSE)
lapply(1:nlayers(catchment), function(x) writeRaster(catchment[[x]], 
	file=paste0(dir, "/", outlets$name[x], "_catchment.tif"), options = tifInt))



#############
# Crop the delineated watershed to the catchments
#############
ybbsStream <- cropToCatchment(outletsSnap[1,], streamChannel$raster, streamChannel$vector, 
      'drainage', gs = gs) 
kampStream <- cropToCatchment(outletsSnap[2,], streamChannel$raster, streamChannel$vector, 'drainage', gs = gs) 
writeOGR(ybbsStream$vector, shpdir, "ybbs_stream", "ESRI Shapefile")
writeOGR(kampStream$vector, shpdir, "kamp_stream", "ESRI Shapefile")
writeRaster(ybbsStream$raster, file.path(dir, "ybbs_stream.tif"), options = tifInt)
writeRaster(kampStream$raster, file.path(dir, "kamp_stream.tif"), options = tifInt)


# split coordinates for catchment area by reach
pixvals <- values(ybbsStream$raster)
coords <- coordinates(ybbsStream$raster)
df <- cbind(pixvals, coords)
df <- df[complete.cases(df),]
caPts <- by(df, df[,1], function(x) x[,2:3])
saveRDS(caPts, file.path(dir, "tmp", "y_caPoints_byReach.rds"))

pixvals <- values(kampStream$raster)
coords <- coordinates(kampStream$raster)
df <- cbind(pixvals, coords)
df <- df[complete.cases(df),]
caPts <- by(df, df[,1], function(x) x[,2:3])
saveRDS(caPts, file.path(dir, "tmp", "k_caPoints_byReach.rds"))

