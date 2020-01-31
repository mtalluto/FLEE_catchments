library(data.table)
library(rgrass7)
library(sp)
library(WatershedTools)
library(raster)
library(rgdal)

#### Think about knitr for this
## the following header
# ```{r, cache=TRUE}
# Sys.sleep(2)
# a <- 1
# ```


tifInt <- c("COMPRESS=LZW", "PREDICTOR=2")
tifFloat <- c("COMPRESS=LZW", "PREDICTOR=3")

# setwd("~/work/projects/FLEE_catchments")

# use this if drainAccum fails with an error about stars
tryCatch(rgrass7::use_sp(), error = function(e) warning(e))

metadat <- fread("catchment_list.csv")[catchment == "ybbs_kamp"]
dir <- file.path(metadat$dir, metadat$catchment, metadat$version)
shpdir <- file.path(dir, "shape")
shareDir <- metadat$dir

dir.create(dir, recursive=TRUE, showWarnings = FALSE)
dir.create(shpdir, showWarnings = FALSE)


outlets <- data.frame(x = c(15.102422, 15.797278), y = c(48.170074, 48.383946), 
	name = c("ybbs", "kamp"))
coordinates(outlets) <- 1:2
proj4string(outlets) <- CRS("+init=epsg:4326")
outlets <- spTransform(outlets, CRS("+init=epsg:3035"))
coordinates(outlets)
dir.create(file.path(dir, "tmp"), showWarnings=FALSE)
writeOGR(outlets, file.path(dir, "tmp"), "yk_outlets", "ESRI Shapefile")

#############
# Grass setup
# 	it is essential that the DEM be projected
#	also do a bit of cropping because the original DEM is huge
#############

gisbase <- WatershedTools:::getGISBase()
dem <- raster(file.path(shareDir, "dem", "austria_dem_10m", "dhm_at_lamb_10m_2018_epsg3035.tif"))
plotx <- c(4615146, 4831020)
ploty <- c(2711004, 2884550)
cropex <- c(4625000, 4770000, 2720000, 2880000)
demyk <- crop(dem, extent(cropex))
plot(demyk, xlim=plotx, ylim=ploty)

gs <- GrassSession(demyk, gisbase, layerName = 'dem')

#############
# Fill the DEM
#############
gs <- fillDEM("dem", gs = gs, filledDEM = 'filledDEM', probs = 'probs')
demFilled <- GSGetRaster('filledDEM', gs)
## note, I wrote these separately these to add compression; 
## using predictor=3 for floats, 2 for integers
demFilled <- writeRaster(demFilled, file.path(dir, "filled_dem.tif"), 
	options = tifFloat)


#############
# Compute drainage direction/flow accumulation
#############
gs <- drainageAccumulation('filledDEM', gs = gs, accumulation = 'accum', drainage = 'drainage')
# compression didn't help these
drain <- GSGetRaster('drainage', gs, file = file.path(dir, "drainage.tif")) 
accum <- GSGetRaster('accum', gs, file = file.path(dir, "accumulation.tif"))

plot(drain, col=rainbow(12), xaxt='n', yaxt='n')
plot(log(accum), xaxt='n', yaxt='n')

thresh <- 0.995
streamChannel <- extractStream(dem = 'filledDEM', gs = gs, accumulation = 'accum', 
	qthresh = thresh, type='both')


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

