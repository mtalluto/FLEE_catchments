library(data.table)
library(sf)
library(raster)
library(rgrass7)
library(WatershedTools)
library(sp)
library(rgdal)

#### Think about knitr for this
## the following header
# ```{r, cache=TRUE}
# Sys.sleep(2)
# a <- 1
# ```

## the DEM is 10x10 meters, and it's causing problems doing both at once
## instead, cropping to each individual watershed, doing separate analyses

tifInt <- c("COMPRESS=LZW", "PREDICTOR=2")
tifFloat <- c("COMPRESS=LZW", "PREDICTOR=3")

# setwd("~/work/projects/FLEE_catchments")


# use this if drainAccum fails with an error about stars
tryCatch(rgrass7::use_sp(), error = function(e) warning(e))

# grab metadata for the most recent version
metadat <- fread("catchment_list.csv")
metadat <- rbind(metadat, data.table(c("ybbs", "kamp"), "~/work/projects/catchments", 
		"2020-03-28", "1.2.0"), use.names = FALSE)
fwrite(metadat, "catchment_list.csv")


outlets <- data.frame(x = c(4690393, 4704445), y = c(2756269, 2847440), 
					  name = c("ybbs", "kamp"))
outlets <- st_as_sf(outlets, coords=c('x','y'), crs=3035)
dem <- raster(file.path(shareDir, "dem", "austria_dem_10m", "dhm_at_lamb_10m_2018_epsg3035.tif"))



#######
##
## YBBS
##
#######
metadat <- fread("catchment_list.csv")
metadat <- metadat[grepl('ybbs', metadat$catchment)]
max_ver <- which.max(order(semver::parse_version(metadat$version)))
metadat <- metadat[max_ver]
dir <- file.path(metadat$dir, metadat$catchment, metadat$version)
shpdir <- file.path(dir, "shape")
shareDir <- metadat$dir
tmpdir <- file.path(dir, "tmp")
dir.create(dir, recursive=TRUE, showWarnings = FALSE)
dir.create(shpdir, showWarnings = FALSE)
dir.create(tmpdir, showWarnings = FALSE)

st_write(outlets, dsn=file.path(tmpdir, "ykoutlets"), layer="ykoutlets", driver="ESRI Shapefile")

#############
# Grass setup
# 	it is essential that the DEM be projected
#	also do a bit of cropping because the original DEM is huge
#############

gisbase <- WatershedTools:::getGISBase()
cropex <- c(4652150, 4721027, 2740449, 2806500)
dem_y <- crop(dem, extent(cropex))
plot(dem_y)
gs <- GrassSession(dem_y, gisbase, layerName = 'dem', override=TRUE)

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

thresh <- 0.98
streamChannel <- extractStream(dem = 'filledDEM', gs = gs, accumulation = 'accum', 
	qthresh = thresh, type='both')


#############
# Find an outlet point (by eye-in QGIS) and delineate catchment
#############
st_write(st_as_sf(streamChannel$vector), dsn=file.path(tmpdir, thresh), layer=paste0("stream", thresh*100), 
	driver="ESRI Shapefile")
outlet <- outlets[outlets$name == 'ybbs',]
outletSnap <- snapToStream(as(outlet, "Spatial"), streamChannel$raster, buff= 400)
catchment <- catchment(outletSnap, drainage = 'drainage', gs = gs, areas=FALSE)
writeRaster(catchment, file = file.path(dir, "catchment.tif"), options=tifInt)

#############
# Crop the delineated watershed to the catchments
#############
ybbsStream <- cropToCatchment(outletSnap, streamChannel$raster, streamChannel$vector, 
      'drainage', gs = gs) 
writeOGR(ybbsStream$vector, shpdir, "ybbs_stream", "ESRI Shapefile")
writeRaster(ybbsStream$raster, file.path(dir, "ybbs_stream.tif"), options = tifInt)

# split coordinates for catchment area by reach
pixvals <- values(ybbsStream$raster)
coords <- coordinates(ybbsStream$raster)
df <- cbind(pixvals, coords)
df <- df[complete.cases(df),]
caPts <- by(df, df[,1], function(x) x[,2:3])
saveRDS(caPts, file.path(tmpdir, "y_caPoints_byReach.rds"))





#######
##
## KAMP
##
#######
metadat <- fread("catchment_list.csv")
metadat <- metadat[grepl('kamp', metadat$catchment)]
max_ver <- which.max(order(semver::parse_version(metadat$version)))
metadat <- metadat[max_ver]
dir <- file.path(metadat$dir, metadat$catchment, metadat$version)
shpdir <- file.path(dir, "shape")
shareDir <- metadat$dir
tmpdir <- file.path(dir, "tmp")
dir.create(dir, recursive=TRUE, showWarnings = FALSE)
dir.create(shpdir, showWarnings = FALSE)
dir.create(tmpdir, showWarnings = FALSE)

st_write(outlets, dsn=file.path(tmpdir, "ykoutlets"), layer="ykoutlets", driver="ESRI Shapefile")

#############
# Grass setup
# 	it is essential that the DEM be projected
#	also do a bit of cropping because the original DEM is huge
#############

gisbase <- WatershedTools:::getGISBase()
cropex <- c(4672494, 4754381, 2811364, 2872912)
dem_k <- crop(dem, extent(cropex))
plot(dem_k)
gs <- GrassSession(dem_k, gisbase, layerName = 'dem', override=TRUE)

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
st_write(st_as_sf(streamChannel$vector), dsn=file.path(tmpdir, thresh), layer=paste0("stream", thresh*100), 
		 driver="ESRI Shapefile")
outlet <- outlets[outlets$name == 'kamp',]
outletSnap <- snapToStream(as(outlet, "Spatial"), streamChannel$raster, buff= 400)
catchment <- catchment(outletSnap, drainage = 'drainage', gs = gs, areas=FALSE)
writeRaster(catchment, file = file.path(dir, "catchment.tif"), options=tifInt)

#############
# Crop the delineated watershed to the catchments
#############
kampStream <- cropToCatchment(outletSnap, streamChannel$raster, streamChannel$vector, 'drainage', gs = gs) 
writeOGR(kampStream$vector, shpdir, "kamp_stream", "ESRI Shapefile")
writeRaster(kampStream$raster, file.path(dir, "kamp_stream.tif"), options = tifInt)


# split coordinates for catchment area by reach
pixvals <- values(kampStream$raster)
coords <- coordinates(kampStream$raster)
df <- cbind(pixvals, coords)
df <- df[complete.cases(df),]
caPts <- by(df, df[,1], function(x) x[,2:3])
saveRDS(caPts, file.path(dir, "tmp", "k_caPoints_byReach.rds"))

