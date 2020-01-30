library(data.table)
library(rgrass7)
library(sp)
library(WatershedTools)
library(raster)
library(rgdal)

tryCatch(rgrass7::use_sp(), error = function(e) warning(e))

metadat <- fread("catchment_list.csv")[catchment == "vjosa"]
dir <- file.path(metadat$dir, metadat$catchment, metadat$version)
shpdir <- file.path(dir, "shape")
shareDir <- metadat$dir

dir.create(dir, recursive=TRUE, showWarnings = FALSE)
dir.create(shpdir, showWarnings = FALSE)

gisbase <- WatershedTools:::getGISBase()
dem <- raster(file.path(dir, "filled_dem.tif"))
gs <- GrassSession(dem, gisbase, layerName = 'filledDEM')


#############
# Compute drainage direction/flow accumulation
#############
gs <- drainageAccumulation('filledDEM', gs = gs, accumulation = 'accum', drainage = 'drainage')
# compression didn't help these
drain <- GSGetRaster('drainage', gs, file = file.path(dir, "drainage.tif")) 
accum <- GSGetRaster('accum', gs, file = file.path(dir, "accumulation.tif"))

plot(drain, col=rainbow(12), xaxt='n', yaxt='n')
plot(log(accum), xaxt='n', yaxt='n')


#############
# Extract the watershed
#	the threshold must be determined empirically, just choosing something that works
#############
thresh <- 0.995
streamChannel <- extractStream(dem = 'filledDEM', gs = gs, accumulation = 'accum', 
							   qthresh = thresh, type='both')

#############
# Find an outlet point (by eye-in QGIS) and delineate catchment
#############
dir.create(file.path(dir, "tmp"), showWarnings=FALSE)
writeOGR(streamChannel$vector, file.path(dir, "tmp"), "vjosa_995_unclipped", "ESRI Shapefile")
outlet <- SpatialPoints(matrix(c(5122456, 2000900), nrow=1), proj4string = CRS(proj4string(dem)))
plot(streamChannel$vector, col='blue')
outlet <- snapToStream(outlet, streamChannel$raster, 200)
plot(outlet, add=T, pch=16)

catchment <- catchment(outlet, drainage = 'drainage', gs = gs, areas=FALSE, 
					   file = file.path(dir, "catchment.tif"))
plot(catchment, add=TRUE, col="#aa000044")


#############
# Crop the delineated watershed to the catchment
#############
vjosaStream <- cropToCatchment(outlet, streamChannel$raster, streamChannel$vector, 
							   'drainage', gs = gs)


#############
# View in QGIS and manually select reaches to delete
#############
writeOGR(vjosaStream$vector, file.path(dir, "tmp"), "vjosa_crop_pretrim", "ESRI Shapefile")

delnodes <- c(354, 352, 350, 11, 4:7, 38, 39, 345, 343, 335, 337, 340, 42, 43, 46, 323, 50:52, 
			  57, 59, 64, 240, 241, 302, 306, 311, 308, 248, 279, 249:251, 259, 255, 267, 265, 235, 67, 
			  68, 226, 78, 79, 89, 98, 99, 100, 102, 153, 217, 163, 167, 169, 206, 212, 204, 173, 174, 
			  175, 178, 180, 186, 181, 182, 190, 191, 193, 105, 141, 135:137, 131, 107, 127, 112, 119)
delPixIDs <- vjosaStream$vector$a_cat_[match(delnodes, vjosaStream$vector$cat)]

### make sure we are deleting the right nodes
test <- vjosaStream$vector
cols <- rep('blue', length(test))
cols[test[[1]] %in% delnodes] <- 'red'
plot(test, col = cols)
testRas <- vjosaStream$raster
plot(testRas, xlim=c(5120000, 5160000), ylim=c(1960000, 2000000))
testRas[testRas %in% delPixIDs] <- -1000
plot(testRas, xlim=c(5120000, 5160000), ylim=c(1960000, 2000000))

indexToKill <- which(vjosaStream$vector[[1]] %in% delnodes)
vjosaStream$vector <- vjosaStream$vector[-indexToKill,]
plot(vjosaStream$vector)
writeOGR(vjosaStream$vector, file.path(dir, "shape"), "vjosa_stream", "ESRI Shapefile")

vjosaStream$raster[vjosaStream$raster %in% delPixIDs] <- NA
plot(vjosaStream$raster)
writeRaster(vjosaStream$raster, file=file.path(dir, "vjosa_stream.tif"), overwrite = TRUE, options=c("COMPRESS=LZW", "PREDICTOR=2"))

# split coordinates for catchment area by reach
pixvals <- values(vjosaStream$raster)
coords <- coordinates(vjosaStream$raster)
df <- cbind(pixvals, coords)
df <- df[complete.cases(df),]
caPts <- by(df, df[,1], function(x) x[,2:3])
saveRDS(caPts, file.path(dir, "tmp", "caPoints_byReach.rds"))

