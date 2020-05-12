library(raster)
library(WatershedTools)
library(sf)
library(data.table)
library(RSQLite)

metabDB <- dbConnect(RSQLite::SQLite(), "../metabolismDB/metabolism.sqlite")
dbExecute(metabDB, "PRAGMA foreign_keys = ON")

metadat <- fread("catchment_list.csv")[catchment == "thur"]
shareDir <- file.path(metadat$dir, metadat$catchment, metadat$version)

drain <- raster(file.path(shareDir, "drainage.tif"))
accum <- raster(file.path(shareDir, "accumulation.tif"))
elev <- raster(file.path(shareDir, "filled_dem.tif"))
stream <- raster(file.path(shareDir, "thur_stream.tif"))
catchmentAreas <- raster(file.path(shareDir, "catchment_area.tif"))


# get discharge
qSites = data.table(dbReadTable(metabDB, "siteDischargeView"))[expedition == 'ThurFall2018']
qSites = st_as_sf(qSites, coords = c('x', 'y'), crs = qSites$epsg[1])
qSites$catchmentArea <- raster::extract(catchmentAreas, qSites)


## make data frame of catchment areas to compute
caDF = data.table(area = values(catchmentAreas), coordinates(catchmentAreas))
caDF = caDF[complete.cases(caDF),]

# get predicted Q for each point
colnames(qSites)[grep("(discharge|catchmentArea)", colnames(qSites))] = c('Q', 'A')
q = discharge_scaling(caDF$area, qSites)
q = rasterFromXYZ(cbind(caDF$x, caDF$y, q))
writeRaster(q, file = file.path(shareDir, "tmp", "discharge.tif"))

val = values(q)
val = val[!is.na(val)]
plot(log(caDF$area), log(val), xlab="log(Catchment Area)", ylab="log(discharge)", 
		pch='x', cex=0.3, col='red', main=x)
points(log(qSites$A), log(qSites$Q), pch=16, cex=0.7, col='blue')


dat = data.table(x = coordinates(q)[,1], y = coordinates(q)[,2], q = values(q))
dat = dat[complete.cases(dat)]
geom <- hydraulic_geometry(dat$q)
coordinates(geom) <- dat[,c('x', 'y')]
gridded(geom) <- TRUE
geom <- stack(geom)
proj4string(geom) <- proj4string(catchmentAreas)

gdir = file.path(shareDir, "geometry_ThurFall2018")
dir.create(gdir, showWarnings = FALSE)

for(x in names(geom)) {
	writeRaster(geom[[x]], file.path(gdir, paste0(x, "_", "ThurFall2018.tif")), 
			options = c("COMPRESS=LZW", "PREDICTOR=3"))
}


# only if needed due to non-matching extents
compareRaster(stream, drain, elev, accum, catchmentAreas, geom)
## two extents
# small one
# 	stream, catchmentAreas, geom
# big one
# 	drain, elev, accum
drain = crop(drain, stream)
elev = crop(elev, stream)
accum = crop(accum, stream)
compareRaster(stream, drain, elev, accum, catchmentAreas, geom)

thurWS = Watershed(stream = stream, drainage = drain, elevation = elev, 
		accumulation = accum, catchmentArea = catchmentAreas, 
		otherLayers = geom)
thurWS$data$slope = wsSlope(thurWS)
saveRDS(thurWS, file.path(shareDir, "watershed_ThurFall2018.rds"))

dbDisconnect(metabDB)

