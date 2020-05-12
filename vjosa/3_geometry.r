library(raster)
library(WatershedTools)
library(sp)
library(data.table)
library(RSQLite)

metabDB <- dbConnect(RSQLite::SQLite(), "../metabolismDB/metabolism.sqlite")
dbExecute(metabDB, "PRAGMA foreign_keys = ON")

metadat <- fread("catchment_list.csv")[catchment == "vjosa"]
shareDir <- file.path(metadat$dir, metadat$catchment, metadat$version)

drain <- raster(file.path(shareDir, "drainage.tif"))
accum <- raster(file.path(shareDir, "accumulation.tif"))
elev <- raster(file.path(shareDir, "filled_dem.tif"))
vjosaChannel <- raster(file.path(shareDir, "vjosa_stream.tif"))
catchmentAreas <- raster(file.path(shareDir, "catchment_area.tif"))


# get discharge
qry <- "SELECT sites.siteName, (expeditions.watershed || expeditions.season || expeditions.year) 
	AS expedition, date, hydromorphology.value AS discharge, x, y, epsg FROM hydromorphology 
	LEFT JOIN sites ON siteID = sites.id 
	LEFT JOIN variableLookUp ON variableCode = variableLookUp.id 
	LEFT JOIN expeditions on expeditionID = expeditions.id 
	WHERE variableLookUp.variable LIKE 'Q' AND expedition LIKE 'Vjosa%'"
qSites <- data.table(dbGetQuery(metabDB, qry))
coordinates(qSites) <- c('x', 'y')
proj4string(qSites) <- CRS(paste0("+init=epsg:", qSites$epsg[1]))
qSites$catchmentArea <- raster::extract(catchmentAreas, qSites)
qSites <- as.data.table(qSites)
qSites <- qSites[, .(siteName = siteName, expedition = expedition, Q = discharge, 
	A = catchmentArea, x, y)]

caDF <- cbind(values(catchmentAreas), coordinates(catchmentAreas))
caDF <- caDF[complete.cases(caDF),]
qByExpedition <- by(qSites, factor(qSites$expedition), function(x) {
	q <- discharge_scaling(caDF[,1], x)
	q <- data.frame(Q = q, x = caDF[,2], y = caDF[,3])
	q <- rasterFromXYZ(q[, c('x', 'y', 'Q')])
	proj4string(q) <- proj4string(catchmentAreas)
	q
})

lapply(names(qByExpedition), function(x) writeRaster(qByExpedition[[x]], 
	file = file.path(shareDir, "tmp", paste0("discharge", x, ".tif"))))

par(mfrow=c(2,2))

lapply(names(qByExpedition), function(x) {
	val <- values(qByExpedition[[x]])
	val <- val[!is.na(val)]
	ind <- which(qSites$expedition == x)
	dat <- qSites[ind,]
	plot(log(caDF[,1]), log(val), xlab="log(Catchment Area)", ylab="log(discharge)", 
		pch='x', cex=0.3, col='red', main=x)
	points(log(dat$A), log(dat$Q), pch=16, cex=0.7, col='blue')
})

geom <- lapply(qByExpedition, function(x) {
	df <- data.frame(x = coordinates(x)[,1], y = coordinates(x)[,2], q = values(x))
	df <- df[complete.cases(df),]
	geom <- hydraulic_geometry(df[,3])
	coordinates(geom) <- df[,1:2]
	gridded(geom) <- TRUE
	geom <- stack(geom)
	proj4string(geom) <- proj4string(catchmentAreas)
	geom
})

nms <- names(geom)
for(x in nms) {
	gdir <- file.path(shareDir, paste0("geometry_", x))
	dir.create(gdir, showWarnings=FALSE)
	for(y in names(geom[[x]])) {
		writeRaster(geom[[x]][[y]], file.path(gdir, paste0(y, "_", x, ".tif")), 
			options = c("COMPRESS=LZW", "PREDICTOR=3"))
	}
}

# only if needed due to non-matching extents
# catchmentAreas <- extend(catchmentAreas, vjosaChannel)
# geom <- lapply(geom, extend, vjosaChannel)

vjosaWS <- list()
for(x in nms) {
	geom <- stack(list.files(paste0(shareDir, "/geometry_", x), pattern="*.tif", 
		full.names = TRUE))
	vjosaWS[[x]] <- Watershed(stream = vjosaChannel, drainage = drain, elevation = elev, 
		accumulation = accum, catchmentArea = catchmentAreas, 
		otherLayers = geom)
}

## get slope and add to all watersheds
slope = wsSlope(vjosaWS[[1]])
for(x in nms) {
	vjosaWS[[x]]$data$slope = slope
	saveRDS(vjosaWS[[x]], paste0(shareDir, "/watershed_", x, ".rds"))
}

dbDisconnect(metabDB)

