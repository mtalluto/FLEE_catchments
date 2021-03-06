library(WatershedTools)
library(raster)
args <- commandArgs(trailingOnly = TRUE)[1]


metadat <- fread("catchment_list.csv")[catchment == "vjosa"]
shareDir <- file.path(metadat$dir, metadat$catchment, metadat$version)
scratchDir <- "~/Desktop/Vjosa/tmp"
dir.create(scratchDir, showWarnings=FALSE)

caPoints <- readRDS(file.path(shareDir, "tmp", "caPoints_byReach.rds"))
max_jobs <- 12
cutoffs <- floor(seq(1, length(caPoints), length.out=max_jobs+1))
jobNum <- as.integer(args[1])

dir <- file.path(scratchDir, jobNum)
gDir <- file.path(dir, 'grass')
if(!dir.exists(dir))
	dir.create(dir)
if(!dir.exists(gDir))
	dir.create(gDir)

drain <- raster(file.path(shareDir, "drainage.tif"))

if(jobNum == 0) {
	inds <- 1
} else if(jobNum == -1) {
	caFiles <- list.files(scratchDir, pattern = "[0-9]+", full.names = TRUE, recursive = TRUE)
	library(data.table)
	caData <- rbindlist(lapply(caFiles, function(x) as.data.table(readRDS(x))))
	caRaster <- rasterFromXYZ(caData, crs = proj4string(drain))
	caRaster <- writeRaster(caRaster, file = file.path(shareDir, "catchment_area.tif"), 
		options = c("COMPRESS=LZW", "PREDICTOR=3"))
} else if(jobNum <= max_jobs) {
	gisbase <- WatershedTools:::getGISBase()
	tryCatch(rgrass7::use_sp(), error = function(e) warning(e))
	gs <- GrassSession(layer = drain, gisBase = gisbase, layerName = "drainage", 
		home = gDir, override = TRUE)
	rm(drain)
	gc()
	
	# choose which reaches to work on
	inds <- cutoffs[jobNum]:cutoffs[jobNum+1]

	# loop over reaches
	done <- 0
	for(i in inds) {
		rName <- names(caPoints)[i]
		oname <- paste0(dir, "/ca_r", rName, ".rds")

		# skip if we are already done, allows resuming
		if(file.exists(oname)) {
			done <- done + 1
			next()
		}

		pts <- as.matrix(caPoints[[i]])
		# loop over points within reaches
		catchmentAreas <- catchment(pts, "drainage", areas = TRUE, gs = gs)
		catchmentAreas <- cbind(pts, catchmentAreas)
		saveRDS(catchmentAreas, oname)


		# print something showing progress
		done <- done + 1
		cat("\n", as.character(Sys.time()), "Finished reach number", rName, 
			"\n   Number", done, "of", length(inds), "\n\n")
	}
	unlink(gDir, recursive = TRUE)
	cat("Finished with job", jobNum, "\n")

} else {
	stop("Please specify a number on the command line:\n  0: test\n  -1: finish\n  1-",
		max_jobs, ": do this job number")
}
