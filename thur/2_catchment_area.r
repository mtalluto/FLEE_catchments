suppressWarnings(suppressPackageStartupMessages(library(WatershedTools, quietly=TRUE)))
suppressWarnings(suppressPackageStartupMessages(library(raster, quietly=TRUE)))
suppressWarnings(suppressPackageStartupMessages(library(data.table, quietly=TRUE)))
args <- commandArgs(trailingOnly = TRUE)

## local variables/storage locations
## change depending on where you are running this
## shareDir is where we will find the data to run the script
## scratchDir is where temporary GIS files will be created

metadat <- fread("catchment_list.csv")[catchment == "thur"]
# shareDir <- file.path(metadat$dir, metadat$catchment, metadat$version)
shareDir <- "~/work/projects/catchments/thur/1.1.0"
outDir <- file.path(shareDir, "tmp", "ca")
scratchDir <- tempdir()
# scratchDir <- file.path(shareDir, "tmp")
# dir.create(scratchDir, showWarnings=FALSE)

caPoints <- readRDS(file.path(shareDir, "tmp", "caPoints_byReach.rds"))
max_jobs <- 60
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
	caFiles <- list.files(outDir, pattern = "[0-9]+", full.names = TRUE, recursive = TRUE)
	library(data.table)
	caData <- rbindlist(lapply(caFiles, function(x) as.data.table(readRDS(x))))
	caRaster <- rasterFromXYZ(caData, crs = proj4string(drain))
	name <- 'catchment_area.tif'
	caRaster <- writeRaster(caRaster, file = file.path(shareDir, name), 
		options = c("COMPRESS=LZW", "PREDICTOR=3"))
} else if(jobNum <= max_jobs) {
	logfile <- paste0("~/Dropbox/catchments/log_t_", jobNum, ".txt")
	suppressWarnings(gisbase <- WatershedTools:::getGISBase())
	tryCatch(rgrass7::use_sp(), error = function(e) warning(e))
	Sys.setenv(LOCATION_NAME='NSmetabolism')
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
		oname <- paste0(outDir, "/ca_r", rName, ".rds")

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
		msg <- paste(as.character(Sys.time()), "Finished reach number", rName, 
			"   Number", done, "of", length(inds), "\n")
		cat(msg, file = logfile, append = TRUE)
	}

	unlink(gDir, recursive = TRUE)
	msg <- paste("Finished with job", jobNum, "\n")
	cat(msg, file = logfile, append = TRUE)

} else {
	stop("Please specify a number on the command line:\n  0: test\n  -1: finish\n  1-",
		max_jobs, ": do this job number")
}

