library(WatershedTools)
library(sp)
library(ggplot2)
ws <- readRDS("vjosa/res/watershed_vjosaSpring2018.rds")
stream <- raster("vjosa/res/vjosa_stream.grd")

cutpoints <- data.frame(x = c(19+ 48/60 + 2.7/3600, 20 + 28/60 + 4.2/3600), 
	y = c(40 + 23/60 + 58.8/3600, 40 + 15/60 + 40.1/3600))
coordinates(cutpoints) <- c(1,2)
proj4string(cutpoints) <- CRS("+init=epsg:4326")
cutpoints <- spTransform(cutpoints, CRS("+init=epsg:3035"))
cutpoints <- snapToStream(cutpoints, stream, 200)

plot.Watershed(ws) + geom_point(data = as.data.frame(cutpoints), col = 'red', aes(x=X1, y=X2)) + coord_fixed() #+ xlim(5212000, 5213000) + ylim(1968000, 1969000)


## total network length, in km
sum(ws[,'length'])/1000

#length upstream from points
pts <- extract(ws, cutpoints)
sapply(pts, function(x) sum(ws[connect(ws, upstream=Inf, downstream=x), 'length']))/1000
