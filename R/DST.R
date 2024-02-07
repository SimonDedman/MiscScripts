# DST.R
setwd("/home/simon/Documents/Si Work/PostDoc Work/Kroetz & Dedman Sawfish BRT/DST/")
expvars <- NULL
resvars <- NULL
# expvars = read.csv("/home/simon/Documents/Si Work/PostDoc Work/Kroetz & Dedman Sawfish BRT/SpatJoinExpvars/SSTSSS01Dep.csv")
# resvars = read.csv("/home/simon/Documents/Si Work/PostDoc Work/Kroetz & Dedman Sawfish BRT/AndreaData2019.07.27.csv")
# colnames(resvars)[which(colnames(resvars) == "Longitude")] <- "lon" #replace/clean/rename colnames by name not number reference
# colnames(resvars)[which(colnames(resvars) == "Latitude")] <- "lat" #replace/clean/rename colnames by name not number reference
# should be independent of expvars / resvars, just does dst for any points

#DST code in R, DistToShoreAppend.R####
library(sf) # sudo apt install libudunits2-dev
library(raster)
# library(maptools) # dead package
# Create a raster template for rasterizing the polys. Set grid resolution with res (degrees)
r <- raster(xmn = min(expvars$lon, na.rm = T),
            xmx = max(expvars$lon, na.rm = T),
            ymn = min(expvars$lat, na.rm = T),
            ymx = max(expvars$lat, na.rm = T),
            res = 0.1) #0.1deg = 49.5mb
data(wrld_simpl) #maptools
r2 <- rasterize(wrld_simpl, r, 1) #values 1 for land, NA for ocean
d <- distance(r2) # Calculate distance to nearest non-NA pixel i.e. land. 0.1 & 0.25 res hangs.
writeRaster(d, filename = "R_Raster", format = "GTiff") #save object 30kb
expvars$DistanceToShoreKm <- rep(NA, nrow(expvars)) #create blank container vec
resvars$DistanceToShoreKm <- rep(NA, nrow(resvars)) #create blank container vec
#compare distances calc####
# https://gis.stackexchange.com/questions/279079/extracting-value-of-raster-with-coordinates-using-r
expvarssp <- SpatialPoints(expvars[,c("lon","lat")])
# colnames(resvars)[which(colnames(resvars) == "MidpointLat")] <- "lat"
# colnames(resvars)[which(colnames(resvars) == "MidpointLon")] <- "lon"
resvarssp <- SpatialPoints(resvars[,c("lon","lat")])
DSTexpvarssp <- raster::extract(d, expvarssp, sp = T) # tidyverse_conflicts() ✖ tidyr::extract() masks raster::extract()
DSTresvarssp <- raster::extract(d, resvarssp, sp = T) # tidyverse_conflicts() ✖ tidyr::extract() masks raster::extract()
expvars[,"DistanceToShoreKm"] <- (DSTexpvarssp$layer)/1000
resvars[,"DistanceToShoreKm"] <- (DSTresvarssp$layer)/1000
write.csv(x = expvars, file = "ExpvarsDST.csv", row.names = FALSE)
# write.csv(x = expvars, file = "Dep4pmSBS4pmSBT4pmDST.csv", row.names = FALSE)
write.csv(x = resvars, file = "ResvarsDST.csv", row.names = FALSE)
# expvarsnonadst <- expvars[which(!is.na(expvars$DistanceToShoreKm)),] #QGis can't work with columns containing NA
# write.csv(x = expvarsnonadst, file = "ExpVarsJoinedQGisEEZdstNONA.csv", row.names = FALSE)
