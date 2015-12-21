#---------------------------------------------------------------------------------------------------*
# ---- SET-UP ----
#===================================================================================================*

# setwd('C:/Users/Brian/Desktop')
library(raster)
library(rgeos)

# Set output and points projection to proj4string matching Google:

gProj <- '+init=epsg:4326'

# Load raster data:

imp <-  raster('impervious2011/nlcd_2011_impervious_2011_edition_2014_10_10.img')
lc <-  raster('lc2011/nlcd_2011_landcover_2011_edition_2014_10_10.img')

# Create points data frame and put in the same projection as the raster:

pts <- data.frame(region = c('Atlanta','Raleigh','Pittsburgh','DC','Springfield','Gainesville'),
                  long = c(-84.388, -78.838, -79.996,-77.037,-72.590,-82.324),
                  lat = c(33.749, 35.780, 40.441, 38.907,42.101, 29.651))

# Make into a spatial points data frame with the Google projection:

ptsSp <- SpatialPointsDataFrame(as.matrix(pts[,c(2,3)]), data = pts, proj4string = CRS(gProj))

# Transform the projection into the same as the land cover layer:

ptsProjR <- spTransform(ptsSp, CRS(projection(imp)))

#---------------------------------------------------------------------------------------------------*
# ---- Function for use in mask extraction ----
#---------------------------------------------------------------------------------------------------*

makeBufferedRaster <- function(r, regionCenter, outProjection){
  # Get general point buffer polygon (100 km):
  pointBuffer <- gBuffer(regionCenter, width = 100000)
  # Crop the raster to the buffer extent:
  rCrop <- crop(r, extent(pointBuffer))
  # Create dummy raster:
  dummyR <- rCrop
  # Set values to NA:
  dummyR[] <- NA
  # To make processing faster, aggregate by a factor of 36:
  dummyRagg <- aggregate(dummyR, 36)
  # Get the cell number associated with the region center:
  pointRaster <- cellFromXY(dummyRagg, regionCenter)
  # Set the value of that cell to 1:
  dummyRagg[pointRaster] <- 1
  # Buffer to 100 km:
  dummyBuffer <- buffer(dummyRagg, width = 100000)
  # Set resolution back to 30 m:
  dummyDisagg <- disaggregate(dummyBuffer, 36)
  # multiply the raster by the dummy to crop:
  rOut <- rCrop*dummyDisagg
  return(projectRaster(rOut, crs = outProjection))
}


#---------------------------------------------------------------------------------------------------*
# ---- Extract impervious and land cover to 100 km circular buffers surround region centers ----
#---------------------------------------------------------------------------------------------------*

# Make empty lists to store lc and imp rasters:

impBufferedList <- list(length = nrow(pts))
lcBufferedList <- list(length = nrow(pts))

# For loop to extract for each region center

for(i in 1:nrow(pts)){
  impBufferedList[[i]] <- makeBufferedRaster(imp, ptsProjR[i,], gProj)
  lcBufferedList[[i]] <- makeBufferedRaster(lc, ptsProjR[i,], gProj)
}

names(impBufferedList) <- as.character(ptsProjR@data$region)
names(lcBufferedList) <- as.character(ptsProjR@data$region)

#---------------------------------------------------------------------------------------------------*
# ---- Use LC file to cut out water from imp ----
#---------------------------------------------------------------------------------------------------*

# Create a reclassification matrix (Note: Water is coded as "11")

reclassMatrix <- matrix(c(-Inf, 11.5, NA, 11.5, Inf, 1), ncol = 3, byrow = T)

# Make an empty list to store impervious surface with water coded as NA:

impNoWaterList <- list(length = nrow(ptsProjR))

for(i in 1:nrow(ptsProjR)){
  landWater <- reclassify(lcBufferedList[[i]], reclassMatrix)
  impNoWaterList[[i]] <- impBufferedList[[i]]*landWater
}

names(impNoWaterList) <- as.character(ptsProjR@data$region)

#---------------------------------------------------------------------------------------------------*
# ---- Write files to disk ----
#---------------------------------------------------------------------------------------------------*
setwd('processedLC')

write.csv(pts, 'regionCenters.csv', row.names = F)


for(i in 1:nrow(ptsProjR)){
  write.csv(pts[i,], paste('centroid', pts[i,1],'.csv', sep = ''), row.names = F)
  writeRaster(impBufferedList[[i]],paste('imp',pts[i,1], sep = ''))
  writeRaster(impNoWaterList[[i]], paste('impNoWater',pts[i,1], sep = ''))
  writeRaster(lcBufferedList[[i]], paste('lc',pts[i,1], sep = ''))
}


#####################################################################################################*
# ---- SCRATCH BELOW ----
#####################################################################################################*
# 
# e <- drawExtent()
# # Note: e <- c(1042305, 2296635, 249855, 3169005) #(xmin, xmax, ymin, ymax)
# 
# rCrop <- crop(r, e)
# 
# rm(r)
# 
# rTrim <- trim(rCrop)
# 
# rm(rCrop)
# 
# gProj <- '+init=epsg:4326'
# 
# rProjected <- projectRaster(rTrim, crs = gProj)
# writeRaster(rProjected, 'impProjected')
# 
# eMA1 <- drawExtent()
# rMA1 <- crop(rTrim, eMA1)
# rMA1trim <-trim(rMA1)
# rMA1proj <- projectRaster(rMA1trim, crs = gProj)
# 
# # eMA2 <- extent(-73.319, -71.858, 41.55, 42.73)
# # rMA2 <- crop(rMA1proj, eMA2)
# # rMA2trim <-trim(rMA2)
# # rMA2proj <- projectRaster(rMA2trim, crs = gProj)
# 
# getRasterCircle <- function(rasterLayer, long, lat){
#   require(rgeos)
#   centerPoint <- SpatialPoints(cbind(long, lat), proj4string = CRS(gProj))
#   centerPointM <- spTransform(centerPoint, CRS('+proj=utm +zone=18 ellps=WGS84'))
#   pointBuffer <- gBuffer(centerPointM, widt = 60000)
#   pointBufferLongLat <- spTransform(pointBuffer, CRS(gProj))
#   rMask <- mask(rasterLayer, pointBufferLongLat)
#   return(trim(rMask, pad = 2))
# }
# 
# plot(rTrim)
# 
# rMAagg <- aggregate(rMA2proj, 8, mean)
# 
# writeRaster(rMA2proj, 'impervious_by_region/imperviousSpringfield')
# writeRast
