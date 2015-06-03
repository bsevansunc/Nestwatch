###################################################################################################*
# Source functions for Neighborhood Nestwatch
###################################################################################################*
# Author: Brian Evans
# Institutional Association: Migratory Bird Center, Smithsonian Conservation Biology Institute, National Zoological Park
# Initial write date: May 13, 2015

#==================================================================================================*
# ---- FUNCTIONS FOR GEOCODING ADDRESSES AND VISUALIZING IN GOOGLE EARTH OR MAPS ----
#==================================================================================================*

# Function to geocode address using Google Earth's address geocoder. This function takes a csv as an input with one field of the csv titled "address" (lower case).

addressToLatLong = function(addressFilePath){
  if (!'RCurl' %in% installed.packages()) install.packages('RCurl')
  if (!'RJSONIO' %in% installed.packages()) install.packages('RJSONIO')
  require(RCurl)
  require(RJSONIO)
  
  construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
    root <- "http://maps.google.com/maps/api/geocode/"
    u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
    return(URLencode(u))
  }

  gGeoCode <- function(address,verbose=FALSE) {
    if(verbose) cat(address,"\n")
    u <- construct.geocode.url(address)
    doc <- getURL(u)
    x <- fromJSON(doc,simplify = FALSE)
    if(x$status=="OK") {
      lat <- x$results[[1]]$geometry$location$lat
      lng <- x$results[[1]]$geometry$location$lng
      return(c(lat, lng))
    } else {
      return(c(NA,NA))
    }
  }
  # Get address file:
    addressFile = read.csv(addressFilePath)
  # Loop across addresses:
    addressFile$Lat = 0 
    addressFile$Long = 0   
    for(i in 1:nrow(addressFile)){
      LatLong = gGeoCode(as.character(addressFile$address[i]))
      addressFile$Lat[i] = LatLong[1]
      addressFile$Long[i] = LatLong[2]  
    }
    return(addressFile)
  }

# Function to write a csv with latitude and longitude columns added:
# Note: make sure that the address field is titled "address" (all lower case)!

writeAddressToLatLong = function(addressFilePath, outPath){
  addressFile = addressToLatLong(addressFilePath)
  write.csv(addressFile, outPath, row.names = F)
}

# Function to project lat longs and write KML file. 
# Note: Make sure that the LatLong field titles are "Lat" and "Long"!

PointsToKML = function(addressFilePath, outPath){
  if (!'rgdal' %in% installed.packages()) install.packages('rgdal')
  addressFile = read.csv(addressFilePath)
  coordinates(addressFile) = c('Long', 'Lat')
  proj4string(addressFile) = CRS("+proj=longlat +datum=WGS84")
  writeOGR(addressFile['site'], outPath, layer = 'site',driver = 'KML')
}

#==================================================================================================*
# ---- FUNCTIONS FOR EXTRACTING LAND COVER (IMPERVIOUS SURFACE) INFORMATION ----
#==================================================================================================*

# Function to read rasters:

getProjectedRasterList = function(PathToRawRasterFolder){
  # Get file names
  rasterFiles = list.files(PathToRawRasterFolder,
                           pattern = "\\.tif$")
  # Create an empty list to store the formatted rasters:
  outList = list(length = length(rasterFiles))
  # Loop writes projected rasters to disc and stores in memory:
  for(i in 1:length(rasterFiles)){
    # Set input and output files:
    inRas = paste('raw_lc', rasterFiles[i], sep = '/')
    outRas = paste(substr(inRas,1, nchar(inRas)-4),
                   'PROJ.tif', sep = '')
    # Re-project raster into WGS84 decimal degrees:
    gdalwarp(inRas, outRas,t_srs = 'EPSG:4326')
    # Read the projected raster back in:
    rasterProj = readGDAL(outRas)
    # Convert to a raster object (from spatial grid dataframe):
    r = raster(rasterProj)
    # Set NA value of 127:
    NAvalue(r) = 127
    # Add to list:
    outList[[i]] = r
  } 
  return(outList)
}

# Function to determine the lat and long of a point a given distance from another (written by Hijmans based on http://www.movable-type.co.uk/scripts/latlong.html):

destPoint <- function(lon, lat, bearing, d, dms=FALSE, R=6378137) {
  # bearing in degrees
  # d in meters
  # R=earth's mean radius in m
  RadDeg <- pi / 180
  lat1 = lat * RadDeg
  lon1 = lon * RadDeg
  brng = bearing * RadDeg
  lat2 <- asin( sin(lat1) * cos(d/R) + cos(lat1) * sin(d/R) * cos(brng) )
  lon2 <- lon1 + atan2(sin(brng) * sin(d/R) * cos(lat1), cos(d/R) - sin(lat1)
                       * sin(lat2))
  lon2 <- (lon2 + pi) %% ( 2 * pi) - pi; # normalise to -180...+180
  if (is.nan(lat2) | is.nan(lon2)) return(NULL)
  return ( (cbind(lon2, lat2)) / RadDeg )
}

# Get extent based on Central Lat and Long

getExtent = function(CentralLat, CentralLong){
  bearings = c(360, 90, 180, 270)
  pointsMat = matrix(ncol = 2, nrow = 4)
  for(i in 1:length(bearings)){
    pointsMat[i,] = destPoint(CentralLong, CentralLat, bearings[i], 100000)
  }
  e = extent(c(min(pointsMat[,1]), max(pointsMat[,1]),
               min(pointsMat[,2]), max(pointsMat[,2])))
  return(e) 
}


rasterCrop = function(rasterList, CentralLat, CentralLong){
  # First make an extent object and crop the rasters to that object:
  e = getExtent(CentralLat, CentralLong)
  # Make a new list and crop each raster:
  rasterCropList = list(length = length(rasterList))
  for(i in 1:length(rasterList)){
    r = rasterList[[i]]
    rasterCropList[[i]] = crop(r, e)
  }
  return(rasterCropList)
}

# Function to merge cropped rasters:

rasterMerge = function(rasterCropList, CentralLat, CentralLong){
  # Modified from: https://www.nceas.ucsb.edu/scicomp/usecases/createrasterimagemosaic
  e = getExtent(CentralLat, CentralLong)
  inputRasters = rasterCropList
  # Make bounding raster of full study region extent:
  boundingRaster = raster(e, crs=projection(inputRasters[[1]]))
  # Set resolution to that of the input rasters
  res(boundingRaster) = res(inputRasters[[1]])
  # Resample each raster to the bounding raster to ensure equivalent origin:
  resampledRasterList = list(length = length(inputRasters))
  for(i in 1:length(inputRasters)){
    # Crop bounding Raster to input raster:
    targetRaster = crop(boundingRaster, inputRasters[[i]])
    # Resample inputRaster to 
    resampledRasterList[[i]] = resample(inputRasters[[i]], targetRaster, method="bilinear")
  }
  # Merge resampled rasters:
  rasterMerged = do.call(merge, resampledRasters)
  return(rasterMerged) 
}

# Wrapper function for all of the above steps. This takes forever to run!

processRaster = function(PathToRawRasterFolder, CentralLat, CentralLong, outPath){
  # Install necessary packages if not already on computer:
  if (!'rgdal' %in% installed.packages()) install.packages('rgdal')
  if (!'raster' %in% installed.packages()) install.packages('raster')
  if (!'gdalUtils' %in% installed.packages()) install.packages('gdalUtils')
  # Load libraries:
  library(rgdal)
  library(raster)
  library(gdalUtils)
  # Load rasters
  rasterList = getProjectedRasterList(PathToRawRasterFolder)
  # Crop rasters:
  rasterCropList = Crop(rasterList, CentralLat, CentralLong)
  # Merge rasters and write:
  rasterMerged = rasterMerge(rasterCropList, CentralLat, CentralLong)
  writeRaster(rasterMerged, outPath)
  # Return raster in case more work is to be done on it:
  return(rasterMerged)
}

# Circular weights matrix:

# Function to make a filter of a given size for a raster of resolution "res" (both resolution and radius should be in meters):

buildWeightsMatrix = function(radius, res){
  cellDistance = radius * 2/res + 1
  weightsMatrix = matrix(nrow = cellDistance, ncol = cellDistance)
  weightsMatrix[,] = 1
  return(weightsMatrix)
}

# Filter for running moving window at a given radius over a map of a given resolution.

movingWindowMean = function(rasterIn, radius, res, outPath){
  if (!'raster' %in% installed.packages()) install.packages('raster')
  require(raster)
  rasterIn[rasterIn < 0] = NA
  weightsMatrix = buildWeightsMatrix(radius, res)
  movingMeanRaster = focal(rasterIn, w=weightsMatrix, fun=mean, na.rm=T)
  writeRaster(movingMeanRaster, outPath, overwrite = T)
  return(movingMeanRaster)
}

# Impervious surface cover in user provided accuracy:

roundImpValues = function(inRaster, accuracy, floorOrCeiling){
  if (!'plry' %in% installed.packages()) install.packages('plyr')
  require(plyr)
  rasterValues = getValues(inRaster)
  rasterValues = rasterValues[rasterValues >=  0]
  rasterValuesRound = round_any(rasterValues, accuracy, f = floorOrCeiling)
  return(rasterValuesRound)
}

# Proportional impervious surface cover relative to the number of map pixels:

rasterImpPropDF = function(inRaster, accuracy, floorOrCeiling){
  if (!'dplyr' %in% installed.packages()) install.packages('dplyr')
  require(dplyr)
  rasterValuesRound = roundImpValues(inRaster, accuracy, floorOrCeiling)
  rasterValues.df = data.frame(imp = rasterValuesRound)
  impProp.df = data.frame(rasterValues.df %>% group_by(imp) %>%
                            summarise (n = n()) %>%
                            mutate(freq = n / ncell(inRaster)))
  return(impProp.df)
}

# Impervious surface cover for Nestwatch Sites

roundImpValuesNN = function(LatLong, inRaster, accuracy, floorOrCeiling){
  if (!'plyr' %in% installed.packages()) install.packages('plyr')
  require(plyr)
  coordinates(LatLong) = ~Long + Lat
  rasterValues = extract(inRaster, LatLong)
  rasterValues = rasterValues[rasterValues >=  0]
  rasterValuesRound = round_any(rasterValues, accuracy, f = floorOrCeiling)
  return(rasterValuesRound)
}

# Proportional impervious surface cover relative to other NN sites:

rasterImpPropDF = function(LatLong, inRaster, accuracy, floorOrCeiling){
  if (!'dplyr' %in% installed.packages()) install.packages('dplyr')
  require(dplyr)
  rasterValuesRound = roundImpValuesNN(LatLong,inRaster, accuracy, floorOrCeiling)
  rasterValues.df = data.frame(imp = rasterValuesRound)
  impProp.df = data.frame(rasterValues.df %>% group_by(imp) %>%
                            summarise (n = n()) %>%
                            mutate(freq = n / length(n)))
  return(impProp.df)
}

