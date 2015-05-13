###################################################################################################*
# Source functions for Neighborhood Nestwatch
###################################################################################################*
# Author: Brian Evans
# Institutional Association: Migratory Bird Center, Smithsonian Conservation Biology Institute, National Zoological Park
# Initial write date: May 13, 2015

#==================================================================================================*
# ---- FUNCTIONS FOR GEOCODING ADDRESSES AND VISUALIZING IN GOOGLE EARTH OR MAPS ----
#==================================================================================================*

# Function to geocode address using Google Earth's address geocoder. This function takes a csv as an input with one field of the csv titled "address".

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

writeAddressToLatLong = function(addressFilePath, outPath){
  addressFile = addressToLatLong(addressFilePath)
  write.csv(addressFile, outPath, row.names = F)
}

# Function to project lat longs and write KML file

PointsToKML = function(addressFilePath, outPath){
  if (!'rgdal' %in% installed.packages()) install.packages('rgdal')
  addressFile = read.csv(addressFilePath)
  coordinates(addressFile) = c('Long', 'Lat')
  proj4string(addressFile) = CRS("+proj=longlat +datum=WGS84")
  writeOGR(addressFile['site'], outPath, layer = 'site',driver = 'KML')
}




