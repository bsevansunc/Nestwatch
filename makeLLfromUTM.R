testPoints <- data.frame(x = 296758, y = 4310941)

getLatLonFromUTM <- function(pointFrame, zone){
  utmProjection <- paste0('+proj=utm +zone=', zone, '+datum=WGS84')
  require(rgdal)
  pointsUTM <- SpatialPoints(pointFrame[,c('x', 'y')], 
                             proj4string = CRS(utmProjection))
  pointsLL <- spTransform(pointsUTM, 
                          CRS("+proj=longlat +datum=WGS84"))@coords %>%
    as.data.frame %>%
    dplyr::rename(lon = x, lat = y)
  return(pointsLL)
}

getLatLonFromUTM(testPoints, 17)
