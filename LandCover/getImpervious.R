
# get rasters -------------------------------------------------------------

rasterDir <- ('C:/Users/EvansBr/Desktop/spatialDataRaw')


raster('C:/Users/EvansBr/Desktop/spatialDataRaw/imp_dc')

library(raster)

select <- dplyr::select

readRaster <- function(rFile){
  raster(paste(rasterDir, rFile, sep = '/'))
}

impList <-
  list(
    atlanta = readRaster('imp_atlanta'),
    dc = readRaster('imp_dc'),
    denver = readRaster('imp_denver'),
    gainesville = readRaster('imp_gainesville'),
    pittsburgh = readRaster('imp_pittsburgh'),
    raleigh = readRaster('imp_raleigh'),
    springfield = readRaster('imp_springfield')
)

impProj <- projection(impList[[1]])

# get point locations -----------------------------------------------------

# Function to add region to a plot (based on siteId field):

addRegion_lowerCase <- function(x){
  xState <- str_sub(x, start = -3, end = -2)
  case_when(
    xState == 'GA' ~ 'atlanta',
    xState == 'CO' ~ 'denver',
    xState == 'FL' ~ 'gainesville',
    xState == 'NC' ~ 'raleigh',
    xState == 'PA' ~ 'pittsburgh',
    xState == 'MA' ~ 'springfield',
    xState %in% c('MD','DC', 'VA') ~ 'dc',
    TRUE ~ 'noData'
  )
}

# locations file:

locations <-
  read_csv('data/databaseBackup_2018-02-01/locationTable.csv') %>%
  setNames(str_replace_all(names(.), 'Location', '')) %>%
  select(siteID, long, lat) %>%
  mutate(region =  addRegion_lowerCase(siteID)) %>%
  SpatialPointsDataFrame(
    select(., long, lat),
    data = .,
    proj4string = CRS(impProj)
  )

# extract raster data to locations ----------------------------------------

distances <- seq(100, 2000, by = 100)

regions <- names(impList)

region_dfList <- vector('list', length = length(regions))

for(i in seq_along(regions)){
  distance_dfList <- vector('list', length = length(distances))
  for(j in seq_along(distances)){
    pointData <- subset(locations, region == regions[i])
    distanceVector <- 
      extract(
        impList[[i]],
        pointData,
        buffer = distances[j],
        fun = mean
        )
    distance_dfList[[j]] <-
      as_tibble(pointData) %>%
      mutate(
        distance = distances[j],
        imp = distanceVector
      )
  }
  region_dfList[[i]] <- bind_rows(distance_dfList)
}

region_df <- bind_rows(region_dfList)

write_csv(region_df, 'data/impervious.csv')




