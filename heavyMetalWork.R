usa <- map_data('usa')

states <- map_data("state")

counties <- map_data("county")

# states <- getData('GADM', country='USA', level=1) %>%
#     subset(!(NAME_1 %in% c('Alaska', 'Hawaii'))) %>%
#     gUnaryUnion

pb <- read_csv('daily_LEAD_2017.csv') %>%
  transmute(
    long = Longitude,
    lat = Latitude,
    date = as.Date(`Date Local`, '%m/%d/%Y'),
    meanLead = `Arithmetic Mean`
  )

blankMap <- ggplot(data = states) + 
  geom_polygon(
    aes(x=long, y = lat, group = group),
    fill = '#00cc44', color = 'white', alpha = .5) +
  coord_fixed(1.3) +
  theme_bw()

pbSummary <- pb %>%
  group_by(long, lat) %>%
  summarize(pb = mean(meanLead))


blankMap +
  geom_point(data = pbSummary %>%
               filter(long >= -130,
                      lat >= 25),
             aes(x = long, y = lat, color = pb, size = pb), alpha = .7) +
  scale_color_gradient2(low = 'blue', high = 'red')

# Mercury:

hg <- raster('Hg_conc_2014.tif') %>%
  projectRaster(crs = CRS('+init=epsg:4326')) 

hgDf <- hg %>%
  as('SpatialPixelsDataFrame') %>%
  as.data.frame %>%
  rename(hg = Hg_conc_2014)

hgMap <- ggplot() + 
  geom_tile(data = hgDf, aes(x = x, y= y, fill = hg)) +
  scale_fill_viridis(option = "magma")  +
  coord_fixed(1.3)
  

hgMap +
  geom_polygon(
    data = counties,
    aes(x = long, y = lat, group = group),
    color = 'black', fill = 'transparent'
    ) +
  theme_bw()

dfToSpatialPolygonFrame <- function(df, projectionInfo = '+init=epsg:4326'){
  groups <- unique(df$group)
  polyList <- vector('list', length = length(groups))
  for(i in 1:length(groups)){
    polyList[[i]] <- counties %>%
      filter(group == groups[i]) %>%
      select(long, lat) %>%
      Polygon %>%
      list %>%
      Polygons(ID = i)
  }
  polySpatial <- SpatialPolygons(polyList,proj4string=CRS(projectionInfo))
  return(polySpatial)
}

test <- dfToSpatialPolygonFrame(counties) %>%
  SpatialPolygonsDataFrame(data  = counties %>%
                             select(group, region, subregion) %>%
                             distinct)

test$mercury <- raster::extract(hg, test, fun = mean)

spdfToGG <- function(spdf){
  require(rgeos)
  spdf@data$id <- rownames(spdf@data)
  spdf.points <- fortify(spdf, group="id")
  outFrame <- full_join(spdf.points, 
                        spdf@data, by="id") %>%
    tbl_df
}



t2 <- spdfToGG(test)


plot(test)

ggplot(t2, aes(x = long, y = lat, group = id, fill = region)) + geom_polygon(color = 'white')

