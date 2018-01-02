library(leaflet)
library(dplyr)

r = raster('LandCover/r500mw')

nn <- tbl_df(read.csv('exampleLL.csv')) %>%
  mutate(siteClass = 'existing')

newSites <- read.csv('nnPotentialSites.csv') %>%
  tbl_df() %>%
  select(email, Lat, Long) %>%
  rename(site = email) %>%
  mutate(siteClass = 'potential')

sites <- bind_rows(nn, newSites)

rAgg <- aggregate(r, 2, mean)

sitePal  <- colorFactor(c('navy', 'red'), domain = c('existing','potential'))
impPal <- colorNumeric(c("#A9A9A9", "#FFD700", "#FF0000"), 
                       values(rAgg), na.color = "transparent")

previewColors(colorNumeric(c('#A9A9A9','#FFD700','#FF0000'), domain = NULL), sort(rexp(16)))

  
leaflet(data = sites) %>%
  addTiles() %>%
  addRasterImage(rAgg, colors = impPal, opacity = 0.8) %>%
  addCircleMarkers(~Long, ~Lat,
                   radius = 6,
                   color = ~sitePal(siteClass),
                   stroke = F,
                   fillOpacity = 0.8,
                   popup = ~site)

?writeRaster
