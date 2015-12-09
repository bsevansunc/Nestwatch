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

sitePal  <- colorFactor(c('navy', 'red'), domain = c('existing','potential'))
impPal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),
                              na.color = "transparent")
  
leaflet(data = sites) %>%
  addTiles() %>%
  addRasterImage(r, colors = impPal, opacity = 0.8) %>%
  addCircleMarkers(~Long, ~Lat,
                   radius = 6,
                   color = ~SitePal(siteClass),
                   stroke = F,
                   fillOpacity = 0.5,
                   popup = ~site)

?writeRaster
