library(leaflet)
library(dplyr)

nn <- tbl_df(read.csv('exampleLL.csv')) %>%
  mutate(siteClass = 'existing')

newSites <- read.csv('nnPotentialSites.csv') %>%
  tbl_df() %>%
  select(email, Lat, Long) %>%
  rename(site = email) %>%
  mutate(siteClass = 'potential')

sites <- bind_rows(nn, newSites)

pal  <- colorFactor(c('navy', 'red'), domain = c('existing','potential'))

leaflet(data = sites) %>%
  addTiles() %>%
  addCircleMarkers(~Long, ~Lat,
                   radius = 6,
                   color = ~pal(siteClass),
                   stroke = F,
                   fillOpacity = 0.5,
                   popup = ~site)
