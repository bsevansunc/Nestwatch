library(tidyverse)
library(maps)

options(stringsAsFactors = FALSE)

# NN regions in space:

nnRegions <- bind_rows(
  data.frame(hub = 'Atlanta', long = -84.3880, lat = 33.7490),
  data.frame(hub = 'Colorado', long = -104.9903, lat = 39.7392),
  data.frame(hub = 'DC', long = -77.0369, lat = 38.9072),
  data.frame(hub = 'Gainesville', long = -82.3248, lat = 29.6516),
  data.frame(hub = 'Pittsburgh', long = -79.9959, lat = 40.4406),
  data.frame(hub = 'Raleigh', long = -78.6382, lat = 35.7796),
  data.frame(hub = 'Springfield', long = -72.5898, lat = 42.1015)
)

nnRegions$hubLabel <- c(
  'Atlanta, GA', 'Denver, CO', 'Washington, DC', 'Gainesville, FL',
  'Pittsburgh, PA', 'Raleigh, NC', 'Springfield, MA'
)

# Background map:


blankMap <- ggplot(map_data("state")) + 
  geom_polygon(aes(long, lat, group = group), fill = 'white', color = '#B0B0B0') +
  coord_fixed(1.3) +
  # xlab(expression(degree~Longitude)) +
  # ylab(expression(degree~Latitude)) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  theme_void() +
  theme(
    panel.background = element_rect(
      fill = '#F5F5F5', 
      color = '#808080', 
      size = .25, 
      linetype = 'solid')
  ) +
  scale_x_continuous(limits = c(-125, -65), expand = c(0,0))

# Add regional points:

blankMapPts <- blankMap +
  geom_point(data = filter(nnRegions, hub != 'DC'), aes(long, lat), size = 5, color = 'black') +
  geom_point(data = filter(nnRegions, hub != 'DC'), 
             aes(long, lat),
             size = 3,
             color = '#337DFF') +
  geom_point(data = filter(nnRegions, hub == 'DC'),
             aes(long, lat),
             pch = "\u2605", size = 8)

# Add regional labels:

blankMapPts +
  geom_text(
    data = filter(nnRegions, hub != 'DC'), 
    aes(long, lat, label = hubLabel),
    nudge_y = 1.1,
    size = 5,
    fontface = 'bold',
    check_overlap = TRUE
  ) +
  geom_text(
    data = filter(nnRegions, hub == 'DC'), 
    aes(long, lat, label = hubLabel),
    nudge_x = 5,
    nudge_y = 0,
    size = 5,
    fontface = 'bold'
    ) 

# Save map:

ggsave('test.png', scale = 1.7)

  
