options(stringsAsFactors = FALSE)

usa <- map_data('usa')

states <- map_data("state")

nnRegions <- list(
  data.frame(hub = 'DC', long = -77.0369, lat = 38.9072),
  data.frame(hub = 'Raleigh', long = -78.6382, lat = 35.7796)
) %>%
  bind_rows



blankMap <- ggplot(data = states) + 
  geom_polygon(
    aes(x=long, y = lat, group = group),
    fill = '#00cc44', color = 'white', alpha = .5) +
  coord_fixed(1.3) +
  theme_bw()

blankMap +
  
