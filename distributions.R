# set up ------------------------------------------------------------------

library(tidyverse)


# functions ---------------------------------------------------------------

# Function to add region to a plot (based on siteId field):

addRegion <- function(x){
  xState <- str_sub(x, start = -3, end = -2)
  case_when(
    xState == 'GA' ~ 'Atlanta',
    xState == 'CO' ~ 'Colorado',
    xState == 'FL' ~ 'Gainesville',
    xState == 'NC' ~ 'Raleigh',
    xState == 'PA' ~ 'Pittsburgh',
    xState == 'MA' ~ 'Springfield',
    xState %in% c('MD','DC', 'VA') ~ 'Washington, DC',
    TRUE ~ 'noData'
  )
}

# Outlier functions:

filter_outliers <- function(df, x){
  # Column to analyze:
  colName <- enquo(x)
  # Get values vector:
  values <- df %>% 
    select(!!colName) %>%
    filter((!!colName) < 1000) %>%
    unlist(use.names = FALSE)
  # Filter:
  df %>%
    filter(!is.na((!!colName))) %>%
    filter(
      (!!colName) > median(values) - 2.5*mad(values)) %>%
    filter(
      (!!colName) < median(values) + 2.5*mad(values)
    )
}

# Function to subset values for a given species and measurement:

subsetMeasure <- function(focalSpp, focalMeasure){
  captures %>%
    filter(spp == focalSpp) %>%
    select(values = focalMeasure) %>%
    na.omit %>%
    filter(values < 1000) %>%
    filter_outliers(values) %>%
    .$values
}

# function to plot distributions

plotMeasureDist <- function(x, xText, titleText){
  ggplot(data.frame(x = x), aes(x)) +
    stat_function(
      fun = dnorm,
      args = list(
        mean = mean(x), 
        sd = sd(x)),
      geom = 'area',
      fill = '#b30000', 
      color = 'black',
      size = 1) +
    stat_function(
      fun = dnorm, 
      args = list(
        mean = mean(x),
        sd = sd(x)),
      xlim = c(mean(x) - sd(x),
               mean(x) + sd(x)),
      geom = "area", 
      fill = '#0000CD',
      color = 'black',
      size = 1,
      alpha = 1) +
    scale_x_continuous(
      limits = c(min(x), max(x)),
      breaks = c(min(x), 
                 mean(x) - sd(x),
                 mean(x),
                 mean(x) + sd(x),
                 max(x)),
      labels = c(round(min(x),1), 
                 round(mean(x) - sd(x), 1),
                 round(mean(x), 1),
                 round(mean(x) + sd(x), 1),
                 round(max(x), 1)
      )
    ) +
    labs(
      title = titleText,
      x = xText
    ) +
    theme_bw() +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}

# function to plot a mass, wing, tail row

plotGrid <- function(focalSpp){
  grid.arrange(
    plotMeasureDist(subsetMeasure(focalSpp, 'mass'), 'Mass (g)','Mass'),
    plotMeasureDist(subsetMeasure(focalSpp, 'wing'), 'Wing (mm)', 'Wing chord'),
    plotMeasureDist(subsetMeasure(focalSpp, 'tl'), 'Tail (mm)', 'Tail'),
    nrow = 1
  )
}

# add data ----------------------------------------------------------------

# captures:

captures <-
  suppressWarnings(
    read_csv('data/databaseBackup_2018-02-01/captureTable.csv') %>%
      setNames(str_replace_all(names(.), 'Capture', '')) %>%
      select(siteID, date, obs:bandNumber, mass:sex) %>%
      mutate(tl  = as.numeric(tl))
  ) %>%
  mutate(region = addRegion(siteID)) %>%
  filter(!region %in% c('Colorado', 'noData')) %>%
  mutate(region = factor(
    region,
    levels = c('Gainesville', 'Atlanta', 'Raleigh', 'Washington, DC', 'Pittsburgh', 'Springfield'))
  )


# get species measurement vectors -----------------------------------------

focalSpp <-
  c('AMRO',
    'BCCH',
    'BRTH',
    "CACH",
    "CARW",
    'EAPH',
    'GRCA',
    'NOCA',
    'NOMO',
    'SOSP',
    'TUTI')

# Function to save grid for a given species:

saveGrid <- function(gridDir, focalSpp){
  g <- plotGrid(focalSpp)
  outPath <- paste(gridDir, focalSpp, sep = '/')
  ggsave(file = outPath, g)
}


for(i in seq_along(focalSpp)){
  g <- plotGrid(focalSpp[i])
  outPath <-  
    paste0(
      'distributionPlots/',
      tolower(focalSpp[i]),
      '.png')
  ggsave(outPath, plot = g, width = 10, height = 3)
}

