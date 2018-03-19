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


get_outlierBottom <- function(x){
  median(x) - 2.5*mad(x)
}

get_outlierTop <- function(x){
  median(x) + 2.5*mad(x)
}

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

measures <- 
  c('mass',
    'wing',
    'tl')

captureList <- vector('list', length = length(focalSpp))


for(i in seq_along(focalSpp)){
  # Get values for a given measure:
  x <- captures %>%
    filter(spp == focalSpp[i]) %>%
    select(measures[j]) %>%
    unlist(use.names = FALSE)
  # Remove outliers:
  x_noOutlier <-
    x %>%
    subset(between(x,
                   get_outlierBottom(x),
                   get_outlierTop(x))) %>%
    # plyr::round_any(.5) %>%
    as_tibble
  
 
  
  # Get cumulative values:
  # Get gradient breaks:
  get_gradBreaks <- function(x){
    c(mean(x) - sd(x), mean(x) + sd(x))
  }
  

  captures %>%
    filter(spp == focalSpp[i]) %>%
    filter(mass < 1000, wing < 1000) #%>%
    # ggplot(aes(x = wing, y = mass)) +
    # stat_density_2d(aes(fill = ..level..), geom = 'polygon')
    
  x_noOutlier %>%
    ggplot(aes(x = value)) +
    geom_density(fill = 'gray') +
    scale_fill_gradientn(breaks = get_gradBreaks(x_noOutlier$value),
                         colors = c('Blue', 'Red'))
  
  rnorm(100000, mean = mean(x_noOutlier$value), sd = sd(x_noOutlier$value)) %>% density %>% plot

    
  
  x_noOutlier %>%
    group_by(value) %>%
    summarize(n = n()) %>%
    mutate(
      propSamples = n/sum(n),
      cumProp = cumsum(propSamples),
      cInterval = case_when(
        cumProp <= .025 | cumProp >= .975 ~ 'c05',
        cumProp <= .05 | cumProp >= .95 ~ 'c10',
        TRUE ~ 'c90'
      )) %>%
    ggplot(aes(x = value, y = propSamples, color = cInterval)) + geom_line()
  
  # Get density
  x_density <-
    x %>%
    density(
      from = get_outlierBottom(x),
      to = get_outlierTop(x)
    )
  densityDf <- data_frame(
    measurement_value = x_density$x, 
    density_value = x_density$y
  ) %>%
    mutate(cumSum = cumsum(density_value))
  
}

captures %>%
  filter(mass > 1000) %>%
  group_by(spp) %>%
  outlierFilter(mass) 
  
  
  ggplot(aes(x = mass)) +
  geom_density()
  

