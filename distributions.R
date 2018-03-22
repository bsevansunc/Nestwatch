# set up ------------------------------------------------------------------

library(tidyverse)


# functions ---------------------------------------------------------------

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
      fill = '#ff6666',#'#b30000', 
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
      fill = '#99c2ff',#0000CD',
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
      # axis.title.x = element_text(size = rel(1.2)),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = rel(1))
    )
}

# function to plot a mass, wing, tail row

plotGrid <- function(focalSpp, mainTitle){
  require(grid)
  grid.arrange(
    plotMeasureDist(subsetMeasure(focalSpp, 'mass'), 'Mass (g)',''),
    plotMeasureDist(subsetMeasure(focalSpp, 'wing'), 'Wing (mm)', ''),
    plotMeasureDist(subsetMeasure(focalSpp, 'tl'), 'Tail (mm)', ''),
    nrow = 1,
    left = textGrob(
      mainTitle,
      rot= 90,
      gp=gpar(fontsize=12, fontface = 'bold'))
  )
}

# add data ----------------------------------------------------------------

captures <-
  suppressWarnings(
    read_csv('data/databaseBackup_2018-02-01/captureTable.csv') %>%
      setNames(str_replace_all(names(.), 'Capture', '')) %>%
      select(siteID, date, obs:bandNumber, mass:sex) %>%
      mutate(tl  = as.numeric(tl))
  )


# get species measurement vectors -----------------------------------------

focalSpp <-
  c('American Robin' = 'AMRO',
    'Black-capped chickadee' = 'BCCH',
    'Brown thrasher' = 'BRTH',
    'Carolina chickadee' = "CACH",
    'Carolina wren' = "CARW",
    'Eastern phoebe' = 'EAPH',
    'Gray catbird' = 'GRCA',
    'Northern cardinal' = 'NOCA',
    'Northern mockingbird' = 'NOMO',
    'Song sparrow' = 'SOSP',
    'Tufted titmouse' =  'TUTI')


# summary plots -----------------------------------------------------------

plotList <- vector('list', length = length(focalSpp))

for(i in seq_along(focalSpp)){
  plotList[[i]] <- plotGrid(focalSpp[i], names(focalSpp)[i])
}


# summary table -----------------------------------------------------------


summaryList <- vector('list', length = length(focalSpp))

focalMeasures <- c('mass', 'wing', 'tl')

for(i in seq_along(focalSpp)){
  focalMeasureList <- vector('list', length = length(focalMeasures))
  for(j in seq_along(focalMeasures)){
    x <- subsetMeasure(focalSpp[i], focalMeasures[j])
    focalMeasureList[[j]] <- data_frame(
      species  = names(focalSpp)[i],
      measure = focalMeasures[j],
      lowerBound = min(x),
      lowerLimit = mean(x) - sd(x),
      mean = mean(x),
      upperLimit = mean(x) + sd(x),
      upperBound = max(x)
    )
  }
  summaryList[[i]] <- bind_rows(focalMeasureList)
}

bind_rows(summaryList)



# save pdf files ----------------------------------------------------------

grid.arrange(
  plotList[[1]],
  plotList[[2]],
  plotList[[3]],
  plotList[[4]],
  nrow = 4
) %>%
  ggsave(file = 'distributionPlots/distributions_amro-cach.pdf',
         width = 7.5,
         height = 10)

grid.arrange(
  plotList[[5]],
  plotList[[6]],
  plotList[[7]],
  plotList[[8]],
  nrow = 4
) %>%
  ggsave(file = 'distributionPlots/distributions_carw-noca.pdf',
         width = 7.5,
         height = 10)

grid.arrange(
  plotList[[9]],
  plotList[[10]],
  plotList[[11]],
  nrow = 3
) %>%
  ggsave(file = 'distributionPlots/distributions_nomo-tuti.pdf',
         width = 7.5,
         height = 7.5)


# save png files ----------------------------------------------------------

for(i in seq_along(focalSpp)){
  plotList[[i]] %>%
    ggsave(
      filename = paste0('distributionPlots/distributions_',
             focalSpp[i],
             '.png'),
      width = 7.5,
      height = 2.5
    )
}


