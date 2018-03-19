
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


outlierBottom <- function(x){
  median(x) - 2.5*mad(x)
}

outlierTop <- function(x){
  median(x) + 2.5*mad(x)
}

outlierFilter <- function(df, var, x){
  df %>%
    filter(var > outlierBottom(x)) %>%
    filter(var < outlierTop(x))
}

outlierFilter <- function(df, x){
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

captures <-
  suppressWarnings(
    read_csv('nocaReport/captureTable.csv') %>%
      setNames(str_replace_all(names(.), 'Capture', '')) %>%
      select(siteID, date, obs:bandNumber, mass:sex) %>%
      mutate(tl  = as.numeric(tl))
  )

# locations:

locations <-
  read_csv('data/databaseBackup_2018-02-01/locationTable.csv') %>%
  setNames(str_replace_all(names(.), 'Location', '')) %>%
  select(siteID, long, lat)

noca <-
  captures %>%
  filter(spp == 'NOCA')


# plotting ----------------------------------------------------------------

# Mass bean plot:

noca %>%
  filter(!sex %in% c('noData', 'U')) %>%
  outlierFilter(mass) %>% 
  ggplot(aes(x = sex, y = mass)) +
  geom_violin() +
  coord_flip()

# Mass by region:

noca %>%
  filter(sex %in% c('M', 'F')) %>%
  outlierFilter(mass) %>%
  mutate(region = addRegion(siteID)) %>%
  ggplot(aes(x = region, y = mass)) +
  geom_boxplot(aes(fill = sex)) +
  coord_flip() +
  theme_bw()

# Tail by region:

noca %>%
  mutate(region = addRegion(siteID)) %>%
  mutate(tl = as.numeric(tl)) %>%
  filter(sex %in% c('M', 'F')) %>%
  outlierFilter(tl) %>%
  mutate(sex = factor(sex, labels = c('Female', 'Male'))) %>%
  ggplot(aes(x = region, y = tl)) +
  geom_boxplot(aes(fill = region)) +
  facet_wrap(~sex) +
  labs(title = 'Northern Cardinal tail length by region',
       x = 'Tail (mm)',
       y = 'Region') +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(80, 120)) +
  theme_bw() +
  theme(
    panel.spacing.x = unit(1, "lines")
  )

# Mass by wing:

noca %>%
  mutate(region = addRegion(siteID)) %>%
  filter(sex %in% c('M', 'F')) %>%
  outlierFilter(mass) %>%
  outlierFilter(wing) %>%
  mutate(sex = factor(sex, labels = c('Female', 'Male'))) %>%
  ggplot(aes(x = wing, y = mass)) +
  geom_point(aes(color = region), alpha = 0.2) +
  stat_smooth(aes(color = region, fill = region), method = 'loess') +
  facet_wrap(~sex) +
  labs(title = 'Northern Cardinal tail length by region',
       x = 'Tail (mm)',
       y = 'Region') +
  coord_flip() +
  # scale_y_continuous(expand = c(0,0),
  #                    limits = c(80, 120)) +
  theme_bw() +
  theme(
    panel.spacing.x = unit(1, "lines")
  )


# some models -------------------------------------------------------------

noca4var <-
  noca %>%
  mutate(region = addRegion(siteID)) %>%
  mutate(tl = as.numeric(tl)) %>%
  filter(tl < 200, mass < 200, wing < 200) %>%
  filter(
    between(tl, outlierBottom(tl), outlierTop(tl)),
    between(mass, outlierBottom(mass), outlierTop(mass)),
    between(wing, outlierBottom(wing), outlierTop(wing))
  ) %>%
  na.omit

nocaSe_across <-
  noca4var %>%
  group_by(region) %>%
  summarize(
    mass_sd = sd(mass)/sqrt(length(mass)),
    wing_sd = sd(wing)/sqrt(length(wing)),
    tl_sd = sd(tl)/sqrt(length(tl))
  ) %>%
  gather(key = measure, value = sd, mass_sd:tl_sd) %>%
  mutate(measure = str_replace_all(measure, '_sd', ''))

nocaRecap <- 
  noca4var %>%
  group_by(region, bandNumber) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  summarize(
    mass = sd(mass)/sqrt(length(mass)),
    wing = sd(wing)/sqrt(length(wing)),
    tl = sd(tl)/sqrt(length(tl))
  ) %>%
  na.omit %>%
  gather(key = measure, value = mn, mass:tl) %>%
  group_by(region, measure) %>%
  summarize(
    mean_Var = mean(mn),
    se_Var = sd(mn)/sqrt(length(unique(mn)))
  )

nocaSe_across %>%
  left_join(nocaRecap, by = c('region', 'measure'))

captures %>%
  filter(spp == 'NOCA') %>%
  mutate(region = addRegion(siteID)) %>%
  mutate(tl = as.numeric(tl)) %>%
  filter(tl < 200, mass < 200, wing < 200) %>%
  filter(
    between(tl, outlierBottom(tl), outlierTop(tl)),
    between(mass, outlierBottom(mass), outlierTop(mass)),
    between(wing, outlierBottom(wing), outlierTop(wing))
  ) %>%
  na.omit %>%
  filter(obs != 'noData') %>%
  do(fit = lm(wing ~ obs, data= .)) %>%
  broom::glance(fit)
  

  


  
noca_across %>%
  ggplot(aes(x = measure, y = se)) +
  geom_boxplot(aes(color = region),
               position = position_dodge(width = .5),
               size = 1) +
  geom_point(data = nocaRecap %>% group_by(region, measure) %>% summarize(se = mean(mn)),
             position = position_dodge(width = .5),
             aes(x = measure, y = se, color = region))
  theme_bw()
  
