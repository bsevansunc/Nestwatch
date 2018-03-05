# Summary for Susannah

#=================================================================================*
# ---- functions ----
#=================================================================================*

# Function searches packages in installed package list, adds them if you don't 
# have them, and loads the library:

smartLibrary <- function(packageVector){
  for(i in 1:length(packageVector)){
    package <- packageVector[i]
    if(!package %in% rownames(installed.packages())){
      install.packages(packageVector[i],repos="http://cran.rstudio.com/",
                       dependencies=TRUE)
    }
  }
  lapply(packageVector, library, character.only = TRUE)
}

# Capitalize a string:

capitalize <- function(x) {
  str_c(toupper(str_sub(x, end = 1)),
        str_sub(x, start  = 2))
}

# Add region to a plot (based on siteId field):

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

# Add theme to all plot output:

theme_add <- function(){
  theme(
    axis.title = element_text(size = rel(1.5)),
    panel.background = element_blank(),
    axis.line = element_line(size = 1, linetype = 'solid', color = 'black'),
    axis.text = element_text(size = rel(1.2))
  )
}

#=================================================================================*
# ---- packages, settings, and options ----
#=================================================================================*

# Add packages:

smartLibrary(c('tidyverse', 'lubridate'))

options(stringsAsFactors = FALSE)

#=================================================================================*
# ---- packages, settings, and options ----
#=================================================================================*

tableNames <- c('visit', 'capture', 'partRs', 'techRs')

fileList <- vector('list', length = length(tableNames))

names(fileList) <- tableNames

for (i in seq_along(tableNames)) {
  databaseTable <- read.csv(paste0(tableNames[i], 'Table.csv')) %>%
    tbl_df
  names(databaseTable) <-
    str_replace_all(names(databaseTable), capitalize(tableNames[i]), '')
  fileList[[i]] <- databaseTable
}

# Vector DC Catbird-only and Zoo banding sites to remove:

removeSites <- fileList$capture %>%
  filter(obs == 'DLM' | str_detect(siteID, 'NZP')|str_detect(siteID, 'SLIG')) %>%
  .$siteID %>%
  unique

#=================================================================================*
# ---- 2017 stats ----
#=================================================================================*

# Number banded:

fileList$capture %>%
  filter(!siteID %in% removeSites) %>%
  filter(enc == 'B', year(date) == 2017) %>%
  .$bandNumber %>% unique %>% length

# Recaptured or resighted:

fileList$capture %>%
  filter(!siteID %in% removeSites) %>%
  filter(enc == 'R', year(date) == 2017) %>%
  select(bandNumber) %>%
  .$bandNumber %>% unique %>% length


bind_rows(fileList$partRs, fileList$techRs) %>%
  filter(!siteID %in% removeSites) %>%
  mutate(
    region = addRegion(siteID),
    year = year(date),
    type = toupper(type),
    type = case_when(type %in% c('P', 'N', 'D') ~ 'Participant',
                     TRUE ~ 'Technician')
  ) %>%
  filter(year == 2017) %>%  
  group_by(year, type) %>%
  summarize(n = length(unique(bandNumber)))

reE <-  bind_rows(
  fileList$partRs %>% mutate(enc = 'P'),
  fileList$techRs %>% mutate(enc = 'T')) %>%
  filter(!siteID %in% removeSites) %>%
  filter(year(date) == 2017) %>%
  mutate(
    region = addRegion(siteID)
  ) %>%
  left_join(
    fileList$capture %>%
      select(bandNumber, spp) %>%
      distinct,
    by = 'bandNumber'
  ) %>%
  select(region, siteID, enc, spp, bandNumber) %>%
  distinct
  

enc <- fileList$capture %>%
  filter(!siteID %in% removeSites,
         enc %in% c('B', 'R')) %>%
  filter(year(date) == 2017) %>%
  mutate(region = addRegion(siteID)) %>%
  select(region, siteID, enc, spp, bandNumber) %>%
  bind_rows(reE) %>%
  distinct

encReformat <- enc %>%
  group_by(region, enc, spp) %>%
  summarize(n = length(unique(bandNumber))) %>%
  ungroup %>%
  filter(!is.na(spp)) %>%
  filter(!spp %in% c('BAOR', 'BLJA', 'CACG', 'RBGR', 'RBWO', 'SAVS', 'TRFL', 'UNCH')) %>%
  mutate(species = factor(
    spp,
    labels = c(
      'American robin',
      'Black-capped chickadee',
      'Brown thrasher',
      'Carolina chickadee',
      'Carolina wren',
      'Eastern phoebe',
      'Gray catbird',
      'House wren',
      'Northern cardinal',
      'Northern mockingbird',
      'Song sparrow',
      'Spotted Towhee',
      'Tufted titmouse'
    )
  ),
  enc = factor(
    enc,
    levels = c('B', 'R', 'T', 'P'),
    labels = c('Band', 'Recapture', 'Scientist resight', 'Participant resight')
  )) %>%
  ungroup

library('ggthemes')

plotLines <- function(){
 
}

basicEncPlot <- encReformat %>%
  mutate(`Encounter type` = enc) %>%
  ggplot(aes(x = species, y = n, fill = `Encounter type`)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  facet_wrap( ~ region, nrow = 2) +
  scale_y_continuous(limits = c(0, 200), expand = c(0, 0)) +
  xlab('Species') +
  ylab('Number of birds') +
  theme(axis.line = element_line()) +
  annotate(
    "segment",
    x = -Inf,
    xend = Inf,
    y = -Inf,
    yend = -Inf
  ) +
  annotate(
    "segment",
    x = -Inf,
    xend = -Inf,
    y = -Inf,
    yend = Inf
  )

basicEncPlot +
  theme_add() +
  # theme_tufte() +
  theme(
    axis.text = element_text(size = rel(.9)),
    axis.text.x = element_text(angle = 60, vjust = 0.5),
    panel.spacing = unit(1.5, "lines")
  )
  

ggsave('encounters2017.png', width = 10, height = 6)


#=================================================================================*
# ---- participation stats ----
#=================================================================================*

sites <- fileList$capture %>%
  filter(!siteID %in% removeSites,
         year(date) >= 2016) %>%
  select(siteID) %>%
  distinct %>%
  mutate(region = addRegion(siteID)) %>%
  select(region, siteID)

participantResights <- fileList$partRs %>%
  filter(!siteID %in% removeSites,
         year(date) >= 2016) %>%
  group_by(siteID) %>%
  summarize(n = length(unique(bandNumber)))

participantPropPlot <- left_join(sites, participantResights, by = 'siteID') %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  group_by(region) %>%
  filter(!region %in% c('noData', 'Colorado')) %>%
  summarize(prop = length(n[n >= 1]) / length(n) * 100) %>%
  ggplot(aes(x = region, y = prop)) +
  geom_bar(stat = 'identity', fill = 'gray70', color = 'black', size = .9) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
  xlab('Region') +
  ylab('Proportion of sites with participant resights (%)') +
  theme_add() +
  theme(plot.margin = unit(c(1,1,1,1), 'cm'))

participantPropPlot +
  ggtitle('Participants who resight') +
  theme(
    legend.title = element_text(size = rel(1.5), face = "bold"),
    legend.text = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(1.8), margin = margin(t = 100, b = 50, l = 50, r = 50)),
    axis.text = element_text(size = rel(1.5)),
    plot.title = element_text(size = 36, margin = margin(
      t = 5,
      b = 20,
      unit = 'pt'
    ))
  )
  

ggsave('C:/Users/EvansBr/Downloads/propSiteResights.png', width = 11, height = 7)

#=================================================================================*
# ---- summary tables ----
#=================================================================================*

# Number of sites visited by region and year:

visitsSummary <- fileList$visit %>%
  filter(!siteID %in% removeSites) %>%
  mutate(region = addRegion(siteID), year = year(date)) %>%
  group_by(region, year) %>%
  summarize(n = length(unique(siteID)))

# Number of captures and recaps by region and year:

captureSummary <- fileList$capture %>%
  filter(!siteID %in% removeSites) %>%
  mutate(region = addRegion(siteID), year = year(date)) %>%
  filter(siteID != 'noData', enc %in% c('B', 'R')) %>%
  group_by(region, year, enc) %>%
  summarize(n = length(unique(bandNumber)))

# Number of resighted birds by type, region, and year:

resightSummary <- bind_rows(fileList$partRs, fileList$techRs) %>%
  filter(!siteID %in% removeSites) %>%
  mutate(
    region = addRegion(siteID),
    year = year(date),
    type = toupper(type),
    type = case_when(type %in% c('P', 'N', 'D') ~ 'Participant',
                     TRUE ~ 'Technician')
  ) %>%
  group_by(region, year, type) %>%
  summarize(n = length(unique(bandNumber)))

# Summarized files for Susannah:

write_csv(visitsSummary, 'visitSummary.csv')

write_csv(captureSummary, 'captureSummary.csv')

write_csv(resightSummary,'resightSummary.csv' )

#---------------------------------------------------------------------------------*
# Email addition, summarize by species and add resights ----
#---------------------------------------------------------------------------------*

# Summarizing by species (based on email):

focalSpp <- c('AMRO',
              'BCCH',
              'CACH',
              'CARW',
              'EAPH',
              'GRCA',
              'HOWR',
              'NOCA',
              'NOMO',
              'SOSP')


capRecapBySpp <- fileList$capture %>%
  filter(!siteID %in% removeSites, 
         enc %in% c('B', 'R'),
         spp %in% focalSpp) %>%
  group_by(spp,enc) %>%
  summarize(n = length(unique(bandNumber))) %>%
  spread(enc, n, fill = 0)

resightBySpp <- bind_rows(fileList$partRs, fileList$techRs) %>%
  left_join(fileList$capture %>%
              select(spp, bandNumber),
            by = 'bandNumber') %>%
  filter(!siteID %in% removeSites,
         spp %in% focalSpp) %>%
  group_by(spp) %>%
  summarize(resights = length(unique(bandNumber)))

susannahSummary <- left_join(capRecapBySpp, resightBySpp, by = 'spp')

#---------------------------------------------------------------------------------*
# Email addition 2, re-encounter rates ----
#---------------------------------------------------------------------------------*
# Function for fancy species names:

speciesNameChange <- function(x){
  case_when(
    x == 'GRCA' ~ 'Gray catbird',
    x == 'NOCA' ~ 'Northern cardinal',
    x == 'HOWR' ~ 'House wren', 
    x == 'CACH' ~ 'Carolina chickadee',
    x == 'CARW' ~ 'Carolina wren',
    x == 'BCCH' ~ 'Black-capped chickadee',
    x == 'SOSP' ~ 'Song sparrow', 
    x == 'AMRO' ~ 'American robin',
    x == 'EAPH' ~ 'Eastern phoebe',
    x == 'NOMO' ~ 'Northern mockingbird'
  )
}

# Function for fancy encounter names:

relevelEncounters <- function(x){
  factor(
    x,
    levels = c(
      'no re-encounter',
      'recap only',
      'rsTech only',
      'rsPart only',
      'recap and rsTech',
      'recap and rsPart',
      'rsPart and rsTech',
      'all classes'
    ),
    labels = c(
      'None',
      'Recap',
      'Technician resight',
      'Participant resight',
      'Recap and technician resight',
      'Recap and participant resight',
      'Technician and participant resight',
      'All'
    )
  )
}

# Colors for re-encounters:

rEnc_colors <-
  c('gray60',
    '#E62A10',
    '#F7DC6F',
    '#6C3483',
    '#10E6E0',
    '#0B22A0',
    '#E310E6',
    '#E67110')

# Banded birds:

bandedBirds <- fileList$capture %>%
  filter(!siteID %in% removeSites, enc == 'B') %>%
  filter(spp %in% focalSpp) %>%
  transmute(
    region = addRegion(siteID),
    spp = speciesNameChange(spp),
    bandNumber = bandNumber
  ) %>%
  distinct

# Prepare encounter summary frame:

encounters <- bandedBirds %>%
  mutate(
    recap = case_when(
      bandNumber %in% filter(fileList$capture, enc == 'R')$bandNumber ~ 1,
      TRUE ~ 0),
    rsPart = case_when(
      bandNumber %in% fileList$partRs$bandNumber ~ 1,
      TRUE ~ 0),
    rsTech = case_when(
      bandNumber %in% fileList$techRs$bandNumber ~ 1,
      TRUE ~ 0),
    rEncClass =  case_when(
      recap == 1 & rsPart == 1 & rsTech == 1 ~ 'all classes',
      recap == 1 & rsPart == 0 & rsTech == 0 ~ 'recap only',
      recap == 1 & rsPart == 1 & rsTech == 0 ~ 'recap and rsPart',
      recap == 1 & rsPart == 0 & rsTech == 1 ~ 'recap and rsTech',
      recap == 0 & rsPart == 1 & rsTech == 0 ~ 'rsPart only',
      recap == 0 & rsPart == 1 & rsTech == 1 ~ 'rsPart and rsTech',
      recap == 0 & rsPart == 0 & rsTech == 1 ~ 'rsTech only',
      TRUE ~ 'no re-encounter'
    )
  ) %>%
  select(-c(recap:rsTech)) %>%
  mutate(rEncClass = relevelEncounters(rEncClass))

# Total encounters by species:

encounters %>%
  group_by(spp, rEncClass) %>%
  summarize(n = length(unique(bandNumber))) %>%
  ungroup %>%
  mutate(
    spp = str_replace_all(spp, ' ', '\n'),
    rEncClass =  case_when(
      rEncClass == 'None' ~ 'Not re-encountered',
      TRUE ~ 'Re-encountered'
    )
  ) %>%
  group_by(spp, rEncClass) %>%
  summarize(n = sum(n)) %>%
  ggplot(aes(x = spp, y = n, fill = rEncClass)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(0, 5000), expand = c(0, 0)) +
  xlab('Species') +
  ylab('Number of birds') +
  scale_fill_manual(values = c('gray80', '#E67110'),
                    name = '') +
  theme_add() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

ggsave('sum_captures_reencounters.png', width = 11, height = 6)


# Proportional encounters by species:

encounters %>%
  group_by(spp, rEncClass) %>%
  summarize(n = length(unique(bandNumber))) %>%
  ungroup %>%
  mutate(
    spp = str_replace_all(spp, ' ', '\n'),
    rEncClass =  case_when(
      rEncClass == 'None' ~ 'Not re-encountered',
      TRUE ~ 'Re-encountered'
    )
  ) %>%
  group_by(spp, rEncClass) %>%
  summarize(n = sum(n)) %>% 
  group_by(spp)  %>%
  mutate(tEncounters = sum(n)) %>%
  ungroup() %>%
  mutate(prop = n/tEncounters*100) %>%
  ggplot(aes(x = spp, y = prop, fill = rEncClass)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  xlab('Species') +
  ylab('Proportion of birds') +
  scale_fill_manual(values = c('gray80', '#E67110'),
                    name = '') +
  theme_add() + coord_flip()

ggsave('prop_captures_reencounters.png', width = 11, height = 7)


# Proportion of encounters by species and type:

encounters %>%
  group_by(spp, rEncClass) %>%
  summarize(n = length(unique(bandNumber))) %>%
  group_by(spp) %>%
  mutate(tEncounters = sum(n)) %>%
  ungroup %>%
  mutate(prop = n/tEncounters*100,spp = str_replace_all(spp, ' ', '\n')) %>%
  ggplot(aes(x = spp, y = prop, fill = rEncClass)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(0, 100.1), expand = c(0, 0)) +
  xlab('Species') +
  ylab('Proportion of birds') +
  theme_add() +
  scale_fill_manual(
    values = c('gray60', '#E62A10','#F7DC6F','#6C3483','#10E6E0', '#0B22A0', '#E310E6', '#E67110'),
    name = 'Re-encounter type'
  ) + coord_flip()

ggsave('prop_captures_reencountersByType.png', width = 11, height = 7)


# Proportion of encounters by species and type, re-encounters only:

encounters %>%
  group_by(spp, rEncClass) %>%
  summarize(n = length(unique(bandNumber))) %>%
  filter(rEncClass != 'None') %>%
  group_by(spp) %>%
  mutate(tEncounters = sum(n)) %>%
  ungroup %>%
  mutate(prop = n/tEncounters*100,spp = str_replace_all(spp, ' ', '\n')) %>%
  ggplot(aes(x = spp, y = prop, fill = rEncClass)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(0, 100.1), expand = c(0, 0)) +
  xlab('Species') +
  ylab('Proportion of birds') +
  theme_add() +
  scale_fill_manual(
    values = c('#E62A10','#F7DC6F','#6C3483','#10E6E0', '#0B22A0', '#E310E6', '#E67110'),
    name = 'Re-encounter type'
  ) +
  coord_flip()

ggsave('prop_reEncountersByType.png', width = 11, height = 7)


  



encounterSumByRegion_withCrossover <- encounters %>%
  group_by(region) %>%
  mutate(nBanded =  length(unique(bandNumber))) %>%
  group_by(region, rEncClass) %>%
  summarize(nBanded = unique(nBanded),
            n = length(unique(bandNumber))) %>%
  ungroup() %>%
  mutate(
    prop = n / nBanded * 100,
    rEncClass = factor(
      rEncClass,
      levels = c(
        'no re-encounter',
        'recap only',
        'rsTech only',
        'rsPart only',
        'recap and rsTech',
        'recap and rsPart',
        'rsPart and rsTech',
        'all classes'
      ),
      labels = c(
        'None',
        'Recap',
        'Technician resight',
        'Participant resight',
        'Recap and technician resight',
        'Recap and participant resight',
        'Technician and participant resight',
        'All'
      )
    )
  ) 




rEnc_colors <- c('gray60', '#E62A10','#F7DC6F','#6C3483','#10E6E0', '#0B22A0', '#E310E6', '#E67110')



# Across encounter types, sum:

encounterSumByRegion_withCrossover %>%
  ggplot(aes(x = region, y  = n, fill = rEncClass)) +
  geom_bar(stat = 'identity', color = 'black', size = 1) +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2))) +
  scale_y_continuous(limits = c(0, 15000), expand = c(0, 0)) +
  xlab('Region') +
  ylab('Number of birds encountered') +
  scale_fill_manual(
    values = c('gray60', '#E62A10','#F7DC6F','#6C3483','#10E6E0', '#0B22A0', '#E310E6', '#E67110'),
    name = 'Re-encounter type'
  ) +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2)))

# Across encounter types, percentage:

encounterSumByRegion_withCrossover %>%
  filter(region != 'Colorado') %>%
  ggplot(aes(x = region, y  = prop, fill = rEncClass)) +
  geom_bar(stat = 'identity', color = 'black', size = 1) +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2))) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  xlab('Region') +
  ylab('Percent of birds re-encountered') +
  scale_fill_manual(
    values = c('gray60', '#E62A10','#F7DC6F','#6C3483','#10E6E0', '#0B22A0', '#E310E6', '#E67110'),
    name = 'Re-encounter type'
  ) +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2)))

# Re-encountered, sum with none, and any classes

encounters_twoClasses <- encounterSumByRegion_withCrossover %>%
  mutate(rEncClass = as.character(rEncClass),
         rEncClass = ifelse(rEncClass == 'None', 'Not re-encountered', 'Re-encountered')) %>%
  group_by(region, rEncClass) %>%
  summarize(
    n = sum(n),
    prop = sum(prop)
  )

# Number: 

encounters_twoClasses %>%
  ggplot(aes(x = region, y  = n, fill = rEncClass)) +
  geom_bar(stat = 'identity', color = 'black', size = 1) +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2))) +
  scale_y_continuous(limits = c(0, 15000), expand = c(0, 0)) +
  xlab('Region') +
  ylab('Number of birds encountered') +
  scale_fill_manual(
    values = c('gray60', '#E67110'),
    name = ''
  ) +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2)))

# Percentage:

encounters_twoClasses %>%
  ggplot(aes(x = region, y  = prop, fill = rEncClass)) +
  geom_bar(stat = 'identity', color = 'black', size = 1) +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2))) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  xlab('Region') +
  ylab('Proportion of birds encountered') +
  scale_fill_manual(
    values = c('gray60', '#E67110'),
    name = ''
  ) +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2)))

# Across encounter types, percentage:

encounterSumByRegion_withCrossover %>%
  filter(region != 'Colorado') %>%
  ggplot(aes(x = region, y  = prop, fill = rEncClass)) +
  geom_bar(stat = 'identity', color = 'black', size = 1) +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2))) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  xlab('Region') +
  ylab('Percent of birds re-encountered') +
  scale_fill_manual(
    values = c('gray60', '#E62A10','#F7DC6F','#6C3483','#10E6E0', '#0B22A0', '#E310E6', '#E67110'),
    name = 'Re-encounter type'
  ) +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2)))

# Across encounter types, just ReE:

encounterSumByRegion_withCrossover %>%
  filter(region != 'Colorado') %>%
  group_by(region) %>%
  mutate(nReEncountered = sum(n[rEncClass != 'None'])) %>%
  ungroup %>%
  filter(rEncClass != 'None') %>%
  ggplot(aes(x = region, y  = n, fill = rEncClass)) +
  geom_bar(stat = 'identity', color = 'black', size = 1) +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2))) +
  scale_y_continuous(limits = c(0, 2500), expand = c(0, 0)) +
  xlab('Region') +
  ylab('Number of birds re-encountered') +
  scale_fill_manual(
    values = c('#E62A10','#F7DC6F','#6C3483','#10E6E0', '#0B22A0', '#E310E6', '#E67110'),
    name = 'Re-encounter type'
  ) +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2)))

# Across encounter types, percentage, just ReE:

encounterSumByRegion_withCrossover %>%
  filter(region != 'Colorado') %>%
  group_by(region) %>%
  mutate(nReEncountered = sum(n[rEncClass != 'None'])) %>%
  ungroup %>%
  filter(rEncClass != 'None') %>%
  mutate(prop = n/nReEncountered*100) %>%
  ggplot(aes(x = region, y  = prop, fill = rEncClass)) +
  geom_bar(stat = 'identity', color = 'black', size = 1) +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2))) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  xlab('Region') +
  ylab('Percent of re-encounters') +
  scale_fill_manual(
    values = c('#E62A10','#F7DC6F','#6C3483','#10E6E0', '#0B22A0', '#E310E6', '#E67110'),
    name = 'Re-encounter type'
  ) +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2)))





#---------------------------------------------------------------------------------*
# Email addition 3, encounters by region, 2017 ----
#---------------------------------------------------------------------------------*

capRecap <- fileList$capture %>%
  filter(year(date) == 2017) %>%
  filter(!siteID %in% removeSites) %>%
  mutate(region = addRegion(siteID)) %>%
  mutate(enc = ifelse(enc == 'B', 'Band', 'Recapture')) %>%
  select(region, enc, bandNumber) %>%
  distinct

resight <- bind_rows(fileList$partRs, fileList$techRs) %>%
  filter(!siteID %in% removeSites, year(date) == 2017) %>%
  mutate(region = addRegion(siteID)) %>%
  transmute(region, enc = 'Resight', bandNumber) %>%
  distinct

basicPlot <- bind_rows(capRecap, resight) %>%
  distinct %>%
  mutate(
    enc = factor(enc, levels = c('Resight', 'Recapture', 'Band')),
    `Encounter type` = enc) %>%
  group_by(region, `Encounter type`) %>%
  summarize(n = n()) %>%
  ungroup %>%
  ggplot(aes(y = n, x = region, fill = `Encounter type`)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(
    values = c('#E62A10','#F7DC6F','#6C3483'),
    name = 'Encounter type'
  ) +
  scale_y_continuous(limits = c(0, 750), breaks = c(0, 250, 500, 750), minor_breaks = seq(0, 750, by = 50), expand = c(0, 0)) +
  xlab('Region') +
  ylab('Number of birds') +
  coord_flip() +
  theme_add() +
  ggtitle('Birds encountered in 2017')

basicPlot +
  theme(
    legend.title = element_text(size = rel(1.5), face = "bold"),
    legend.text = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(1.8), margin = margin(t = 10, b = 5)),
    axis.text = element_text(size = rel(1.5)),
    plot.title = element_text(size = 36, margin = margin(
      t = 5,
      b = 20,
      unit = 'pt'
    )),
    panel.grid.minor = element_line(colour="gray60", size=0.5)
  )

ggsave('C:/Users/EvansBr/Downloads/encounters2017.png', width = 11, height = 7)






#=================================================================================*
# ---- summary plots ----
#=================================================================================*

# Encounters ----
#---------------------------------------------------------------------------------*
# Across encounter types and years:

captureSummary %>%
  group_by(region, enc) %>%
  summarize(n = sum(n)) %>%
  ggplot(aes(x = region, y = n, fill = enc)) +
  geom_bar(stat = 'identity', color = 'black') +
  scale_y_continuous(limits = c(0, 15000), expand = c(0, 0)) +
  xlab('Region') +
  ylab('Number of encounters') +
  scale_fill_manual(
    values = c('#E68510', '#0B22A0'),
    name = 'Encounter type',
    breaks = c('B', 'R'),
    labels = c('Band', 'Recap')
  ) +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2)))

#---------------------------------------------------------------------------------*
# Birds banded only ----
#---------------------------------------------------------------------------------*

# Just number of birds banded, across years:

captureSummary %>%
  filter(enc == 'B') %>%
  group_by(region) %>%
  summarize(n = sum(n)) %>%
  ggplot(aes(x = region, y = n)) +
  geom_bar(stat = 'identity', color = 'black', fill = 'gray80') +
  scale_y_continuous(limits = c(0, 15000), expand = c(0, 0)) +
  xlab('Region') +
  ylab('Number of birds banded') +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2)))

ggsave('bandedAcrossYears.png', width = 10, height = 6)

# Just number of birds banded, 2017:

captureSummary %>%
  filter(enc == 'B', year == 2017) %>%
  ggplot(aes(x = region, y = n)) +
  geom_bar(stat = 'identity', color = 'black', fill = 'gray80') +
  scale_y_continuous(limits = c(0, 500), expand = c(0, 0)) +
  xlab('Region') +
  ylab('Number of birds banded') +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2)))

ggsave('banded2017.png', width = 10, height = 6)

# Birds banded by year:

captureSummary %>%
  filter(enc == 'B') %>%
  group_by(year) %>%
  summarize(n = sum(n)) %>%
  ggplot(aes(x = year, y = n)) +
  geom_bar(stat = 'identity', color = 'black', fill = 'gray80') +
  scale_y_continuous(limits = c(0, 5000), expand = c(0, 0)) +
  xlab('Year') +
  ylab('Number of birds banded') +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2)))

# Birds banded by year and region:

captureSummary %>%
  filter(enc == 'B') %>%
  ggplot(aes(x = year, y = n, fill = region)) +
  geom_bar(stat = 'identity', color = 'black') +
  scale_y_continuous(limits = c(0, 5000), expand = c(0, 0)) +
  scale_fill_manual(
    values = c('#E62A10','#F7DC6F','#6C3483','#10E6E0', '#0B22A0', '#E310E6', '#E67110'),
    name = 'Encounter type'
  ) +
  xlab('Year') +
  ylab('Number of birds banded') +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2)))

ggsave('bandedByYear.png', width = 10, height = 6)

#---------------------------------------------------------------------------------*
# Recaptures only ----
#---------------------------------------------------------------------------------*

# Just number of birds recaptured, across years:

captureSummary %>%
  filter(enc == 'R') %>%
  group_by(region) %>%
  summarize(n = sum(n)) %>%
  ggplot(aes(x = region, y = n)) +
  geom_bar(stat = 'identity', color = 'black', fill = 'gray80') +
  scale_y_continuous(limits = c(0, 1000), expand = c(0, 0)) +
  xlab('Region') +
  ylab('Number of birds recaptured') +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2)))

ggsave('recapturedAcrossYears.png', width = 10, height = 6)

# Just number of birds recaptured, 2017:

captureSummary %>%
  filter(enc == 'R', year == 2017) %>%
  ggplot(aes(x = region, y = n)) +
  geom_bar(stat = 'identity', color = 'black', fill = 'gray80') +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  xlab('Region') +
  ylab('Number of birds recaptured') +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2)))

ggsave('recaptured2017.png', width = 10, height = 6)

# Recaptured by region and year

captureSummary %>%
  filter(enc == 'R') %>%
  ggplot(aes(x = year, y = n, fill = region)) +
  geom_bar(stat = 'identity', color = 'black') +
  scale_y_continuous(limits = c(0, 400), expand = c(0, 0)) +
  scale_fill_manual(
    values = c('#E62A10','#F7DC6F','#6C3483','#10E6E0', '#0B22A0', '#E310E6', '#E67110'),
    name = 'Region'
  ) +
  xlab('Year') +
  ylab('Number of birds recaptured') +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2)))

ggsave('recapturedRegionYear.png', width = 10, height = 6)

#---------------------------------------------------------------------------------*
# Resights ----
#---------------------------------------------------------------------------------*

# Resights across years:

resightSummary %>%
  group_by(region, type) %>%
  summarize(n = sum(n)) %>%
  ggplot(aes(x = region, y = n, fill = type)) +
  geom_bar(stat = 'identity', color = 'black') +
  scale_y_continuous(limits = c(0, 2500), expand = c(0, 0)) +
  xlab('Region') +
  ylab('Number of resighted birds') +
  scale_fill_manual(
    values = c('#E68510', '#0B22A0'),
    name = 'Resight type'
  ) +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2)))

ggsave('resighted.png', width = 10, height = 6)

# Resights, 2017:

resightSummary %>%
  filter(year == 2017) %>%
  ggplot(aes(x = region, y = n, fill = type)) +
  geom_bar(stat = 'identity', color = 'black') +
  scale_y_continuous(limits = c(0, 300), expand = c(0, 0)) +
  xlab('Region') +
  ylab('Number of resighted birds') +
  scale_fill_manual(
    values = c('#E68510', '#0B22A0'),
    name = 'Resight type'
  ) +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2)))

ggsave('resighted2017.png', width = 10, height = 6)

# Resights by region and year:

resightSummary %>%
  ggplot(aes(x = year, y = n, fill = region)) +
  facet_wrap(~type) +
  geom_bar(stat = 'identity', color = 'black') +
  scale_y_continuous(limits = c(0, 750), expand = c(0, 0)) +
  scale_fill_manual(
    values = c('#E62A10','#F7DC6F','#6C3483','#10E6E0', '#0B22A0', '#E310E6', '#E67110'),
    name = 'Region'
  ) +
  xlab('Year') +
  ylab('Number of birds resighted') +
  theme_add() +
  theme(axis.text = element_text(size = rel(1.2)))

ggsave('resightedByYear.png', width = 10, height = 6)


