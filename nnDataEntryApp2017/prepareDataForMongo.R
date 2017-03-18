# 'C:/Users/EvansBr/Desktop/nnPowerAnalysis/nnData/'

# Script to generate color combinations.

#===============================================================================*
# ---- SET UP ----
#===============================================================================*

# Smart installer will check list of packages that are installed, install any
# necessary package that is missing, and load the library:

smartInstallAndLoad <- function(packageVector){
  for(i in 1:length(packageVector)){
    package <- packageVector[i]
    if(!package %in% rownames(installed.packages())){
      install.packages(packageVector[i],repos="http://cran.rstudio.com/",
                       dependencies=TRUE)
    }
  }
  lapply(packageVector, library, character.only = TRUE)
}

# Load and potentially install libraries:

smartInstallAndLoad(c('tidyverse', 'stringr', 'lubridate'))

options(stringsAsFactors = FALSE)

siteIdTable <- read.csv('cleanedFiles/siteIdentifierTableWithPitt.csv') %>%
  tbl_df %>%
  select(siteID, region) %>% distinct

siteLocationTable <- read.csv('cleanedFiles/siteLocationTable.csv') %>%
  tbl_df %>%
  filter(siteID != 'PARKFORMA1') %>%
  mutate(
    sitePlotID = str_replace_all(sitePlotID, 'ECOSFPMA1', 'FORESPAMA1'),
    siteID = str_replace_all(sitePlotID, 'ECOSFPMA1', 'FORESPAMA1')
  )



siteLocationTableWeb <- siteLocationTable %>%
  select(-c(sitePlotID, plotID)) %>%
  mutate(accuracy = as.numeric(accuracy)) %>%
  mutate(locationMethod = ifelse(locationNotes == 'locationEstimate',
                                 'locationEstimated', locationMethod),
         locationMethod = ifelse(!is.na(accuracy),
                                 'GPS', locationMethod),
         locationMethod = ifelse(is.na(locationMethod), 'U', locationMethod)
  ) %>%
  mutate(locationNotes = ifelse(locationNotes == 'locationEstimate',
                                'estimated from map',
                                locationNotes)) %>%
  select(siteID:zip, locationMethod, locationNotes) %>%
  distinct %>%
  full_join(siteIdTable, by = 'siteID')


siteIdTableWeb <- siteIdTable %>%
  filter(siteID != 'PARKFORMA1') %>%
  mutate(
    sitePlotID = str_replace_all(siteID, 'ECOSFPMA1', 'FORESPAMA1'),
    siteID = str_replace_all(siteID, 'ECOSFPMA1', 'FORESPAMA1')
  ) %>%
  full_join(siteLocationTableWeb %>%
              select(siteID), by = 'siteID') %>%
  mutate(region = ifelse(siteID == 'FORESPAMA1', 'Springfield', region),
         region = ifelse(siteID == 'ATHEJIMMA2', 'Springfield', region),
         region = ifelse(siteID == 'DELUKARVA3', 'DC', region),
         region = ifelse(siteID == 'HOROBONVA2', 'DC', region),
         region = ifelse(siteID == 'NZPELEPDC1', 'DC', region),
         region = ifelse(siteID == 'MAULSHAFL1', 'Gainesville', region),
         region = ifelse(siteID == 'NAHIWILFL1', 'Gainesville', region),
         region = ifelse(siteID == 'WINTCHARFL1', 'Gainesville', region),
         region = ifelse(siteID == 'HERBMERGA1', 'Atlanta', region),
         region = ifelse(siteID == 'LORYSPKCO1_maintenanceYard', 'CO', region),
         region = ifelse(siteID == 'LORYSPKCO1_wellsGulch', 'CO', region),
         region = ifelse(siteID == 'LORYSPKCO1_maintenanceYard2', 'CO', region),
         region = ifelse(is.na(region), 'DC', region)) %>%
  filter(!is.na(siteID)) %>%
  select(siteID, region)

visitTableStart <- read.csv('cleanedFiles/visitTableWithPitt.csv') %>%
  tbl_df %>%
  mutate(visitID = visitID %>%
           str_replace_all('PARKFORMA1', 'FORESPAMA1') %>%
           str_replace_all('ECOSFPMA1', 'FORESPAMA1')
  ) %>%
  mutate(siteID = siteID %>%
           str_replace_all('PARKFORMA1', 'FORESPAMA1') %>%
           str_replace_all('ECOSFPMA1', 'FORESPAMA1')
  ) %>%
  distinct

visitTableWeb <- visitTableStart %>%
  select(siteID, dateVisit, observerVisit, participantEngagement, encounteredBirds, netHours, notesVisit) %>%
  distinct

captureTableWeb <- read.csv('cleanedFiles/captureTableWithPitt.csv') %>%
  tbl_df() %>%
  mutate(visitID = visitID %>%
           str_replace_all('PARKFORMA1', 'FORESPAMA1') %>%
           str_replace_all('ECOSFPMA1', 'FORESPAMA1')
  ) %>%
  left_join(visitTableStart %>%
              select(visitID, siteID, dateVisit), by = 'visitID') %>%
  select(siteID, dateVisit, birdID, timeEnc:typeCapture, age:sex) %>%
  distinct %>%
  left_join(read.csv('cleanedFiles/birdTableWithPitt.csv'), by = 'birdID') %>%
  select(-bandNumber) %>%
  rename(time = timeEnc, obs = observerCapture, enc = typeCapture, spp  = species, colorL = colorComboL, colorR = colorComboR) %>%
  select(siteID, dateVisit, time:enc, spp, birdID, colorL:colorR, age, sex) %>%
  left_join(read.csv('cleanedFiles/measurementTableWithPitt.csv'), by = 'birdID') %>% 
  select(siteID:colorR, mass:tl, age, sex, breedingCondition, fat) %>%
  rename(bandNumber = birdID, cpBp = breedingCondition) %>%
  distinct %>%
  mutate(bloodID = '', featherID = '', notes = '')

resightEffortTableWeb <- read.csv('cleanedFiles/resightEffortTableWithPitt.csv') %>% tbl_df %>%
  mutate(visitID = visitID %>%
           str_replace_all('PARKFORMA1', 'FORESPAMA1') %>%
           str_replace_all('ECOSFPMA1', 'FORESPAMA1')
  ) %>%
  left_join(visitTableStart %>%
              select(visitID, siteID, dateVisit), by = 'visitID') %>%
  select(siteID, dateVisit, observerRs, rsStart, rsEnd, pathDistance, spUnbanded, countUnbanded)

resightTechTableWeb <- read.csv('cleanedFiles/resightTechTableWithPitt.csv') %>% tbl_df %>%
  mutate(visitID = visitID %>%
           str_replace_all('PARKFORMA1', 'FORESPAMA1') %>%
           str_replace_all('ECOSFPMA1', 'FORESPAMA1')
  ) %>%
  separate(visitID, into = c('siteID', 'date'), sep = '_') %>%
  rename(observer = observerResight, time = timeResight, 
         longitude = longResight, latitude = latResight,
         resightType = resightTypeTech, bandNumber = birdID,
         notes = notesResightTech) %>%
  select(siteID, date, observer, time, bandNumber, longitude, latitude, resightType, notes)

pcTableWeb <- data.frame(
  visitID = rep('REITBOBMD1',3),
  date = rep(Sys.Date(), 3),
  obs = rep('bse', 3),
  startTime = 05:13,
  interval = 1:3,
  spp = 'ABDU',
  d0_10 = c(5, 0, 59),
  d10_20 = c(5, 0, 59),
  d20_30 = c(5, 0, 59),
  d30_40 = c(5, 0, 59),
  d40_50 = c(5, 0, 59),
  detection = c('V', 'A', 'B')
)

# 'C:/Users/EvansBr/Desktop/nnPowerAnalysis/nnData/'


# 'C:/Users/EvansBr/Desktop/gits/Nestwatch/nnDataEntryApp'

write_csv(siteIdTableWeb, 'siteIdTable.csv')

write_csv(siteLocationTableWeb, 'siteLocationTable.csv')

write_csv(visitTableWeb, 'visitTable.csv')

write_csv(captureTableWeb, 'captureTable.csv')

queryTableWeb <- captureTableWeb %>%
  filter(enc == 'B') %>%
  select(siteID, dateVisit, spp, bandNumber, colorL, colorR) %>%
  distinct %>%
  mutate(year = year(dateVisit)) %>%
  select(-dateVisit) %>%
  select(siteID, spp, year, bandNumber, colorL, colorR) %>%
  arrange(siteID, spp, year)

addressContactTableWeb <- siteLocationTable %>%
  select(siteID, houseNumber, street, city, state, zip, locationNotes) %>%
  mutate(siteID = siteID %>%
           str_replace_all('PARKFORMA1', 'FORESPAMA1') %>%
           str_replace_all('ECOSFPMA1', 'FORESPAMA1')
  ) %>%
  left_join(read.csv('contactInfoTable.csv') %>% tbl_df %>%
              mutate(siteID = siteID %>%
                       str_replace_all('PARKFORMA1', 'FORESPAMA1') %>%
                       str_replace_all('ECOSFPMA1', 'FORESPAMA1')
              ) %>%
              select(siteID, groupName, lastName, firstName, phone1, phone2, email1, contactNotes),
            by = 'siteID'
            ) %>% 
  distinct

write_csv(addressContactTableWeb %>%
            select(siteID, houseNumber, street, city, state, zip,locationNotes),
          'addressTable.csv')

write_csv(addressContactTableWeb %>%
            select(siteID, groupName, lastName, firstName, phone1, phone2,email1, contactNotes),
          'contactTable.csv')

write_csv(queryTableWeb, 'queryTable.csv')

write_csv(resightEffortTableWeb, 'resightEffortTable.csv')

write_csv(resightTechTableWeb, 'resightTechTable.csv')

write_csv(pcTableWeb, 'pcTable.csv')

