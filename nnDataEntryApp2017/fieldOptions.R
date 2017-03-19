#---------------------------------------------------------------------------------*
# ---- SITE ID TABLE ----
#---------------------------------------------------------------------------------*

siteIdTable <- read.csv('startData/siteIdTable.csv') %>%
  tbl_df

fieldCodesSite <- names(siteIdTable)

fieldNamesSite <- c('siteID', 'Region')

# Choices of NN regions:

choiceRegions <- c('noData','Atlanta','Colorado', 'DC', 'Gainesville', 'Pittsburgh',
                   'Raleigh', 'Springfield')

names(choiceRegions) <- choiceRegions

#---------------------------------------------------------------------------------*
# ---- CONTACT TABLE ----
#---------------------------------------------------------------------------------*

fieldCodesContactInfo <- read.csv('startData/contactTable.csv') %>% names

fieldNamesContactInfo <- fieldNamesSite <- c(
  'siteID', 'School or group name', 'Last name', 'First name', 'Primary phone',
  'Secondary phone', 'Email', 'Contact notes')

#---------------------------------------------------------------------------------*
# ---- ADDRESS TABLE ----
#---------------------------------------------------------------------------------*

fieldCodesAddress <- read.csv('startData/addressTable.csv') %>% names

fieldNamesAddress <- fieldNamesSite <- c(
  'siteID', 'House number', 'Street', 'City', 'State',
  'Zip code', 'Location notes')
  
#---------------------------------------------------------------------------------*
# ---- LOCATION TABLE ----
#---------------------------------------------------------------------------------*

fieldCodesLocation <- read.csv('startData/siteLocationTable.csv') %>% names

fieldNamesLocation <- c('siteID', 'Date', 'Longitude', 'Latitude', 'Accuracy',
                        'Location method', 'Location notes')

choiceDate <- c('noData', seq(
  as.Date(ISOdate(2000, 1, 15)),
  as.Date(ISOdate(2030, 1, 1)), 1) %>%
    as.character)

choiceLocationMethod <- c('noData', 'GPS', 'map')

#---------------------------------------------------------------------------------*
# ---- VISIT TABLE ----
#---------------------------------------------------------------------------------*

fieldCodesVisit <- read.csv('startData/visitTable.csv') %>% names

fieldNamesVisit <- c('siteID', 'Date', 'Observer(s)', 'Participant engagement',
                     'Encountered birds', 'Net hours', 'Visit notes')

choiceNetMinutes <- c('noData', 0:2000)

choiceParticipantEngagement <- c('noData', '-', 0:5)

names(choiceParticipantEngagement) <- c(
  'noData',
  'No visit was scheduled',
  'Participant should be avoided in the future',
  'Participant was not present or showed no interest during visit activities',
  'Participant showed only slight interest during visit activities',
  'Participant contributed during one of the visit activities (e.g., banding)',
  'Participant contributed during two-three of visit activities (e.g., banging and resight foray)',
  'Participant contributed during all visit activities'
)

choiceEncounteredBirds <- c('noData', 'Yes', 'No')

#---------------------------------------------------------------------------------*
# ---- CAPTURE TABLE ----
#---------------------------------------------------------------------------------*

fieldCodesCaptureTable <- read.csv('startData/captureTable.csv') %>% names

fieldNamesCaptureTable <- c(
  'siteID', 'Date', 'Time', 'Obs','ENC', 'SPP', 'Band #',
  'Color combo', 'Mass','Wing', 'Tail', 'Age', 'Sex', 'CP/BP', 'FAT',
  'Blood ID', 'Feather ID', 'Notes'
)

# Field choices:

timeOfDay <- data.frame(
  time = format(seq(ISOdate(2000, 1, 1), ISOdate(2000,1,2), by = 'min'), '%H:%M')) %>% 
  distinct %>%
  arrange(time) %>%
  slice(301:1321) %>%
  .$time

choiceTimeOfDay <- c('noData',timeOfDay)

choiceEnc <- c('noData','R', 'B')

choiceSpecies <- c('noData',aouCodes$Alpha)

colorValues <- c('B', 'N', 'K', 'G', 'E', 'O', 'P', 'M', 'R', 'Y', 'W')

choiceColors <- getAllPossibleCombos(colorValues)

choiceAge <- c('noData', 'HY', 'AHY', 'SY', 'ASY', 'UNK')

choiceSex <- c('noData', 'M', 'F', 'UNK')

choiceBreedingCond <-  c('noData','CP', 'BP')

choiceFat <- c('noData', 0, 0.5, seq(1:5))

#---------------------------------------------------------------------------------*
# ---- RESIGHT FORAY TABLE ----
#---------------------------------------------------------------------------------*

fieldCodesResightEffort <- read.csv('startData/resightEffortTable.csv') %>% names

fieldNamesResightEffort <- c(
  'siteID', 'Date', 'Obs', 'Foray start', 'Foray end', 'Path distance (m)',
  'spp unbanded', 'Count unbanded'
)

choicePathDistance <- c('noData', 0:10000)

choiceCountUnbaded <- c('noData',  0:100)

#---------------------------------------------------------------------------------*
# ---- POINT COUNTS ----
#---------------------------------------------------------------------------------*

fieldCodesPointCount <- read.csv('startData/pcTable.csv') %>%
  rename(siteID = visitID) %>% names

fieldNamesPointCount <- c(
  'siteID','Date', 'Obs', 'Start time', 'Interval', 'SPP',
  'D0-10', 'D10-20', 'D20-30', 'D30-40', 'D40-50'
)

choiceInterval <- c('noData', 1:3)

choiceDetection <- c('noData', 'V', 'A', 'B')

# Define field names for point count data table:

# emptyMatrixPc <- matrix(nrow = 0, ncol = length(fieldNamesPc))

# emptyDfPc <- data.frame(emptyMatrixPc)

# names(emptyDfPc) <- fieldNamesPc

# blankFieldsPc <- c('distancePc','countPc', 'detectionPc','notesPc')

