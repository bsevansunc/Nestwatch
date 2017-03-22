#---------------------------------------------------------------------------------*
# ---- SITE ID TABLE ----
#---------------------------------------------------------------------------------*

siteIdTable <- read.csv('startData/siteIdTable.csv') %>%
  tbl_df

fieldCodesSiteId <- names(siteIdTable)

fieldNamesSiteId <- c('siteID', 'Region')

# Choices of NN regions:

choiceRegions <- c('noData','Atlanta','Colorado', 'DC', 'Gainesville', 'Pittsburgh',
                   'Raleigh', 'Springfield')

names(choiceRegions) <- choiceRegions

#---------------------------------------------------------------------------------*
# ---- CONTACT TABLE ----
#---------------------------------------------------------------------------------*

fieldCodesContactInfo <- readTbl('startData/contactNames.csv') %>%
  # mutate(names = str_replace_all(names, 'Contact', '')) %>%
  .$names

blankFieldsContactInfo <- fieldCodesContactInfo[-1]

fieldNamesContactInfo <- fieldNamesSite <- c(
  'siteID', 'School or group name', 'Last name', 'First name', 'Primary phone',
  'Secondary phone', 'Email', 'Contact notes')

#---------------------------------------------------------------------------------*
# ---- ADDRESS TABLE ----
#---------------------------------------------------------------------------------*

fieldCodesAddress <- read.csv('startData/addressNames.csv')$names

blankFieldsAddress <- fieldCodesAddress[-1]

fieldNamesAddress <- fieldNamesSite <- c(
  'siteID', 'House number', 'Street', 'City', 'State',
  'Zip code', 'Location notes')
  
#---------------------------------------------------------------------------------*
# ---- LOCATION TABLE ----
#---------------------------------------------------------------------------------*

fieldCodesLocation <- read.csv('startData/locationNames.csv')$names

blankFieldsLocation <- fieldCodesLocation[-c(1,2)]

fieldNamesLocation <- c('siteID', 'Date', 'Longitude', 'Latitude', 'Accuracy',
                        'Location method', 'Location notes')

choiceDate <- c('1999-09-09', seq(
  as.Date(ISOdate(2000, 1, 15)),
  as.Date(ISOdate(2030, 1, 1)), 1) %>%
    as.character)

choiceLocationMethod <- c('noData', 'GPS', 'map')

#---------------------------------------------------------------------------------*
# ---- VISIT TABLE ----
#---------------------------------------------------------------------------------*

fieldCodesVisit <- read.csv('startData/visitNames.csv')$names

blankFieldsVisit <- fieldCodesVisit[-c(1,2)]

fieldNamesVisit <- c('siteID', 'Date', 'Observer(s)', 'Participant engagement',
                     'Encountered birds', 'Net hours', 'Visit notes')

choiceNetMinutes <- c(99999, 0:2000)

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

choiceEncounteredBirds <- c('noData', 'Y', 'N')

#---------------------------------------------------------------------------------*
# ---- CAPTURE TABLE ----
#---------------------------------------------------------------------------------*

fieldCodesCapture <- read.csv('startData/captureNames.csv')$names

blankFieldsCapture <- fieldCodesCapture[-c(1,2,4)]

fieldNamesCapture <- c(
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

choiceTimeOfDay <- c('00:01',timeOfDay)

choiceEnc <- c('noData','R', 'B')

choiceSpecies <- c('noData',aouCodes$Alpha)

colorValues <- c('B', 'N', 'K', 'G', 'E', 'O', 'P', 'M', 'R', 'Y', 'W')

choiceColors <- getAllPossibleCombos(colorValues)

choiceAge <- c('noData', 'HY', 'AHY', 'SY', 'ASY', 'U')

choiceSex <- c('noData', 'M', 'F', 'U')

choiceBreedingCond <-  c('noData','CP', 'BP')

choiceFat <- c('noData', 0, 0.5, seq(1:5))

#---------------------------------------------------------------------------------*
# ---- RESIGHT FORAY EFFORT TABLE ----
#---------------------------------------------------------------------------------*

fieldCodesForayEffort <- read.csv('startData/forayEffortNames.csv')$names

blankFieldsForayEffort <- fieldCodesForayEffort[-c(1,2,3)]

fieldNamesForayEffort <- c(
  'siteID', 'Date', 'Obs','Foray #', 'Foray start', 'Foray end', 'Path distance (m)'
)

choicePathDistance <- c(99999, 0:10000)

#---------------------------------------------------------------------------------*
# ---- RESIGHT FORAY COUNT UNBANDED TABLE ----
#---------------------------------------------------------------------------------*

fieldCodesForayCountUnbanded <- read.csv('startData/forayCountUnbandedNames.csv')$names

blankFieldsForayCountUnbanded <- fieldCodesForayCountUnbanded[-c(1,2)]

fieldNamesForayCountUnbanded <- c('siteID', 'Date', 'spp', 'Count')

choiceCountUnbanded <- c('noData',  0:100)

#---------------------------------------------------------------------------------*
# ---- RESIGHT TECHNICIAN TABLE ----
#---------------------------------------------------------------------------------*

fieldCodesTechRs <- read.csv('startData/techRsNames.csv')$names

blankFieldsTechRs <- fieldCodesTechRs[-c(1,2)]

fieldNamesTechRs <- c('siteID', 'Date', 'Obs', 'Time', 'Foray #', 'Band #',
                      'Longitude', 'Latitude', 'rsType', 'Notes')

choiceTypeRs <- c('noData', 'I', 'F', 'P')

#---------------------------------------------------------------------------------*
# ---- POINT COUNTS ----
#---------------------------------------------------------------------------------*

fieldCodesPc <- read.csv('startData/pcNames.csv')$names

blankFieldsPc <- fieldCodesPc[-c(1:5)]

fieldNamesPc <- c(
  'siteID','Date', 'Obs', 'Start time', 'Interval', 'SPP',
  'D0-10', 'D10-20', 'D20-30', 'D30-40', 'D40-50',
  'Detection'
)

choiceInterval <- c('noData', 1:3)

choiceDetection <- c('noData', 'V', 'A', 'B')

# Define field names for point count data table:

# emptyMatrixPc <- matrix(nrow = 0, ncol = length(fieldNamesPc))

# emptyDfPc <- data.frame(emptyMatrixPc)

# names(emptyDfPc) <- fieldNamesPc

# blankFieldsPc <- c('distancePc','countPc', 'detectionPc','notesPc')

