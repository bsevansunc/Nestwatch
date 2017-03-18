#---------------------------------------------------------------------------------*
# ---- SITE INFO ----
#---------------------------------------------------------------------------------*

fieldCodesSite <- c(
  'hub',	'site',	'lastNameOrSchoolSite',	'firstNameSite',	'longSite',
  'latSite', 'accuracySite',	'addressSite','startYearSite','locationNotes',	
  'phone1Site','phone2Site','email1Site',	'email2Site',	'notesSite'
)

fieldNamesSite <- c(
  'Hub', 'Site', 'Last name or school', 'First name', 'Longitude',
  'Latitude','Accuracy', 'Address', 'Start year','Location notes', 'Primary phone',
  'Secondary phone', 'Primary email', 'Secondary email', 'Notes'
)

#---------------------------------------------------------------------------------*
# ---- VISIT ----
#---------------------------------------------------------------------------------*

# Define fields for visit data:

fieldCodesVisit <- c('hub','site','date',	'observer',	'engagement',
                     'encounteredBirds',	'notesVisit',	'netHours',
                     'rsStart',	'rsEnd',	'rsTime',	'pathDistance',
                     'spUnbanded',	'countUnbanded'	)

fieldNamesVisit <- c('hub','site','date',	'observer',	'engagement',	'encounteredBirds',	'notesVisit',	'netHours',	'rsStart',	'rsEnd',	'rsTime',	'pathDistance',	'spUnbanded',	'countUnbanded'	)

emptyMatrixVisit <- matrix(nrow = 0, ncol = length(fieldNamesVisit))

emptyDfVisit <- data.frame(emptyMatrixVisit)

names(emptyDfVisit) <- fieldNamesVisit


blankFieldsVisit <- c('spUnbanded', 'countUnbanded')


# Visit choices

choiceRegions <- c('','Atlanta', 'DC', 'Gainesville', 'Pittsburgh',
                   'Raleigh', 'Springfield')

names(choiceRegions) <- choiceRegions

# choiceSites <- c('', encounters$site %>% unique %>% sort)

choiceDate <- c('', seq(
  as.Date(ISOdate(2000, 1, 15)),
  as.Date(ISOdate(2030, 1, 1)), 1) %>%
    as.character)

timeOfDay0 <- format(seq(ISOdate(2000, 1, 1), ISOdate(2000,1,2), 
                         by = 'min'), '%H:%M') %>% 
  unique %>% sort %>% as.character

timeOfDay <- timeOfDay0[301:1321]

choiceTimeOfDay <- c('',timeOfDay)

choiceSpecies <- c('', justAlphaCode)# 'AMRO', 'BCCH', 'BRTH', 'CACH', 'CARW', 
#'EAPH','GRCA','HOWR','NOCA','NOMO','SOSP',
#'TUTI','UNCH')

colorValues <- c('', 'A', 'BU', 'BK', 'G', 'O','PK', 'P','R', 'Y', 'W')

choiceNetCount <- c('', seq(0, 12, by = 1))

choiceNetMinutes <- c('', 0:2000)

choiceCount <- c('', 1:100)

#---------------------------------------------------------------------------------*
# ---- ENCOUNTERS ----
#---------------------------------------------------------------------------------*

# Define fields for encounter data:

# hubEnc  siteEnc	dateEnc	timeEnc	observerEnc	encounterType	speciesEnc	bandNumber	colorCombo	age	sex	breedingCond	fat	mass	wing	tl	tarsus	featherID	toenailID	bloodID	fecalID	attachmentID	rsLong	rsLat	notesEnc	

fieldCodesEnc <-  c('hub', 'site', 'date', 'timeEnc', 
                    'observerEnc','encounterType', 'speciesEnc', 
                    'bandNumber','colorCombo', 'age', 'sex', 
                    'breedingCond','fat', 'mass', 'wing', 'tl',
                    'tarsus','featherID', 'toenailID', 'bloodID',
                    'fecalID', 'attachmentID', 'rsLong', 'rsLat', 'notesEnc')

# Define field names for encounter data table:

fieldNamesEnc <- c('Hub', 'Site', 'Date', 'Time', 
                   'Obs.', 'Encounter','SPP',
                   'Band #', 'Color c.', 'Age', 'Sex',
                   'CP/BP','Fat', 'Mass', 'Wing', 'Tl',
                   'Tars', 'Feather','Toenail','Blood',
                   'Fecal', 'Attachment', 'rsLong', 'rsLat', 'Notes')

emptyMatrixEnc <- matrix(nrow = 0, ncol = length(fieldNamesEnc))

emptyDfEnc <- data.frame(emptyMatrixEnc)

names(emptyDfEnc) <- fieldCodesEnc

# Define fields for encounter data that will be blank between records:

blankFieldsEnc <- c('timeEnc', 'encounterType', 'speciesEnc', 
                    'bandNumber','colorCombo', 'age', 'sex', 
                    'breedingCond','fat', 'mass', 'wing', 'tl',
                    'tarsus','featherID', 'toenailID', 'bloodID',
                    'fecalID', 'attachmentID', 'rsLong', 'rsLat', 'notesEnc')

# Band choices:

choiceAge <- c('', 'HY', 'AHY', 'SY', 'ASY', 'UNK')

choiceEncounterType <- c('','Band', 'Recap',
                         'Resight-incidental','Resight-targeted',
                         'Resight-participant', 'Resight-neighbor')

choiceSex <- c('', 'M', 'F', 'UNK')

choiceBreedingCond <-  c('','CP', 'BP','CP-', 'BP-','CP+', 'BP+')

choiceFat <- c('', 0, 0.5, seq(1:5))

choiceDistance <- c('', '0-10', '11-20', '21-30', '31-40', '41-50')

choiceTime <- c('', 3, 2, 5)

#---------------------------------------------------------------------------------*
# ---- Query ----
#---------------------------------------------------------------------------------*

#---------------------------------------------------------------------------------*
# ---- POINT COUNTS ----
#---------------------------------------------------------------------------------*

# Define fields for point count data:

fieldCodesPc <- c('hub', 'site', 'observerPc', 'date',
                  'startTimePc', 'timePc', 'speciesPc', 'distancePc',
                  'countPc', 'detectionPc','notesPc')

# Define field names for point count data table:

fieldNamesPc <- c('Hub', 'Site', 'Observer', 'Date', 'Start time', 
                  'Time interval', 'SPP', 'Distance', 'Count', 
                  'Detection', 'Notes')


emptyMatrixPc <- matrix(nrow = 0, ncol = length(fieldNamesPc))

emptyDfPc <- data.frame(emptyMatrixPc)

names(emptyDfPc) <- fieldNamesPc

# Define fields for point count data that WILL be blank between records:

blankFieldsPc <- c('distancePc','countPc', 'detectionPc','notesPc')

#---------------------------------------------------------------------------------*
# ---- NESTS ----
#---------------------------------------------------------------------------------*

fieldCodesNestSummary <- c('hub', 'site', 'speciesNest', 
                           'colorComboMale', 'colorComboFemale',
                           'fwsMale', 'fwsFemale', 'nestID',
                           'plotNest','fateNest', 'dateClutchNest',
                           'plantSpNest','heightNest', 'dateHatchNest',
                           'descriptionNest', 'dateFledgeFailNest',
                           'locationNest', 'nestFateExplanation')

fieldNamesNestSummary <- c('Hub', 'Site', 'Species',
                           'Color combo, male', 'Color combo, female',
                           'FWS bands, male', 'FWS bands, female', 'Nest yr-#',
                           'Plot','Fate', 'Clutch date',
                           'Plant species','Height', 'Hatch date',
                           'Description', 'Fledge/fail date',
                           'Location', 'Explanation of other fate')

emptyMatrixNestSummary <- matrix(nrow = 0, ncol = length(fieldNamesNestSummary))

emptyDfNestSummary <- data.frame(emptyMatrixNestSummary)

names(emptyDfNestSummary) <- fieldCodesNestSummary

blankFieldsNestSummary <- c('plotNest','fateNest', 'dateClutchNest',
                            'plantSpNest','heightNest', 'dateHatchNest',
                            'descriptionNest', 'dateFledgeFailNest',
                            'locationNest', 'nestFateExplanation')

# Define fields for nest data:

fieldCodesNest <- c('hub', 'site', 'nestID', 'speciesNest',
                    'dateNest', 'timeNest', 'stageNest', 'adAttNest',
                    'nEggNest', 'nYoungNest', 'notesNest',
                    'observerNest')

# Define field names for nest data table:

fieldNamesNest <- c('Hub', 'Site', 'Nest ID', 'SPP',
                    'Date', 'Time', 'Stage', 'adAtt',
                    'nEgg', 'nYoung', 'Notes', 'Obs')


emptyMatrixNest <- matrix(nrow = 0, ncol = length(fieldNamesNest))

emptyDfNest <- data.frame(emptyMatrixNest)

names(emptyDfNest) <- fieldNamesNest

# Define fields for nest data that will be blank between records:

blankFieldsNest <- c('dateNest', 'timeNest', 'stageNest', 'adAttNest',
                     'nEggNest', 'nYoungNest', 'notesNest',
                     'observerNest')

# Nest choices:

nestLocationChoices <- c('', 'Nestbox', 'Shrub', 'Tree', 'Other')

nestFateChoices <- c('', 'Successful', 'Successful but parasitized', 
                     'Failed: Predated', 
                     'Failed: Starvation',
                     'Failed: Human activity related', 
                     'Failed: Weather related ',
                     'Failed: Parasitized',
                     'Failed: Unknown',
                     'Failed: Other')

nestStageChoices <- c('', 'B', 'L', 'I', 'H', 'N', 'F', 'P', 'A')

nestAttendChoices <- c('', '-', 'F', 'M', 'F+M')

nestEggsYoungChoices <- c('', 0:10)


