#=================================================================================*
# ---- SET-UP ----
#=================================================================================*
# setwd('C:/Users/Brian/Dropbox/nnDataEntryApp')
# setwd('/Users/bsevans/Dropbox/nnDataEntryApp')
library(tidyverse)
library(mongolite)

select <- dplyr::select

# Convert mongo data frame to a dplyr table dataframe:

mongoToTblDf <- function(data){
  if(nrow(data) > 0 & ncol(data) > 0){
    for(j in 1:ncol(data)){
      data[,j] <- as.character(data[,j])
    }
    data <- data %>% tbl_df
  }
  data
}

# Set url for mongo:

mongoURL <- 'mongodb://bsevans:33shazam@ds025232.mlab.com:25232/nndataentry'

siteMongo <- mongo('site_data', url = mongoURL)

# Function to turn blanks into NA:

makeNA <- function(data){
  ifelse(data == ''|data == 'FILL'|data == 'noData'|data == 'nodata'|data == 'NULL',
         NA, data)
}

# Function to convert degMin to decimal degrees:

replace_dm_with_dd <- function(longLatVector){
  llMat <- str_split_fixed(longLatVector, ' ',2)
  as.numeric(llMat[,1]) + as.numeric(llMat[,2])/60
}

# Function to remove string across columns:

str_remove <- function(string, pattern){
  str_replace_all(string, pattern, '')
}

#=================================================================================*
# ---- GET DATA ----
#=================================================================================*

siteTable <- siteMongo$find(
  fields = '{"_row" : 0, "_id" : 0}') %>%
  mongoToTblDf %>%
  mutate_each(funs(makeNA))

visitMongo <- mongo('visit_data', url = mongoURL)

visitTable <- visitMongo$find(
  fields = '{"_row" : 0, "_id" : 0}'
  ) %>%
  mongoToTblDf %>%
  mutate_each(funs(makeNA))

encMongo <- mongo('encounter_data', url = mongoURL)

encTable <- encMongo$find(
  fields = '{"_row" : 0, "_id" : 0}') %>%
  mongoToTblDf %>%
  mutate_each(funs(makeNA))

pcMongo <- mongo('pointCount_data', url = mongoURL)

pcTable <- pcMongo$find(
  fields = '{"_row" : 0, "_id" : 0}') %>%
  mongoToTblDf %>%
  mutate_each(funs(makeNA))
  
# nestSummaryMongo <- mongo('nest_summary_data', url = mongoURL)
# 
# nestSummaryTable <- nestSummaryMongo$find(
#   fields = '{"_row" : 0, "_id" : 0}') %>%
#   mongoToTblDf %>%
#   mutate_each(funs(makeNA))
# 
# nestMongo <- mongo('nest_observation_data', url = mongoURL)
# 
# nestTable <- nestMongo$find(
#   fields = '{"_row" : 0, "_id" : 0}') %>%
#   mongoToTblDf

write.csv(siteTable, 'siteTable.csv', row.names = FALSE)

write.csv(visitTable, 'visitTable.csv', row.names = FALSE)

write.csv(encTable, 'encTable.csv', row.names = FALSE)

write.csv(pcTable, 'pcTable.csv', row.names = FALSE)

siteTable <- read.csv('siteTable.csv', stringsAsFactors = FALSE) %>%
  tbl_df

visitTable <- read.csv('visitTable.csv', stringsAsFactors = FALSE) %>%
  tbl_df

encTable <- read.csv('encTable.csv', stringsAsFactors = FALSE) %>%
  tbl_df

pcTable <- read.csv('pcTable.csv', stringsAsFactors = FALSE) %>%
  tbl_df

###

vt <- visitTable %>%
  select(hub, site, date, observer, engagement, encounteredBirds, netHours) %>%
  distinct %>%
  mutate(encounteredBirds = tolower(encounteredBirds) %>%
           trimws %>%
           str_replace('yes', 'Y') %>%
           str_replace('1', 'Y') %>%
           str_replace('no', 'N'),
         netHours = suppressWarnings(as.numeric(netHours)) %>%
           round(digits = 2),
         engagement = suppressWarnings(as.numeric(engagement)),
         date = as.Date(date)) %>%
  distinct

vt %>% 
  group_by(hub, observer,date) %>%
  summarize(dates = n()) %>%
  filter(dates > 1) %>%
  ungroup %>%
  left_join(vt, by = c('observer','date')) %>% 
  write.csv('visitCheck.csv', row.names = FALSE)

vtSiteDate <- vt %>%
  select(hub, site, date, encounteredBirds) %>%
  mutate(inVt = 1)

vEnc <- encTable %>%
  filter(encounterType != 'Resight-neighbor',
         encounterType != 'Resight-participant') %>%
  filter(!is.na(encounterType)) %>%
  select(hub, site, date) %>%
  distinct %>%
  mutate(date = as.Date(date)) 

vEnc2016 <- vEnc %>%
  filter(year(date) == 2016) %>%
  mutate(inEnc = 1)

vEncCheck <- vEnc2016 %>%
  full_join(vtSiteDate, by = c('hub','site', 'date')) %>%
  mutate(inVt = ifelse(is.na(inVt), 0, 1))

vTCheck <- vtSiteDate %>%
  full_join(vEnc2016, by = c('hub','site', 'date')) %>%
  mutate(inEnc = ifelse(is.na(inEnc), 0, 1))
  
vEncCheck %>% filter(inVt == 0)

vTCheck %>% filter(inEnc == 0, encounteredBirds == 'Y') %>%
  arrange(hub, site) %>%
  View

vEncCheck %>% 
  filter(inVt == 0) %>% 
  arrange(desc(date)) %>% 
  group_by(hub) %>%
  summarize(count = n())

site1 <- vTCheck %>%
  select(hub, site) %>%
  bind_rows(vEncCheck %>%
              select(hub, site)) %>%
  bind_rows(pcTable %>%
              select(hub, site)) %>%
  bind_rows(siteTable %>%
              select(hub, site)) %>%
  distinct %>%
  mutate(inSite1 = 1) %>%
  arrange(hub, site)

#######
# Modify site names for encounters, point counts, nest records:

str_detectVector <- function(vectorIn, string){
  sum(str_detect(vectorIn, string)) > 0
}

checkOrRenameSiteField <- function(data){
  if(!str_detectVector(names(data), 'siteName')){
    dataOut <- data %>%
      rename(siteName = site)
  } else {
    dataOut <- data
  }
  return(dataOut)
}

#=================================================================================*
# ---- siteIdentifierTable ----
#=================================================================================*

fix_siteName <- function(data){
  # This rename operation must occur across all dataTables
  dataOut <- data %>%
    checkOrRenameSiteField %>%
    mutate(
      siteName = str_replace(siteName, 'COOPMIDGA1', 'COOPEMSGA1'),
      siteName = str_replace(siteName, 'RICHMIDGA1', 'RICHMSGA1'),
      siteName = str_replace(siteName, 'KRICMAUGA1', 'KRIVMAUGA1'),
      siteName = str_replace(siteName, 'NICKELEGA1', 'NICKAESGA1'),
      siteName = str_replace(siteName, 'ADAMSESVA1', 'ADAMELEVA1'),
      siteName = str_replace(siteName, 'CAPITALDC1', 'CAPITCSDC1'),
      siteName = str_replace(siteName, 'LYCEEROMD1', 'LYCEEESMD1'),
#       siteName = str_replace(siteName, 'MUTRCYNVA1', 'MUTRCYNMD1'),
#       siteName = str_replace(siteName, 'MUTRCYNVA2', 'MUTRCYNVA1'),
      siteName = str_replace(siteName, 'NZP-SMBCDC1', 'NZPSMBCDC1'),
      siteName = str_replace(siteName, 'WAPLMILMD1', 'WAPLEESVA1'),
      siteName = str_replace(siteName, 'AKREMIKMA3', 'AKREMICMA2'),
      siteName = str_replace(siteName, 'AKREMIKMA3', 'AKREMICMA2'),
      siteName = str_replace(siteName, 'ARCASUMMA1', 'ARCADIAMA1'),
      siteName = str_replace(siteName, 'BGCHICMA1', 'CHICOBGMA1'),
      siteName = str_replace(siteName, 'BGCHICMA1', 'CHICOBGMA1'),
      siteName = str_replace(siteName, 'CHICBOYMA1', 'CHICOBGMA1'),
      siteName = str_replace(siteName, 'BGCHOLYMA1', 'HOLYOBGMA1'),
      siteName = str_replace(siteName, 'BOYSGIRMA1', 'SPRINBGMA1'),
      siteName = str_replace(siteName, 'CLAYBRYMA1', 'CLARBRYMA1'),
      siteName = str_replace(siteName, 'EMERSONMA1', 'EMERSWPMA1'),
      siteName = str_replace(siteName, 'HOLYMOMA1', 'HOLYOYMMA1'),
      siteName = str_replace(siteName, 'HOLYYMCAMA1', 'HOLYOYMMA1'),
      siteName = str_replace(siteName, 'BGCSPRIMA1', 'SPRIBGCMA1'),
      siteName = str_replace(siteName, 'MMAANDMA1', 'MCMAANDMA1'),
      siteName = str_replace(siteName, 'SPRIBOYMA1', 'SPRINBGMA1'),
      siteName = str_replace(siteName, 'CHILFIRMA1', 'CHILDPSMA1'),
      siteName = str_replace(siteName, 'CLAYBRYMA1', 'CLARBRYMA1'),
      siteName = str_replace(siteName, 'EMERSOWIMA1', 'EMERSWPMA1'),
      siteName = str_replace(siteName, 'EMERSONMA1', 'EMERSWPMA1'),
      siteName = str_replace(siteName, 'FARMNUEMA1', 'NUESTFAMA1'),
      siteName = str_replace(siteName, 'GILLELEMA1', 'GILLESMA1'),
      siteName = str_replace(siteName, 'HERITAGE', 'GIRLSINMA1'),
      siteName = str_replace(siteName, 'HOLYYMOMA1', 'HOLYOYMMA1'),
      siteName = str_replace(siteName, 'HOLYYMCMA1', 'HOLYOYMMA1'),
      siteName = str_replace(siteName, 'GIRLINCMA1', 'GIRLSINMA1'),
      siteName = str_replace(siteName, 'MMAANDMA1', 'MCMAANDMA1'),
      siteName = str_replace(siteName, 'NUESRAIMA1', 'NUESTFAMA1'),
      siteName = str_replace(siteName, 'PELLVOLGA1', 'PELLVOLMA1'),
      siteName = str_replace(siteName, 'RENAISSMA1', 'SPRINRSMA1'),
      siteName = str_replace(siteName, 'SPRIPARMA1', 'ECOSFPMA1'),
      siteName = str_replace(siteName, 'SPRIYMCMA1', 'WEBERCAMA1'),
      siteName = str_replace(siteName, 'STEMSPRMA1', 'STEMMSMA1'),
      siteName = str_replace(siteName, 'WEBBCAMMA1', 'WEBERCAMA1'),
      siteName = str_replace(siteName, 'COCKNICKFL1', 'COCKNICFL1'),
      siteName = str_replace(siteName, 'GRINJENFL2', 'GRINJENFL1'),
      siteName = str_replace(siteName, 'GRINJENFL3', 'GRINJENFL1'),
      siteName = str_replace(siteName, 'GRINJENFL4', 'GRINJENFL1'),
      siteName = str_replace(siteName, 'KNOWMIKFL1', 'KNOXMIKFL1'),
      siteName = str_replace(siteName, 'LAURSARFL1', 'LAUESARFL1'),
      siteName = str_replace(siteName, 'MILLKARLFL1', 'MILLKARFL1'),
      siteName = str_replace(siteName, 'MILLKARLFL', 'MILLKARFL1'),
      siteName = str_replace(siteName, 'LAURSARFL1', 'LAUESARFL1'),
      siteName = str_replace(siteName, 'WALNCRE', 'WALNCRENC1'),
      siteName = str_replace(siteName, 'WALNCRENC1NC1', 'WALNCRENC1'),
      siteName = str_replace(siteName, 'MCCLELEPA1', 'MCCLEESPA1'),
      siteName = str_replace(siteName, 'RiceMicPA1', 'RICEMICPA1'),
      siteName = str_replace(siteName, 'WOODPAUMD1', 'WOODPAUVA1')
    )
  # Tables with date may require additional renaming:
  if(str_detectVector(names(dataOut), 'date')){
    dataOut <- dataOut %>%
      mutate(date = as.Date(date),
             siteName = ifelse(siteName == 'HUNTDAVMD1' & year(date) == 2016,
                               'HUNTDAVMD2', siteName),
             siteName = ifelse(siteName == 'NZPSMBCDC1' & year(date) == 2013,
                               'JUBILHODC1', siteName))
  }
  return(dataOut)
}

additionalSites <- data_frame(
  hub = 'DC',
  site = c('DODGEESMD1','HUNTDAVMD1','JOHNAESVA1','JUBILHODC1','MUTRCYNVA1', 'NZPELEPDC1', 'UNK')
) %>%
  bind_rows(
    data.frame(
      hub = 'Gainesville',
      site = c('NAHIWILFL1', 'PRESHOUFL1')
    )
  ) %>%
  bind_rows(
    data_frame(
      hub = 'Raleigh',
      site = 'WALNCRENC1NC1'
    )
  ) %>%
  bind_rows(
    data_frame(
      hub = 'Atlanta',
      site = c('ALLERILGA1','DECKJASGA2','DULUADVGA1','NICKAESGA1',
               'RICHMIDGA1','SMOKELEGA1','WOODMIDGA1')
    )
  ) %>%
  bind_rows(
    data_frame(
      hub = 'Springfield',
      site = c('BOYSGIRMA1','CHICBOYMA1','CLAYBRYMA1','ELLIBARMA1',
               'EMERSONMA1','GIARORCMA1','HOLYYMOMA1','LEELINDMA1',
               'MCMAANDMA1','NUESRAIMA1','RENAISSMA1','ROBIBEAMA1',
               'ROCHJPMA1','SHEPLYNMA1','SPRIPARMA1','SPRIYMCMA1')
    )
  ) %>%
  fix_siteName

springfieldSiteFixes <- read_csv('springSiteFix.csv') %>%
  mutate(last = stringi::stri_trans_totitle(last),
         first = stringi::stri_trans_totitle(first),
         first = ifelse(first == 'Jp', toupper(first), first),
         last = ifelse(last == 'Renaisance School', 'Springfield Renaissance School',
                       last),
         last = ifelse(last == 'Renaisance School', 'Springfield Renaissance School',
                       last),
         street = str_replace_all(street, '[0-9]','')) %>%
  fix_siteName

siteIdentifierTable <- encTable %>%
  select(hub, site, date) %>%
  bind_rows(pcTable %>%
              select(hub, site, date)) %>%
  bind_rows(visitTable %>%
              select(hub, site, date)) %>%
  distinct %>%
  fix_siteName %>%
  arrange(siteName) %>%
  select(hub, siteName) %>%
  bind_rows(siteTable %>%
              select(hub, site) %>%
              fix_siteName) %>%
  bind_rows(additionalSites) %>%
  bind_rows(springfieldSiteFixes %>%
              transmute(
                siteName = siteName,
                project = 'NN',
                region = 'Springfield'
              )) %>%
  fix_siteName %>%
  transmute(siteID = siteName,
            project = 'NN',
            region = hub
            ) %>%
  filter(!is.na(region)) %>%
  distinct

siteIdentifierTableSiteType <- siteIdentifierTable %>% 
  mutate(siteType = 'R',
         siteType = ifelse(str_detect(siteID, 'NZP'), 'O', siteType),
         siteType = ifelse(str_sub(siteID, 6,7) == 'ES', 'G', siteType),
         siteType = ifelse(str_sub(siteID, 6,7) == 'MS', 'G', siteType),
         siteType = ifelse(str_sub(siteID, 6,7) == 'YM', 'G', siteType),
         siteType = ifelse(str_sub(siteID, 6,7) == 'BG', 'G', siteType),
         siteType = ifelse(siteID == 'AAROJESFL1', 'R', siteType),
         siteType = ifelse(siteID == 'EMERSWPMA1', 'G', siteType),
         siteType = ifelse(siteID == 'CHILDPSMA1', 'G', siteType),
         siteType = ifelse(siteID == 'NUESTFAMA1', 'G', siteType),
         siteType = ifelse(siteID == 'GIRLSINMA1', 'G', siteType),
         siteType = ifelse(siteID == 'SPRINRSMA1', 'G', siteType),
         siteType = ifelse(siteID == 'ECOSFPMA1', 'G', siteType),
         siteType = ifelse(siteID == 'WEBERCAMA1', 'G', siteType),
         siteType = ifelse(siteID == 'ARCADIAMA1', 'G', siteType))

siteIdentifierTableStatus <- siteIdentifierTableSiteType %>%
  left_join(encTable %>%
              fix_siteName %>%
              select(siteName, date) %>%
              rename(siteID = siteName),
            by = 'siteID') %>%
  group_by(siteID, project, region, siteType) %>%
  summarize(lastYear = max(year(date))) %>%
  ungroup %>%
  mutate(
    siteStatus = ifelse(lastYear < 2014, 'D', 'A'),
    siteStatus = ifelse(lastYear < 2016 & lastYear >= 2014 & region != 'Gainesville',
                        'R-O', siteStatus)
    ) %>%
  select(-lastYear)
  
write_csv(siteIdentifierTableStatus, 'siteIdentifierTable.csv')

#=================================================================================*
# ---- siteLocationTable ----
#=================================================================================*

siteLocationsAll <- siteTable %>%
  fix_siteName %>%
  filter(!siteName %in% springfieldSiteFixes$siteName) %>%
  select(siteName, longSite, latSite, accuracySite,
                addressSite, locationNotes) %>%
  bind_rows(springfieldSiteFixes %>%
              select(siteName, long, lat, street) %>%
              transmute(siteName = siteName,
                     longSite = as.character(long),
                     latSite = as.character(lat),
                     accuracySite = NA,
                     addressSite = street,
                     locationNotes = NA)) %>%
  select(siteName, longSite, latSite, accuracySite, locationNotes) %>%
  distinct %>%
  full_join(siteIdentifierTable %>%
              rename(siteName = siteID) %>%
              select(siteName), by = 'siteName') %>%
  select(siteName:locationNotes) %>%
  arrange(siteName) %>%
  mutate(longSite = str_replace(longSite, '\t',''),
         longSite = str_replace(longSite, 'W0',''),
         latSite = str_replace(latSite, '\t','')) %>%
  distinct

siteMultiples <- siteLocationsAll %>%
  group_by(siteName) %>%
  summarize(t = n()) %>%
  filter(t > 1) %>% .$siteName

siteLocationsMultipleEntries <- siteLocationsAll %>%
  filter(siteName %in% siteMultiples) %>%
  filter(!is.na(longSite)) %>%
  distinct %>% 
  slice(-c(2,5,7,9))

siteLocationsCombinedMultiples <- siteLocationsAll %>%
  filter(!siteName %in% siteMultiples) %>%
  bind_rows(siteLocationsMultipleEntries) %>%
  arrange(siteName) %>%
  mutate(longSite = trimws(longSite))

siteLocationsDegMinutes <- siteLocationsCombinedMultiples %>%
  filter(str_detect(.$longSite, '[0-9]{2} ')) %>%
  mutate(longSite = replace_dm_with_dd(longSite),
         latSite = replace_dm_with_dd(latSite))

siteLocationDDfix <- siteLocationsCombinedMultiples %>%
  filter(!siteName %in% siteLocationsDegMinutes$siteName) %>%
  mutate(longSite = abs(as.numeric(longSite))*-1,
         latSite = as.numeric(latSite)) %>%
  bind_rows(siteLocationsDegMinutes) %>%
  arrange(siteName)

# siteLocationAddresses <- siteLocationDDfix %>%
#   select(siteID, siteName, addressSite) %>%
#   mutate(fullAddress = addressSite) %>%
#   separate(addressSite, into = c('streetAddress', 'City', 'StateZip'), sep = ',') %>%
#   separate(streetAddress, into = c('houseNumber', 'street'), sep = ' ')

siteLocationTable <- siteLocationDDfix %>%
  left_join(
    read_csv('addressesNN.csv') %>%
      select(-c(siteID)),
    by = 'siteName'
  ) %>%
  filter(!siteName %in% springfieldSiteFixes$siteName) %>%
  bind_rows(springfieldSiteFixes %>%
              mutate(houseNumber = as.character(houseNumber))) %>%
  select(siteName:accuracySite, houseNumber:zip, locationNotes) %>%
  rename(siteID = siteName, long = longSite, lat = latSite, accuracy = accuracySite)

write_csv(siteLocationTable,'siteLocationTable.csv')

#=================================================================================*
# ---- contactInformationTable ----
#=================================================================================*
  
# Contact information table:

contactInformationTable <- siteIdentifierTable %>%
  left_join(
    siteTable %>%
      select(site, lastNameOrSchoolSite, firstNameSite, phone1Site:notesSite) %>%
      rename(siteName = site, lastNameOrSchool = lastNameOrSchoolSite,
             firstName = firstNameSite, phone1 = phone1Site, phone2 = phone2Site,
             email1 = email1Site, email2 = email2Site,
             contactNotes = notesSite) %>%
      fix_siteName %>%
      rename(siteID = siteName),
    by = 'siteID'
  ) %>%
  distinct

contactInformationTableMultiples <- contactInformationTable %>%
  group_by(siteID) %>%
  summarize(t = n()) %>%
  filter(t > 1) %>% .$siteID
  
contactInformationTableMultipleEntries <- contactInformationTable %>%
  filter(siteID %in% contactInformationTableMultiples) %>% 
  slice(-c(1,3,6,7,9,11))

contactInformationTableCombinedMultiples <- contactInformationTable %>%
  filter(!siteID %in% contactInformationTableMultiples) %>%
  bind_rows(contactInformationTableMultipleEntries) %>%
  arrange(siteID) %>%
  # Trying to get all phone numbers into the same format:
  mutate_all(funs('str_replace_all'),pattern = '\t', replacement = '') %>%
  mutate(phone1 = phone1 %>% 
           str_replace_all('\\.', '-') %>%
           str_replace_all('\\(', '') %>%
           str_replace_all('\\)', '') %>% 
           str_replace_all(' ', '-'),
         phone2 = phone2 %>% 
           str_replace_all('\\.', '-') %>%
           str_replace_all('\\(', '') %>%
           str_replace_all('\\)', '') %>% 
           str_replace_all(' ', '-')
         )

contactInformationTable_splitContact <- contactInformationTableCombinedMultiples %>%
  select(-c(project, region)) %>%
  left_join(siteIdentifierTableSiteType, by = 'siteID') %>%
  mutate(groupName = ifelse(siteType == 'G',lastNameOrSchool, NA)) %>%
  rename(lastName = lastNameOrSchool) %>%
  select(siteID, groupName, lastName:contactNotes, siteType) %>%
  mutate(lastName = ifelse(!is.na(groupName), NA, lastName))
  
write_csv(contactInformationTable_splitContact %>%
            select(-siteType),
          'contactInformationTable.csv')

#=================================================================================*
# ---- contactRecordTable ----
#=================================================================================*

# This table is fake, as we have no data

contactRecordTable <- data.frame(
  siteID = 'REITBOBMD1',
  initialContactDate = '2016-12-01',
  initialContactMode = 'E',
  initialContactNotes = 'This is a fake entry',
  secondContactDate = '2016-12-02',
  secondContactMode = 'P',
  secondContactNotes = 'This is also a fake entry',
  thirdContactDate = '2016-12-03',
  thirdContactMode = 'P',
  thirdContactNotes = 'This is one is a fake entry as well',
  visitScheduled = 0,
  visitScheduledDate = '2016-12-04'
)

write_csv(contactRecordTable, 'contactRecordTable.csv')

#=================================================================================*
# ---- visitTable ----
#=================================================================================*

visitTableOut <- visitTable %>%
  select(site, date) %>%
  fix_siteName %>%
  distinct %>%
  bind_rows(
    encTable %>%
      filter(!encounterType %in% c('Resight-participant', 'Resight-neighbor')) %>% 
      select(site, date) %>%
      fix_siteName %>%
      distinct
  ) %>%
  distinct %>%
  bind_rows(
    pcTable %>%
      select(site, date) %>%
      fix_siteName %>%
      distinct
  ) %>%
  left_join(
    visitTable %>%
      select(site:netHours) %>%
      rename(siteName  = site) %>%
      fix_siteName %>%
      distinct,
    by = c('siteName', 'date')
  ) %>%
  mutate(netHours =round(netHours, 2)) %>%
  distinct %>%
  arrange(date, siteName) %>%
  mutate(visitID = paste(siteName, date, sep = '_')) %>%
  rename(siteID = siteName, observerVisit = observer,
         dateVisit = date,
         participantEngagement = engagement) %>%
  select(visitID,siteID, dateVisit, observerVisit:netHours)

write_csv(visitTableOut, 'visitTable.csv')

#=================================================================================*
# ---- resightEffortTable ----
#=================================================================================*

resightEffortTable <- visitTable %>%
  select(site, date) %>%
  fix_siteName %>%
  distinct %>%
  bind_rows(
    encTable %>%
      filter(!encounterType %in% c('Resight-participant', 'Resight-neighbor')) %>% 
      select(site, date) %>%
      fix_siteName %>%
      distinct
  ) %>%
  distinct %>%
  bind_rows(
    pcTable %>%
      select(site, date) %>%
      fix_siteName %>%
      distinct
  ) %>%
  left_join(
    visitTable %>%
      rename(siteName  = site) %>%
      fix_siteName %>%
      distinct,
    by = c('siteName', 'date')
  ) %>%
  distinct %>%
  arrange(date, siteName) %>%
  fix_siteName %>%
  mutate(visitID = paste(siteName, date, sep = '_')) %>%
  select(visitID, rsStart, rsEnd, pathDistance:countUnbanded)

#=================================================================================*
# ---- encounterTable processed ----
#=================================================================================*

encTable %>%
  # filter(year(as.Date(date)) == 2016) %>%
  filter(encounterType == 'Band') %>%
#   filter(encounterType != 'Band') %>%
#   filter(encounterType != 'Recap') %>%
  # filter(encounterType == 'Recap') %>%
#   filter(encounterType != 'Resight-participant') %>%
#   filter(encounterType != 'Resight-neighbor') %>%
  # filter(encounterType == 'Resight-participant'|encounterType == 'Resight-neighbor') %>%
  # filter(site == 'ROBIRONFL1') %>%
  filter(hub == 'Springfield') %>%
  .$speciesEnc %>% table

# Start by fixing Gainesville 2008

encTable %>% 
  mutate(date = as.Date(date)) %>%
  # filter(year(date) == 2008 & hub == 'Gainesville') %>%
  filter(site == 'BLOOLEEFL1')
#   filter(bandNumber == '1991-18040')
#   filter(encounterType == 'Band') %>%

addOneDayToDate <- function(date){
  day(date) <- day(date) + 1
  as.character(date)
}

addYearsToDate <- function(date, yearsToAdd){
  date <- as.Date(date)
  year(date) <- year(date) + yearsToAdd
  as.character(date)
}

gainesvilleExtract <- encTable %>%
  mutate(date = as.Date(date),
         year = year(date)) %>%
  filter(hub == 'Gainesville') %>%
  select(hub, site, year, date, timeEnc, observerEnc:notesEnc) %>%
  mutate(date = ifelse(month(date) == 1 & day(date) == 1, NA, as.character(date)),
         date = as.Date(date)) %>%
  filter(year != 2005) %>%
  mutate(date = ifelse(year %in% c(2008:2011, 2013:2015), addOneDayToDate(date), as.character(date))) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(date)) %>%
  mutate(
    date = as.character(date),
    date = ifelse(site == 'KENTADAFL1' & year == 2008, '2013-05-13', date),
    date = ifelse(site == 'WILLNAHFL1' & year == 2008, '2012-06-21', date),
    year = year(as.Date(date)),
    date = ifelse(year == 2008, addYearsToDate(date, 4), date),
    date = as.Date(date),
    year = year(date)
    ) %>%
  mutate(
    date = as.character(date),
    date = ifelse(year == 2009, addYearsToDate(date, 4), date),
    year = year(as.Date(date))
  ) %>%
  arrange(year, site) 

gainesvilleExtract %>%
  mutate(date = as.character(date),
         date = ifelse(date == '2010-06-13', '2010-06-23', date), # KENTADAFL1
         site = ifelse(site == 'WINTCHARFL1', 'WINTCHAFL1', site),
         site = ifelse(date == '2010-07-11', 'MILLKARFL1', site),
         date = ifelse(year == 2010, addYearsToDate(date, 4), date),
         date = ifelse(year == 2011, addYearsToDate(date, 4), date),
         date = as.Date(date),
         careful = ifelse(year(date) < 2012, 'carefulBecca', 'checkAnyways'),
         year = year(date)
         ) %>%
  arrange(year, site) %>% View
  write_csv('gainesvilleExtract.csv')
  
#   View
  # filter(date == '2015-04-10') %>% View
  # filter(bandNumber == '2661-07789') %>%
  View


  
  .$encounterType %>% unique
  # filter(!(bandNumber == '2331-09326' & year == 2012)) %>%
  filter(str_detect(site, 'ANDR')) %>%
  filter(!(year %in% c(2012,2103) & is.na(date)))
  filter(str_detect(bandNumber, '07643'))
  # filter(colorCombo == 'P/A,G')


    
           
  # filter(hub == 'Gainesville', year %in% c(2012)) %>%
  filter(bandNumber == '2331-09326')
  # filter(str_detect(site, 'ANDR')) %>%
  # .$date %>% unique

encRaw <- encTable %>%
  mutate(date = as.Date(date)) %>%
  mutate_all(funs('str_replace_all'),pattern = '\t', replacement = '') %>%
  rename(siteName = site) %>%
  fix_siteName %>%
  arrange(date, siteName) %>%
  mutate(visitID = paste(siteName, date, sep = '_'),
         year = year(date),
         observationID = paste(speciesEnc, year(date), sep = '_')) %>%
  group_by(speciesEnc, year) %>%
  mutate(observationID = paste(observationID, 1:n(), sep = '-')) %>%
  ungroup %>%
  select(observationID, visitID, siteName, date, observerEnc:timeEnc)

#=================================================================================*
# ---- birdTable ----
#=================================================================================*

birdTable <- encRaw %>%
  select(speciesEnc, bandNumber, colorCombo, date) %>%
  mutate(year = year(date)) %>%
  # select(-c(date)) %>%
  group_by(speciesEnc, bandNumber, colorCombo) %>%
  summarize(year = min(year), date = min(date)) %>%
  arrange(date)
  group_by(speciesEnc) %>%
  

bandTableRaw <- encRaw %>%
  filter(encounterType == 'Band') %>%
  mutate(year = year(date),
         birdID = paste(speciesEnc, year, sep = '_')) %>%
  group_by(speciesEnc, year) %>%
  mutate(birdID = paste(birdID, 1:n(), sep = '-')) %>%
  ungroup %>%
  mutate(bandYear = year, bandSite = siteName, species = speciesEnc, ageBand = age) %>%
  select(birdID, observationID, visitID, bandYear, bandSite, species, ageBand, sex, bandNumber, colorCombo)

observationTable <- encRaw %>%
  # select(observationID, visitID, encounterType) %>%
  left_join(bandTableRaw %>%
              rename(speciesEnc = species) %>%
              select(speciesEnc, bandNumber, colorCombo, birdID),
            by = c('speciesEnc', 'bandNumber')) %>%
  filter(is.na(birdID)) %>% View


observationTable %>%
  filter(encounterType != 'Band')

observationTable %>%
  filter(is.na(birdID))


filter(encRaw, bandNumber == '2331-09148')

filter(encRaw, bandNumber == '2571-70413')

encRaw %>% filter(year(date) == 2008 & str_sub(siteName, 8,9) == 'FL')

encRaw %>% filter(year(date) == 2008 & str_sub(siteName, 8,9) == 'FL') %>% .$encounterType %>% table

fl2008bn <- encRaw %>% filter(year(date) == 2008 & str_sub(siteName, 8,9) == 'FL') %>% .$bandNumber

encRaw %>% filter(bandNumber %in% fl2008bn) %>%
  arrange(bandNumber) %>%
  View

filter(encRaw, bandNumber == '2571-70407')

filter(encRaw, bandNumber == '2571-70413')



encounterTable <- encRaw %>%
  select(-c(bandNumber, colorCombo)) %>%
  left_join(bandTable, by = 'observationID') %>%
  select(observationID, visitID, timeEnc, observerEnc, 
                encounterType, rsLong, rsLat, notesEnc)

encounterTable %>%
  filter(encounterType != 'Band') %>%
  filter()


#=================================================================================*
# ---- captureTable ----
#=================================================================================*

#=================================================================================*
# ---- measurementTable ----
#=================================================================================*


#=================================================================================*
# ---- biosampleTable ----
#=================================================================================*


#=================================================================================*
# ---- attachmentTable ----
#=================================================================================*


#=================================================================================*
# ---- resightTechnicianTable ----
#=================================================================================*


#=================================================================================*
# ---- resightParticipantTable ----
#=================================================================================*

  
#=================================================================================*
# ---- pointCountTable ----
#=================================================================================*
  
pcRaw <- pcTable %>%
  mutate(date = as.Date(date)) %>%
  mutate_all(funs('str_replace_all'),pattern = '\t', replacement = '') %>%
  rename(siteName = site) %>%
  fix_siteName %>%
  arrange(date, siteName) %>%
  mutate(visitID = paste(siteName, date, sep = '_')) %>%
  select(visitID, observerPc, startTimePc:countPc, detectionPc) %>%
  rename(timeIntervalPc = timePc, detectionMethod = detectionPc)

#=================================================================================*
# ---- nestObservationTable ----
#=================================================================================*

# Springfield entered the wrong dates for HALALEEMA1 removing, having becca fix:

read_csv('nestObservationTable.csv') %>%
  filter(hub == 'Springfield') %>%
  filter(year(dateNest) == 2016) %>%
  filter(site == 'HALALEEMA1') %>%
  write_csv('nestHALALEEMA1_2016.csv')

read_csv('nestObservationTable.csv') %>%
  filter(hub == 'Springfield') %>%
  filter(!is.na(dateNest)) %>%
  arrange(desc(dateNest))

read_csv('nestObservationTable.csv') %>%
  filter(hub == 'Springfield') %>%
  filter(!is.na(dateNest)) %>%
  arrange(desc(dateNest)) %>%
  filter(year(dateNest) == 2016) %>%
  filter(site == 'HALALEEMA1')

read.csv('springfieldNest2013_2015.csv')

#=================================================================================*
# ---- nestSummaryTable ----
#=================================================================================*

nestSummaries <- read_csv('nestSummary.csv') %>%
  mutate(site = str_replace_all(site, 'BLECLANIMA1','BLECLANMA1'),
         site = str_replace_all(site, 'HAZZAANNMA1','HAZZANNMA1')) %>%
  filter(hub == 'Springfield') %>% dim
  filter(hub == 'Gainesville') %>% dim
  .$site %>% unique %>% length
  

#=================================================================================*
# 
# 
# convertNAtoDash <- function(x){
#   ifelse(is.na(x), '-', x)
# }
# 
# encSpringfield2016 %>%
#   filter(encounterType == 'Recap') %>%
#   select(siteName, bandNumber) %>%
#   bind_cols(
#     encSpringfield2016 %>%
#       filter(encounterType == 'Recap') %>%
#       select(colorCombo) %>%
#       separate(colorCombo, ',', into = c('left', 'right')) %>%
#       separate(left, '/', into = c('leftUpper', 'leftLower')) %>%
#       separate(right, '/', into = c('rightUpper', 'rightLower')) %>%
#       mutate_all(.funs = 'convertNAtoDash')
#   ) %>%
#   write.csv('recapMarkers_Springfield2016.csv', row.names = FALSE)
# 
# encSpringfield2016 %>%
#   filter(encounterType == 'Band') %>%
#   select(siteName, bandNumber) %>%
#   bind_cols(
#     encSpringfield2016 %>%
#       filter(encounterType == 'Band') %>%
#       select(colorCombo) %>%
#       separate(colorCombo, ',', into = c('left', 'right')) %>%
#       separate(left, '/', into = c('leftUpper', 'leftLower')) %>%
#       separate(right, '/', into = c('rightUpper', 'rightLower')) %>%
#       mutate_all(.funs = 'convertNAtoDash')
#   ) %>%
#   write.csv('bandingMarkers_Springfield2016.csv', row.names = FALSE)
# 
# encSpringfield2016 %>%
#   select(siteName, date, observerEnc) %>%
#   distinct %>%
#   group_by(date, observerEnc) %>%
#   summarize(countsForDate = n()) %>%
#   filter(countsForDate > 1) %>%
#   filter(observerEnc != 'BSE') %>%
#   left_join(encSpringfield2016 %>%
#               select(siteName, date, observerEnc) %>%
#               distinct,
#             by = c('date', 'observerEnc')
#               )
# 
# encSpringfield2016 %>% filter(siteName == 'GIRLSINMA1')
# 
# encTable %>% 
#   filter(hub == 'Springfield',
#          date %in% c('2016-07-12', '2016-07-19')) %>%
#   View
#   select(site) %>% distinct
# 
# encSpringfield2016 %>% filter(bandNumber == '2780-93259')
# 
# read.csv('susannahUpdatedSiteInfo.csv')
# 
# encSpringfield2016 %>% filter(bandNumber == '2780-93316')
# 
# encTable %>% filter(bandNumber == '2780-93316')
# 
# encTable %>% filter(date == '2016-07-01', hub == 'Springfield') %>% .$site %>% unique
# 
#   
# 
