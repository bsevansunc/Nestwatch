library(mongolite)

mongoURL <- 'mongodb://bsevans:33shazam@ds025232.mlab.com:25232/nndataentry'

# Connect to databases:

dbSite <- mongo('site_data', url = mongoURL)

dbVisit <- mongo('visit_data', url = mongoURL)

dbEncounters <- mongo('encounter_data', url = mongoURL)

dbEncounterSummary <- mongo('encounter_summary', url = mongoURL)

dbPointCount <-  mongo('pointCount_data', url = mongoURL)

dbNestObservations <- mongo('nest_observation_data', url = mongoURL)

dbNestSummary <- mongo('nest_summary_data', url = mongoURL)

# Check headers (first two fields should be hub and site):

head(dbSite$find())

head(dbVisit$find())

head(dbEncounters$find())

head(dbEncounterSummary$find())

head(dbPointCount$find())

head(dbNest$find())

#-------------------------------------------------------------------------------*
# ---- Collect, remove, replace ----
#-------------------------------------------------------------------------------*
# Encounter data:

dfEncounter <- dbEncounters$find()

head(dfEncounter)

names(dfEncounter)[1:3] <- c('hub', 'site', 'date')

head(dfEncounter)

dbEncounters$drop()

dbEncounters$insert(dfEncounter)

# Point count data:

dfPointCount <- dbPointCount$find()

head(dfPointCount)

names(dfPointCount)[c(1:2, 4)] <- c('hub', 'site', 'date')

head(dfPointCount)

dbPointCount$drop()

dbPointCount$insert(dfPointCount)

# Nest summary data:

fieldCodesNestSummary

naLine <- rep(NA, length(fieldCodesNestSummary))

dummyNestSummaryDf <- data.frame(t(naLine))

names(dummyNestSummaryDf) <- fieldCodesNestSummary

dbNestSummary$insert(dummyNestSummaryDf)

# Nest observation data:

fieldCodesNest

naLine <- rep(NA, length(fieldCodesNest))

dummyNestDf <- data.frame(t(naLine))

names(dummyNestDf) <- fieldCodesNest

dbNest$insert(dummyNestDf)

###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
siteTest <- "REITBOBMD1"
siteQuery <- str_c('{', shQuote('site'),':', shQuote(siteTest),'}')

dbVisit$find('{ "site" : "REITBOBMD1" }')

dbVisit$find(siteQuery)

siteQuery <- str_c('{"site":', ' "REITBOBMD1" ','}')

siteField <- 'site'

inSite <- 'REITBOBMD1'

siteQuery <- str_c('{',
      shQuote(siteField, type = 'cmd'),
      ':',
      shQuote(inSite, type = 'cmd'),
      '}')

dbVisit$find()

dbVisit$find(siteQuery)

vTest <- dbVisit$find(siteQuery(siteField, inSite)) 

vTest1 <- vTest %>% tbl_df %>%
  mutate(pathDistance = 0) %>%
  mutate(spUnbanded = '') %>%
  select(hub:rsTime, pathDistance, spUnbanded, countUnbanded) 

dbVisit$insert(vTest1)

dbVisit$remove('{ "site" : "REITBOBMD1" }', multiple = TRUE)

dbVisit$find()

dbVisit$find('{ "site" : siteTest }')

db$find('{ "helloWorld" : "foo" }')

dbVisit$remove('{ "site" : "REITBOBMD1" }', multiple = TRUE)

db$remove()


db$find('{ "helloWorld" : "bar" }')

db$remove('{ "helloWorld" : "bar" }', multiple = TRUE)

visit_data <- tail(visit_data)

visit_data %>%
  mutate(date = as.Date(date, '%m/%d/%Y'))





class(db$find())
db$aggregate()


fieldCodesSite <- c(
  'hubSite',	'siteSite',	'lastNameOrSchoolSite',	'firstNameSite',	'longSite',
  'latSite', 'accuracySite',	'addressSite',	'startYearSite',	'phone1Site',
  'phone2Site','email1Site',	'email2Site',	'notesSite'
)

siteData <- sites %>%
  rename(hubSite = hub, siteSite = site) %>%
  mutate(lastNameOrSchoolSite = 'FILL',
         firstNameSite= 'FILL',
         longSite= 'FILL',
         latSite= 'FILL',
         accuracySite= 'FILL',
         addressSite= 'FILL',
         startYearSite= 'FILL',
         phone1Site= 'FILL',
         phone2Site= 'FILL',
         email1Site= 'FILL',
         email2Site= 'FILL',
         notesSite= 'FILL') %>%
  arrange(siteSite) %>%
  arrange(hubSite)


dbSite <- mongo('site_data', url = 'mongodb://bsevans:33shazam@ds025232.mlab.com:25232/nndataentry')

dbSite$find(siteQuery('siteSite', 'REITBOBMD1'))

db2 <- dbSite$find()
##

dbSite$drop()

names(db2)[1:2] <- c('hub', 'site')

dbSite$insert(db2)

db2 <- db2[,-c(15, 16)]

dbEnc <- mongo('encounter_data', url = 'mongodb://bsevans:33shazam@ds025232.mlab.com:25232/nndataentry')

dbEnc$find()

hubQuery <- function(hubField, inHub){
  str_c('{',
        shQuote(hubField, type = 'cmd'),
        ':',
        shQuote(inHub, type = 'cmd'),
        '}')
}

dbSite$find(hubQuery('hubSite', 'DC')) %>% .$siteSite

dbSite$find() %>% tbl_df()

###
# AOU

aou <- read.csv('speciesAouForApp.csv', stringsAsFactors = FALSE)

dbAou$insert(vTest1)

# Encounters:

dbEncounterSummary <- mongo('encounter_summary', url = 'mongodb://bsevans:33shazam@ds025232.mlab.com:25232/nndataentry')

encountersNewOrder <- encounters %>%
  select(hub:site, date, species:encounterType)

dbEncounterSummary$insert(encountersNewOrder)

# Nest:

dbNest <- mongo('nest_data', url = 'mongodb://bsevans:33shazam@ds025232.mlab.com:25232/nndataentry')

nestDf <- data.frame(matrix(NA, nrow = 1, ncol = length(fieldCodesNest)))

names(nestDf) <- fieldCodesNest

dbNest$insert(nestDf)




