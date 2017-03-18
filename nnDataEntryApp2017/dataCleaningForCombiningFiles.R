options(stringsAsFactors = FALSE)

library(R.utils)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)

str_insert <- function(string, position, sepCharacter){
  if(!is.character(string)) string <- as.character(string)
  strStart <- str_sub(string, end = position -1)
  strEnd <- str_sub(string, start = position)
  return(paste(strStart, strEnd, sep = sepCharacter))
}

str_paste_columns <- function(column1, column2){
  columnOut <- ''
  for(i in 1:length(column1)){
    if(
      !is.na(column1[i]) &
      !is.na(column2[i])
      ){
      if(column1[i] != column2[i]){
        columnOut[i] <- paste(column1[i], column2[i], sep = ';')
      } else {
        columnOut[i] <- column1[i]
      }
    } 
    if(
      !is.na(column1[i]) &
      is.na(column2[i])
    ){
      columnOut[i] <- column1[i]
    }
    if(
      is.na(column1[i]) &
      !is.na(column2[i])
    ){
      columnOut[i] <- column2[i]
    }
    if(
      is.na(column1[i]) &
      is.na(column2[i])
    ){
      columnOut[i] <- ''
    }
  }
  return(columnOut)
}

dateYearReplace <- function(badDate, yearReplacement){
  date <- as.Date(badDate)
  newDate <- paste(yearReplacement, month(date), day(date), sep = '-')
  return(as.Date(newDate))
} 

measures <- read.csv('measures.csv') %>%
  tbl_df %>%
  rename(measureNotes = notes) %>% 
  filter(hub != 'springfield') %>%
  mutate(bandNumber = bandNumber %>%
           str_extract('[:digit:]+') %>%
           as.numeric,
         date = as.Date(date))

measuresSpringfield <- read.csv('springfieldMeasurementEdit1.csv') %>%
  tbl_df %>%
  rename(measureNotes = notes) %>% 
  mutate(bandNumber = bandNumber %>%
           str_extract('[:digit:]+') %>%
           as.numeric,
         date = as.Date(date, '%m/%d/%Y'),
         year = year(date)) %>%
  select(-c(hub, site, date, species) )

measures1 <- measures %>%
  bind_rows(measuresSpringfield) %>%
  select(-c(site)) %>%
  mutate(
    species = toupper(species),
    year = year(as.Date(date))
    ) %>%
  distinct %>%
  select(-c(date, species))

measuresSpringfield1 <- measures1 %>%
  filter(hub == 'springfield') 


# measures %>% select(hub, date) %>% transmute(hub = hub, year = year(date)) %>% group_by(hub, year) %>% summarize(N = n()) %>% View
# 
# measures %>% filter(year(date) == 1984)
# encounters %>% filter(bandNumber == 257189932)

encounters <- read.csv('cleanedData/encounters3.csv') %>%
  tbl_df %>%
  rename(encounterNotes = notes) %>%
  distinct %>%
  mutate(date = as.Date(date),
         year = year(date))
# 
# encountersSpringfield <- read.csv('encountersSpringfield.csv') %>%
#   tbl_df %>%
#   mutate(year = year(as.Date(date)))

eSpringfield <- encounters %>%
  filter(hub == 'springfield')

emSpringfield <- full_join(
  eSpringfield, measuresSpringfield, #encounters, measures1,
  by = c('bandNumber', 'year')
  ) %>%
  transmute(hub = capitalize(hub),
         hub = ifelse(hub == 'Dc', 'DC', hub),
         site = toupper(site),
         date = date,
         timeEnc = time,
         observerEnc = NA,
         encounterType = encounterType,
         speciesEnc = toupper(species),
         bandNumber = bandNumber %>%
           str_insert(-5, '-'),
         colorCombo = toupper(bandCombo) %>%
           str_replace_all(' ', ''),
         age = age,
         sex = sex,
         breedingCond = breedingCond,
         fat = fat,
         mass = mass,
         wing = wing,
         tl = tail,
         notesEnc = str_paste_columns(encounterNotes, measureNotes)
         )


# Fix bad date objects:

for(i in 1:nrow(encMe)){
  date <- encMe$date[i]
  if(!is.na(date) & year(date) == 1984) {
    encMe$date[i] <- dateYearReplace(date, 2014)
  }
  if(!is.na(date) & year(date) == 212) {
    encMe$date[i] <- dateYearReplace(date, 2012)
  }
  if(!is.na(date) & year(date) == 215) {
    encMe$date[i] <- dateYearReplace(date, 2015)
  }
  if(!is.na(date) & year(date) == 2103) {
    encMe$date[i] <- dateYearReplace(date, 2013)
  }
  if(!is.na(date) & year(date) == 2018) {
    encMe$date[i] <- dateYearReplace(date, 2015)
  }
}

encMeBadDC <- filter(encMe, is.na(site), hub == 'DC')

encMeNotBadDC <- filter(encMe, !(is.na(site) & hub == 'DC'))

encMeBadDC %>%
  arrange(date) %>%
  mutate(
    bandNumber = ifelse(bandNumber = '-1152', '1152-87541', bandNumber)
  )
##

encMe %>% filter(date == '2012-07-09')

encMe %>% filter(bandNumber == '2341-30388')


write.csv(filter(encMe, is.na(site), hub == 'DC'), 'deleteMe.csv', row.names = FALSE)



