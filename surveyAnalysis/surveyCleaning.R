#========================================================================================*
# ---- setup ----
#========================================================================================*

# Survey location: http://www.surveygizmo.com/s3/3099540/New-Survey

library(tidyverse)
library(readr)
library(stringr)
library(lubridate)

select <- dplyr::select

options(stringsAsFactors = FALSE)


# Function for string detection and replacement across multiple values:

multi_strDetectRecode <-
  function(x, searchStringAndRecode, otherValue = NA) {
    x <- str_trim(x)
    for (j in 1:length(names(searchStringAndRecode))) {
      x <- ifelse(str_detect(x, names(searchStringAndRecode)[j]),
                  searchStringAndRecode[j], x)
    }
    x <- ifelse(!(x %in% searchStringAndRecode), otherValue, x)
    return(x)
  }

#----------------------------------------------------------------------------------------*
# ---- get and clean data ----
#----------------------------------------------------------------------------------------*

# Load data and fixing responseID column name:

rawData <- read_csv('nestwatchSurveyRaw.csv') %>%
  rename(responseID = `Response ID`) %>%
  filter(!responseID %in% c(1, 2, 4, 6))

# Get rid of bad characters in column names:

names(rawData) <- str_replace_all(names(rawData), '\\?', '') %>%
  str_trim

# Generate a response ID table with all relevant identifying information:

responseIdTable <- rawData %>%
  transmute(
    responseID = responseID,
    dateSubmitted = `Date Submitted` %>%
      as.Date('%m/%d/%Y'),
    ipAddress = `IP Address`,
    longitude = Longitude,
    latitude = Latitude,
    city = City,
    state = `State/Region`,
    Postal = as.character(Postal),
    zip = case_when(str_count(Postal) == 4 ~ paste0('0', Postal),
                    TRUE ~ Postal)
  )

# Empty container to store output:

surveyResults <- list()

#========================================================================================*
# ---- question wrangling ----
#========================================================================================*

# Q1: table of proportion of total yard area managed for wildlife ----

surveyResults$q1 <- rawData %>%
  select(responseID, propYardManaged = 22) %>%
  mutate(propYardManaged = propYardManaged %>%
           multi_strDetectRecode(
             c(
               'None' = 'none',
               'less than a tenth' = '<10',
               'between a tenth' = '11-25',
               'between a quarter' = '26-50',
               'half and three' = '51-75',
               'More than' = '>75'
             )
           ))

# Q2: longform table of bird friendly features and # of features in yards ----

surveyResults$q2 <- rawData %>%
  select(c(1, 23:28)) %>%
  gather(key = object,
         value = number,
         2:7) %>%
  mutate(
    object = object %>%
      multi_strDetectRecode(
        c(
          'Wildlife pond' = 'pond',
          'Bird bath' = 'birdBath',
          'Brush pile' = 'brushPile',
          'Nestbox' = 'nestbox',
          'Trees or bushes' = 'berryProducingTrees',
          'Bird feeder' = 'birdFeeder'
        )
      ),
    number = ifelse(str_detect(number, '5 or more'), '5', number)
  )

# Q3,4: table of when and how regularly participants feed birds ----

surveyResults$q3to4 <- rawData %>%
  select(1, whenFeedBirds = 29,  howOftenFeedBirds = 30) %>%
  mutate(
    whenFeedBirds = whenFeedBirds %>%
      multi_strDetectRecode(
        c(
          'Only in winter' = 'winterOnly',
          'All year' = 'allYear',
          'Only in summer' = 'summerOnly',
          'Never' = 'never'
        )
      ),
    howOftenFeedBirds = howOftenFeedBirds %>%
      multi_strDetectRecode(
        c(
          'Every day' = 'everyday',
          'A few days per week' = 'aFewDaysPerWeek',
          'Every 2-3 weeks' = 'everyTwoToThreeWeeks',
          'Most days every week' = 'mostDaysPerWeek',
          'One day per week' = 'oneDayPerWeek'
        )
      )
  )

# Q5,6: table of number of dogs and cats and time dogs/cats spend outside ----

surveyResults$q5to6 <- rawData %>%
  select(
    1,
    numberCats = 31,
    numberDogs = 32,
    timeOutsideCat = 33,
    timeOutsideDog = 34
  ) %>%
  mutate(
    numberCats = ifelse(str_detect(numberCats, '5 or more'), '5', numberCats),
    numberDogs = ifelse(str_detect(numberDogs, '5 or more'), '5', numberDogs),
    timeOutsideCat = timeOutsideCat %>%
      multi_strDetectRecode(
        c(
          '1 hour or' = '<1',
          '2-11 hours' = '2-11',
          '12 -24' = '12-24',
          'go outside' = 'indoorOnly'
        )
      ),
    timeOutsideDog = timeOutsideDog %>%
      multi_strDetectRecode(
        c(
          '1 hour or' = '<1',
          '2-11 hours' = '2-11',
          '12 -24' = '12-24',
          'go outside' = 'indoorOnly'
        )
      )
  )

#----------------------------------------------------------------------------------------*
# ---- question 7 ----
#----------------------------------------------------------------------------------------*

# Q7: table of whether nestwatch changed the way they manage their yard ----

surveyResults$q7 <- rawData %>%
  select(1, nestwatchChangedHowIManageYard = 35) %>%
  mutate(nestwatchChangedHowIManageYard = nestwatchChangedHowIManageYard %>%
           multi_strDetectRecode(
             c(
               'Strongly agree' = 'stronglyAgree',
               'Somewhat agree' = 'somewhatAgree',
               'Neither agree or disagree' = 'neitherAgreeOrDisagree',
               'Somewhat disagree' = 'somewhatDisagree',
               'Strongly disagree' = 'stronglyDisagree'
             )
           ))

# Q8: longform table of who participants talk to about birds in their yard and in what geographic area do they talk to people about birds in their yard ----

surveyResults$q8 <- rawData %>%
  select(1, 36:143) %>%
  gather(key = peopleITalkToAboutBirdsInYard,
         value = location,
         2:109) %>%
  filter(location != '<NA>') %>%
  mutate(
    peopleITalkToAboutBirdsInYard = peopleITalkToAboutBirdsInYard %>%
      multi_strDetectRecode(
        c(
          'Other Neighborhood Nestwatch participants' = 'nnParticipants',
          'not Neighborhood Nestwatch participants' = 'friends',
          'non Neighborhood Nestwatch participants' = 'family'
        ),
        otherValue = 'other'
      ),
    location = location %>%
      multi_strDetectRecode(
        c(
          'talk with anyone about birds in my yard' = 'iDontTalkAboutBirds',
          'In my neighborhood' = 'neighborhood',
          'In other cities or towns' = 'otherCitiesOrTowns',
          'In my city or town' = 'myCityOrTown'
        )
      )
  ) %>%
  arrange(responseID)

# Q9: longform table of who participants talk to about landscaping their yard and in what geographic area do they talk to people about landscaping their yard ----

surveyResults$q9 <- rawData %>%
  select(1, 144:195) %>%
  gather(key = peopleITalkToAboutHowILandscapeYard,
         value = location,
         2:53) %>%
  filter(location != '<NA>') %>%
  mutate(
    peopleITalkToAboutHowILandscapeYard = peopleITalkToAboutHowILandscapeYard %>%
      multi_strDetectRecode(
        c(
          'Other Neighborhood Nestwatch' = 'nnParticipants',
          'Friends' = 'friends',
          'Family' = 'family'
        ),
        otherValue = 'other'
      ),
    location = location %>%
      multi_strDetectRecode(
        c(
          'talk with anyone about how I landscape my yard' = 'iDontTalkAboutYard',
          'In my neighborhood' = 'neighborhood',
          'In other cities or towns' = 'otherCitiesOrTowns',
          'In my city or town' = 'myCityOrTown'
        )
      )
  ) %>%
  arrange(responseID)

# Q10: table of how long they have participated ----

surveyResults$q10 <- rawData %>%
  select(1, 196:198) %>%
  gather(key = region,
         value = numberYears,
         2:4) %>%
  mutate(
    region = region %>%
      multi_strDetectRecode(
        c(
          'Springfield' = 'Springfield',
          'Raleigh' = 'Raleigh',
          'DC' = 'DC'
        )
      ),
    numberYears = numberYears %>%
      str_replace_all(' years', '') %>%
      str_replace_all(' year', '') %>%
      str_replace_all('More than 10', 'over10'),
    region = ifelse(numberYears == 'over10', 'DC', region)
  ) %>%
  filter(!is.na(numberYears)) %>%
  arrange(responseID)


# Q11: table with number of acres of participants' properties ----

surveyResults$q11 <- rawData %>%
  select(1, numberAcres = 199)  %>%
  mutate(numberAcres = numberAcres %>%
           multi_strDetectRecode(
             c(
               'Less than a tenth' = '<.1',
               'to a quarter' = '.1-.25',
               'Quarter acre to half' = '.25-.5',
               'Half acre to 1' = '.5-1',
               '1 to 5 acres' = '1-5',
               'Larger than 5 acres' = '>5'
             )
           ))
