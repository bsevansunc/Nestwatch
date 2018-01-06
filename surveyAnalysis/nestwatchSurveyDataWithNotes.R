#========================================================================================*
# ---- setup ----
#========================================================================================*

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

# Generate a response ID table with all relevant identifying information

responseIdTable <- rawData %>%
  select(1, 3, 15:17, 19:21, springfield = 196, dc = 197, raleigh = 198) %>%
  transmute(
    responseID = responseID, 
    region = ifelse(!is.na(springfield), 'Springfield', 
                    ifelse(!is.na(dc), 'DC',
                           ifelse(!is.na(raleigh), 'Raleigh', NA))),
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
  ) %>% 
arrange(responseID)
  

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
          'my yard:Other Neighborhood Nestwatch' = 'nnParticipants',
          'my yard:Friends' = 'friends',
          'my yard:Family' = 'family'
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
    numberYears = numberYears %>%
      str_replace_all('  years', '') %>%
      str_replace_all('  year', '') %>%
      str_replace_all('More than 10', 'over10')
  ) %>%
  select(-region) %>%
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

#=================================================================================*
# ---- summary data ----
#=================================================================================*

# added column for proportion of respondents that gave each answer for each region

q1Summary <- surveyResults$q1 %>%
  left_join(q10 %>%
              select(-numberYears),
            by = 'responseID') %>%
  filter(!is.na(propYardManaged),!is.na(region)) %>%
  mutate(tRespondents = length(unique(responseID))) %>%
  group_by(region) %>%
  mutate(tRespondentsRegion = length(unique(responseID))) %>%
  group_by(region, propYardManaged) %>%
  summarize(
    nRespondents = n(),
    pRespondents = n() / unique(tRespondentsRegion) * 100
  ) %>%
  ungroup %>%
  mutate(
    propYardManaged = factor(
      propYardManaged,
      levels = c('>75', '51-75', '26-50', '11-25', '<10', 'none')
    ),
    pRespondents = plyr::round_any(pRespondents, 0.01)
  ) %>%
  arrange(propYardManaged)

# bar graph of proportion of yard managed for wildlife for each region

q1Summary %>%
  mutate(`Proportion of yard` = propYardManaged,
         `Region` = factor(region)) %>%
  ggplot(aes(x = `Region`, y = pRespondents, fill = `Proportion of yard`)) +
  ylab('Proportion of respondents') +
  geom_bar(stat = 'identity', color = 'black', size = 1) +
  geom_bar(
    data =  q1Summary %>%
      mutate(tRespondents = sum(nRespondents)) %>%
      group_by(propYardManaged) %>%
      summarize(pRespondents = sum(nRespondents) / unique(tRespondents) * 100) %>%
      ungroup %>%
      mutate(`Proportion of yard` = propYardManaged),
    aes(x = 'Across regions', y = pRespondents, fill = `Proportion of yard`),
    stat = 'identity',
    color = 'black',
    size = 1
  ) +
  ggtitle('Proportion of yard managed for birds') +
  theme_bw() +
  theme(
    axis.text = element_text(size = rel(1.25)),
    axis.title = element_text(size = rel(1.4)),
    plot.title = element_text(size = rel(1.7), hjust = .5)
  ) +
  scale_fill_brewer(palette = 'Set2')

ggsave('q1Bar.png')

# generating table of numbner and proportion of respondents with each feature in each region

q2Summary <- surveyResults$q2 %>%
  left_join(q10 %>%
              select(-numberYears),
            by = 'responseID') %>%
  filter(!is.na(region)) %>%
  mutate(
    number = ifelse(is.na(number), 0, number),
    number = as.numeric(number),
    number = ifelse(number > 0, 1, 0)
  ) %>%
  group_by(region) %>%
  mutate(tRespondents = length(unique(responseID))) %>%
  group_by(region, object) %>%
  summarize(n = sum(number),
            p = sum(number) / unique(tRespondents)) %>%
  ungroup %>%
  mutate(region = factor(region))

# graph of proportion of respondents with each feature by region

q2Summary %>%
  mutate(Feature = factor(
    object,
    labels = c(
      'Berry-producing trees',
      'Bird bath',
      'Bird feeder',
      'Brush pile',
      'Nestbox',
      'Pond'
    )
  )) %>%
  ggplot(aes(x = region, y = p, fill = Feature)) +
  geom_bar(
    position = 'dodge',
    stat = 'identity',
    color = 'black',
    size = 1
  ) +
  ylab('Proportion of respondents with feature') +
  xlab('Region') +
  ggtitle('Proportion of participants with bird-friendly features') +
  theme_bw() +
  theme(
    axis.text = element_text(size = rel(1.25)),
    axis.title = element_text(size = rel(1.4)),
    plot.title = element_text(size = rel(1.7), hjust = .5)
  ) +
  scale_fill_brewer(palette = 'Set2')

ggsave('q2Bar.png')

# generating table of number and proportion of respondents who feed birds and when they feed them

q3to4Summary <- surveyResults$q3to4 %>%
  left_join(q10 %>%
              select(-numberYears),
            by = 'responseID') %>%
  filter(!is.na(region)) %>%
  group_by(region) %>%
  mutate(nRespondents = n()) %>%
  group_by(region, whenFeedBirds) %>%
  summarize(n = n(),
            p = n() / unique(nRespondents) * 100) %>%
  ungroup %>%
  bind_rows(data.frame(
    region = 'Raleigh',
    whenFeedBirds = 'summerOnly',
    n = 0,
    p = 0
  )) %>%
  mutate(whenFeedBirds = factor(
    whenFeedBirds,
    levels = c('never', 'summerOnly', 'winterOnly', 'allYear'),
    labels = c('Never', 'Summer only', 'Winter only', 'All year')
  ))

# bar graph of if/when people feed birds by region

q3to4Summary %>%
  rename(`When birds are fed` = whenFeedBirds) %>%
  ggplot(aes(x = region, y = p, fill = `When birds are fed`)) +
  geom_bar(
    position = 'dodge',
    stat = 'identity',
    color = 'black',
    size = 1
  ) +
  ylab('Proportion of respondents') +
  xlab('Region') +
  ggtitle('Bird feeding behavior of Nestwatch participants') +
  theme_bw() +
  theme(
    axis.text = element_text(size = rel(1.25)),
    axis.title = element_text(size = rel(1.4)),
    plot.title = element_text(size = rel(1.7), hjust = .5)
  ) +
  scale_fill_brewer(palette = 'Set2')

ggsave('q3Bar.png')

# generating table of number and proportion of participants with indoor, outdoor, or no cat by region

q56cat <- surveyResults$q5to6 %>%
  left_join(q10 %>%
              select(-numberYears),
            by = 'responseID') %>%
  filter(!is.na(region)) %>%
  group_by(region) %>%
  mutate(numberCats = as.numeric(numberCats)) %>%
  mutate(
    cat = ifelse(timeOutsideCat == 'indoorOnly', 'indoorCat', 'outdoorCat'),
    cat = ifelse(numberCats == 0 |
                   is.na(numberCats), 'noCat', cat),
    cat = factor(
      cat,
      levels = c('noCat', 'indoorCat', 'outdoorCat'),
      labels = c('No cat', 'Indoor-only cat', 'Outdoor cat')
    )
  ) %>%
  filter(!(numberCats > 0 & is.na(timeOutsideCat))) %>%
  group_by(region) %>%
  mutate(nRespondents = n()) %>%
  group_by(region, cat) %>%
  summarize(n = n(),
            p = n() / unique(nRespondents) * 100) %>%
  ungroup

# stacked bar graph of cat ownership and indoor/outdoor cats by region

q56cat %>%
  rename(`Cat ownership` = cat) %>%
  ggplot(aes(x = region, y = p, fill = `Cat ownership`)) +
  ylab('Proportion of respondents') +
  geom_bar(
    stat = 'identity',
    color = 'black',
    size = 1,
    width = .5
  ) +
  ggtitle('Proportion of participants with indoor or outdoor cats') +
  xlab('Region') +
  theme_bw() +
  theme(
    axis.text = element_text(size = rel(1.25)),
    axis.title = element_text(size = rel(1.4)),
    plot.title = element_text(size = rel(1.7), hjust = .5)
  ) +
  scale_fill_brewer(palette = 'Set2')

ggsave('q56cat.png')

# graph of proportion of cat-owning participants with outdoor cats by region

q56cat %>%
  filter(cat != 'No cat') %>%
  group_by(region) %>%
  mutate(nRespondents = sum(n)) %>%
  filter(cat == 'Outdoor cat') %>%
  mutate(p = n / nRespondents * 100) %>%
  ggplot(aes(x = region, y = p)) +
  ylab('Proportion of respondents') +
  geom_bar(
    stat = 'identity',
    color = 'black',
    size = 1,
    width = .5
  ) +
  ggtitle('Proportion of cat-owning participants with outdoor cats') +
  xlab('Region') +
  theme_bw() +
  theme(
    axis.text = element_text(size = rel(1.25)),
    axis.title = element_text(size = rel(1.4)),
    plot.title = element_text(size = rel(1.7), hjust = .5)
  ) +
  scale_fill_brewer(palette = 'Set2')

ggsave('q56catOutdoor.png')

# generating table of number and proportion of dog-owners and whether the dog spends time off-leash

q56dog <- surveyResults$q5to6 %>%
  left_join(q10 %>%
              select(-numberYears),
            by = 'responseID') %>%
  filter(!is.na(region)) %>%
  group_by(region) %>%
  mutate(numberDogs = as.numeric(numberDogs)) %>%
  mutate(
    dog = ifelse(timeOutsideDog == 'indoorOnly', 'indoorDog', 'outdoorDog'),
    dog = ifelse(numberDogs == 0 |
                   is.na(numberDogs), 'noDog', dog),
    dog = factor(
      dog,
      levels = c('noDog', 'indoorDog', 'outdoorDog'),
      labels = c('No dog', 'On leash-only dog', 'Sometimes off-leash dog')
    )
  ) %>%
  filter(!(numberDogs > 0 & is.na(timeOutsideDog))) %>%
  group_by(region) %>%
  mutate(nRespondents = n()) %>%
  group_by(region, dog) %>%
  summarize(n = n(),
            p = n() / unique(nRespondents) * 100)

# stacked bar graph of proportion of participants with dog and if the dog is kept on or off leash by region

q56dog %>%
  rename(`Dog ownership` = dog) %>%
  ggplot(aes(x = region, y = p, fill = `Dog ownership`)) +
  ylab('Proportion of respondents') +
  geom_bar(
    stat = 'identity',
    color = 'black',
    size = 1,
    width = .5
  ) +
  ggtitle('Proportion of participants with dogs kept on or off-leash') +
  xlab('Region') +
  theme_bw() +
  theme(
    axis.text = element_text(size = rel(1.25)),
    axis.title = element_text(size = rel(1.4)),
    plot.title = element_text(size = rel(1.7), hjust = .5)
  ) +
  scale_fill_brewer(palette = 'Set2')

ggsave('q56dog.png')

# graph of proportion of dog owning participants who let their dog off-leash by region

q56dog %>%
  filter(dog != 'No dog') %>%
  group_by(region) %>%
  mutate(nRespondents = sum(n)) %>%
  filter(dog == 'Sometimes off-leash dog') %>%
  mutate(p = n / nRespondents * 100) %>%
  ggplot(aes(x = region, y = p)) +
  ylab('Proportion of respondents') +
  geom_bar(
    stat = 'identity',
    color = 'black',
    size = 1,
    width = .5
  ) +
  ggtitle('Proportion of dog-owning participants with sometimes off-leash dogs') +
  xlab('Region') +
  theme_bw() +
  theme(
    axis.text = element_text(size = rel(1.25)),
    axis.title = element_text(size = rel(1.4)),
    plot.title = element_text(size = rel(1.7), hjust = .5)
  ) +
  scale_fill_brewer(palette = 'Set2')

ggsave('q56dogOutdoor.png')

# generating table of time that dog-owning participants allow their dog outside across all regions

q5to6DogSummary <- q5to6 %>%
  group_by(numberDogs, timeOutsideDog) %>%
  summarize(n = n()) %>%
  ungroup %>%
  filter(numberDogs > 0,!is.na(timeOutsideDog)) %>%
  group_by(timeOutsideDog) %>%
  summarize(n = sum(n))

# generating table of number and proportion of respondents who
# agree or disagree that nestwatch changed how they manage their yard

q7Summary <- surveyResults$q7 %>%
  left_join(q10 %>%
              select(-numberYears),
            by = 'responseID') %>%
  filter(!is.na(nestwatchChangedHowIManageYard),!is.na(region)) %>%
  mutate(tRespondents = length(unique(responseID))) %>%
  group_by(region) %>%
  mutate(tRespondentsRegion = length(unique(responseID))) %>%
  group_by(region, nestwatchChangedHowIManageYard) %>%
  summarize(
    nRespondents = n(),
    pRespondents = n() / unique(tRespondentsRegion) * 100
  ) %>%
  ungroup %>%
  mutate(
    nestwatchChangedHowIManageYard = factor(
      nestwatchChangedHowIManageYard,
      levels = c(
        'stronglyAgree',
        'somewhatAgree',
        'neitherAgreeOrDisagree',
        'somewhatDisagree',
        'stronglyDisagree'
      ),
      labels = c(
        'Strongly agree',
        'Somewhat agree',
        'Neither agree nor disagree',
        'Somewhat disagree',
        'Strongly disagree'
      )
    )
  )

# bar graph of proportion of respondents who agree or disagree with statement "nestwatch changed how i manage my yard

q7Summary %>%
  mutate(`Level of agreement` = nestwatchChangedHowIManageYard,
         `Region` = factor(region)) %>%
  ggplot(aes(x = `Region`, y = pRespondents, fill = `Level of agreement`)) +
  ylab('Proportion of respondents') +
  xlab('Region') +
  geom_bar(
    position = 'dodge',
    stat = 'identity',
    color = 'black',
    size = 1
  ) +
  geom_bar(
    data =  q7Summary %>%
      mutate(tRespondents = sum(nRespondents)) %>%
      group_by(nestwatchChangedHowIManageYard) %>%
      summarize(pRespondents = sum(nRespondents) / unique(tRespondents) * 100) %>%
      ungroup %>%
      mutate(`Level of agreement` = nestwatchChangedHowIManageYard),
    aes(x = 'Across regions', y = pRespondents, fill = `Level of agreement`),
    position = 'dodge',
    stat = 'identity',
    color = 'black',
    size = 1
  ) +
  ggtitle('Agreement with statement "Nestwatch changed how I manage my yard"') +
  theme_bw() +
  theme(
    axis.text = element_text(size = rel(1.25)),
    axis.title = element_text(size = rel(1.4)),
    plot.title = element_text(size = rel(1), hjust = .5)
  ) +
  scale_fill_brewer(palette = 'Set2')

ggsave('q7.png')

# creating vector of multiple choice options (to filter out free-form responses)

q8CannedRecipients <- c('family', 'friends', 'nnParticipants')

# table of number and proportion of respondents who talk about birds
# in their yard, who they talk to, and where they talk about birds in yard by region

q8Summary <- q8 %>%
  left_join(q10 %>%
              select(-numberYears),
            by = 'responseID') %>%
  filter(!is.na(region)) %>%
  filter(peopleITalkToAboutBirdsInYard %in% q8CannedRecipients) %>%
  filter(location != 'iDontTalkAboutBirdsInYard') %>%
  group_by(region) %>%
  mutate(nRespondents = n()) %>%
  group_by(region, peopleITalkToAboutBirdsInYard, location) %>%
  summarize(n = n(),
            p = n() / unique(nRespondents) * 100) %>%
  ungroup

# series of bar graphs of who participants talk to about birds in their yard
# and where geographically they talk to people about birds in their yard by region

q8Summary %>%
  mutate(
    `Communicate with` = factor(
      peopleITalkToAboutBirdsInYard,
      labels = c('Family', 'Friends', 'NN Participants')
    ),
    location = factor(
      location,
      levels = c('inMyNeighborhood', 'inMyCityOrTown', 'inOtherCitiesOrTowns'),
      labels = c('In my neighborhood', 'In my city or town', 'In other cities or towns')
    )
  ) %>%
  ggplot(aes(x = region, y = p, fill = `Communicate with`)) +
  geom_bar(
    position = 'dodge',
    stat = 'identity',
    color = 'black',
    size = 1
  ) +
  ylab('Proportion of respondents') +
  xlab('Region') +
  ggtitle('Who do participants communicate with regarding birds?') +
  theme_bw() +
  facet_wrap( ~ location) +
  theme(
    axis.text = element_text(size = rel(1.25)),
    axis.title = element_text(size = rel(1.4)),
    plot.title = element_text(size = rel(1.7), hjust = .5)
  ) +
  scale_fill_brewer(palette = 'Set2')

ggsave('q8.png')

# creating vector of multiple choice options (to filter out free-form responses)

q9CannedRecipients <- c('family', 'friends', 'nnParticipants')

# table of number and proportion of respondents who talk about how they
# landscape their yard, who they talk to, and where they talk about landscaping their yard by region

q9Summary <- surveyResults$q9 %>%
  left_join(q10 %>%
              select(-numberYears),
            by = 'responseID') %>%
  filter(!is.na(region)) %>%
  filter(peopleITalkToAboutHowILandscapeYard %in% q9CannedRecipients) %>%
  filter(location != 'iDontTalkAboutYard') %>%
  group_by(region) %>%
  mutate(nRespondents = n()) %>%
  group_by(region, peopleITalkToAboutHowILandscapeYard, location) %>%
  summarize(n = n(),
            p = n() / unique(nRespondents) * 100) %>%
  ungroup

# series of bar graphs of who participants talk to about landscaping their yard
# and where geographically they talk to people about landscaping their yard by region

q9Summary %>%
  mutate(
    `Communicate with` = factor(
      peopleITalkToAboutHowILandscapeYard,
      labels = c('Family', 'Friends', 'NN Participants')
    ),
    location = factor(
      location,
      levels = c('inMyNeighborhood', 'inMyCityOrTown', 'inOtherCitiesOrTowns'),
      labels = c('In my neighborhood', 'In my city or town', 'In other cities or towns')
    )
  ) %>%
  ggplot(aes(x = region, y = p, fill = `Communicate with`)) +
  geom_bar(
    position = 'dodge',
    stat = 'identity',
    color = 'black',
    size = 1
  ) +
  ylab('Proportion of respondents') +
  xlab('Region') +
  ggtitle('Who do participants communicate with regarding landscaping their yard?') +
  theme_bw() +
  facet_wrap( ~ location) +
  theme(
    axis.text = element_text(size = rel(1.25)),
    axis.title = element_text(size = rel(1.4)),
    plot.title = element_text(size = rel(1.7), hjust = .5)
  ) +
  scale_fill_brewer(palette = 'Set2')

ggsave('q9.png')

# generating table of number and proportion of respondents who have been in the program for x number of years by region

q10Summary <- surveyResults$q10 %>%
  filter(!is.na(region),!is.na(numberYears)) %>%
  mutate(tRespondents = length(unique(responseID))) %>%
  group_by(region) %>%
  mutate(tRespondentsRegion = length(unique(responseID))) %>%
  group_by(region, numberYears) %>%
  summarize(
    nRespondents = n(),
    pRespondents = n() / unique(tRespondentsRegion) * 100
  ) %>%
  ungroup

# generating a table of number and proportion of respondents with different size yards for all regions

q11Summary <- surveyResults$q11 %>%
  left_join(q10 %>%
              select(-numberYears),
            by = 'responseID') %>%
  filter(!is.na(numberAcres),!is.na(region)) %>%
  mutate(tRespondents = length(unique(responseID))) %>%
  group_by(region) %>%
  mutate(tRespondentsRegion = length(unique(responseID))) %>%
  group_by(region, numberAcres) %>%
  summarize(
    nRespondents = n(),
    pRespondents = n() / unique(tRespondentsRegion) * 100
  ) %>%
  ungroup %>%
  mutate(numberAcres = factor(
    numberAcres,
    levels = c('<.1', '.1-.25', '.25-.5', '.5-1', '1-5', '>5', 'unknown')
  ))

# bar graph of number of acres of participant's properties for all regions

q11Summary %>%
  mutate(`Number of acres` = numberAcres,
         `Region` = factor(region)) %>%
  ggplot(aes(x = `Region`, y = pRespondents, fill = `Number of acres`)) +
  ylab('Proportion of respondents') +
  xlab('Region') +
  geom_bar(
    position = 'dodge',
    stat = 'identity',
    color = 'black',
    size = 1
  ) +
  geom_bar(
    data =  q11Summary %>%
      mutate(tRespondents = sum(nRespondents)) %>%
      group_by(numberAcres) %>%
      summarize(pRespondents = sum(nRespondents) / unique(tRespondents) * 100) %>%
      ungroup %>%
      mutate(`Number of acres` = numberAcres),
    aes(x = 'Across regions', y = pRespondents, fill = `Number of acres`),
    position = 'dodge',
    stat = 'identity',
    color = 'black',
    size = 1
  ) +
  ggtitle('Number of acres of NN participant property') +
  theme_bw() +
  theme(
    axis.text = element_text(size = rel(1.25)),
    axis.title = element_text(size = rel(1.4)),
    plot.title = element_text(size = rel(1), hjust = .5)
  ) +
  scale_fill_brewer(palette = 'Set2')

ggsave('q11.png')
