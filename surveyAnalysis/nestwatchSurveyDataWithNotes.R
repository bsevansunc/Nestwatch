#========================================================================================*
# ---- setup ----
#========================================================================================*

library(tidyverse)
library(readr)
library(stringr)
library(lubridate)

select <- dplyr::select

options(stringsAsFactors = FALSE)

#----------------------------------------------------------------------------------------*
# ---- Get and clean data ----
#----------------------------------------------------------------------------------------*

# Load data and fixing responseID column name:

rawData <- read_csv('nestwatchSurveyRaw.csv') %>%
  rename(responseID = `Response ID`)

# Get rid of bad characters in column names:

names(rawData) <- str_replace_all(names(rawData), '\\?', '') %>%
  str_trim

# Generate a response ID table with all relevant identifying information

responseIdTable <- rawData %>%
  transmute(
    responseID  = responseID,
    dateSubmitted = `Date Submitted`,
    ipAddress = `IP Address`,
    longitude = Longitude,
    latitude = Latitude,
    city = City,
    state = `State/Region`,
    zip = Postal
    ) %>%
  separate(
    dateSubmitted,
    into = c('dateSubmitted', 'timeSubmitted'),
    sep = ' '
  ) %>%
  mutate(
    dateSubmitted = as.Date(dateSubmitted, '%m/%d/%Y'),
    zip = ifelse(str_count(zip) == 4,
                 paste0('0', zip),
                 zip)
    )

#========================================================================================*
# ---- question wrangling ----
#========================================================================================*

#----------------------------------------------------------------------------------------*
# ---- question 1 ----
#----------------------------------------------------------------------------------------*

# Generate table of proportion of total yard area managed for wildlife:

q1 <- rawData %>%
  select(responseID, propYardManaged = 22) %>%
  mutate(
    propYardManaged = str_trim(propYardManaged),
    propYardManaged = case_when(
      str_detect(propYardManaged, 'None') ~ 'none',
      str_detect(propYardManaged, 'less than a tenth') ~ '<10',
      str_detect(propYardManaged, 'between a tenth') ~ '11-25',
      str_detect(propYardManaged, 'between a quarter') ~ '26-50',
      str_detect(propYardManaged, 'half and three') ~ '51-75',
      str_detect(propYardManaged, 'More than') ~ '>75'
    )
  )

#----------------------------------------------------------------------------------------*
# ---- question 2 ----
#----------------------------------------------------------------------------------------*

# Generate longform table of bird friendly features and number of features in yards:

q2 <- rawData %>%
  select(c(1, 23:28)) %>%
  gather(key = object,
         value = number,
         2:7) %>%
  mutate(
    object = case_when(
      str_detect(object, 'Wildlife pond') ~ 'pond',
      str_detect(object, 'Bird bath') ~ 'birdBath',
      str_detect(object, 'Brush pile') ~ 'brushPile',
      str_detect(object, 'Nestbox') ~ 'nestbox',
      str_detect(object, 'Trees or bushes') ~ 'berryProducingTrees',
      str_detect(object, 'Bird feeder') ~ 'birdFeeder'
    ),
    number = ifelse(str_detect(number, '5 or more'), '5', number)
  )
  
#----------------------------------------------------------------------------------------*
# ---- questions 3 and 4 ----
#----------------------------------------------------------------------------------------*

# Generate table of when participants feed birds and how regularly they feed birds:

q3to4 <- rawData %>%
  select(1, whenFeedBirds = 29,  howOftenFeedBirds = 30) %>%
  mutate(
    whenFeedBirds = factor(whenFeedBirds),
    howOftenFeedBirds = factor(howOftenFeedBirds)
  )
  # mutate(
  #   whenFeedBirds = whenFeedBirds %>%
  #     str_replace_all('Only in winter', 'winterOnly') %>%
  #     str_replace_all('All year', 'allYear') %>%
  #     str_replace_all('Only in summer', 'summerOnly') %>%
  #     str_replace_all('Never', 'never'),
  #   howOftenFeedBirds = howOftenFeedBirds %>%
  #     str_replace_all('Every day', 'everyday') %>%
  #     str_replace_all('A few days per week', 'aFewDaysPerWeek') %>%
  #     str_replace_all('Every 2-3 weeks', 'everyTwoToThreeWeeks') %>%
  #     str_replace_all('Most days every week', 'mostDaysPerWeek') %>%
  #     str_replace_all('One day per week', 'oneDayPerWeek'),
  #   howOftenFeedBirds = ifelse(
  #     str_detect(howOftenFeedBirds, 'Not applicable'),
  #     NA,
  #     howOftenFeedBirds
  #   )
  # )

#----------------------------------------------------------------------------------------*
# ---- questions 5 and 6 ----
#----------------------------------------------------------------------------------------*

# Generate table of number of dogs and cats and time dogs/cats spend outside:
  
q5to6 <- rawData %>%
  select(1, numberCats = 31, numberDogs = 32,
         timeOutsideCat = 33, timeOutsideDog = 34) %>%
  mutate(
    numberCats = ifelse(str_detect(numberCats, '5 or more'), '5', numberCats),
    numberDogs = ifelse(str_detect(numberDogs, '5 or more'), '5', numberDogs),
    timeOutsideCat = ifelse(str_detect(timeOutsideCat, 'Not applicable'), NA, timeOutsideCat),
    timeOutsideDog = ifelse(str_detect(timeOutsideDog, 'Not applicable'), NA, timeOutsideDog),
    timeOutsideCat = factor(timeOutsideCat),
    timeOutsideDog = factor(timeOutsideDog)
  )
# 
#     timeOutsideCat = ifelse(str_detect(timeOutsideCat, 'Not applicable'), NA, timeOutsideCat),
#     timeOutsideCat = ifelse(str_detect(timeOutsideCat, '1 hour or'), '<1', timeOutsideCat),
#     timeOutsideCat = ifelse(str_detect(timeOutsideCat, '2-11 hours'), '2-11', timeOutsideCat),
#     timeOutsideCat = ifelse(str_detect(timeOutsideCat, '12 -24'), '12-24', timeOutsideCat),
#     timeOutsideCat = ifelse(str_detect(timeOutsideCat, 'go outside'), 'indoorOnly', timeOutsideCat),
#     timeOutsideDog = ifelse(str_detect(timeOutsideDog, 'Not applicable'), NA, timeOutsideDog),
#     timeOutsideDog = ifelse(str_detect(timeOutsideDog, '1 hour or'), '<1', timeOutsideDog),
#     timeOutsideDog = ifelse(str_detect(timeOutsideDog, '2-11 hours'), '2-11', timeOutsideDog),
#     timeOutsideDog = ifelse(str_detect(timeOutsideDog, '12 -24'), '12-24', timeOutsideDog),
#     timeOutsideDog = ifelse(str_detect(timeOutsideDog, 'go outside'), 'indoorOnly', timeOutsideDog)
#   )

#----------------------------------------------------------------------------------------*
# ---- question 7 ----
#----------------------------------------------------------------------------------------*

# Generate table of whether people agree or disagree that nestwatch changed the way they
# manage their yard:

q7 <- rawData %>%
  select(1,nestwatchChangedHowIManageYard = 35) %>%
  mutate(
    nestwatchChangedHowIManageYard = nestwatchChangedHowIManageYard %>%
    str_replace_all('Strongly agree', 'stronglyAgree') %>%
    str_replace_all('Somewhat agree', 'somewhatAgree') %>%
    str_replace_all('Neither agree or disagree', 'neitherAgreeOrDisagree') %>%
    str_replace_all('Somewhat disagree', 'somewhatDisagree') %>%
    str_replace_all('Strongly disagree', 'stronglyDisagree')
  )

#----------------------------------------------------------------------------------------*
# ---- question 8 ----
#----------------------------------------------------------------------------------------*

# Generate longform table of who participants talk to about birds in their yard and 
# in what geographic area do they talk to people about birds in their yard:

q8 <- rawData %>%
  select(1,36:143) %>%
  gather(key = peopleITalkToAboutBirdsInYard,
         value = location,
         2:109) %>%
  filter(location != '<NA>') %>%
  
  mutate(
    peopleITalkToAboutBirdsInYard = ifelse(str_detect(peopleITalkToAboutBirdsInYard,
                                                      'Other Neighborhood Nestwatch participants'),
                                           'nnParticipants',
                                           peopleITalkToAboutBirdsInYard),
    peopleITalkToAboutBirdsInYard = ifelse(str_detect(peopleITalkToAboutBirdsInYard,
                                                      'not Neighborhood Nestwatch participants'),
                                           'friends',
                                           peopleITalkToAboutBirdsInYard),
    peopleITalkToAboutBirdsInYard = ifelse(str_detect(peopleITalkToAboutBirdsInYard,
                                                      'non Neighborhood Nestwatch participants'),
                                           'family',
                                           peopleITalkToAboutBirdsInYard),
    peopleITalkToAboutBirdsInYard = peopleITalkToAboutBirdsInYard %>%
      str_replace_all('In my neighborhood:', '') %>% 
      str_replace_all('In my city or town:', '') %>%
      str_replace_all('In other cities or towns:', '') %>%
      str_replace_all(':Please check the boxes if you talk with the following groups of people about birds you observe in your yard:', '')
  ) %>%
  mutate(
    location = ifelse(str_detect(location, 't talk with anyone about birds in my yard'), 'iDontTalkAboutBirdsInYard', location),
    location = ifelse(str_detect(location, 'In my neighborhood'), 'inMyNeighborhood', location),
    location = ifelse(str_detect(location, 'In other cities or towns'), 'inOtherCitiesOrTowns', location),
    location = ifelse(str_detect(location, 'In my city or town'), 'inMyCityOrTown', location)    
  ) %>%
arrange(responseID)

#----------------------------------------------------------------------------------------*
# ---- question 9 ----
#----------------------------------------------------------------------------------------*

# generating longform table of who participants talk to about landscaping their yard and 
# in what geographic area do they talk to people about landscaping their yard

q9 <- rawData %>%
  select(1, 144:195) %>%
  gather(key = peopleITalkToAboutHowILandscapeYard,
         value = location,
         2:53) %>%
  filter(location != '<NA>') %>%
  mutate(
    peopleITalkToAboutHowILandscapeYard = ifelse(str_detect(peopleITalkToAboutHowILandscapeYard,
                                                      'Other Neighborhood Nestwatch participants'),
                                           'nnParticipants',
                                           peopleITalkToAboutHowILandscapeYard),
    peopleITalkToAboutHowILandscapeYard = ifelse(str_detect(peopleITalkToAboutHowILandscapeYard,
                                                      'not Neighborhood Nestwatch participants'),
                                           'friends',
                                           peopleITalkToAboutHowILandscapeYard),
    peopleITalkToAboutHowILandscapeYard = ifelse(str_detect(peopleITalkToAboutHowILandscapeYard,
                                                      'non Neighborhood Nestwatch participants'),
                                           'family',
                                           peopleITalkToAboutHowILandscapeYard),
    peopleITalkToAboutHowILandscapeYard = peopleITalkToAboutHowILandscapeYard %>%
      str_replace_all('In my neighborhood:', '') %>% 
      str_replace_all('In my city or town:', '') %>%
      str_replace_all('In other cities or towns:', '') %>%
      str_replace_all(':Please check the boxes if you talk with the following groups of people about how you landscape or manage your yard:', '')
  ) %>%
  mutate(
    location = ifelse(str_detect(location, 't talk with anyone about how I landscape my yard'), 'iDontTalkAboutYard', location),
    location = ifelse(str_detect(location, 'In my neighborhood'), 'inMyNeighborhood', location),
    location = ifelse(str_detect(location, 'In other cities or towns'), 'inOtherCitiesOrTowns', location),
    location = ifelse(str_detect(location, 'In my city or town'), 'inMyCityOrTown', location)    
  ) %>%
  arrange(responseID)

#----------------------------------------------------------------------------------------*
# ---- question 10 ----
#----------------------------------------------------------------------------------------*

# generating table of what region participants are from and how long they have participated

q10 <- rawData %>%
  select(1, 196:198) %>%
  gather(key = region, 
         value = numberYears, 
         2:4) %>%
  mutate(
    region = ifelse(str_detect(region, 'Springfield, MA'), 'Springfield', region),
    region = ifelse(str_detect(region, 'Raleigh, NC'), 'Raleigh', region),
    region = ifelse(str_detect(region, 'DC Metro area'), 'DC', region)
  ) %>%
  filter(numberYears != '<NA>') %>%
  mutate(
    numberYears = numberYears %>%
      str_replace_all('1 year', '1') %>%
      str_replace_all('2 years', '2') %>%
      str_replace_all('3 years', '3') %>%
      str_replace_all('4 years', '4') %>%
      str_replace_all('5 years', '5') %>%
      str_replace_all('6 years', '6') %>%
      str_replace_all('7 years', '7') %>%
      str_replace_all('8 years', '8') %>%
      str_replace_all('9 years', '9') %>%
      str_replace_all('10 years', '10') %>%
      str_replace_all('More than 10', 'over10')
  ) %>%
  arrange(responseID) %>%
  filter(!(responseID == '2' & region == 'Raleigh')) %>%
  filter(!(responseID == '2' & region == 'Springfield'))

#----------------------------------------------------------------------------------------*
# ---- question 11 ----
#----------------------------------------------------------------------------------------*

# generating table with number of acres of participants' properties

q11 <- rawData %>%
  transmute(
    responseID = responseID,
    numberAcres = `How many acres is your property (1 acre is a little smaller than a football field)`
  ) %>%
  mutate(
    numberAcres = numberAcres %>%
      str_replace_all('Less than a tenth of an acre', '<.1') %>%
      str_replace_all('A tenth of an acre to a quarter acre', '.1-.25') %>%
      str_replace_all('Quarter acre to half acre', '.25-.5') %>%
      str_replace_all('Half acre to 1 acre', '.5-1') %>%
      str_replace_all('1 to 5 acres', '1-5') %>%
      str_replace_all('Larger than 5 acres', '>5')
  ) %>%
  mutate(
    numberAcres = ifelse(str_detect(numberAcres, 'I don'), 'unknown', numberAcres)
  )


#----------------------------------------------------------------------------------------*
# ---- Compile tidy tables to list ----
#----------------------------------------------------------------------------------------*

surveyResults <- list(responseIdTable = responseIdTable, 
                      q1 = q1, q2 = q2, q3to4 = q3to4,
                      q5to6 = q5to6, q7 = q7, q8 = q8, 
                      q9 = q9, q10 = q10, q11 = q11)
  
#=================================================================================*
# ---- summary data ----
#=================================================================================*

# joinging responseID table with q10 to get regional hub column

responseIdTable %>%
  left_join(q10, by = 'responseID') %>%
  select(-numberYears)

# added column for proportion of respondents that gave each answer for each region

q1Summary <- surveyResults$q1 %>%
  left_join(q10 %>%
              select(-numberYears),
            by = 'responseID') %>%
  filter(!is.na(propYardManaged), !is.na(region)) %>%
  mutate(tRespondents = length(unique(responseID))) %>%
  group_by(region) %>%
  mutate(tRespondentsRegion = length(unique(responseID))) %>%
  group_by(region, propYardManaged) %>%
  summarize(
    nRespondents = n(),
    pRespondents = n()/unique(tRespondentsRegion) * 100) %>%
  ungroup %>%
  mutate(
    propYardManaged = factor(
      propYardManaged, 
      levels = c('>75','51-75','26-50','11-25','<10','none')
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
  geom_bar(data =  q1Summary %>%
             mutate(tRespondents = sum(nRespondents)) %>%
             group_by(propYardManaged) %>%
             summarize(pRespondents = sum(nRespondents)/unique(tRespondents) * 100) %>%
             ungroup %>%
             mutate(`Proportion of yard` = propYardManaged),
           aes(x = 'Across regions', y = pRespondents, fill = `Proportion of yard`),
               stat = 'identity', color = 'black', size = 1) +
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
            p = sum(number)/unique(tRespondents)) %>%
  ungroup %>%
  mutate(region = factor(region))

# graph of proportion of respondents with each feature by region

q2Summary %>%
  mutate(
    Feature = factor(object, labels = c(
      'Berry-producing trees', 'Bird bath', 'Bird feeder', 'Brush pile', 'Nestbox', 'Pond'
    ))
  ) %>%
  ggplot(aes(x = region, y = p, fill = Feature)) +
  geom_bar(position = 'dodge', stat = 'identity', color = 'black', size = 1) + 
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
            p = n()/unique(nRespondents)*100) %>%
  ungroup %>%
  bind_rows(data.frame(
    region = 'Raleigh', whenFeedBirds = 'summerOnly', n = 0, p = 0
  )) %>%
  mutate(
    whenFeedBirds = factor(whenFeedBirds, 
                           levels = c('never', 'summerOnly', 'winterOnly', 'allYear'),
                           labels = c(
                             'Never', 'Summer only', 'Winter only', 'All year'
                           ))
  )

# bar graph of if/when people feed birds by region

q3to4Summary %>%
  rename(`When birds are fed` = whenFeedBirds) %>%
  ggplot(aes(x = region, y = p, fill = `When birds are fed`)) +
  geom_bar(position = 'dodge', stat = 'identity', color = 'black', size = 1) + 
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
  mutate(cat = ifelse(timeOutsideCat == 'indoorOnly', 'indoorCat', 'outdoorCat'),
         cat = ifelse(numberCats == 0|is.na(numberCats), 'noCat', cat),
         cat = factor(cat, 
                      levels = c('noCat', 'indoorCat', 'outdoorCat'),
                      labels = c('No cat', 'Indoor-only cat', 'Outdoor cat'))) %>%
  filter(!(numberCats > 0 & is.na(timeOutsideCat))) %>%
  group_by(region) %>%
  mutate(nRespondents = n()) %>%
  group_by(region, cat) %>%
  summarize(n = n(),
            p = n()/unique(nRespondents)*100) %>%
  ungroup

# stacked bar graph of cat ownership and indoor/outdoor cats by region

q56cat %>%
  rename(`Cat ownership` = cat) %>%
  ggplot(aes(x = region, y = p, fill = `Cat ownership`)) +
  ylab('Proportion of respondents') +
  geom_bar(stat = 'identity', color = 'black', size = 1, width = .5) +
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
  mutate(p = n/nRespondents * 100) %>%
  ggplot(aes(x = region, y = p)) +
  ylab('Proportion of respondents') +
  geom_bar(stat = 'identity', color = 'black', size = 1, width = .5) +
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
  mutate(dog = ifelse(timeOutsideDog == 'indoorOnly', 'indoorDog', 'outdoorDog'),
         dog = ifelse(numberDogs == 0|is.na(numberDogs), 'noDog', dog),
         dog = factor(dog, 
                      levels = c('noDog', 'indoorDog', 'outdoorDog'),
                      labels = c('No dog', 'On leash-only dog', 'Sometimes off-leash dog'))) %>%
  filter(!(numberDogs > 0 & is.na(timeOutsideDog))) %>%
  group_by(region) %>%
  mutate(nRespondents = n()) %>%
  group_by(region, dog) %>%
  summarize(n = n(),
            p = n()/unique(nRespondents)*100)

# stacked bar graph of proportion of participants with dog and if the dog is kept on or off leash by region

q56dog %>%
  rename(`Dog ownership` = dog) %>%
  ggplot(aes(x = region, y = p, fill = `Dog ownership`)) +
  ylab('Proportion of respondents') +
  geom_bar(stat = 'identity', color = 'black', size = 1, width = .5) +
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
  mutate(p = n/nRespondents * 100) %>%
  ggplot(aes(x = region, y = p)) +
  ylab('Proportion of respondents') +
  geom_bar(stat = 'identity', color = 'black', size = 1, width = .5) +
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
  filter(numberDogs > 0, !is.na(timeOutsideDog)) %>%
  group_by(timeOutsideDog) %>%
  summarize(n = sum(n))

# generating table of number and proportion of respondents who 
# agree or disagree that nestwatch changed how they manage their yard

q7Summary <- surveyResults$q7 %>%
  left_join(q10 %>%
              select(-numberYears),
            by = 'responseID') %>%
  filter(!is.na(nestwatchChangedHowIManageYard), !is.na(region)) %>%
  mutate(tRespondents = length(unique(responseID))) %>%
  group_by(region) %>%
  mutate(tRespondentsRegion = length(unique(responseID))) %>%
  group_by(region, nestwatchChangedHowIManageYard) %>%
  summarize(
    nRespondents = n(),
    pRespondents = n()/unique(tRespondentsRegion) * 100) %>%
  ungroup %>%
  mutate(
    nestwatchChangedHowIManageYard = factor(
      nestwatchChangedHowIManageYard, 
      levels = c('stronglyAgree','somewhatAgree','neitherAgreeOrDisagree','somewhatDisagree','stronglyDisagree'),
      labels = c('Strongly agree','Somewhat agree','Neither agree nor disagree','Somewhat disagree','Strongly disagree')
    ))

# bar graph of proportion of respondents who agree or disagree with statement "nestwatch changed how i manage my yard

q7Summary %>%
  mutate(`Level of agreement` = nestwatchChangedHowIManageYard,
         `Region` = factor(region)) %>%
  ggplot(aes(x = `Region`, y = pRespondents, fill = `Level of agreement`)) +
  ylab('Proportion of respondents') +
  xlab('Region') +
  geom_bar(position = 'dodge', stat = 'identity', color = 'black', size = 1) +
  geom_bar(data =  q7Summary %>%
             mutate(tRespondents = sum(nRespondents)) %>%
             group_by(nestwatchChangedHowIManageYard) %>%
             summarize(pRespondents = sum(nRespondents)/unique(tRespondents) * 100) %>%
             ungroup %>%
             mutate(`Level of agreement` = nestwatchChangedHowIManageYard),
           aes(x = 'Across regions', y = pRespondents, fill = `Level of agreement`),
           position = 'dodge', stat = 'identity', color = 'black', size = 1) +
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
            p = n()/unique(nRespondents) * 100) %>%
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
  geom_bar(position = 'dodge', stat = 'identity', color = 'black', size = 1) + 
  ylab('Proportion of respondents') +
  xlab('Region') +
  ggtitle('Who do participants communicate with regarding birds?') +
  theme_bw() +
  facet_wrap(~location)+
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
            p = n()/unique(nRespondents) * 100) %>%
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
  geom_bar(position = 'dodge', stat = 'identity', color = 'black', size = 1) + 
  ylab('Proportion of respondents') +
  xlab('Region') +
  ggtitle('Who do participants communicate with regarding landscaping their yard?') +
  theme_bw() +
  facet_wrap(~location)+
  theme(
    axis.text = element_text(size = rel(1.25)),
    axis.title = element_text(size = rel(1.4)),
    plot.title = element_text(size = rel(1.7), hjust = .5)
  ) +
  scale_fill_brewer(palette = 'Set2')

ggsave('q9.png')

# generating table of number and proportion of respondents who have been in the program for x number of years by region

q10Summary <- surveyResults$q10 %>%
  filter(!is.na(region), !is.na(numberYears)) %>%
  mutate(tRespondents = length(unique(responseID))) %>%
  group_by(region) %>%
  mutate(tRespondentsRegion = length(unique(responseID))) %>%
  group_by(region, numberYears) %>%
  summarize(
    nRespondents = n(),
    pRespondents = n()/unique(tRespondentsRegion) * 100) %>%
  ungroup

# generating a table of number and proportion of respondents with different size yards for all regions

q11Summary <- surveyResults$q11 %>%
  left_join(q10 %>%
              select(-numberYears),
            by = 'responseID') %>%
  filter(!is.na(numberAcres), !is.na(region)) %>%
  mutate(tRespondents = length(unique(responseID))) %>%
  group_by(region) %>%
  mutate(tRespondentsRegion = length(unique(responseID))) %>%
  group_by(region, numberAcres) %>%
  summarize(
    nRespondents = n(),
    pRespondents = n()/unique(tRespondentsRegion) * 100) %>%
  ungroup %>%
  mutate(
    numberAcres = factor(
      numberAcres, 
      levels = c('<.1','.1-.25','.25-.5','.5-1','1-5', '>5', 'unknown')
    ))

# bar graph of number of acres of participant's properties for all regions

q11Summary %>%
  mutate(`Number of acres` = numberAcres,
         `Region` = factor(region)) %>%
  ggplot(aes(x = `Region`, y = pRespondents, fill = `Number of acres`)) +
  ylab('Proportion of respondents') +
  xlab('Region') +
  geom_bar(position = 'dodge', stat = 'identity', color = 'black', size = 1) +
  geom_bar(data =  q11Summary %>%
             mutate(tRespondents = sum(nRespondents)) %>%
             group_by(numberAcres) %>%
             summarize(pRespondents = sum(nRespondents)/unique(tRespondents) * 100) %>%
             ungroup %>%
             mutate(`Number of acres` = numberAcres),
           aes(x = 'Across regions', y = pRespondents, fill = `Number of acres`),
           position = 'dodge', stat = 'identity', color = 'black', size = 1) +
  ggtitle('Number of acres of NN participant property') +
  theme_bw() +
  theme(
    axis.text = element_text(size = rel(1.25)),
    axis.title = element_text(size = rel(1.4)),
    plot.title = element_text(size = rel(1), hjust = .5)
  ) +
  scale_fill_brewer(palette = 'Set2')

ggsave('q11.png')
  