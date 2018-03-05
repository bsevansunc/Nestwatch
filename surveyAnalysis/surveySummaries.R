#=================================================================================*
# ---- setup ----
#=================================================================================*

source('surveyCleaning.R')

# Participant info:

participantInfo <- surveyResults$q10 %>%
  left_join(surveyResults$q11, by = 'responseID') %>%
  mutate(
    numberYears = factor(numberYears, levels = c(1:10, 'over10')),
    numberAcres = factor(
      numberAcres,
      levels = c("<.1", ".1-.25", ".25-.5", ".5-1", "1-5", ">5")
    )
  ) %>%
  filter(!is.na(numberYears))

joinBind <- function(x){
  x %>%
    left_join(participantInfo, by = 'responseID') %>%
    bind_rows(x %>%
                left_join(participantInfo %>%
                            mutate(region = 'All regions'), by = 'responseID'))
}

#=================================================================================*
#  Q1: proportion of total yard area managed for wildlife ----
#=================================================================================*

q1Data <- surveyResults$q1 %>%
  left_join(participantInfo, by = 'responseID') %>%
  mutate(
    propYardManaged = factor(
      propYardManaged,
      levels = c('none', '<10', '11-25', '26-50', '51-75', '>75')
    ) 
  ) %>%
  filter(!is.na(propYardManaged)) %>%
  mutate(tRespondents = n())

#=================================================================================*
# Q2: bird friendly features and # of features in yards ----
#=================================================================================*

q2Data <- surveyResults$q2 %>%
  left_join(participantInfo, by = 'responseID') %>%
  mutate(tRespondents = length(unique(responseID)))

#=================================================================================*
# Q3,4: when and how regularly participants feed birds ----
#=================================================================================*

# Prepare data for plotting:

q3_4Data <- joinBind(surveyResults$q3to4) %>%
  mutate(
    whenFeedBirds = factor(
      whenFeedBirds,
      levels = c('never', 'summerOnly', 'winterOnly', 'allYear'),
      labels = c('Never', 'Summer only', 'Winter only', 'All year')
    ),
    howOftenFeedBirds = ifelse(whenFeedBirds == 'Never', 'Never', howOftenFeedBirds) %>%
      factor(
        levels = c(
          'Never',
          'everyTwoToThreeWeeks',
          'oneDayPerWeek',
          'aFewDaysPerWeek',
          'mostDaysPerWeek',
          'everyday'
        ),
        labels = c(
          'Never',
          'Every 2 to 3 weeks',
          'Once day a week',
          'A few days a week',
          'Most days',
          'Every day'
        )
      )
  ) %>%
  filter(!is.na(region))

# Plot how often participants feed birds:

q3_4Data %>%
  group_by(howOftenFeedBirds, region) %>%
  summarize(nRespondents = n()) %>%
  ungroup %>%
  group_by(region) %>%
  mutate(tRespondentsRegion = sum(nRespondents)) %>%
  ungroup %>%
  mutate(propRespondents = nRespondents/tRespondentsRegion*100) %>%
  ggplot(aes(x = howOftenFeedBirds, y = propRespondents)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~region) +
  scale_y_continuous(limits = c(0, 50), expand = c(0,0)) +
  ylab('Propotion of respondents (%)') +
  xlab('How often participants feed birds') +
  theme_bw()

# Plot when participants feed birds:

q3_4Data %>%
  group_by(whenFeedBirds, region) %>%
  summarize(nRespondents = n()) %>%
  ungroup %>%
  group_by(region) %>%
  mutate(tRespondentsRegion = sum(nRespondents)) %>%
  ungroup %>%
  mutate(propRespondents = nRespondents/tRespondentsRegion*100) %>%
  ggplot(aes(x = whenFeedBirds, y = propRespondents)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~region) +
  scale_y_continuous(limits = c(0, 100), expand = c(0,0)) +
  ylab('Propotion of respondents (%)') +
  xlab('When do participants feed birds') +
  theme_bw()

#=================================================================================*
# Q5,6: number of dogs and cats and time dogs/cats spend outside ----
#=================================================================================*

q5to6Data <- joinBind(surveyResults$q5to6) %>%
  filter(numberCats != '0', !(is.na(timeOutsideCat))) %>%
  mutate(timeOutsideCat = factor(
    levels = c(
      'Never',
      'everyTwoToThreeWeeks',
      'oneDayPerWeek',
      'aFewDaysPerWeek',
      'mostDaysPerWeek',
      'everyday'
    ),
    labels = c(
      'Never',
      'Every 2 to 3 weeks',
      'Once day a week',
      'A few days a week',
      'Most days',
      'Every day'
    )
  )

joinBind(surveyResults$q5to6) %>%
  filter(numberCats != '0', !(is.na(timeOutsideCat))) %>%
  group_by(timeOutsideCat, region) %>%
  summarize(nRespondents = n()) %>%
  ungroup %>%
  group_by(region) %>%
  mutate(tRespondentsRegion = sum(nRespondents)) %>%
  ungroup %>%
  mutate(propRespondents = nRespondents/tRespondentsRegion*100) %>%
  ggplot(aes(x = timeOutsideCat, y = propRespondents)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~region) +
  scale_y_continuous(limits = c(0, 100), expand = c(0,0)) +
  ylab('Propotion of respondents (%)') +
  xlab('How much time does your cat spend outside (hours per day)') +
  theme_bw()

#=================================================================================*
# Q7: whether nestwatch changed the way they manage their yard ----
#=================================================================================*

q7Data <- surveyResults$q7 %>%
  left_join(participantInfo, by = 'responseID') %>%
  mutate(tRespondents = length(unique(responseID)))

#=================================================================================*
# Q8: who participants talk to about birds in their yard and in what  ----
# geographic area do they talk to people about birds in their yard
#=================================================================================*

# Q8: who participants talk to about birds in their yard and in what geographic area do they talk to people about birds in their yard ----

q8Data <- surveyResults$q8 %>%
  left_join(participantInfo, by = 'responseID') %>%
  mutate(tRespondents = length(unique(responseID)))

#=================================================================================*
# Q9: who participants talk to about landscaping their yard and in what  ----
# geographic area do they talk to people about landscaping their yard
#=================================================================================*

q9Data <- surveyResults$q9 %>%
  left_join(participantInfo, by = 'responseID') %>%
  mutate(tRespondents = length(unique(responseID)))





#=================================================================================*
# ---- Research questions ----
#=================================================================================*

# Does length of time in NN influence proportion of yard managed for birds?
# How does yard size interact?

q1DataPrep <- surveyResults$q1 %>%
  left_join(surveyResults$q10, by = 'responseID') %>%
  left_join(surveyResults$q11, by = 'responseID') %>%
  mutate(
    propYardManaged = factor(
      propYardManaged,
      levels = c('none', '<10', '11-25', '26-50', '51-75', '>75')
    ),
    numberYears = factor(numberYears, levels = c(1:10, 'over10')),
    numberAcres = factor(
      numberAcres,
      levels = c("<.1", ".1-.25", ".25-.5", ".5-1", "1-5", ">5")
    )
  ) %>%
  filter(!is.na(numberYears), !is.na(propYardManaged))

tRespondents <- length(unique(q1DataPrep$responseID))


q1DataPrep %>%
  group_by(numberYears) %>%
  mutate(tSampleSize = n()) %>%
  group_by(numberYears, propYardManaged, tSampleSize) %>%
  summarize(n = n()) %>%
  ungroup %>%
  mutate(pSamples = n/tSampleSize) %>%
  # filter(region == 'Springfield') %>%
  ggplot(aes(x = numberYears, y = propYardManaged, size = pSamples)) +
  geom_point() +
  theme_bw()

# Does length of time in NN influence how often you talk to your neighbors / city / other city?

tRespondents <- length(unique(rawData$responseID))

surveyResults$q8 %>%
  mutate(tRespondents = tRespondents) %>%
  filter(location != 'iDontTalkAboutBirds') %>%
  filter(!(location == 'neighborhood' & peopleITalkToAboutBirdsInYard == 'nnParticipants')) %>%
  group_by(location, peopleITalkToAboutBirdsInYard, tRespondents) %>%
  summarize(nRespondents = n()) %>%
  ungroup %>%
  mutate(propRespondents = nRespondents/tRespondents*100) %>%
  # filter(peopleITalkToAboutBirdsInYard != 'other') %>%
  arrange(peopleITalkToAboutBirdsInYard) %>%
  ggplot(aes(x = location, y = propRespondents)) +
  geom_bar(stat = 'identity')+
  facet_wrap(~peopleITalkToAboutBirdsInYard) +
  theme_bw()

surveyResults$q9 %>%
  filter(!(location == 'neighborhood' & peopleITalkToAboutHowILandscapeYard == 'nnParticipants')) %>%
  mutate(tRespondents = tRespondents) %>%
  filter(location != 'iDontTalkAboutBirds') %>%
  group_by(location, tRespondents,peopleITalkToAboutHowILandscapeYard) %>%
  summarize(nRespondents = n()) %>%
  ungroup %>%
  mutate(propRespondents = nRespondents/tRespondents*100) %>%
  # filter(peopleITalkToAboutBirdsInYard != 'other') %>%
  # arrange(peopleITalkToAboutHowILandscapeYard) %>%
  ggplot(aes(x = location, y = propRespondents)) +
  geom_bar(stat = 'identity')+
  facet_wrap(~peopleITalkToAboutHowILandscapeYard) +
  theme_bw()

nResponseTime <- surveyResults$q10 %>%
  group_by(numberYears) %>%
  summarize(n = n())

surveyResults$q9 %>%
  filter(location != 'iDontTalkAboutBirds') %>%
  select(responseID, location) %>%
  filter(location != 'iDontTalkAboutYard') %>%
  distinct %>%
  left_join(surveyResults$q10, by = 'responseID') %>%
  group_by(location, numberYears) %>%
  summarize(nRespondents = n()) %>%
  ungroup %>%
  left_join(nResponseTime, by = 'numberYears') %>%
  mutate(propRespondents = nRespondents/n*100) %>%
  filter(!is.na(numberYears)) %>%
  mutate(numberYears = factor(numberYears, levels = c(1:10, 'over10'))) %>%
  ggplot(aes(x = numberYears, y = propRespondents)) +
  geom_bar(stat = 'identity')+
  facet_wrap(~location)+
  theme_bw()

nResponseTime <- surveyResults$q10 %>%
  filter(region == 'Springfield') %>%
  group_by(numberYears) %>%
  summarize(n = n())

surveyResults$q9 %>%
  filter(location != 'iDontTalkAboutBirds') %>%
  select(responseID, location) %>%
  filter(location != 'iDontTalkAboutYard') %>%
  distinct %>%
  left_join(surveyResults$q10, by = 'responseID') %>%
  filter(region ==  'Springfield') %>%
  group_by(location, numberYears) %>%
  summarize(nRespondents = n()) %>%
  ungroup %>%
  left_join(nResponseTime, by = 'numberYears') %>%
  mutate(propRespondents = nRespondents/n*100) %>%
  filter(!is.na(numberYears)) %>%
  mutate(numberYears = factor(numberYears, levels = c(1:10, 'over10'))) %>%
  ggplot(aes(x = numberYears, y = propRespondents)) +
  geom_bar(stat = 'identity')+
  facet_wrap(~location)+
  theme_bw()

#=================================================================================*
# ---- summary data ----
#=================================================================================*

# added column for proportion of respondents that gave each answer for each region

q1Summary <- surveyResults$q1 %>%
  left_join(surveyResults$q10 %>%
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
