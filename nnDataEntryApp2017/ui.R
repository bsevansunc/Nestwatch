library(shiny)
library(DT)

source('setUp.R', local=TRUE)

#=================================================================================*
# ---- USER INTERFACE ----
#=================================================================================*

shinyUI(
  navbarPage(
    title = p(
      h4(strong("Neighborhood Nestwatch technician data submission interface")),
      br()
    ),
    windowTitle = 'Neighborhood Nestwatch Technician data submission',
    header = div(id = 'header',
                 sidebarLayout(
                   sidebarPanel(
                     fluidRow(
                       column(3, selectInput('hub','Regional Hub:', choiceRegions)),
                       column(3, selectInput('inputType', 'New or existing site:',
                                             c('Existing site',
                                               'New site'))),
                       column(3, uiOutput("ui")),
                       column(2, dateInput('date', 'Date of visit'))
                     ), width = 8
                   ),
                   mainPanel(
                     fluidRow(
                       column(4, ''),
                       column(8, img(src = 'nnBirdBranchImage.png'))
                     ), width = 4),
                   position = 'left'),
                 hr()
    ),
    footer = p(
      br(),
      hr(),
      fluidRow(
        column(4, ''),
        column(4, img(src = 'logo.png')), 
        column(4, '')
      )
    ),
    inverse = TRUE,
    #-------------------------------------------------------------------------------*
    # ---- UI TAB PANEL: VISIT DATA ----
    #-------------------------------------------------------------------------------*
    tabPanel(strong('Visit data'),
             sidebarLayout(
               sidebarPanel(
                 h4(strong('Location:')),
                 br(),
                 fluidRow(
                   column(4, 
                          textInput('longSite', 
                                    'Longitude (decimal degrees):')),
                   column(4, 
                          textInput('latSite', 
                                    'Latitude (decimal degrees):')),
                   column(4,
                          textInput('accuracySite', 'Accuracy (m):'))
                 ),
                 fluidRow(
                   column(8, textInput('addressSite', 'Address (street, city, state, zip):', '')),
                   column(4, textInput('startYearSite', 'Start year', ''))
                 ),
                 fluidRow(
                   column(12, textInput('locationNotes', 
                                        label = 'Location notes:'))),
                 
                 hr(),
                 h4(strong('Contact info')),
                 br(),
                 fluidRow(
                   column(6, textInput('lastNameOrSchoolSite', 'Last name or name of school:')),
                   column(6, textInput('firstNameSite', 'First name:'))
                 ),
                 fluidRow(
                   column(6, textInput('phone1Site', 'Primary phone number:')),
                   column(6, textInput('phone2Site', 'Secondary phone number:'))
                 ),
                 fluidRow(
                   column(6, textInput('email1Site', 'Primary email:')),
                   column(6, textInput('email2Site', 'Secondary email:'))
                 ),
                 fluidRow(
                   column(12, textInput('notesSite', 'Site notes:'))
                 ),
                 br(),
                 fluidRow(column(6, ''),
                          column(3, actionButton('submitSite', 
                                                 'Add record to table',
                                                 class = "btn-primary"))),
                 width = 6, position = 'left'),
               mainPanel(
                 h4(strong('Start by proofing or entering site data')),
                 textSite, width = 6, position = 'right')
             ),
             h3(strong('Data-proofing and submission of site records:')),
             br(),
             DT::dataTableOutput("responsesSite"),
             br(),
             fluidRow(column(1, ''),
                      column(4, actionButton("deleteSite",
                                             "Delete site record", 
                                             class = "btn-primary")),
                      column(3, ' '),
                      column(4, actionButton('submitSiteData', 
                                             'Submit site data',
                                             class = "btn-primary"))
             ),
             br(), shinyjs::hidden(
               div(
                 id = "thankyou_msgSite",
                 h3("Thanks, your site data have been recorded!")
               )
             ),
             br()
    ),
    #-------------------------------------------------------------------------------*
    # ---- UI TAB PANEL: VISIT ----
    #-------------------------------------------------------------------------------*
    tabPanel(strong('Visit data'),
             sidebarLayout(
               sidebarPanel(
                 br(),
                 # ---- Site and observer -----------------------------------------
                 h4(strong('Visit:')),
                 br(),
                 fluidRow(
                   column(6, textInput("observer", "Observer(s) initials:")),
                   column(6, '')
                 ),
                 br(),
                 h5(strong('Participant engagement')),
                 fluidRow(
                   column(4, selectInput('engagement', '', 0:4)),
                   column(8, '')
                 ),
                 fluidRow(
                   column(12, '0 = No response to attempts at communication, resulting in no banding visit')),
                 fluidRow(
                   column(12, '1 = Participant was not present or showed no interest during visit activities')),
                 fluidRow(
                   column(12, '2 = Participant showed only slight interest during visit activities')),
                 fluidRow(
                   column(12, '3 = Participant showed some interest during visit activities')),
                 fluidRow(
                   column(12, '4 = Participant showed genuine interest and actively participated in visit activities')),
                 br(),
                 fluidRow(
                   column(4, selectInput('encounteredBirds',
                                         label = 'Did you band or encounter a banded bird during your visit?',
                                         choices = c('Yes', 'No'),
                                         selected = 'Yes')),
                   column(8, '')
                 ),
                 br(),
                 fluidRow(column(12, textInput('notesVisit', 
                                               label = 'Visit notes:'))),
                 hr(),
                 # ---- Banding effort --------------------------------------------
                 h4(strong('Banding effort:')),
                 h5(em(strong('Net hours calculator.'),
                       'Enter the total minutes that each length of net was open (across nets). The net hours field will automatically be calculated from these values.')),
                 br(),
                 h5(strong('Total time, in minutes, that nets of a given length were open:')),
                 fluidRow(
                   column(3, numericInput('netMinutes6', 
                                          '6 m nets', 
                                          0)),
                   column(3, numericInput('netMinutes9', 
                                          '9 m nets', 
                                          0)),
                   column(3, numericInput('netMinutes12', 
                                          '12 m nets', 
                                          0)),
                   column(3, numericInput('netMinutes18', 
                                          '18 m nets', 
                                          0))),
                 shinyjs::hidden(fluidRow(
                   column(6, numericInput('netHours','Net hours:', 0)),
                   column(6, '')
                 )),
                 hr(),
                 # ---- Resight effort --------------------------------------------
                 h4(strong('Resight effort:')),
                 br(),
                 fluidRow(
                   column(3, selectizeInput('rsStart', 'Resight, start time:', 
                                            choices = choiceTimeOfDay)),
                   column(3, selectizeInput('rsEnd', 'Resight, end time:', 
                                            choices = choiceTimeOfDay)),
                   column(3, numericInput('rsTime',
                                          'Resight time (min.)', 60)),
                   column(3, textInput('pathDistance',
                                       'Path distance (m):'))),
                 br(),
                 h5(strong('Observations of unbanded birds:')),
                 fluidRow(column(6,
                                 selectizeInput('spUnbanded', label = 'Species',
                                                choiceSpecies)),
                          column(6, numericInput('countUnbanded',
                                                 label = 'Count', 0))),
                 br(),
                 fluidRow(column(1, ''),
                          column(3, actionButton("newVisit", "Clear fields",
                                                 class = 'btn-primary')),
                          column(2, ''),
                          column(3, actionButton('submitVisit', 'Add record to table',
                                                 class = "btn-primary")),
                          column(3, '')),
                 br(),
                 width = 6, position = 'left'),
               # ---- Visit text ----------------------------------------------------
               mainPanel(
                 h3('Visit data'),
                 textVisit, width = 6, position = 'right')
             ),
             hr(),
             # ---- QC and submission ---------------------------------------------------
             h3(strong('2. Data-proofing and submission of visit records:')),
             br(),
             DT::dataTableOutput("responsesVisit"),
             br(),
             fluidRow(column(1, ''),
                      column(4, actionButton("deleteVisit", "Delete record", 
                                             class = "btn-primary")),
                      column(3, ' '),
                      column(4, actionButton('submitVisitData', 
                                             'Submit visit data',
                                             class = "btn-primary"))
             ),
             br(), 
             shinyjs::hidden(
               div(
                 id = "thankyou_msgVisit",
                 h3("Thanks, your visit data have been recorded!"),
                 'To submit data for another site, please refresh your browser')
             ),
             br()
    ),
    #-------------------------------------------------------------------------------*
    # ---- UI TAB PANEL: ENCOUNTERS ----
    #-------------------------------------------------------------------------------*
    tabPanel(strong('Encounter data'),
             div(id = 'encounterData', 
                 sidebarLayout(
                   # ---- Record entry -------------------------------------------------*
                   sidebarPanel(
                     shinyjs::useShinyjs(),
                     h3(strong('1. Enter encounter record:')),
                     strong(em('IMPORTANT! Be sure to enter encounter data AFTER entering visit data!')),
                     br(), br(),
                     fluidRow(
                       column(4, selectizeInput('timeEnc', 'Time:',
                                                choices = choiceTimeOfDay)),
                       column(4, textInput('observerEnc', 'Observer initials:')),
                       column(4, selectizeInput('encounterType', 
                                                'Encounter type:',
                                                choices = choiceEncounterType, 
                                                selected = 'Band'))),
                     br(),
                     fluidRow(
                       column(4, selectizeInput('speciesEnc', 
                                                'Species:',
                                                choices = c('',justAlphaCode))),
                       column(4, textInput('bandNumber', 'Band number:')),
                       column(4, selectizeInput('colorCombo', 
                                                'Color combination:',
                                                choices = choiceColorCombos))),
                     hr(),
                     h4(strong('Color bands:')),
                     fluidRow(
                       column(3, p(strong('- :'), 'No band')),
                       column(3, p(strong('A :'), 'Aluminum')),
                       column(3, p(strong('BK :'), 'Black')),
                       column(3, p(strong('BU :'), 'Blue'))
                     ),
                     fluidRow(
                       column(3, p(strong('BR :'), 'Brown')),
                       column(3, p(strong('GY :'), 'Gray')),
                       column(3, p(strong('G :'), 'Green')),
                       column(3, p(strong('O :'), 'Orange'))
                     ),
                     fluidRow(
                       column(3, p(strong('PK :'), 'Pink')),
                       column(3, p(strong('P :'), 'Purple')),
                       column(3, p(strong('P :'), 'Purple')),
                       column(3, p(strong('R :'), 'Red'))
                     ),
                     fluidRow(
                       column(3, p(strong('Y :'), 'Yellow')),
                       column(3, p(strong('W :'), 'White')),
                       column(6, ' ')
                     ),
                     hr(),
                     fluidRow(
                       column(3, selectizeInput('age','Age:',
                                                choices = choiceAge)),
                       column(3, selectizeInput('sex', label = 'Sex:',
                                                choices = choiceSex)),
                       column(3, selectizeInput('breedingCond', label = 'CP/BP:',
                                                choices = choiceBreedingCond)),
                       column(3, selectizeInput('fat',label = 'Fat:',
                                                choices = choiceFat))),
                     br(),
                     fluidRow(
                       column(3, textInput('mass',label = 'Mass (g):')),
                       column(3, textInput('wing',label = 'Wing (mm):')),
                       column(3, textInput('tl',label = 'Tail (mm):')),
                       column(3, textInput('tarsus',label = 'Tarsus (mm):'))),
                     br(),
                     fluidRow(
                       column(6, textInput('featherID', label = 'Feather ID:')),
                       column(6, textInput('toenailID', label = 'Toenail ID:'))),
                     br(),
                     fluidRow(
                       column(6, textInput('bloodID', label = 'Blood ID:')),
                       column(6, textInput('fecalID', label = 'Fecal ID:'))),
                     br(),
                     fluidRow(
                       column(6, textInput('attachmentID',
                                           label = 'Attachment ID (e.g., transmitter or geolocator):')),
                       column(6, ' ')),
                     br(),
                     fluidRow(
                       column(6, textInput('rsLong', 'Resight longitude:')),
                       column(6, textInput('rsLat', 'Resight latitude:'))
                     ),
                     br(),
                     fluidRow(
                       column(12, textInput('notesEnc', label = 'Notes:'))),
                     br(),
                     fluidRow(column(1, ''),
                              column(3, actionButton("newEnc", "Clear fields",
                                                     class = 'btn-primary')),
                              column(2, ''),
                              column(3, actionButton('submitEnc', 'Add record to table',
                                                     class = "btn-primary")),
                              column(3, '')),
                     br(),
                     width = 6, position = 'right'),
                   # ---- Encounter text ----------------------------------------------------
                   mainPanel(
                     introTextEncounter,
                     hr(),
                     fieldDescriptionsEncounter,
                     width = 6, position = 'left')
                 ),
                 hr(),
                 # ---- QC and submission ---------------------------------------------------
                 h3(strong('2. Data-proofing and submission of encounter records:')),
                 br(),
                 DT::dataTableOutput("responsesEnc"),
                 br(),
                 fluidRow(column(1, ''),
                          column(4, actionButton("deleteEnc", "Delete record", 
                                                 class = "btn-primary")),
                          column(3, ' '),
                          column(4, actionButton('submitEncData', 
                                                 'Submit encounter data',
                                                 class = "btn-primary"))
                 ),
                 br(), 
                 shinyjs::hidden(
                   div(
                     id = "thankyou_msgEnc",
                     h3("Thanks, your encounter data have been recorded!"),
                     'To submit data for another site, please refresh your browser')
                 ),
                 br()
             )),
    #--------------------------------------------------------------------------------*
    # ---- UI TAB PANEL: QUERY RECORDS ----
    #--------------------------------------------------------------------------------*
    tabPanel(strong('Query encounter records'),
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   column(7, checkboxInput('showAllSites',
                                           'Show all sites',
                                           value = TRUE)),
                   column(5, '')
                 ),
                 hr(),
                 fluidRow(
                   column(7, h5(strong('Species:'))),
                   column(5, checkboxInput('showAllSpecies',
                                           'Show all',
                                           value = TRUE))),
                 fluidRow(
                   column(10, selectInput('speciesQuery', '', '')),
                   column(2, '')),
                 hr(),
                 fluidRow(
                   column(7, h5(strong('Sex:'))),
                   column(5, checkboxInput('showAllSex',
                                           'Show all',
                                           value = TRUE))),
                 fluidRow(
                   column(10, selectInput('sexQuery', '', 
                                          choices = choiceSex,
                                          selected = 'ALL')),
                   column(2, '')),
                 hr(),
                 fluidRow(
                   column(7, h5(strong('Color combo:'))),
                   column(5, checkboxInput('showAllBandCombo',
                                           'Show all',
                                           value = TRUE))),
                 fluidRow(
                   column(10, textInput('colorComboQuery','',  'ALL')),
                   column(2, '')),
                 hr(),
                 fluidRow(
                   column(7, h5(strong('Band number:'))),
                   column(5, checkboxInput('showAllBandNumber',
                                           'Show all',
                                           value = TRUE))),
                 fluidRow(
                   column(10, textInput('bandNumberQuery', '')),
                   column(2, '')),
                 hr(),
                 fluidRow(
                   column(7, h5(strong('Encounter type:'))),
                   column(5, checkboxInput('showAllEncounterType',
                                           'Show all',
                                           value = TRUE))),
                 fluidRow(
                   column(10, 
                          selectizeInput('encounterTypeQuery', '',
                                         choices = choiceEncounterType)),
                   column(2, '')),
                 fluidRow(
                   column(12, downloadButton('downloadData', 'Download encounter data'))
                 ),
                 width = 3, position = 'left'),
               mainPanel(
                 textQuery,
                 hr(),
                 DT::dataTableOutput('encounterTable'),
                 width = 9, position = 'right')
             )),
    #--------------------------------------------------------------------------------*
    # ---- UI TAB PANEL: POINT COUNT ----
    #--------------------------------------------------------------------------------*
    tabPanel(strong('Point count data'),
             sidebarLayout(
               sidebarPanel(
                 div(id = 'pcData',
                     # ---- Point count entry ------------------------------------------
                     h3(strong('1. Add point count records:')),
                     strong(em('IMPORTANT! Be sure to enter point count data AFTER entering visit data!')),
                     br(), br(),
                     fluidRow(
                       column(4, textInput('observerPc', 'Observer:')),
                       column(4, selectizeInput('startTimePc', 'Start time:',
                                                choices = choiceTimeOfDay)),
                       column(4, '')
                     ), 
                     hr(),
                     fluidRow(column(12, textInput('notesPc', 
                                                   label = 'Point count notes:'))),
                     hr(),
                     textAouQuery,
                     fluidRow(column(11, DT::dataTableOutput('aouTable'))),
                     hr(),
                     fluidRow(
                       column(2, selectizeInput('timePc', 'Time:',
                                                choices = choiceTime)),
                       column(3, selectizeInput('speciesPc', 
                                                'Species:',
                                                choices = c('',justAlphaCode))),
                       column(2, selectizeInput('distancePc', 'Distance:',
                                                choices = choiceDistance)),
                       column(2, selectizeInput('countPc', 'Count:', 
                                                choices = c('',1:100))), 
                       column(3, selectizeInput('detectionPc',
                                                'Detection:',
                                                choices = c('', 'Visual','Auditory', 'Both')))
                     ),
                     hr(),
                     fluidRow(column(1, ''),
                              column(3, actionButton("newPc", "Clear fields",
                                                     class = 'btn-primary')),
                              column(2, ''),
                              column(3, actionButton('submitPc', 'Add record to table',
                                                     class = "btn-primary")),
                              column(3, '')),
                     br()),
                 width = 6, position = 'right'),
               # ---- PC text ----------------------------------------------------
               mainPanel(
                 introTextPc,
                 hr(),
                 fieldDescriptionsPc,
                 width = 6, position = 'left')
             ),
             hr(),
             # ---- QC and submission ---------------------------------------------------
             h3(strong('2. Data-proofing and submission of point count records:')),
             br(),
             DT::dataTableOutput("responsesPc"),
             br(),
             fluidRow(column(1, ''),
                      column(4, actionButton("deletePc",
                                             "Delete point count record", 
                                             class = "btn-primary")),
                      column(3, ' '),
                      column(4, actionButton('submitPcData', 
                                             'Submit point count data',
                                             class = "btn-primary"))
             ),
             br(), shinyjs::hidden(
               div(
                 id = "thankyou_msgPc",
                 h3("Thanks, your point count data have been recorded!"),
                 'To submit data for another site, please refresh your browser'
               )
             ),
             br()),
    #-------------------------------------------------------------------------------*
    # ---- UI TAB PANEL: NEST DATA ----
    #-------------------------------------------------------------------------------*
    tabPanel(strong('Nest data'),
             textNestIntro,
             sidebarLayout(
               sidebarPanel(
                 div(id = 'summaryDataNest',
                     h3(strong('1. Nest summary data:')),
                     br(), br(),
                     fluidRow(
                       column(3, strong('Species:')),
                       column(3, strong('Color combo:')),
                       column(3, strong('FWS Bands:')),
                       column(3, strong('Nest: Yr-#'))
                     ),
                     fluidRow(
                       column(3, selectInput('speciesNest', ' ', choices = c('',justAlphaCode))),
                       column(3, selectInput('colorComboMale', 
                                             'Male:',
                                             choices = choiceColorCombos)),
                       column(3, textInput('fwsMale', 'Male:')),
                       column(3, textInput('nestID', ' '))
                     ),
                     fluidRow(
                       column(3, ''),
                       column(3, selectInput('colorComboFemale', 
                                             'Female:',
                                             choices = choiceColorCombos)),
                       column(3, textInput('fwsFemale', 'Female:')),
                       column(3, '')
                     ),
                     hr(),
                     fluidRow(
                       column(9, ''),
                       column(3, tags$u(strong('Summary dates')))
                     ),
                     fluidRow(
                       column(2, textInput('plotNest', 'Plot:')),
                       column(7, selectInput('fateNest', 'Nest Fate:',
                                             nestFateChoices)),
                       column(3, dateInput('dateClutchNest',
                                           label = 'Clutch completion:',
                                           value = ''))
                     ),
                     fluidRow(
                       column(7, textInput('plantSpNest', 
                                           'Plant species (if applicable):')),
                       column(2, selectInput('heightNest', 'Nest ht (m):',
                                             c('', 0:12), '')),
                       column(3, dateInput('dateHatchNest',
                                           label = 'Hatch:',
                                           value = ''))
                     ),
                     fluidRow(
                       column(9, textInput('descriptionNest', 'Nest description:')),
                       column(3, dateInput('dateFledgeFailNest',
                                           label = 'Fledge/fail (date):',
                                           value = ''))
                     ),
                     fluidRow(
                       column(3, selectInput('locationNest', 'Nest Location',
                                             nestLocationChoices)),
                       column(9, textInput('nestFateExplanation', 
                                           'If nest fate was "other", explain:'))
                     ),
                     br(),
                     fluidRow(column(1, ''),
                              column(3, actionButton("newNestSummary", 
                                                     "Clear fields",
                                                     class = 'btn-primary')),
                              column(2, ''),
                              column(3, actionButton('submitNestSummary', 
                                                     'Add record to table',
                                                     class = "btn-primary")))
                 ), width = 8),
               mainPanel('Field descriptions coming soon!', width = 4),
               position = 'left'),
             hr(),
             DT::dataTableOutput("responsesNestSummary"),
             br(),
             fluidRow(column(1, ''),
                      column(4, actionButton("deleteNestSummary",
                                             "Delete nest summary record", 
                                             class = "btn-primary")),
                      column(3, ' '),
                      column(4, actionButton('submitNestSummaryData', 
                                             'Submit nest summary data',
                                             class = "btn-primary"))
             ),
             br(), shinyjs::hidden(
               div(
                 id = "thankyou_msgNestSummary",
                 h3("Thanks, your nest summary data have been recorded!")
               )
             ),
             hr(),
             sidebarLayout(
               sidebarPanel(
                 div(id = 'nestData',
                     h3(strong('2. Nest observation records:')),
                     br(), br(),
                     fluidRow(
                       column(3, dateInput('dateNest', label = 'Date:',
                                           value = '')),
                       column(3, selectizeInput('timeNest', 'Time:',
                                                choices = choiceTimeOfDay,
                                                selected = '')),
                       column(3, selectInput('stageNest', 'Stage:',
                                             choices = nestStageChoices,
                                             selected = '')),
                       column(3, selectInput('adAttNest', 'Adult attending:',
                                             choices = nestAttendChoices,
                                             selected = ''))),
                     fluidRow(
                       column(2, selectInput('nEggNest', '# egg:',
                                             choices = c('', 0:10))),
                       column(2, selectInput('nYoungNest', '# yng:',
                                             choices = c('', 0:10))),
                       column(6, textInput('notesNest', 
                                           'Notes: Building, age, fate, behavior, etc.')),
                       column(2, textInput('observerNest', 'Obs:'))),
                     br(),
                     fluidRow(column(1, ''),
                              column(3, actionButton("newNest", 
                                                     "Clear fields",
                                                     class = 'btn-primary')),
                              column(2, ''),
                              column(3, actionButton('submitNest', 
                                                     'Add record to table',
                                                     class = "btn-primary")))
                 ), width = 8),
               mainPanel('Field descriptions coming soon', width = 4),
               position = 'left'),
             hr(),
             DT::dataTableOutput("responsesNest"),
             br(),
             fluidRow(column(1, ''),
                      column(4, actionButton("deleteNest",
                                             "Delete nest record", 
                                             class = "btn-primary")),
                      column(3, ' '),
                      column(4, actionButton('submitNestData', 
                                             'Submit nest data',
                                             class = "btn-primary"))
             ),
             br(), shinyjs::hidden(
               div(
                 id = "thankyou_msgNest",
                 h3("Thanks, your nest data have been recorded!")
               )
             ),
             br()
    ),
    #-------------------------------------------------------------------------------*
    # ---- UI TAB PANEL: HABITAT SURVEY ----
    #-------------------------------------------------------------------------------*
    tabPanel(strong('Habitat survey data'),
             h1('Coming soon!'))
  )
)