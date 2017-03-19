# USER INTERFACE

library(shiny)
library(DT)

source('setUp.R', local=TRUE)

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
                                             c('Existing site', 'New site'))),
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
    #===============================================================================*
    # ---- UI TAB PANEL: VISIT DATA ----
    #===============================================================================*
    tabPanel(strong('Visit data'),
             fluidPage(
               #--------------------------------------------------------------------*
               # ---- Contact table ----
               #--------------------------------------------------------------------*
               h4(strong('Site contact information:')),
               br(),
               fluidRow(
                 column(3, textInput('groupName', 'Group name')),
                 column(3, textInput('lastName' ,'Last name')),
                 column(3, textInput('firstName', 'First name'))
               ),
               br(),
               fluidRow(
                 column(2, textInput('phone1', 'Primary phone')),
                 column(2, textInput('phone2' ,'Secondary phone')),
                 column(3, textInput('email1', 'Email')),
                 column(5, textInput('contactNotes', 'Contact notes'))
               ),
               br(),
               fluidRow(column(6, ''),
                        column(3, actionButton('addRecordContact', 
                                               'Add record to table',
                                               class = "btn-primary"))),
               hr(),
               DT::dataTableOutput("responsesContact"),
               br(),
               fluidRow(column(1, ''),
                        column(4, actionButton("deleteContact",
                                               "Delete record from table", 
                                               class = "btn-primary")),
                        column(3, ' '),
                        column(4, actionButton('submitContact', 
                                               'Submit contact data',
                                               class = "btn-primary"))
               ),
               br(), shinyjs::hidden(
                 div(
                   id = "thankyou_msgContact",
                   h3("Thanks, your contact data have been recorded!")
                 )
               ),
               br(),
               hr(),
               #--------------------------------------------------------------------*
               # ---- Address table ----
               #--------------------------------------------------------------------*
               h4(strong('Site address information:')),
               br(),
               fluidRow(
                 column(1, textInput('houseNumber', 'House number')),
                 column(3, textInput('street' ,'Street')),
                 column(3, textInput('city', 'City')),
                 column(3, textInput('state', 'State')),
                 column(2, textInput('zip', 'Zip code'))
               ),
               br(),
               fluidRow(
                 column(12, textInput('locationNotes', 'Location notes'))
               ),
               br(),
               fluidRow(column(6, ''),
                        column(3, actionButton('addRecordAddress', 
                                               'Add record to table',
                                               class = "btn-primary"))),
               hr(),
               DT::dataTableOutput("responsesAddress"),
               br(),
               fluidRow(column(1, ''),
                        column(4, actionButton("deleteAddress",
                                               "Delete record from table", 
                                               class = "btn-primary")),
                        column(3, ' '),
                        column(4, actionButton('submitAddress', 
                                               'Submit address data',
                                               class = "btn-primary"))
               ),
               br(), shinyjs::hidden(
                 div(
                   id = "thankyou_msgAddress",
                   h3("Thanks, your address data have been recorded!")
                 )
               ),
               br(),
               hr(),
               #--------------------------------------------------------------------*
               # ---- Location table ----
               #--------------------------------------------------------------------*
               h4(strong('Site location information:')),
               br(),
               fluidRow(
                 column(2, dateInput('dateLocation', 'Date')),
                 column(3, numericInput('long' ,'Longitude', NA)),
                 column(3, numericInput('lat', 'Latitude', NA)),
                 column(2, numericInput('accuracy', 'Accuracy', NA)),
                 column(2, selectizeInput('locationMethod', 'Location method', 
                                          choices = choiceLocationMethod))
               ),
               br(),
               fluidRow(
                 column(12, textInput('locationNotes', 'Location notes'))
               ),
               br(),
               fluidRow(column(6, ''),
                        column(3, actionButton('addRecordLocation', 
                                               'Add record to table',
                                               class = "btn-primary"))),
               hr(),
               DT::dataTableOutput("responsesLocation"),
               br(),
               fluidRow(column(1, ''),
                        column(4, actionButton("deleteLocation",
                                               "Delete record from table", 
                                               class = "btn-primary")),
                        column(3, ' '),
                        column(4, actionButton('submitLocation', 
                                               'Submit location data',
                                               class = "btn-primary"))
               ),
               br(), shinyjs::hidden(
                 div(
                   id = "thankyou_msgLocation",
                   h3("Thanks, your location data have been recorded!")
                 )
               ),
               br(),
               hr(),
               #--------------------------------------------------------------------*
               # ---- Visit table ----
               #--------------------------------------------------------------------*
               h4(strong('Site visit information:')),
               br(),
               fluidRow(
                 column(2, dateInput('dateLocation', 'Date')),
                 column(3, textInput('observerVisit' ,'Observer(s)')),
                 column(7, '')
               ),
               fluidRow(
                 selectizeInput('participantEngagement', 'Participant Engagement',
                                choices = choiceParticipantEngagement)
               ),
               fluidRow(
                 column(2, selectizeInput('encounteredBirds', 'Encountered birds',
                                          choices = choiceEncounteredBirds)),
                 column(2, numericInput('netMinutes6', 
                                        '6 m nets', 
                                        0)),
                 column(2, numericInput('netMinutes9', 
                                        '9 m nets', 
                                        0)),
                 column(2, numericInput('netMinutes12', 
                                        '12 m nets', 
                                        0)),
                 column(2, numericInput('netMinutes18', 
                                        '18 m nets', 
                                        0))
               ),
               br(),
               fluidRow(
                 column(12, textInput('notesVisit', 'Visit notes'))
               ),
               br(),
               fluidRow(column(6, ''),
                        column(3, actionButton('addRecordVisit', 
                                               'Add record to table',
                                               class = "btn-primary"))),
               hr(),
               DT::dataTableOutput("responsesVisit"),
               br(),
               fluidRow(column(1, ''),
                        column(4, actionButton("deleteVisit",
                                               "Delete record from table", 
                                               class = "btn-primary")),
                        column(3, ' '),
                        column(4, actionButton('submitVisit', 
                                               'Submit visit data',
                                               class = "btn-primary"))
               ),
               br(), shinyjs::hidden(
                 div(
                   id = "thankyou_msgVisit",
                   h3("Thanks, your visit data have been recorded!")
                 )
               ),
               br(),
               hr(),
               #--------------------------------------------------------------------*
               # ---- Capture table ----
               #--------------------------------------------------------------------*
               br(),
               fluidRow(
                 column(3, textInput('siteID', 'siteID')),
                 column(3, textInput('dateVisit', 'Date')),
                 column(6, '')
               ),
               fluidRow(
                 column(2, selectizeInput('timeEnc', 'Time',
                                          choices = choiceTimeOfDay)),
                 column(1, textInput('obs', 'Observer')),
                 column(1, selectizeInput('capType', 'Encounter',
                                          choices = choiceEnc)),
                 column(2, selectizeInput('sppEnc', 'SPP',
                                          choices = choiceSpecies)),
                 column(3, textInput('bandNumber', 'Band number')),
                 column(3, selectizeInput('colorCombo', 'Color combo',
                                          choices = choiceColors))
               ),
               fluidRow(
                 column(2, numericInput('mass', 'Mass', NA)),
                 column(2, numericInput('wing', 'Wing', NA)),
                 column(2, numericInput('tl', 'Tail', NA)),
                 column(2, selectizeInput('age', 'Age', choices = choiceAge)),
                 column(1, selectizeInput('sex', 'Sex', choices = choiceSex)),
                 column(1, selectizeInput('cpBp', 'CP/BP',
                                          choices = choiceBreedingCond)),
                 column(1, selectizeInput('fat', 'Fat', choices = choiceFat))
               ),
               fluidRow(
                 column(2, textInput('bloodID', 'Blood sample ID')),
                 column(2, textInput('featherID', 'Feather sample ID')),
                 column(8, textInput('notes', 'Capture notes'))
               ),
               br(),
               fluidRow(column(6, ''),
                        column(3, actionButton('addRecordCapture', 
                                               'Add record to table',
                                               class = "btn-primary"))),
               hr(),
               DT::dataTableOutput("responsesCapture"),
               br(),
               fluidRow(column(1, ''),
                        column(4, actionButton("deleteCapture",
                                               "Delete record from table", 
                                               class = "btn-primary")),
                        column(3, ' '),
                        column(4, actionButton('submitCapture', 
                                               'Submit capture data',
                                               class = "btn-primary"))
               ),
               br(), shinyjs::hidden(
                 div(
                   id = "thankyou_msgCapture",
                   h3("Thanks, your capture data have been recorded!")
                 )
               ),
               br(),
               hr(),
               #--------------------------------------------------------------------*
               # ---- Resight foray time and distance table ----
               #--------------------------------------------------------------------*
               br(),
               fluidRow(
                 column(3, textInput('siteID', 'siteID')),
                 column(3, textInput('dateVisit', 'Date')),
                 column(6, '')
               ),
               br(),
               fluidRow(
                 column(2, textInput('observerRsForay', 'Observer')),
                 column(1, numericInput('forayNumber', 'Foray number', 1)),
                 column(3, selectizeInput('rsStart', 'Start time',
                                          choices = choiceTimeOfDay)),
                 column(3, selectizeInput('rsEnd', 'End time',
                                          choices = choiceTimeOfDay)),
                 column(3, numericInput('pathDisance', 'Path distance (m)', NA))
               ),
               br(),
               fluidRow(column(6, ''),
                        column(3, actionButton('addRecordForayTimeDistance', 
                                               'Add record to table',
                                               class = "btn-primary"))),
               hr(),
               DT::dataTableOutput("responsesForayTimeDistance"),
               br(),
               fluidRow(column(1, ''),
                        column(4, actionButton("deleteForayTimeDistance",
                                               "Delete record from table", 
                                               class = "btn-primary")),
                        column(3, ' '),
                        column(4, actionButton('submitForayTimeDistance', 
                                               'Submit foray time and distance data',
                                               class = "btn-primary"))
               ),
               br(), shinyjs::hidden(
                 div(
                   id = "thankyou_msgForayTimeDistance",
                   h3("Thanks, your foray time and distance data have been recorded!")
                 )
               ),
               br(),
               hr(),
               #--------------------------------------------------------------------*
               # ---- Resight foray observed unbanded table ----
               #--------------------------------------------------------------------*
               br(),
               fluidRow(
                 column(3, textInput('siteID', 'siteID')),
                 column(3, textInput('dateVisit', 'Date')),
                 column(2, selectizeInput('sppForay', 'Species',
                                          choices = choiceSpecies)),
                 column(2, numericInput('countUnbanded', 'Count', 0)),
                 column(2, '')
               ),
               br(),
               fluidRow(column(6, ''),
                        column(3, actionButton('addRecordForayObservedUnbanded', 
                                               'Add record to table',
                                               class = "btn-primary"))),
               hr(),
               DT::dataTableOutput("responsesForayObservedUnbanded"),
               br(),
               fluidRow(column(1, ''),
                        column(4, actionButton("deleteForayObservedUnbanded",
                                               "Delete record from table", 
                                               class = "btn-primary")),
                        column(3, ' '),
                        column(4, actionButton('submitForayObservedUnbanded', 
                                               'Submit observed unbanded data',
                                               class = "btn-primary"))
               ),
               br(), shinyjs::hidden(
                 div(
                   id = "thankyou_msgForayObservedUnbanded",
                   h3("Thanks, your foray observed unbanded data have been recorded!")
                 )
               ),
               br(),
               hr(),
               #--------------------------------------------------------------------*
               # ---- Resighted birds by technician table ----
               #--------------------------------------------------------------------*
               br(),
               fluidRow(
                 column(3, textInput('siteID', 'siteID')),
                 column(3, textInput('dateVisit', 'Date')),
                 column(6, '')
               ),
               fluidRow(
                 column(1, textInput('observerRs', 'Observer')),
                 column(2, selectizeInput('timeRs', 'Time',
                                          choices = choiceTimeOfDay)),
                 column(1, numericInput('forayNumberRs', 'Foray number', NA)),
                 column(3, textInput('bandNumber', 'Band number')),
                 column(2, numericInput('longRs', 'Longitude (-dd.dddd)', NA)),
                 column(2, numericInput('latRs', 'Latitude (dd.dddd)', NA)),
                 column(1, selectizeInput('typeRs', 'Resight type',
                                          choices = choiceTypeRs))
               ),
               fluidRow(textInput('notesRs', 'Notes')),
               br(),
               fluidRow(column(6, ''),
                        column(3, actionButton('addRecordRs', 
                                               'Add record to table',
                                               class = "btn-primary"))),
               hr(),
               DT::dataTableOutput("responsesRs"),
               br(),
               fluidRow(column(1, ''),
                        column(4, actionButton("deleteRs",
                                               "Delete record from table", 
                                               class = "btn-primary")),
                        column(3, ' '),
                        column(4, actionButton('submitRs', 
                                               'Submit resight data',
                                               class = "btn-primary"))
               ),
               br(), shinyjs::hidden(
                 div(
                   id = "thankyou_msgRs",
                   h3("Thanks, your resight data have been recorded!")
                 )
               ),
               br(),
               hr(),
               #--------------------------------------------------------------------*
               # ---- Point count table ----
               #--------------------------------------------------------------------*
               br(),
               fluidRow(
                 column(3, textInput('siteID', 'siteID')),
                 column(3, textInput('dateVisit', 'Date')),
                 column(1, textInput('observerPc', 'Observer')),
                 column(2, selectizeInput('timePc', 'Start time',
                                          choices = choiceTimeOfDay)),
                 
                 column(3, '')
               ),
               fluidRow(
                 column(1, selectizeInput('interval', 'Interval', 
                                          choices = choiceInterval)),
                 column(2, selectizeInput('sppPc', 'Species',
                                          choices = choiceSpecies)),
                 column(1, numericInput('count10', 'DIST < 10', 0)),
                 column(1, numericInput('count20', 'DIST 10 - 20', 0)),
                 column(1, numericInput('count30', 'DIST 20 - 30', 0)),
                 column(1, numericInput('count40', 'DIST 30 - 40', 0)),
                 column(1, numericInput('count50', 'DIST 50 - 50', 0)),
                 column(2, selectizeInput('detection', 'Detection',
                                          choices = choiceDetection))
               ),
               br(),
               fluidRow(column(6, ''),
                        column(3, actionButton('addRecordPc', 
                                               'Add record to table',
                                               class = "btn-primary"))),
               hr(),
               DT::dataTableOutput("responsesPc"),
               br(),
               fluidRow(column(1, ''),
                        column(4, actionButton("deletePc",
                                               "Delete record from table", 
                                               class = "btn-primary")),
                        column(3, ' '),
                        column(4, actionButton('submitPc', 
                                               'Submit point count data',
                                               class = "btn-primary"))
               ),
               br(), shinyjs::hidden(
                 div(
                   id = "thankyou_msgPc",
                   h3("Thanks, your point count data have been recorded!")
                 )
               ),
               br(),
               hr()
             )
    ),
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
    #-------------------------------------------------------------------------------*
    # ---- UI TAB PANEL: HABITAT SURVEY ----
    #-------------------------------------------------------------------------------*
    tabPanel(strong('Habitat survey data'),
             h1('Coming soon!'))
  )
)