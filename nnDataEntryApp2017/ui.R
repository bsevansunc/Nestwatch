# USER INTERFACE

library(sp)
library(geosphere)
library(shiny)
# library(tidyverse)
# library(shinyBS)
library(stringr)
library(readr)
library(mongolite)
library(tidyr)
library(dplyr)
library(lubridate)
library(shinyjs)
library(DT)
# library(R.utils)
library(markdown)

# No strings as factors!

options(stringsAsFactors = FALSE)

# Where's mongo?

mongoURL <- "mongodb://bsevans:33shazam@ds025232.mlab.com:25232/nndataentry"

# The functions that make things go:

source('helperFunctions.R', local = TRUE)

# Paragraphs, field descriptions, and such:

source('textComponents.R', local = TRUE)

# Field options:

source('fieldOptions.R', local = TRUE)


# Choices of NN regions:

choiceRegions <- c('noData','Atlanta','Colorado', 'DC', 'Gainesville', 'Pittsburgh',
                   'Raleigh', 'Springfield')

names(choiceRegions) <- choiceRegions

# source('setUp.R', local=TRUE)
source('fieldOptions.R', local=TRUE)

#===============================================================================*
# ---- UI ----
#===============================================================================*

shinyUI(
  navbarPage(
    title = p(
      h4(strong('Neighborhood Nestwatch technician data submission interface')),
      br()
    ),
    tabPanel(strong('Visit record'),
      fluidPage(
        useShinyjs(),
        fluidRow(
               column(3, selectInput('hub','Regional Hub:', choiceRegions,
                                     selected = 'noData')),
               column(3, selectInput('inputType', 'New or existing site:',
                                     c('Existing site', 'New site'))),
               column(4, uiOutput('uiSite')),
               column(2, '')
      )
    ),
    br(),
    hr(),
    br(),
    #--------------------------------------------------------------------*
    # ---- Contact table ----
    #--------------------------------------------------------------------*
    h4(strong('Site contact information:')),
    br(),
    fluidRow(
      column(3, textInput('siteIDContact', 'Site ID', 'noData')),
      column(3, textInput('groupNameContact', 'Group name', 'noData')),
      column(3, textInput('lastNameContact' ,'Last name', 'noData')),
      column(3, textInput('firstNameContact', 'First name', 'noData'))
    ),
    br(),
    fluidRow(
      column(2, textInput('phone1Contact', 'Primary phone', 'noData')),
      column(2, textInput('phone2Contact' ,'Secondary phone', 'noData')),
      column(3, textInput('email1Contact', 'Email', 'noData')),
      column(5, textInput('notesContact', 'Contact notes', 'noData'))
    ),
    br(),
    fluidRow(column(6, ''),
             column(3, actionButton('addRecordContact', 
                                    'Add record to table',
                                    class = 'btn-primary'))),
    hr(),
    DT::dataTableOutput('responsesContact'),
    br(),
    fluidRow(column(1, ''),
             column(4, actionButton('deleteContact',
                                    'Delete record from table', 
                                    class = 'btn-primary')),
             column(3, ' '),
             column(4, actionButton('submitContact', 
                                    'Submit contact data',
                                    class = 'btn-primary'))
    ),
    br(),
    br(),
    hr(),
    #--------------------------------------------------------------------*
    # ---- Address table ----
    #--------------------------------------------------------------------*
    h4(strong('Site address information:')),
    br(),
    fluidRow(
      column(3, textInput('siteIDAddress', 'Site ID', 'noData')),
      column(2, textInput('houseNumberAddress', 'House number', 'noData')),
      column(2, textInput('streetAddress' ,'Street', 'noData')),
      column(2, textInput('cityAddress', 'City', 'noData')),
      column(1, textInput('stateAddress', 'State', 'noData')),
      column(2, textInput('zipAddress', 'Zip code', 'noData'))
    ),
    br(),
    fluidRow(
      column(11, textInput('notesAddress', 'Address notes', 'noData')),
      column(1, '')
    ),
    br(),
    fluidRow(column(6, ''),
             column(3, actionButton('addRecordAddress', 
                                    'Add record to table',
                                    class = 'btn-primary'))),
    hr(),
    DT::dataTableOutput('responsesAddress'),
    br(),
    fluidRow(column(1, ''),
             column(4, actionButton('deleteAddress',
                                    'Delete record from table', 
                                    class = 'btn-primary')),
             column(3, ' '),
             column(4, actionButton('submitAddress', 
                                    'Submit address data',
                                    class = 'btn-primary'))
    ),
    br(),
    br(),
    hr(),
    #--------------------------------------------------------------------*
    # ---- Location table ----
    #--------------------------------------------------------------------*
    h4(strong('Site location information:')),
    br(),
    fluidRow(
      column(3, textInput('siteIDLocation', 'Site ID', 'noData')),
      column(2, dateInput('dateLocation', 'Date', '2017-01-01')),
      column(2, numericInput('longLocation' ,'Longitude', 99999)),
      column(2, numericInput('latLocation', 'Latitude', 99999)),
      column(1, numericInput('accuracyLocation', 'Accuracy', 99999)),
      column(2, selectizeInput('locationMethodLocation',
                               'Location method', 
                               choices = choiceLocationMethod,
                               selected = 'noData'))
    ),
    br(),
    fluidRow(
      column(12, textInput('notesLocation', 'Location notes', 'noData'))
    ),
    br(),
    fluidRow(column(6, ''),
             column(3, actionButton('addRecordLocation', 
                                    'Add record to table',
                                    class = 'btn-primary'))),
    hr(),
    DT::dataTableOutput('responsesLocation'),
    br(),
    fluidRow(column(1, ''),
             column(4, actionButton('deleteLocation',
                                    'Delete record from table', 
                                    class = 'btn-primary')),
             column(3, ' '),
             column(4, actionButton('submitLocation', 
                                    'Submit location data',
                                    class = 'btn-primary'))
    ),
    br(), 
    br(),
    hr(),
    #--------------------------------------------------------------------*
    # ---- Visit table ----
    #--------------------------------------------------------------------*
    h4(strong('Site visit information:')),
    br(),
    fluidRow(
      column(3, textInput('siteIDVisit', 'Site ID', 'noData')),
      column(3, dateInput('dateVisit', 'Date', '2017-01-01')),
      column(3, radioButtons('proofSwitchVisit', label = '',
                             choices = c('Enter and modify new records',
                                         'Proof and modify existing records'))),
      column(3, '')
    ),
    fluidRow(
      column(2, textInput('observerVisit' ,'Observer(s)', 'noData')),
      column(5, selectizeInput('participantEngagementVisit',
                               'Participant Engagement',
                               choices = choiceParticipantEngagement,
                               selected = 'noData')),
      column(5, '')
    ),
    fluidRow(
      column(2, selectizeInput('encounteredBirdsVisit', 'Encountered birds',
                               choices = choiceEncounteredBirds,
                               selected = 'noData')),
      column(2, numericInput('netMinutes6Visit', 
                             '6 m nets', 
                             0)),
      column(2, numericInput('netMinutes9Visit', 
                             '9 m nets', 
                             0)),
      column(2, numericInput('netMinutes12Visit', 
                             '12 m nets', 
                             0)),
      column(2, numericInput('netMinutes18Visit', 
                             '18 m nets', 
                             0)),
      column(2, numericInput('netHoursVisit', 'Net hours', 0))
    ),
    br(),
    fluidRow(
      column(12, textInput('notesVisit', 'Visit notes', 'noData'))
    ),
    br(),
    fluidRow(column(6, ''),
             column(3, actionButton('addRecordVisit', 
                                    'Add record to table',
                                    class = 'btn-primary'))),
    hr(),
    DT::dataTableOutput('responsesVisit'),
    br(),
    fluidRow(column(1, ''),
             column(4, actionButton('deleteVisit',
                                    'Delete record from table', 
                                    class = 'btn-primary')),
             column(3, ' '),
             column(4, actionButton('submitVisit', 
                                    'Submit visit data',
                                    class = 'btn-primary'))
    ),
    br(), 
    br(),
    hr(),
    #--------------------------------------------------------------------*
    # ---- Capture table ----
    #--------------------------------------------------------------------*
    h4(strong('Capture information:')),
    br(),
    fluidRow(
      column(3, textInput('siteIDCapture', 'Site ID', 'noData')),
      column(3, textInput('dateCapture', 'Date', 'noData')),
      column(3, radioButtons('proofSwitchCapture', label = '',
                             choices = c('Enter and modify new records',
                                         'Proof and modify existing records'))),
      column(3, '')
    ),
    fluidRow(
      column(2, selectizeInput('timeCapture', 'Time',
                               choices = choiceTimeOfDay,
                               selected = '00:01')),
      column(1, textInput('obsCapture', 'Observer', 'noData')),
      column(1, selectizeInput('encCapture', 'Encounter',
                               choices = choiceEnc,
                               selected = 'noData')),
      column(2, selectizeInput('sppCapture', 'SPP',
                               choices = choiceSpecies,
                               selected = 'noData')),
      column(3, textInput('bandNumberCapture', 'Band number', 'noData')),
      column(3, selectizeInput('colorComboCapture', 'Color combo',
                               choices = choiceColors,
                               selected = 'noData'))
    ),
    fluidRow(
      column(2, numericInput('massCapture', 'Mass', 99999)),
      column(2, numericInput('wingCapture', 'Wing', 99999)),
      column(2, numericInput('tlCapture', 'Tail', 99999)),
      column(2, selectizeInput('ageCapture', 'Age', choices = choiceAge,
                               selected = 'noData')),
      column(1, selectizeInput('sexCapture', 'Sex', choices = choiceSex,
                               selected = 'noData')),
      column(1, selectizeInput('cpBpCapture', 'CP/BP',
                               choices = choiceBreedingCond,
                               selected = 'noData')),
      column(1, selectizeInput('fatCapture', 'Fat', choices = choiceFat,
                               selected = 'noData'))
    ),
    fluidRow(
      column(2, textInput('bloodIDCapture', 'Blood sample ID', 'noData')),
      column(2, textInput('featherIDCapture', 'Feather sample ID', 'noData')),
      column(8, textInput('notesCapture', 'Capture notes', 'noData'))
    ),
    br(),
    fluidRow(column(6, ''),
             column(3, actionButton('addRecordCapture', 
                                    'Add record to table',
                                    class = 'btn-primary'))),
    hr(),
    DT::dataTableOutput('responsesCapture'),
    br(),
    fluidRow(column(1, ''),
             column(4, actionButton('deleteCapture',
                                    'Delete record from table', 
                                    class = 'btn-primary')),
             column(3, ' '),
             column(4, actionButton('submitCapture', 
                                    'Submit capture data',
                                    class = 'btn-primary'))
    ),
    br(),
    br(),
    hr(),
    #--------------------------------------------------------------------*
    # ---- Resight foray effort table ----
    #--------------------------------------------------------------------*
    h4(strong('Resight foray information:')),
    br(),
    fluidRow(
      column(3, textInput('siteIDForayEffort', 'Site ID', 'noData')),
      column(3, textInput('dateForayEffort', 'Date', '2017-01-01')),
      column(3, radioButtons('proofSwitchForayEffort', label = '',
                             choices = c('Enter and modify new records',
                                         'Proof and modify existing records'))),
      column(3, '')
    ),
    br(),
    fluidRow(
      column(2, textInput('observerForayEffort', 'Observer', 'noData')),
      column(2, numericInput('numberForayEffort', 'Foray number', 1)),
      column(2, selectizeInput('startTimeForayEffort', 'Start time',
                               choices = choiceTimeOfDay,
                               selected = '00:01')),
      column(2, selectizeInput('endTimeForayEffort', 'End time',
                               choices = choiceTimeOfDay,
                               selected = '00:01')),
      column(2, numericInput('pathDistanceForayEffort', 'Path distance (m)', 99999)),
      column(2, '')
    ),
    br(),
    fluidRow(column(6, ''),
             column(3, actionButton('addRecordForayEffort', 
                                    'Add record to table',
                                    class = 'btn-primary'))),
    hr(),
    DT::dataTableOutput('responsesForayEffort'),
    br(),
    fluidRow(column(1, ''),
             column(4, actionButton('deleteForayEffort',
                                    'Delete record from table', 
                                    class = 'btn-primary')),
             column(3, ' '),
             column(4, actionButton('submitForayEffort', 
                                    'Submit foray time and distance data',
                                    class = 'btn-primary'))
    ),
    br(), 
    br(),
    hr(),
    #--------------------------------------------------------------------*
    # ---- Resight foray observed unbanded table ----
    #--------------------------------------------------------------------*
    h4(strong('Unbanded focal birds observed while on foray:')),
    br(),
    fluidRow(
      column(3, textInput('siteIDForayCountUnbanded', 'Site ID', 'noData')),
      column(3, textInput('dateForayCountUnbanded', 'Date', '2017-01-01')),
      column(3, radioButtons('proofSwitchForayCountUnbanded', label = '',
                             choices = c('Enter and modify new records',
                                         'Proof and modify existing records'))),
      column(3, '')
    ),
    fluidRow(
      column(2, selectizeInput('sppForayCountUnbanded', 'Species',
                               choices = choiceSpecies,
                               selected = 'noData')),
      column(2, numericInput('countForayCountUnbanded', 'Count', 0)),
      column(8, '')  
    ),
    br(),
    fluidRow(column(6, ''),
             column(3, actionButton('addRecordForayCountUnbanded', 
                                    'Add record to table',
                                    class = 'btn-primary'))),
    hr(),
    DT::dataTableOutput('responsesForayCountUnbanded'),
    br(),
    fluidRow(column(1, ''),
             column(4, actionButton('deleteForayCountUnbanded',
                                    'Delete record from table', 
                                    class = 'btn-primary')),
             column(3, ' '),
             column(4, actionButton('submitForayCountUnbanded', 
                                    'Submit foray unbanded count data',
                                    class = 'btn-primary'))
    ),
    br(), shinyjs::hidden(
      div(
        id = 'thankyou_msgForayCountUnbanded',
        h3('Thanks, your counts of unbanded birds during your foray have been recorded!')
      )
    ),
    br(),
    hr(),
    #--------------------------------------------------------------------*
    # ---- Resighted birds by technician table ----
    #--------------------------------------------------------------------*
    h4(strong('Resighted birds during the visit:')),
    br(),
    fluidRow(
      column(3, textInput('siteIDTechRs', 'Site ID', 'noData')),
      column(3, textInput('dateTechRs', 'Date', 'noData')),
      column(3, radioButtons('proofSwitchTechRs', label = '',
                             choices = c('Enter and modify new records',
                                         'Proof and modify existing records'))),
      column(3, '')
    ),
    fluidRow(
      column(1, textInput('observerTechRs', 'Observer', 'noData')),
      column(2, selectizeInput('timeTechRs', 'Time',
                               choices = choiceTimeOfDay,
                               selected = '00:01')),
      column(2, textInput('forayNumberTechRs', 'Foray number', 'noData')),
      column(2, textInput('bandNumberTechRs', 'Band number', 'noData')),
      column(2, numericInput('longTechRs', 'Longitude (-dd.dddd)', 99999)),
      column(2, numericInput('latTechRs', 'Latitude (dd.dddd)', 99999)),
      column(1, selectizeInput('typeTechRs', 'Resight type',
                               choices = choiceTypeTechRs,
                               selected = 'noData'))
    ),
    fluidRow(
      column(12, textInput('notesTechRs', 'Notes', 'noData'))
      ),
    br(),
    fluidRow(column(6, ''),
             column(3, actionButton('addRecordTechRs', 
                                    'Add record to table',
                                    class = 'btn-primary'))),
    hr(),
    DT::dataTableOutput('responsesTechRs'),
    br(),
    fluidRow(column(1, ''),
             column(4, actionButton('deleteTechRs',
                                    'Delete record from table', 
                                    class = 'btn-primary')),
             column(3, ' '),
             column(4, actionButton('submitTechRs', 
                                    'Submit resight data',
                                    class = 'btn-primary'))
    ),
    br(), 
    br(),
    hr(),
    #--------------------------------------------------------------------*
    # ---- Point count table ----
    #--------------------------------------------------------------------*
    br(),
    hr(),
    textAouQuery,
    fluidRow(column(11, DT::dataTableOutput('aouTable'))),
    hr(),
    h4(strong('Point count information:')),
    br(),
    fluidRow(
      column(3, textInput('siteIDPc', 'Site ID', 'noData')),
      column(3, textInput('datePc', 'Date', 'noData')),
      column(3, radioButtons('proofSwitchPc', label = '',
                             choices = c('Enter and modify new records',
                                         'Proof and modify existing records'))),
      column(3, '')
    ),
    fluidRow(
      column(2, textInput('observerPc', 'Observer', 'noData')),
      column(3, selectizeInput('startTimePc', 'Start time',
                               choices = choiceTimeOfDay,
                               selected = '00:01')),
      column(7, '')
    ),
    fluidRow(
      column(1, selectizeInput('intervalPc', 'Interval', 
                               choices = choiceInterval,
                               selected = 1)),
      column(2, selectizeInput('sppPc', 'Species',
                               choices = choiceSpecies,
                               selected = 'noData')),
      column(1, numericInput('count10Pc', 'DIST < 10', 0)),
      column(1, numericInput('count20Pc', 'DIST 10 - 20', 0)),
      column(1, numericInput('count30Pc', 'DIST 20 - 30', 0)),
      column(1, numericInput('count40Pc', 'DIST 30 - 40', 0)),
      column(1, numericInput('count50Pc', 'DIST 50 - 50', 0)),
      column(2, selectizeInput('detectionPc', 'Detection',
                               choices = choiceDetection,
                               selected = 'noData'))
    ),
    br(),
    fluidRow(column(6, ''),
             column(3, actionButton('addRecordPc', 
                                    'Add record to table',
                                    class = 'btn-primary'))),
    hr(),
    DT::dataTableOutput('responsesPc'),
    br(),
    fluidRow(column(1, ''),
             column(4, actionButton('deletePc',
                                    'Delete record from table', 
                                    class = 'btn-primary')),
             column(3, ' '),
             column(4, actionButton('submitPc', 
                                    'Submit point count data',
                                    class = 'btn-primary'))
    ),
    br(), shinyjs::hidden(
      div(
        id = 'thankyou_msgPc',
        h3('Thanks, your point count data have been recorded!')
      )
    )
    ),
    #--------------------------------------------------------------------------------*
    # ---- UI TAB PANEL: QUERY RECORDS ----
    #--------------------------------------------------------------------------------*
    tabPanel(strong('Query encounter records'),
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   column(7, h5(strong('Site ID:'))),
                   column(5, checkboxInput('showAllSites',
                                           'Show all',
                                           value = TRUE))),
                 fluidRow(
                   column(10, textInput('siteIDQuery', '')),
                   column(2, '')),
                 hr(),
                 fluidRow(
                   column(7, h5(strong('Species:'))),
                   column(5, checkboxInput('showAllSpp',
                                           'Show all',
                                           value = TRUE))),
                 fluidRow(
                   column(10, uiOutput('uiQuerySppChoices')),
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
                   column(5, checkboxInput('showAllColorCombos',
                                           'Show all',
                                           value = TRUE))),
                 fluidRow(
                   column(10, textInput('colorComboQuery','',  'ALL')),
                   column(2, '')),
                 hr(),
                 fluidRow(
                   column(2, '')),
                 fluidRow(
                   column(12, downloadButton('downloadData', 'Download query data'))
                 ),
                 width = 3, position = 'left'),
               mainPanel(
                 textQuery,
                 hr(),
                 DT::dataTableOutput('queryTable'),
                 width = 9, position = 'right')
             )),
    #-------------------------------------------------------------------------------*
    # ---- UI TAB PANEL: RESIGHT BY PARTICIPANT TABLE ----
    #-------------------------------------------------------------------------------*
    tabPanel(strong('Participant resights'),
             fluidPage(
               useShinyjs(),
               fluidRow(
                 column(4, selectizeInput('siteIDPartRs', 'Site ID',
                                          choices = siteIdTable %>%
                                            arrange(siteID) %>%
                                            .$siteID %>%
                                            c('noData'),
                                          selected = 'noData')),
                 column(2, dateInput('datePartRs', 'Date', '2017-01-01')),
                 column(4, textInput('bandNumberPartRs', 'Band number', 'noData')),
                 column(2, '')
               ),
               fluidRow(
                 column(2, selectizeInput('typePartRs', 'Resight type',
                                          choices = choiceTypePartRs,
                                          selected = 'noData')),
                 column(10, textInput('notesPartRs', 'Notes', 'noData'))
               ),
               br(),
               fluidRow(column(6, ''),
                        column(3, actionButton('addRecordPartRs', 
                                               'Add record to table',
                                               class = 'btn-primary'))),
               hr(),
               DT::dataTableOutput('responsesPartRs'),
               br(),
               fluidRow(column(1, ''),
                        column(4, actionButton('deletePartRs',
                                               'Delete record from table', 
                                               class = 'btn-primary')),
                        column(3, ' '),
                        column(4, actionButton('submitPartRs', 
                                               'Submit resight data',
                                               class = 'btn-primary'))
               ),
               br(), 
               br()
               )
             ),
    #-------------------------------------------------------------------------------*
    # ---- UI TAB PANEL: HABITAT SURVEY ----
    #-------------------------------------------------------------------------------*
    tabPanel(strong('Habitat survey data'),
             h1('Coming soon!'))
  )
)