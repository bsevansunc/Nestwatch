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
               column(3, selectInput('hub','Regional Hub:', choiceRegions)),
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
      column(3, textInput('siteIDContact', 'Site ID')),
      column(3, textInput('groupNameContact', 'Group name')),
      column(3, textInput('lastNameContact' ,'Last name')),
      column(3, textInput('firstNameContact', 'First name'))
    ),
    br(),
    fluidRow(
      column(2, textInput('phone1Contact', 'Primary phone')),
      column(2, textInput('phone2Contact' ,'Secondary phone')),
      column(3, textInput('email1Contact', 'Email')),
      column(5, textInput('notesContact', 'Contact notes'))
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
      column(3, textInput('siteIDAddress', 'Site ID')),
      column(2, textInput('houseNumberAddress', 'House number')),
      column(2, textInput('streetAddress' ,'Street')),
      column(2, textInput('cityAddress', 'City')),
      column(1, textInput('stateAddress', 'State')),
      column(2, textInput('zipAddress', 'Zip code'))
    ),
    br(),
    fluidRow(
      column(11, textInput('notesAddress', 'Address notes')),
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
      column(3, textInput('siteIDLocation', 'Site ID')),
      column(2, dateInput('dateLocation', 'Date')),
      column(2, numericInput('longLocation' ,'Longitude', NA)),
      column(2, numericInput('latLocation', 'Latitude', NA)),
      column(1, numericInput('accuracyLocation', 'Accuracy', NA)),
      column(2, selectizeInput('locationMethodLocation',
                               'Location method', 
                               choices = choiceLocationMethod))
    ),
    br(),
    fluidRow(
      column(12, textInput('notesLocation', 'Location notes'))
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
      column(3, textInput('siteIDVisit', 'Site ID')),
      column(3, dateInput('dateVisit', 'Date')),
      column(3, radioButtons('proofSwitchVisit', label = '',
                             choices = c('Enter and modify new records' = 'new',
                                         'Proof and modify existing records' = 'proof'))),
      column(3, '')
    ),
    fluidRow(
      column(2, textInput('observerVisit' ,'Observer(s)')),
      column(5, selectizeInput('participantEngagementVisit',
                               'Participant Engagement',
                               choices = choiceParticipantEngagement)),
      column(5, '')
    ),
    fluidRow(
      column(2, selectizeInput('encounteredBirdsVisit', 'Encountered birds',
                               choices = choiceEncounteredBirds)),
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
      column(12, textInput('notesVisit', 'Visit notes'))
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
      column(3, textInput('siteIDCapture', 'Site ID')),
      column(3, textInput('dateCapture', 'Date')),
      column(3, radioButtons('proofSwitchCapture',  label = '',
                             choices = c('Enter and modify new records' = 'new',
                                         'Proof and modify existing records' = 'proof'))),
      column(3, '')
    ),
    fluidRow(
      column(2, selectizeInput('timeCapture', 'Time',
                               choices = choiceTimeOfDay)),
      column(1, textInput('obsCapture', 'Observer')),
      column(1, selectizeInput('encCapture', 'Encounter',
                               choices = choiceEnc)),
      column(2, selectizeInput('sppCapture', 'SPP',
                               choices = choiceSpecies)),
      column(3, textInput('bandNumberCapture', 'Band number')),
      column(3, selectizeInput('colorComboCapture', 'Color combo',
                               choices = choiceColors))
    ),
    fluidRow(
      column(2, numericInput('massCapture', 'Mass', NA)),
      column(2, numericInput('wingCapture', 'Wing', NA)),
      column(2, numericInput('tlCapture', 'Tail', NA)),
      column(2, selectizeInput('ageCapture', 'Age', choices = choiceAge)),
      column(1, selectizeInput('sexCapture', 'Sex', choices = choiceSex)),
      column(1, selectizeInput('cpBpCapture', 'CP/BP',
                               choices = choiceBreedingCond)),
      column(1, selectizeInput('fatCapture', 'Fat', choices = choiceFat))
    ),
    fluidRow(
      column(2, textInput('bloodIDCapture', 'Blood sample ID')),
      column(2, textInput('featherIDCapture', 'Feather sample ID')),
      column(8, textInput('notesCapture', 'Capture notes'))
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
      column(3, textInput('siteIDForayEffort', 'Site ID')),
      column(3, textInput('dateForayEffort', 'Date')),
      column(3, radioButtons('proofSwitchForayEffort', label = '',
                             choices = c('Enter and modify new records' = 'new',
                                         'Proof and modify existing records' = 'proof'))),
      column(3, '')
    ),
    br(),
    fluidRow(
      column(2, textInput('observerForayEffort', 'Observer')),
      column(2, numericInput('numberForayEffort', 'Foray number', 1)),
      column(2, selectizeInput('startTimeForayEffort', 'Start time',
                               choices = choiceTimeOfDay)),
      column(2, selectizeInput('endTimeForayEffort', 'End time',
                               choices = choiceTimeOfDay)),
      column(2, numericInput('pathDistanceForayEffort', 'Path distance (m)', NA)),
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
      column(3, textInput('siteIDForayCountUnbanded', 'Site ID')),
      column(3, textInput('dateForayCountUnbanded', 'Date')),
      column(3, radioButtons('proofSwitchForayCountUnbanded', label = '',
                             choices = c('Enter and modify new records' = 'new',
                                         'Proof and modify existing records' = 'proof'))),
      column(3, '')
    ),
    fluidRow(
      column(2, selectizeInput('sppForayCountUnbanded', 'Species',
                               choices = choiceSpecies)),
      column(2, numericInput('countForayCountUnbanded', 'Count', NA)),
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
      column(3, textInput('siteIDTechRs', 'Site ID')),
      column(3, textInput('dateTechRs', 'Date')),
      column(3, radioButtons('proofSwitchTechRs', label = '',
                             choices = c('Enter and modify new records' = 'new',
                                         'Proof and modify existing records' = 'proof'))),
      column(3, '')
    ),
    fluidRow(
      column(1, textInput('observerTechRs', 'Observer')),
      column(2, selectizeInput('timeTechRs', 'Time',
                               choices = choiceTimeOfDay)),
      column(2, textInput('forayNumberTechRs', 'Foray number')),
      column(2, textInput('bandNumberTechRs', 'Band number')),
      column(2, numericInput('longTechRs', 'Longitude (-dd.dddd)', NA)),
      column(2, numericInput('latTechRs', 'Latitude (dd.dddd)', NA)),
      column(1, selectizeInput('typeTechRs', 'Resight type',
                               choices = choiceTypeTechRs))
    ),
    fluidRow(
      column(12, textInput('notesTechRs', 'Notes'))
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
      column(3, textInput('siteIDPc', 'Site ID')),
      column(3, textInput('datePc', 'Date')),
      column(3, radioButtons('proofSwitchPc', label = '',
                             choices = c('Enter and modify new records' = 'new',
                                         'Proof and modify existing records' = 'proof'))),
      column(3, '')
    ),
    fluidRow(
      column(2, textInput('observerPc', 'Observer')),
      column(3, selectizeInput('startTimePc', 'Start time',
                               choices = choiceTimeOfDay)),
      column(7, '')
    ),
    fluidRow(
      column(1, selectizeInput('intervalPc', 'Interval', 
                               choices = choiceInterval,
                               selected = 1)),
      column(2, selectizeInput('sppPc', 'Species',
                               choices = choiceSpecies)),
      column(1, numericInput('count10Pc', 'DIST < 10', 0)),
      column(1, numericInput('count20Pc', 'DIST 10 - 20', 0)),
      column(1, numericInput('count30Pc', 'DIST 20 - 30', 0)),
      column(1, numericInput('count40Pc', 'DIST 30 - 40', 0)),
      column(1, numericInput('count50Pc', 'DIST 50 - 50', 0)),
      column(2, selectizeInput('detectionPc', 'Detection',
                               choices = choiceDetection))
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
                 column(3, column(3, selectInput('hub','Regional Hub:', choiceRegions))),
                 column(3, selectizeInput('siteIDPartRs', 'Site ID',
                                          choices = siteIdTable %>%
                                            arrange(siteID) %>%
                                            .$siteID)),
                 column(2, dateInput('datePartRs', 'Date')),
                 column(3, radioButtons('proofSwitchPartRs', label = '',
                                        choices = c('Enter and modify new records' = 'new',
                                                    'Proof and modify existing records' = 'proof'))),
                 column(1, '')
               ),
               fluidRow(
                 column(4, textInput('bandNumberPartRs', 'Band number', 'noData')),
                 column(3, selectizeInput('typePartRs', 'Resight type',
                                          choices = choiceTypePartRs,
                                          selected = 'noData')),
                 column(5, textInput('notesPartRs', 'Notes', 'noData'))
               ),
               br(),
               fluidRow(column(9, ''),
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