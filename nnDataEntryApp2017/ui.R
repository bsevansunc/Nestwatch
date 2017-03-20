# USER INTERFACE

library(shiny)
library(DT)

source('setUp.R', local=TRUE)

shinyUI(
  navbarPage(
    title = p(
      h4(strong('Neighborhood Nestwatch technician data submission interface')),
      br()
    ),
    tabPanel(strong('Visit record'),
      fluidPage(
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
    #===============================================================================*
    # ---- UI TAB PANEL: VISIT DATA ----
    #===============================================================================*
    #--------------------------------------------------------------------*
    # ---- Contact table ----
    #--------------------------------------------------------------------*
    h4(strong('Site contact information:')),
    br(),
    fluidRow(
      column(3, textInput('siteContact', 'Site ID')),
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
    # DT::dataTableOutput('responsesContact'),
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
    br(), shinyjs::hidden(
      div(
        id = 'thankyou_msgContact',
        h3('Thanks, your contact data have been recorded!')
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
      column(3, textInput('siteAddress', 'Site ID')),
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
    # DT::dataTableOutput('responsesAddress'),
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
    br(), shinyjs::hidden(
      div(
        id = 'thankyou_msgAddress',
        h3('Thanks, your address data have been recorded!')
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
      column(3, textInput('siteLocation', 'Site ID')),
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
    # DT::dataTableOutput('responsesLocation'),
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
    br(), shinyjs::hidden(
      div(
        id = 'thankyou_msgLocation',
        h3('Thanks, your location data have been recorded!')
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
      column(3, textInput('siteVisit', 'Site ID')),
      column(2, dateInput('dateVisit', 'Date')),
      column(2, textInput('observerVisit' ,'Observer(s)')),
      column(5, selectizeInput('participantEngagementVisit',
                               'Participant Engagement',
                               choices = choiceParticipantEngagement))
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
                                    class = 'btn-primary'))),
    hr(),
    # DT::dataTableOutput('responsesVisit'),
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
    br(), shinyjs::hidden(
      div(
        id = 'thankyou_msgVisit',
        h3('Thanks, your visit data have been recorded!')
      )
    ),
    br(),
    hr(),
    #--------------------------------------------------------------------*
    # ---- Capture table ----
    #--------------------------------------------------------------------*
    br(),
    fluidRow(
      column(3, textInput('siteCapture', 'Site ID')),
      column(3, textInput('dateCapture', 'Date')),
      column(6, '')
    ),
    fluidRow(
      column(2, selectizeInput('timeCapture', 'Time',
                               choices = choiceTimeOfDay)),
      column(1, textInput('observerCapture', 'Observer')),
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
    # DT::dataTableOutput('responsesCapture'),
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
    br(), shinyjs::hidden(
      div(
        id = 'thankyou_msgCapture',
        h3('Thanks, your capture data have been recorded!')
      )
    ),
    br(),
    hr(),
    #--------------------------------------------------------------------*
    # ---- Resight foray time and distance table ----
    #--------------------------------------------------------------------*
    br(),
    fluidRow(
      column(3, textInput('siteForayEffort', 'Site ID')),
      column(3, textInput('dateForayEffort', 'Date')),
      column(6, '')
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
    # DT::dataTableOutput('responsesForayTimeEffort'),
    br(),
    fluidRow(column(1, ''),
             column(4, actionButton('deleteForayEffort',
                                    'Delete record from table', 
                                    class = 'btn-primary')),
             column(3, ' '),
             column(4, actionButton('submitForayForayEffort', 
                                    'Submit foray time and distance data',
                                    class = 'btn-primary'))
    ),
    br(), shinyjs::hidden(
      div(
        id = 'thankyou_msgForayEffort',
        h3('Thanks, your foray time and distance data have been recorded!')
      )
    ),
    br(),
    hr(),
    #--------------------------------------------------------------------*
    # ---- Resight foray observed unbanded table ----
    #--------------------------------------------------------------------*
    br(),
    fluidRow(
      column(3, textInput('siteForayCountUnbanded', 'Site ID')),
      column(3, textInput('dateForayCountUnbanded', 'Date')),
      column(2, selectizeInput('sppForayCountUnbanded', 'Species',
                               choices = choiceSpecies)),
      column(2, numericInput('countForayCountUnbanded', 'Count', 0)),
      column(2, '')
    ),
    br(),
    fluidRow(column(6, ''),
             column(3, actionButton('addRecordForayCountUnbanded', 
                                    'Add record to table',
                                    class = 'btn-primary'))),
    hr(),
    # DT::dataTableOutput('responsesForayObservedUnbanded'),
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
    br(),
    fluidRow(
      column(3, textInput('siteTechRs', 'Site ID')),
      column(3, textInput('dateTechRs', 'Date')),
      column(6, '')
    ),
    fluidRow(
      column(1, textInput('observerTechRs', 'Observer')),
      column(2, selectizeInput('timeTechRs', 'Time',
                               choices = choiceTimeOfDay)),
      column(2, numericInput('forayNumberTechRs', 'Foray number', NA)),
      column(2, textInput('bandNumberTechRs', 'Band number')),
      column(2, numericInput('longTechRs', 'Longitude (-dd.dddd)', NA)),
      column(2, numericInput('latTechRs', 'Latitude (dd.dddd)', NA)),
      column(1, selectizeInput('typeTechRs', 'Resight type',
                               choices = choiceTypeRs))
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
    # DT::dataTableOutput('responsesTechRs'),
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
    br(), shinyjs::hidden(
      div(
        id = 'thankyou_msgTechRs',
        h3('Thanks, your resight data have been recorded!')
      )
    ),
    br(),
    hr(),
    #--------------------------------------------------------------------*
    # ---- Point count table ----
    #--------------------------------------------------------------------*
    br(),
    fluidRow(
      column(3, textInput('sitePc', 'Site ID')),
      column(3, textInput('datePc', 'Date')),
      column(1, textInput('observerPc', 'Observer')),
      column(2, selectizeInput('startTimePc', 'Start time',
                               choices = choiceTimeOfDay)),
      
      column(3, '')
    ),
    fluidRow(
      column(1, selectizeInput('intervalPc', 'Interval', 
                               choices = choiceInterval)),
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
    # DT::dataTableOutput('responsesPc'),
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
                   column(10, textInput('siteQuery', '')),
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
    # ---- UI TAB PANEL: HABITAT SURVEY ----
    #-------------------------------------------------------------------------------*
    tabPanel(strong('Habitat survey data'),
             h1('Coming soon!'))
  )
)