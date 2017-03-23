# Sources used in app development:

# Form input: http://deanattali.com/2015/06/14/mimicking-google-form-shiny/
# CRUD application: http://ipub.com/shiny-crud-app/
# DT instructions: http://rstudio.github.io/DT/shiny.html
# Data storage: http://deanattali.com/blog/shiny-persistent-data-storage/

#=================================================================================*
# ---- SET-UP ----
#=================================================================================*

# Libraries:

library(sp)
library(geosphere)
library(shiny)
library(stringr)
library(readr)
library(mongolite)
library(tidyr)
library(dplyr)
library(lubridate)
library(shinyjs)
library(DT)
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


# ---- SERVER ----

server <- function(input, output, session) {

  # Storage container for reactive data:
  
  tableValues <- reactiveValues()
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: MONGO CONNECTIONS ----
  #-------------------------------------------------------------------------------*
  
  # Mongo connections:
  
  queryMongo <- mongo('queryTable', url = mongoURL)
  contactInfoMongo <- mongo('contactTable', url = mongoURL)
  addressMongo <- mongo('addressTable', url = mongoURL)
  locationMongo <- mongo('locationTable', url = mongoURL)
  visitMongo <- mongo('visitTable', url = mongoURL)
  captureMongo <- mongo('captureTable', url = mongoURL)
  forayEffortMongo <- mongo('forayEffortTable', url = mongoURL)
  forayCountUnbandedMongo <- mongo('forayCountUnbandedTable', url = mongoURL)
  techRsMongo <- mongo('techRsTable', url = mongoURL)
  pcMongo <- mongo('pcTable', url = mongoURL)
  partRsMongo <- mongo('partRsTable', url = mongoURL)
  
  # Location data will only be read once from mongo:
  
  locationTableFull <- locationMongo$find(fields = '{"_row" : 0, "_id" : 0}') %>%
    mongoToTblDf

  # SiteId table filtered by hub:
  
  observe({
    tableValues$siteHub <- siteIdTable %>%
      filter(region == input$hub)
  })
  
  # Given a hub input, get vector of sites:
  
  siteChoices <- reactive({
    sites <- c(
      'noData', tableValues$siteHub %>%
        arrange(siteID) %>%
        .$siteID)
    sites
  })
  
  output$uiSite <- renderUI({
    switch(input$inputType,
           'New site' = textInput("siteId", "Site:", ''),
           'Existing site' = selectInput("siteId", "Site:",
                                         choices = siteChoices())
    )
  })
  
  # Once site is chosen above, have this be the default input:
  
  observe({
    siteInputs <- c('siteIDContact','siteIDAddress','siteIDLocation', 'siteIDVisit',
                    'siteIDCapture', 'siteIDForayEffort','siteIDForayCountUnbanded',
                    'siteIDTechRs', 'siteIDPc','siteIDQuery')
    for(i in 1:length(siteInputs)){
      updateTextInput(session, siteInputs[i], value = input$siteId)
    }
  })
  
#   # For the query table have the default input be EITHER from the visit record
#   # or participant resights:
#   
#   observe({
#     if(is.null(input$siteId)){
#       if(!is.null(input$siteIDPartRs)){
#         if(input$siteIDPartRs != 'noData'){
#           updateTextInput(session, 'siteIDQuery', input$siteIDPartRs)
#         }
#       }
#     }
#     if(!is.null(input$siteID)){
#       if(input$siteID != 'noData'){
#         updateTextInput(session, 'siteIDQuery', input$siteId)
#       }
#     }
#   })
  
  
  # Once an observer has been recorded, have this be the default input:
  
  observe({
    observerInputs <- c('observerCapture', 'observerForayEffort',
                        'observerTechRs', 'observerPc')
    for(i in 1:length(observerInputs)){
      updateTextInput(session, observerInputs[i], value = input$observerVisit)
    }
  })
  

  observe({
    dateInputs <- c('dateCapture', 'dateForayEffort',
                    'dateForayCountUnbanded','dateTechRs',
                    'datePc')
    for(i in 1:length(dateInputs)){
      updateDateInput(session, dateInputs[i], value = input$dateVisit)
    }
  })
  
  # Query data filtered by hub:
  
  observe({
    tableValues$queryTable <- queryMongo$find(
      query = hubQuery('region', input$hub)
    )
  })
  
  #--------------------------------------------------------------------*
  # ContactInfo table filtered by site:
  #--------------------------------------------------------------------*
  
  # Input fields:
  
  formDataContact <- reactive({
    sapply(names(getTableMetadata(fieldCodesContactInfo, fieldNamesContactInfo)$fields),
           function(x) as.character(input[[x]]))
  })
  
  # Get data:
  
  observe({
    if(!is.null(input$hub)){
      if(input$hub != 'noData'){
        if(!is.null(input$siteId)){
          if(input$siteId != 'noData'){
            contactInfoTable <- contactInfoMongo$find(
              query = siteQuery('siteIDContact', input$siteIDContact),
              fields = '{"_row" : 0, "_id" : 0}') %>%
              mongoToTblDf
            if(nrow(contactInfoTable) > 0){
              tableValues$contactInfo <- contactInfoTable %>%
                select(one_of(fieldCodesContactInfo))
            } else {
              tableValues$contactInfo <- emptyDataFrame(fieldCodesContactInfo)
            }
          }
        }
      }
    }
  })
  
  # Add or modify records:
  
  observeEvent(input$addRecordContact, {
    tableValues$contactInfo <- dataAddOrModify(
      tableValues$contactInfo,
      input$responsesContact_rows_selected, 
      formDataContact())
    createBlankInputs(blankFieldsContactInfo, session)
  }, priority = 1)
  
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesContact_rows_selected, {
    showRecordInputs(tableValues$contactInfo, input$responsesContact_rows_selected,
                     fieldCodesContactInfo, session)
  })
  
  # Delete selected record:
  
  observeEvent(input$deleteContact, {
    tableValues$contactInfo <- deleteRecord(
      tableValues$contactInfo,
      input$responsesContact_rows_selected)
  }, priority = 1)
  
  # Data table output:
  
  output$responsesContact <- DT::renderDataTable({
    # Update after submit or delete is clicked
    input$addRecordContact ; input$deleteContact
    # Table display:
    tableValues$contactInfo
  }, options = list(lengthChange = FALSE, paging = FALSE, columns.defaultContent = ''),
  server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodesContactInfo, fieldNamesContactInfo)$fields))
  
  # Save data:
  
  observeEvent(input$submitContact, {
    # Remove data associated with site from database:
    contactInfoMongo$remove(siteQuery('siteIDContact', input$siteIDContact), multiple = TRUE)
    # Add data to database:
    contactInfoMongo$insert(tableValues$contactInfo)
    shinyjs::alert('Thank you, your contact data have been submitted!')
  })
  
  #--------------------------------------------------------------------*
  # Address table filtered by site:
  #--------------------------------------------------------------------*
  
  # Input fields:
  
  formDataAddress <- reactive({
    sapply(names(getTableMetadata(fieldCodesAddress,
                                  fieldNamesAddress)$fields),
           function(x) as.character(input[[x]]))
  })
  
  # Get data:
  
  observe({
    if(!is.null(input$hub)){
      if(input$hub != 'noData'){
        if(!is.null(input$siteId)){
          if(input$siteId != 'noData'){
            addressTable <- addressMongo$find(
              query = siteQuery('siteIDAddress', input$siteIDAddress),
              fields = '{"_row" : 0, "_id" : 0}') %>%
              mongoToTblDf
            if(nrow(addressTable) > 0){
              tableValues$address <- addressTable %>%
                select(one_of(fieldCodesAddress))
            } else {
              tableValues$address <- emptyDataFrame(fieldCodesAddress)
            }
          }
        }
      }
    }
  })
  
  # Add or modify records:
  
  observeEvent(input$addRecordAddress, {
    tableValues$address <- dataAddOrModify(
      tableValues$address,
      input$responsesAddress_rows_selected, 
      formDataAddress())
    createBlankInputs(blankFieldsAddress, session)
  }, priority = 1)
  
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesAddress_rows_selected, {
    showRecordInputs(tableValues$address, input$responsesAddress_rows_selected,
                     fieldCodesAddress, session)
  })
  
  # Delete selected record:
  
  observeEvent(input$deleteAddress, {
    tableValues$address <- deleteRecord(
      tableValues$address,
      input$responsesAddress_rows_selected)
  }, priority = 1)
  
  # Data table output:
  
  output$responsesAddress <- DT::renderDataTable({
    # Update after submit or delete is clicked
    input$addRecordAddress ; input$deleteAddress
    # Table display:
    tableValues$address
  }, options = list(lengthChange = FALSE, paging = FALSE, columns.defaultContent = ''),
  server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodesAddress, fieldNamesAddress)$fields))
  
  # Save data:
  
  observeEvent(input$submitAddress, {
    # Remove data associated with site from database:
    addressMongo$remove(siteQuery('siteIDAddress', input$siteIDAddress), multiple = TRUE)
    # Add data to database:
    addressMongo$insert(tableValues$address)
    shinyjs::alert('Thank you, your address data have been submitted!')
  })
  
  #--------------------------------------------------------------------*
  # Location table filtered by site:
  #--------------------------------------------------------------------*
  
  # Input fields:
  
  formDataLocation <- reactive({
    sapply(names(getTableMetadata(fieldCodesLocation,
                                  fieldNamesLocation)$fields),
           function(x) as.character(input[[x]]))
  })
  
  observe({
    if(!is.null(input$hub)){
      if(input$hub != 'noData'){
        if(!is.null(input$siteId)){
          if(input$siteId != 'noData'){
            locationTable <- locationTableFull %>%
              filter(siteIDLocation == input$siteIDLocation)
            if(nrow(locationTable) > 0){
              tableValues$location <- locationTable %>%
                select(one_of(fieldCodesLocation))
            } else {
              tableValues$location <- emptyDataFrame(fieldCodesLocation)
            }
          }
        }
      }
    }
  })
  
  # Add or modify records:
  
  observeEvent(input$addRecordLocation, {
    tableValues$location <- dataAddOrModify(
      tableValues$location,
      input$responsesLocation_rows_selected, 
      formDataLocation())
    createBlankInputs(blankFieldsLocation, session)
  }, priority = 1)
  
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesLocation_rows_selected, {
    showRecordInputs(tableValues$location, input$responsesLocation_rows_selected,
                     fieldCodesLocation, session)
  })
  
  # Delete selected record:
  
  observeEvent(input$deleteLocation, {
    tableValues$location <- deleteRecord(
      tableValues$location,
      input$responsesLocation_rows_selected)
  }, priority = 1)
  
  # Data table output:
  
  output$responsesLocation <- DT::renderDataTable({
    # Update after submit or delete is clicked
    input$addRecordLocation ; input$deleteLocation
    # Table display:
    tableValues$location
  }, options = list(lengthChange = FALSE, paging = FALSE, columns.defaultContent = ''),
  server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodesLocation, fieldNamesLocation)$fields))
  
  # Save data:
  
  observeEvent(input$submitLocation, {
    # Remove data associated with site from database:
    locationMongo$remove(siteQuery('siteIDLocation', input$siteIDLocation), multiple = TRUE)
    # Add data to database:
    locationMongo$insert(tableValues$location)
    shinyjs::alert('Thank you, your location data have been submitted!')
  })
  
  #--------------------------------------------------------------------*
  # Visit table filtered by site and date:
  #--------------------------------------------------------------------*
  
  # Input fields:
  
  formDataVisit <- reactive({
    sapply(names(getTableMetadata(fieldCodesVisit,
                                  fieldNamesVisit)$fields),
           function(x) as.character(input[[x]]))
  })
  
  # Get data:
  
  observe({
    if(!is.null(input$hub)){
      if(input$hub != 'noData'){
        if(!is.null(input$siteId)){
          if(input$siteId != 'noData'){
            if(!is.null(input$dateVisit)){
              visitTable <- visitMongo$find(
                query = siteDateQuery('siteIDVisit', input$siteIDVisit, 'dateVisit', input$dateVisit),
                fields = '{"_row" : 0, "_id" : 0}') %>%
                mongoToTblDf
              if(nrow(visitTable) > 0){
                tableValues$visit <- visitTable %>%
                  select(one_of(fieldCodesVisit))
              } else {
                tableValues$visit <- emptyDataFrame(fieldCodesVisit)
              }
            }
          }
        }
      }
    }
  })
  
  # Add or modify records:
  
  observeEvent(input$addRecordVisit, {
    tableValues$visit <- dataAddOrModify(
      tableValues$visit,
      input$responsesVisit_rows_selected, 
      formDataVisit())
    createBlankInputs(blankFieldsVisit, session)
  }, priority = 1)
  
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesVisit_rows_selected, {
    showRecordInputs(tableValues$visit, input$responsesVisit_rows_selected,
                     fieldCodesVisit, session)
  })
  
  # Delete selected record:
  
  observeEvent(input$deleteVisit, {
    tableValues$visit <- deleteRecord(
      tableValues$visit,
      input$responsesVisit_rows_selected)
  }, priority = 1)
  
  # Data table output:
  
  output$responsesVisit <- DT::renderDataTable({
    # Update after submit or delete is clicked
    input$addRecordVisit ; input$deleteVisit
    # Table display:
    tableValues$visit
  }, options = list(lengthChange = FALSE, paging = FALSE, columns.defaultContent = ''),
  server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodesVisit, fieldNamesVisit)$fields))
  
  # Save data:
  
  observeEvent(input$submitVisit, {
    # Remove data associated with site from database:
    visitMongo$remove(siteDateQuery('siteIDVisit', input$siteIDVisit, 'dateVisit',
                                    input$dateVisit), multiple = TRUE)
    # Add data to database:
    visitMongo$insert(tableValues$visit)
    shinyjs::alert('Thank you, your visit data have been submitted!')
  })
  
  #--------------------------------------------------------------------*
  # Capture table filtered by site:
  #--------------------------------------------------------------------*
  
  # Input fields:
  
  formDataCapture <- reactive({
    sapply(names(getTableMetadata(fieldCodesCapture,
                                  fieldNamesCapture)$fields),
           function(x) as.character(input[[x]]))
  })
  
  # Get data:
  
  observe({
    if(!is.null(input$hub)){
      if(input$hub != 'noData'){
        if(!is.null(input$siteId)){
          if(input$siteId != 'noData'){
            if(!is.null(input$dateCapture)){
              captureTable <- captureMongo$find(
                query = siteDateQuery('siteIDCapture', input$siteIDCapture,
                                      'dateCapture', input$dateCapture),
                fields = '{"_row" : 0, "_id" : 0}') %>%
                mongoToTblDf
              if(nrow(captureTable) > 0){
                tableValues$capture <- captureTable %>%
                  select(one_of(fieldCodesCapture))
              } else {
                tableValues$capture <- emptyDataFrame(fieldCodesCapture)
              }
            }
          }
        }
      }
    }
  })
  
  # Add or modify records:
  
  observeEvent(input$addRecordCapture, {
    tableValues$capture <- dataAddOrModify(
      tableValues$capture,
      input$responsesCapture_rows_selected, 
      formDataCapture())
    createBlankInputs(blankFieldsCapture, session)
  }, priority = 1)
  
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesCapture_rows_selected, {
    showRecordInputs(tableValues$capture, input$responsesCapture_rows_selected,
                     fieldCodesCapture, session)
  })
  
  # Delete selected record:
  
  observeEvent(input$deleteCapture, {
    tableValues$capture <- deleteRecord(
      tableValues$capture,
      input$responsesCapture_rows_selected)
  }, priority = 1)
  
  # Data table output:
  
  output$responsesCapture <- DT::renderDataTable({
    # Update after submit or delete is clicked
    input$addRecordCapture ; input$deleteCapture
    # Table display:
    tableValues$capture
  }, options = list(lengthChange = FALSE, paging = FALSE, columns.defaultContent = ''),
  server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodesCapture, fieldNamesCapture)$fields))
  
  # Save data:
  
  observeEvent(input$submitCapture, {
    # Remove data associated with site from database:
    captureMongo$remove(siteDateQuery('siteIDCapture', input$siteIDCapture,
                                      'dateCapture', input$dateCapture), multiple = TRUE)
    # Add data to database:
    captureMongo$insert(tableValues$capture)
    shinyjs::alert('Thank you, your capture data have been submitted!')
  })
  
  #--------------------------------------------------------------------*
  # ForayEffort table filtered by site:
  #--------------------------------------------------------------------*
  
  # Input fields:
  
  formDataForayEffort <- reactive({
    sapply(names(getTableMetadata(fieldCodesForayEffort,
                                  fieldNamesForayEffort)$fields),
           function(x) as.character(input[[x]]))
  })
  
  # Get data:
  
  observe({
    if(!is.null(input$hub)){
      if(input$hub != 'noData'){
        if(!is.null(input$siteId)){
          if(input$siteId != 'noData'){
            if(!is.null(input$dateForayEffort)){
              forayEffortTable <- forayEffortMongo$find(
                query = siteDateQuery('siteIDForayEffort', input$siteIDForayEffort,
                                      'dateForayEffort', input$dateForayEffort),
                fields = '{"_row" : 0, "_id" : 0}') %>%
                mongoToTblDf
              if(nrow(forayEffortTable) > 0){
                tableValues$forayEffort <- forayEffortTable %>%
                  select(one_of(fieldCodesForayEffort))
              } else {
                tableValues$forayEffort <- emptyDataFrame(fieldCodesForayEffort)
              }
            }
          }
        }
      }
    }
  })
  
  # Add or modify records:
  
  observeEvent(input$addRecordForayEffort, {
    tableValues$forayEffort <- dataAddOrModify(
      tableValues$forayEffort,
      input$responsesForayEffort_rows_selected, 
      formDataForayEffort())
    createBlankInputs(blankFieldsForayEffort, session)
  }, priority = 1)
  
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesForayEffort_rows_selected, {
    showRecordInputs(tableValues$forayEffort, input$responsesForayEffort_rows_selected,
                     fieldCodesForayEffort, session)
  })
  
  # Delete selected record:
  
  observeEvent(input$deleteForayEffort, {
    tableValues$forayEffort <- deleteRecord(
      tableValues$forayEffort,
      input$responsesForayEffort_rows_selected)
  }, priority = 1)
  
  # Data table output:
  
  output$responsesForayEffort <- DT::renderDataTable({
    # Update after submit or delete is clicked
    input$addRecordForayEffort ; input$deleteForayEffort
    # Table display:
    tableValues$forayEffort
  }, options = list(lengthChange = FALSE, paging = FALSE, columns.defaultContent = ''),
  server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodesForayEffort, fieldNamesForayEffort)$fields))
  
  # Save data:
  
  observeEvent(input$submitForayEffort, {
    # Remove data associated with site from database:
    forayEffortMongo$remove(siteDateQuery('siteIDForayEffort', input$siteIDForayEffort,
                                          'dateForayEffort', input$dateForayEffort), multiple = TRUE)
    # Add data to database:
    forayEffortMongo$insert(tableValues$forayEffort)
    shinyjs::alert('Thank you, your forayEffort data have been submitted!')
  })
  
  #--------------------------------------------------------------------*
  # ForayCountUnbanded table filtered by site:
  #--------------------------------------------------------------------*
  
  # Input fields:
  
  formDataForayCountUnbanded <- reactive({
    sapply(names(getTableMetadata(fieldCodesForayCountUnbanded,
                                  fieldNamesForayCountUnbanded)$fields),
           function(x) as.character(input[[x]]))
  })
  
  # Get data:
  
  observe({
    if(!is.null(input$hub)){
      if(input$hub != 'noData'){
        if(!is.null(input$siteId)){
          if(input$siteId != 'noData'){
            if(!is.null(input$dateForayCountUnbanded)){
              forayCountUnbandedTable <- forayCountUnbandedMongo$find(
                query = siteDateQuery('siteIDForayCountUnbanded', input$siteIDForayCountUnbanded,
                                      'dateForayCountUnbanded', input$dateForayCountUnbanded),
                fields = '{"_row" : 0, "_id" : 0}') %>%
                mongoToTblDf
              if(nrow(forayCountUnbandedTable) > 0){
                tableValues$forayCountUnbanded <- forayCountUnbandedTable %>%
                  select(one_of(fieldCodesForayCountUnbanded))
              } else {
                tableValues$forayCountUnbanded <- emptyDataFrame(fieldCodesForayCountUnbanded)
              }
            }
          }
        }
      }
    }
  })
  
  # Add or modify records:
  
  observeEvent(input$addRecordForayCountUnbanded, {
    tableValues$forayCountUnbanded <- dataAddOrModify(
      tableValues$forayCountUnbanded,
      input$responsesForayCountUnbanded_rows_selected, 
      formDataForayCountUnbanded())
    createBlankInputs(blankFieldsForayCountUnbanded, session)
  }, priority = 1)
  
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesForayCountUnbanded_rows_selected, {
    showRecordInputs(tableValues$forayCountUnbanded, input$responsesForayCountUnbanded_rows_selected,
                     fieldCodesForayCountUnbanded, session)
  })
  
  # Delete selected record:
  
  observeEvent(input$deleteForayCountUnbanded, {
    tableValues$forayCountUnbanded <- deleteRecord(
      tableValues$forayCountUnbanded,
      input$responsesForayCountUnbanded_rows_selected)
  }, priority = 1)
  
  # Data table output:
  
  output$responsesForayCountUnbanded <- DT::renderDataTable({
    # Update after submit or delete is clicked
    input$addRecordForayCountUnbanded ; input$deleteForayCountUnbanded
    # Table display:
    tableValues$forayCountUnbanded
  }, options = list(lengthChange = FALSE, paging = FALSE, columns.defaultContent = ''),
  server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodesForayCountUnbanded, fieldNamesForayCountUnbanded)$fields))
  
  # Save data:
  
  observeEvent(input$submitForayCountUnbanded, {
    # Remove data associated with site from database:
    forayCountUnbandedMongo$remove(
      siteDateQuery('siteIDForayCountUnbanded', input$siteIDForayCountUnbanded,
                    'dateForayCountUnbanded', input$dateForayCountUnbanded), 
      multiple = TRUE)
    # Add data to database:
    forayCountUnbandedMongo$insert(tableValues$forayCountUnbanded)
    shinyjs::alert('Thank you, your forayCountUnbanded data have been submitted!')
  })
  
  #--------------------------------------------------------------------*
  # TechRs table filtered by site:
  #--------------------------------------------------------------------*
  
  # Input fields:
  
  formDataTechRs <- reactive({
    sapply(names(getTableMetadata(fieldCodesTechRs,
                                  fieldNamesTechRs)$fields),
           function(x) as.character(input[[x]]))
  })
  
  # Get data:
  
  observe({
    if(!is.null(input$hub)){
      if(input$hub != 'noData'){
        if(!is.null(input$siteId)){
          if(input$siteId != 'noData'){
            if(!is.null(input$dateTechRs)){
              techRsTable <- techRsMongo$find(
                query = siteDateQuery('siteIDTechRs', input$siteIDTechRs,
                                      'dateTechRs', input$dateTechRs),
                fields = '{"_row" : 0, "_id" : 0}') %>%
                mongoToTblDf
              if(nrow(techRsTable) > 0){
                tableValues$techRs <- techRsTable %>%
                  select(one_of(fieldCodesTechRs))
              } else {
                tableValues$techRs <- emptyDataFrame(fieldCodesTechRs)
              }
            }
          }
        }
      }
    }
  })
  
  # Add or modify records:
  
  observeEvent(input$addRecordTechRs, {
    tableValues$techRs <- dataAddOrModify(
      tableValues$techRs,
      input$responsesTechRs_rows_selected, 
      formDataTechRs())
    createBlankInputs(blankFieldsTechRs, session)
  }, priority = 1)
  
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesTechRs_rows_selected, {
    showRecordInputs(tableValues$techRs, input$responsesTechRs_rows_selected,
                     fieldCodesTechRs, session)
  })
  
  # Delete selected record:
  
  observeEvent(input$deleteTechRs, {
    tableValues$techRs <- deleteRecord(
      tableValues$techRs,
      input$responsesTechRs_rows_selected)
  }, priority = 1)
  
  # Data table output:
  
  output$responsesTechRs <- DT::renderDataTable({
    # Update after submit or delete is clicked
    input$addRecordTechRs ; input$deleteTechRs
    # Table display:
    tableValues$techRs
  }, options = list(lengthChange = FALSE, paging = FALSE, columns.defaultContent = ''),
  server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodesTechRs, fieldNamesTechRs)$fields))
  
  # Save data:
  
  observeEvent(input$submitTechRs, {
    # Remove data associated with site from database:
    techRsMongo$remove(siteDateQuery('siteIDTechRs', input$siteIDTechRs,
                                     'dateTechRs', input$dateTechRs), multiple = TRUE)
    # Add data to database:
    techRsMongo$insert(tableValues$techRs)
    shinyjs::alert('Thank you, your techRs data have been submitted!')
  })
  
  #--------------------------------------------------------------------*
  # Pc table filtered by site:
  #--------------------------------------------------------------------*
  
  # Input fields:
  
  formDataPc <- reactive({
    sapply(names(getTableMetadata(fieldCodesPc,
                                  fieldNamesPc)$fields),
           function(x) as.character(input[[x]]))
  })
  
  # Get data:
  
  observe({
    if(!is.null(input$hub)){
      if(input$hub != 'noData'){
        if(!is.null(input$siteId)){
          if(input$siteId != 'noData'){
            if(!is.null(input$datePc)){
              pcTable <- pcMongo$find(
                query = siteDateQuery('siteIDPc', input$siteIDPc,
                                      'datePc', input$datePc),
                fields = '{"_row" : 0, "_id" : 0}') %>%
                mongoToTblDf
              if(nrow(pcTable) > 0){
                tableValues$pc <- pcTable %>%
                  select(one_of(fieldCodesPc))
              } else {
                tableValues$pc <- emptyDataFrame(fieldCodesPc)
              }
            }
          }
        }
      }
    }
  })
  
  # Add or modify records:
  
  observeEvent(input$addRecordPc, {
    tableValues$pc <- dataAddOrModify(
      tableValues$pc,
      input$responsesPc_rows_selected, 
      formDataPc())
    createBlankInputs(blankFieldsPc, session)
  }, priority = 1)
  
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesPc_rows_selected, {
    showRecordInputs(tableValues$pc, input$responsesPc_rows_selected,
                     fieldCodesPc, session)
  })
  
  # Delete selected record:
  
  observeEvent(input$deletePc, {
    tableValues$pc <- deleteRecord(
      tableValues$pc,
      input$responsesPc_rows_selected)
  }, priority = 1)
  
  # Data table output:
  
  output$responsesPc <- DT::renderDataTable({
    # Update after submit or delete is clicked
    input$addRecordPc ; input$deletePc
    # Table display:
    tableValues$pc
  }, options = list(lengthChange = FALSE, paging = FALSE, columns.defaultContent = ''),
  server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodesPc, fieldNamesPc)$fields))
  
  # Save data:
  
  observeEvent(input$submitPc, {
    # Remove data associated with site from database:
    pcMongo$remove(siteDateQuery('siteIDPc', input$siteIDPc,
                                 'datePc', input$datePc), multiple = TRUE)
    # Add data to database:
    pcMongo$insert(tableValues$pc)
    shinyjs::alert('Thank you, your pc data have been submitted!')
  })
  
  #--------------------------------------------------------------------*
  # PartRs table filtered by site:
  #--------------------------------------------------------------------*
  
  # Input fields:
  
  formDataPartRs <- reactive({
    sapply(names(getTableMetadata(fieldCodesPartRs,
                                  fieldNamesPartRs)$fields),
           function(x) as.character(input[[x]]))
  })
  
  # Get data:
  
  observe({
    if(!is.null(input$siteIDPartRs)){
      if(input$siteIDPartRs != 'noData'){
        partRsTable <- partRsMongo$find(
          query = siteQuery('siteIDPartRs', input$siteIDPartRs),
          fields = '{"_row" : 0, "_id" : 0}') %>%
          mongoToTblDf
        if(nrow(partRsTable) > 0){
          tableValues$partRs <- partRsTable %>%
            select(one_of(fieldCodesPartRs))
        } else {
          tableValues$partRs <- emptyDataFrame(fieldCodesPartRs)
        }
      }
    }
  })
  
  # Add or modify records:
  
  observeEvent(input$addRecordPartRs, {
    tableValues$partRs <- dataAddOrModify(
      tableValues$partRs,
      input$responsesPartRs_rows_selected, 
      formDataPartRs())
    createBlankInputs(blankFieldsPartRs, session)
  }, priority = 1)
  
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesPartRs_rows_selected, {
    showRecordInputs(tableValues$partRs, input$responsesPartRs_rows_selected,
                     fieldCodesPartRs, session)
  })
  
  # Delete selected record:
  
  observeEvent(input$deletePartRs, {
    tableValues$partRs <- deleteRecord(
      tableValues$partRs,
      input$responsesPartRs_rows_selected)
  }, priority = 1)
  
  # Data table output:
  
  output$responsesPartRs <- DT::renderDataTable({
    # Update after submit or delete is clicked
    input$addRecordPartRs ; input$deletePartRs
    # Table display:
    tableValues$partRs
  }, options = list(lengthChange = FALSE, paging = FALSE, columns.defaultContent = ''),
  server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodesPartRs, fieldNamesPartRs)$fields))
  
  # Save data:
  
  observeEvent(input$submitPartRs, {
    # Remove data associated with site from database:
    partRsMongo$remove(siteQuery('siteIDPartRs', input$siteIDPartRs), multiple = TRUE)
    # Add data to database:
    partRsMongo$insert(tableValues$partRs)
    shinyjs::alert('Thank you, your partRs data have been submitted!')
  })
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: QUERY AOU names ----
  #-------------------------------------------------------------------------------*
  output$aouTable = DT::renderDataTable(
    datatable(aouCodes, filter = 'none', rownames = FALSE,
              options = list(pageLength = 3)))
  
  #-------------------------------------------------------------------------------*
  # ---- NET HOURS CALCULATOR ----
  #-------------------------------------------------------------------------------*
  
  observe({
    totalMinutes <- sum(
      input$netMinutes6Visit,
      input$netMinutes9Visit*1.5,
      input$netMinutes12Visit * 2,
      input$netMinutes18Visit*3)
    updateNumericInput(session, 'netHoursVisit',
                       value = round(totalMinutes/60,1))
  })
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: QUERY BANDING RECORDS ----
  #-------------------------------------------------------------------------------*
  
  queryTableBySite <- reactive({
    if(!is.null(tableValues$queryTable)){
      outTable <- tableValues$queryTable
    }
    if(input$showAllSites != TRUE){
      outTable <- outTable %>% filter(siteID == input$siteIDQuery)
    }
    outTable
  })
  
  output$uiQuerySppChoices <- renderUI({
    selectInput("sppQuery", "",choices = queryTableBySite()$spp)
  })
  
  queryTable <- reactive({
    if(!is.null(queryTableBySite())){
      queryOut <- queryTableBySite()
      if(input$showAllSpp != TRUE) {
        queryOut <- queryOut %>% filter(spp == input$sppQuery)
      }
      if(input$showAllSex != TRUE) {
        queryOut <- queryOut %>% filter(sex == input$sexQuery)
      }
      if(input$showAllColorCombos != TRUE) {
        queryOut <- queryOut %>%
          filter(str_detect(colorCombo, toupper(input$colorComboQuery)))
      }
      queryOut
    }
  })
  
  output$queryTable <- DT::renderDataTable(
    DT::datatable({
      queryTable()
    }, rownames = FALSE, selection = 'none')
  )
  
  output$downloadData <- downloadHandler(
    filename = function() { str_c('captureHistory-', input$siteIDQuery, '.csv') },
    content = function(file) {
      write.csv(queryTable(), file, row.names = FALSE)
    }
  )
}
