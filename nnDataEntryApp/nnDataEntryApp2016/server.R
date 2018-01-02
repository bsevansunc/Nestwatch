#=================================================================================*
# ---- SERVER ----
#=================================================================================*
# setwd('C:/Users/Brian/Dropbox/nnDataEntryApp')
# setwd('/Users/bsevans/Dropbox/nnDataEntryApp')

source('setUp.R', local=TRUE)
library(readr)
library(mongolite)
library(tidyr)
library(dplyr)

mongoURL <- 'mongodb://bsevans:33shazam@ds025232.mlab.com:25232/nndataentry'

server <- function(input, output, session) {
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: LOAD MONGO DATA ----
  #-------------------------------------------------------------------------------*
  
  tableValues <- reactiveValues()
  
  # Site data frame filtered by hub:
  
  observe({
    if(!is.null(input$hub)){
      if(input$hub != ''){
        siteMongo <- mongo('site_data', url = mongoURL)
        siteHubTable <- siteMongo$find(
          query = hubQuery('hub', input$hub),
          fields = '{"_row" : 0, "_id" : 0}') %>%
          mongoToTblDf
        if(nrow(siteHubTable) > 0){
          tableValues$siteHub <- siteHubTable %>%
            select(one_of(fieldCodesSite))
        } else {
          tableValues$siteHub <- emptyDataFrame(fieldCodesSite)
        }
      }
    }
  })
  
  # Visit data filtered by site:
  
  observe({
    if(!is.null(input$site)){
      if(input$site != ''){
        visitMongo <- mongo('visit_data', url = mongoURL)
        visitTable <- visitMongo$find(
          query = hubQuery('hub', input$hub),
          fields = '{"_row" : 0, "_id" : 0}') %>%
          mongoToTblDf %>%
          filter(site == input$site)
        if(nrow(visitTable) > 0){
          tableValues$visit <- visitTable %>%
            select(one_of(fieldCodesVisit))
        } else {
          tableValues$visit <- emptyDataFrame(fieldCodesVisit)
        }
      }
    }
  })
  
  # Encounter data filtered by hub:
  
  observe({
    if(!is.null(input$hub)){
      if(input$hub != ''){
        encMongo <- mongo('encounter_data', url = mongoURL)
        encTable <- encMongo$find(
          query = hubQuery('hub', input$hub),
          fields = '{"_row" : 0, "_id" : 0}') %>%
          mongoToTblDf
        if(nrow(encTable) > 0){
          tableValues$enc <- encTable %>%
            select(one_of(fieldCodesEnc))
        } else {
          tableValues$enc <- emptyDataFrame(fieldCodesEnc)
        }
      }
    }
  })
  
  # Point count data filtered by site:
  
  observe({
    if(!is.null(input$site)){
      if(input$site != ''){
        pcMongo <- mongo('pointCount_data', url = mongoURL)
        pcTable <- pcMongo$find(
          query = hubQuery('hub', input$hub),
          fields = '{"_row" : 0, "_id" : 0}') %>%
          mongoToTblDf %>%
          filter(site == input$site)
        if(nrow(pcTable) > 0){
          tableValues$pointCount <- pcTable %>%
            select(one_of(fieldCodesPc))
        } else {
          tableValues$pointCount <- emptyDataFrame(fieldCodesPc)
        }
      }
    }
  })
  
  # Nest summary data filtered by site:
  
  observe({
    if(!is.null(input$site)){
      if(input$site != ''){
        nestSummaryMongo <- mongo('nest_summary_data', url = mongoURL)
        nestSummaryTable <- nestSummaryMongo$find(
          query = hubQuery('hub', input$hub),
          fields = '{"_row" : 0, "_id" : 0}') %>%
          mongoToTblDf %>%
          filter(site == input$site)
        if(nrow(nestSummaryTable) > 0){
          tableValues$nestSummary <- nestSummaryTable %>%
            select(one_of(fieldCodesNestSummary))
        } else {
          tableValues$nestSummary <- emptyDataFrame(fieldCodesNestSummary)
        }
      }
    }
  })
  
  # Nest observation data filtered by site:
  
  observe({
    if(!is.null(input$site)){
      if(input$site != ''){
        nestMongo <- mongo('nest_observation_data', url = mongoURL)
        nestTable <- nestMongo$find(
          query = hubQuery('hub', input$hub),
          fields = '{"_row" : 0, "_id" : 0}') %>%
          mongoToTblDf %>%
          filter(site == input$site)
        if(nrow(nestTable) > 0){
          tableValues$nest <- nestTable %>%
            select(one_of(fieldCodesNest))
        } else {
          tableValues$nest <- emptyDataFrame(fieldCodesNest)
        }
      }
    }
  })
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: REACTIVE INPUTS ----
  #-------------------------------------------------------------------------------*
  
  # Given a hub input, get vector of sites:
  
  siteChoices <- reactive({
    sites <- ''
    if(!is.null(input$hub)){
      if(input$hub != ''){
        sites <- tableValues$siteHub %>% 
          arrange(site) %>%
          .$site
      }
    }
    c('', sites)
  })
  
  # If it is a new site, type it in, otherwise retrieve vector of sites:
  
  output$ui <- renderUI({
    if (is.null(input$inputType))
      return()
    switch(input$inputType,
           'New site' = textInput("site", "Site:", ''),
           'Existing site' = selectInput("site", "Site:",
                                         choices = siteChoices())
    )
  })
  
  selectedSite <- reactive(input$site)
  
  # Once OBSERVERS are written on the visit page, have this be the default entry:
  
  observe({
    observerInputs <- c('observerEnc', 'observerPc')
    for(i in 1:length(observerInputs)){
      updateTextInput(session, observerInputs[i], value = input$observer)
    }
  })
  
  # Net hours calculator:
  
  observe({
    totalMinutes <- sum(
      input$netMinutes6,
      input$netMinutes9*1.5,
      input$netMinutes12 * 2,
      input$netMinutes18*3)
    updateNumericInput(session, 'netHours',
                       value = round(totalMinutes/60,1))
  })
  
  # Once species has been written on the encounter page, have this be the default entry for the query:
  
  observe({
    inSpecies <- input$speciesEnc
    print(inSpecies)
    if(is.null(inSpecies) | inSpecies == '')
      return(NULL)
    updateTextInput(session, 'speciesQuery', value = input$speciesEnc)
  })
  
  # Once sex has been written on the encounter page, have this be the default entry for the query:
  
  observe({
    inSex <- input$sex
    print(inSex)
    if(is.null(inSex) | inSex == '')
      return(NULL)
    updateTextInput(session, 'sexQuery', value = input$sex)
  })
  
  # Once bandCombo has been written on the encounter page, have this be the default entry for query:
  
  observe({
    inCombo <- input$colorCombo
    print(inCombo)
    if(is.null(inCombo)| inCombo == '')
      return(NULL)
    updateTextInput(session, 'bandComboQuery', value = input$colorCombo)
  })
  
  #-------------------------------------------------------------------------------*
  # ---- SUBMIT SITE DATA ----
  #-------------------------------------------------------------------------------*
  # Input fields:
  
  formDataSite <- reactive({
    sapply(names(getTableMetadata(fieldCodesSite, fieldNamesSite)$fields),
           function(x) as.character(input[[x]]))
  })
  
  # Read data associated with site into reactive values container:
  
  observe({
    if(!is.null(input$site)){
      if(input$site != ''){
        siteTable <- tableValues$siteHub %>%
          filter(site == input$site)
        if(nrow(siteTable) > 0){
          tableValues$site <- siteTable %>%
            select(one_of(fieldCodesSite))
        } else {
          tableValues$site <- emptyDataFrame(fieldCodesSite)
        }
      }
    }
  })
  
  # Add or modify records:
  
  observeEvent(input$submitSite, {
    tableValues$site <- dataAddOrModify(
      tableValues$site,
      input$responsesSite_rows_selected, 
      formDataSite())
  }, priority = 1)
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesSite_rows_selected, {
    showRecordInputs(tableValues$site, input$responsesSite_rows_selected,
                     fieldCodesSite, session)
  })
  
  # Delete selected record:
  
  observeEvent(input$deleteSite, {
    tableValues$site <- deleteRecord(
      tableValues$site,
      input$responsesSite_rows_selected)
  }, priority = 1)
  
  # Data table output:
  
  output$responsesSite <- DT::renderDataTable({
    # Update after submit or delete is clicked
    input$submitSite ; input$deleteSite
    # Table display:
    tableValues$site
  }, options = list(lengthChange = FALSE, paging = FALSE, columns.defaultContent = ''),
  server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodesSite, fieldNamesSite)$fields))
  
  # Save data:
  
  observeEvent(input$submitSiteData, {
    mongoSite <- mongo('site_data', url = mongoURL)
    # Remove data associated with site from database:
    mongoSite$remove(siteQuery('site', input$site), multiple = TRUE)
    # Add data to database:
    mongoSite$insert(tableValues$site)
    shinyjs::show("thankyou_msgSite")
  })
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: VISIT DATA ----
  #-------------------------------------------------------------------------------*
  # Input fields:
  
  formDataVisit <- reactive({
    sapply(names(getTableMetadata(fieldCodesVisit, fieldNamesVisit)$fields),
           function(x) as.character(input[[x]]))
  })
  
  # Read data associated with site into reactive values container:
  
  observe({
    if(!is.null(input$site)){
      if(input$site != ''){
        visitTable <- tableValues$visit
        if(nrow(visitTable) > 0){
          tableValues$visit <- visitTable %>%
            select(one_of(fieldCodesVisit))
        } else {
          tableValues$visit <- emptyDataFrame(fieldCodesVisit)
        }
      }
    }
  })
  
  # Add or modify records:
  
  observeEvent(input$submitVisit, {
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
  
  # When "clear inputs" is pressed, make some inputs blank:
  
  observeEvent(input$newVisit, {
    createBlankInputs(blankFieldsVisit, session)
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
    input$submitVisit ; input$deleteVisit
    # Table display:
    tableValues$visit
  }, options = list(paging = FALSE, lengthChange = FALSE, columns.defaultContent = ''),
  server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodesVisit, fieldNamesVisit)$fields))
  
  # Save data:
  
  observeEvent(input$submitVisitData, {
    mongoVisit <- mongo('visit_data', url = mongoURL)
    # Remove data associated with site from database:
    mongoVisit$remove(siteQuery('site', input$site), multiple = TRUE)
    # Add data to database:
    mongoVisit$insert(tableValues$visit)
    shinyjs::show("thankyou_msgVisit")
  })
  
  #----------------------------------------------------------------------*
  # ENCOUNTER DATA
  #----------------------------------------------------------------------*
  # Input fields:
  
  formDataEnc <- reactive({
    sapply(names(getTableMetadata(fieldCodesEnc, fieldNamesEnc)$fields),
           function(x) as.character(input[[x]]))
  })
  
  # Read data associated with site into reactive values container:
  
  observe({
    if(!is.null(input$site)){
      if(input$site != ''){
        encTable <- tableValues$enc %>%
          filter(site == input$site)
        if(nrow(encTable) > 0){
          tableValues$encTable <- encTable %>%
            select(one_of(fieldCodesEnc))
        } else {
          tableValues$encTable <- emptyDataFrame(fieldCodesEnc)
        }
      }
    }
  })
  
  # Add or modify records:
  
  observeEvent(input$submitEnc, {
    tableValues$encTable <- dataAddOrModify(
      tableValues$encTable,
      input$responsesEnc_rows_selected, 
      formDataEnc())
    createBlankInputs(blankFieldsEnc, session)
  }, priority = 1)
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesEnc_rows_selected, {
    showRecordInputs(tableValues$encTable, input$responsesEnc_rows_selected,
                     fieldCodesEnc, session)
  })
  
  # When "clear inputs" is pressed, make some inputs blank:
  
  observeEvent(input$newEnc, {
    createBlankInputs(blankFieldsEnc, session)
  })
  
  # Delete selected record:
  
  observeEvent(input$deleteEnc, {
    tableValues$encTable <- deleteRecord(
      tableValues$encTable,
      input$responsesEnc_rows_selected)
  }, priority = 1)
  
  # Data table output:
  
  output$responsesEnc <- DT::renderDataTable({
    # Update after submit or delete is clicked
    input$submitEnc ; input$deleteEnc
    # Table display:
    tableValues$encTable
  }, options = list(lengthChange = FALSE, paging = FALSE, columns.defaultContent = ''),
  server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodesEnc, fieldNamesEnc)$fields))
  
  # Save data:
  
  observeEvent(input$submitEncData, {
    mongoEnc <- mongo('encounter_data', url = mongoURL)
    # Remove data associated with site from database:
    mongoEnc$remove(siteQuery('site', input$site), multiple = TRUE)
    # Add data to database:
    mongoEnc$insert(tableValues$encTable)
    shinyjs::show("thankyou_msgEnc")
  })
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: QUERY BANDING RECORDS ----
  #-------------------------------------------------------------------------------*
  # !!!This is temporary and will be replaced by the true encounter records once all files from previous years are added in the correct format!!!
  
  # Query encounters as a function of hub and site
  
  #   observe({
  #     if(!is.null(input$hub)){
  #       if(input$hub != '' & input$showAllSites == TRUE){
  #         queryMongoDf <- tableValues$enc %>%
  #           select(hub, site, date, speciesEnc, sex, bandNumber, colorCombo, encounterType) %>%
  #           rename(speciesQuery = speciesEnc,
  #                  sexQuery = sex,
  #                  bandNumberQuery = bandNumber,
  #                  colorComboQuery = colorCombo,
  #                  encounterTypeQuery = encounterType)
  #         updateSelectInput(session, 'speciesQuery', 
  #                           choices = queryMongoDf$species %>% unique %>% sort)
  #         tableValues$queryTable <- queryMongoDf
  #       }
  #     }
  #   })
  
  encQuery <- reactive({
    if(!is.null(input$hub)){
      if(input$hub != ''){
        encounters <- tableValues$enc %>%
          select(hub, site, date, speciesEnc, sex, bandNumber, colorCombo, encounterType) %>%
          rename(speciesQuery = speciesEnc,
                 sexQuery = sex,
                 bandNumberQuery = bandNumber,
                 colorComboQuery = colorCombo,
                 encounterTypeQuery = encounterType)
        if(input$showAllSites == FALSE){
          encounters <- filter(encounters, site == input$site)
        }
        if(!is.null(encounters)){
          if(input$showAllSpecies == 'FALSE'  & input$speciesQuery != ''){
            encounters <- encounters %>%
              filter(str_detect(speciesQuery, input$speciesQuery))
          }
          if(input$showAllSex == 'FALSE'  & input$sexQuery != ''){
            encounters <- encounters %>%
              filter(str_detect(sexQuery, toupper(input$sexQuery)))
          }
          if(input$showAllBandCombo == 'FALSE'  & input$colorComboQuery != ''){
            encounters <- encounters %>%
              filter(str_detect(colorComboQuery, toupper(input$colorComboQuery)))
          }
          if(input$showAllBandNumber == 'FALSE'  & input$bandNumberQuery != ''){
            encounters <- encounters %>%
              filter(str_detect(bandNumberQuery, toupper(input$bandNumberQuery)))
          }
          if(input$showAllEncounterType == 'FALSE'  & input$encounterTypeQuery != ''){
            encounters <- encounters %>%
              filter(str_detect(toupper(encounterTypeQuery), toupper(input$encounterTypeQuery)))
          }
        }
      }
      encounters
    }
  })
  
  observe({
    if(!is.null(input$hub)){
      if(input$hub != ''){
        updateSelectInput(session, 'speciesQuery', 
                          choices = tableValues$enc$species %>% unique %>% sort)
      }
    }
  })
  
  output$encounterTable <- DT::renderDataTable(
    DT::datatable({
      encQuery()
    }, rownames = FALSE, selection = 'none')
  )
  
  output$downloadData <- downloadHandler(
    filename = function() { str_c('encounterData-', input$site, '.csv') },
    content = function(file) {
      write.csv(encQuery(), file, row.names = FALSE)
    }
  )
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: QUERY AOU names ----
  #-------------------------------------------------------------------------------*
  output$aouTable = DT::renderDataTable(
    datatable(aouCodes, filter = 'none', rownames = FALSE,
              options = list(pageLength = 3)))
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: SUBMIT POINT COUNT DATA ----
  #-------------------------------------------------------------------------------*
  # Input fields:
  
  formDataPc <- reactive({
    sapply(names(getTableMetadata(fieldCodesPc, fieldNamesPc)$fields),
           function(x) as.character(input[[x]]))
  })
  
  # Read table values into reactive container:
  
  observe({
    if(!is.null(input$site)){
      if(input$site != ''){
        pcTable <- tableValues$pointCount
        if(nrow(pcTable) > 0){
          tableValues$pcTable <- pcTable %>%
            select(one_of(fieldCodesPc))
        } else {
          tableValues$pcTable <- emptyDataFrame(fieldCodesPc)
        }
      }
    }
  })
  
  # Add or modify records:
  
  observeEvent(input$submitPc, {
    tableValues$pcTable <- dataAddOrModify(
      tableValues$pcTable,
      input$responsesPc_rows_selected, 
      formDataPc())
    createBlankInputs(blankFieldsPc, session)
  }, priority = 1)
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesPc_rows_selected, {
    showRecordInputs(tableValues$pcTable, input$responsesPc_rows_selected,
                     fieldCodesPc, session)
  })
  
  # When "clear inputs" is pressed, make some inputs blank:
  
  observeEvent(input$newPc, {
    createBlankInputs(blankFieldsPc, session)
  })
  
  # Delete selected record:
  
  observeEvent(input$deletePc, {
    tableValues$pcTable <- deleteRecord(
      tableValues$pcTable,
      input$responsesPc_rows_selected)
  }, priority = 1)
  
  # Data table output:
  
  output$responsesPc <- DT::renderDataTable({
    # Update after submit or delete is clicked
    input$submitPc ; input$deletePc
    # Table display:
    tableValues$pcTable
  }, options = list(lengthChange = FALSE, paging = FALSE, columns.defaultContent = ''),
  server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodesPc, fieldNamesPc)$fields))
  
  # Save data:
  
  observeEvent(input$submitPcData, {
    mongoPc <- mongo('pointCount_data', url = mongoURL)
    # Remove data associated with site from database:
    mongoPc$remove(siteQuery('site', input$site), multiple = TRUE)
    # Add data to database:
    mongoPc$insert(tableValues$pcTable)
    shinyjs::show("thankyou_msgPc")
  })
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: NEST SUMMARY DATA ----
  #-------------------------------------------------------------------------------*
  # Input fields:
  
  formDataNestSummary <- reactive({
    sapply(names(getTableMetadata(fieldCodesNestSummary, fieldNamesNestSummary)$fields),
           function(x) as.character(input[[x]]))
  })
  
  # Add or modify records:
  
  observeEvent(input$submitNestSummary, {
    tableValues$nestSummary <- dataAddOrModify(
      tableValues$nestSummary,
      input$responsesNestSummary_rows_selected, 
      formDataNestSummary())
    createBlankInputs(blankFieldsNestSummary, session)
  }, priority = 1)
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesNestSummary_rows_selected, {
    showRecordInputs(tableValues$nestSummary, input$responsesNestSummary_rows_selected,
                     fieldCodesNestSummary, session)
  })
  
  # When "clear inputs" is pressed, make some inputs blank:
  
  observeEvent(input$newNestSummary, {
    createBlankInputs(blankFieldsNestSummary, session)
  })
  
  # Delete selected record:
  
  observeEvent(input$deleteNestSummary, {
    tableValues$nestSummary <- deleteRecord(
      tableValues$nestSummary,
      input$responsesNestSummary_rows_selected)
  }, priority = 1)
  
  # Data table output:
  
  output$responsesNestSummary <- DT::renderDataTable({
    # Update after submit or delete is clicked
    input$submitNestSummary ; input$deleteNestSummary
    # Table display:
    tableValues$nestSummary
  }, options = list(lengthChange = FALSE, paging = FALSE, columns.defaultContent = ''),
  server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodesNestSummary, fieldNamesNestSummary)$fields))
  
  # Save data:
  
  observeEvent(input$submitNestSummaryData, {
    mongoNestSummary <- mongo('nest_summary_data', url = mongoURL)
    # Remove data associated with site from database:
    mongoNestSummary$remove(siteQuery('site', input$site), multiple = TRUE)
    # Add data to database:
    mongoNestSummary$insert(tableValues$nestSummary)
    shinyjs::show("thankyou_msgNestSummary")
  })
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: NEST OBSERVATION DATA ----
  #-------------------------------------------------------------------------------*
  # Input fields:
  
  formDataNest <- reactive({
    sapply(names(getTableMetadata(fieldCodesNest, fieldNamesNest)$fields),
           function(x) as.character(input[[x]]))
  })
  
  # Add or modify records:
  
  observeEvent(input$submitNest, {
    tableValues$nest <- dataAddOrModify(
      tableValues$nest,
      input$responsesNest_rows_selected, 
      formDataNest())
    createBlankInputs(blankFieldsNest, session)
  }, priority = 1)
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesNest_rows_selected, {
    showRecordInputs(tableValues$nest, input$responsesNest_rows_selected,
                     fieldCodesNest, session)
  })
  
  # When "clear inputs" is pressed, make some inputs blank:
  
  observeEvent(input$newNest, {
    createBlankInputs(blankFieldsNest, session)
  })
  
  # Delete selected record:
  
  observeEvent(input$deleteNest, {
    tableValues$nest <- deleteRecord(
      tableValues$nest,
      input$responsesNest_rows_selected)
  }, priority = 1)
  
  # Data table output:
  
  output$responsesNest <- DT::renderDataTable({
    # Update after submit or delete is clicked
    input$submitNest ; input$deleteNest
    # Table display:
    tableValues$nest
  }, options = list(lengthChange = FALSE, paging = FALSE, columns.defaultContent = ''),
  server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodesNest, fieldNamesNest)$fields))
  
  # Save data:
  
  observeEvent(input$submitNestData, {
    mongoNest <- mongo('nest_observation_data', url = mongoURL)
    # Remove data associated with site from database:
    mongoNest$remove(siteQuery('site', input$site), multiple = TRUE)
    # Add data to database:
    mongoNest$insert(tableValues$nest)
    shinyjs::show("thankyou_msgNest")
  })
}
