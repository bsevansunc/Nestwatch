library(rhandsontable)
library(shiny)
library(stringi)

foray_blank <- data_frame(
  `Observer` = vector('character', length = 4),
  `Foray #` = vector('integer', length = 4),
  `Start time (hh:mm)` = vector('character', length = 4),
  `End time (hh:mm)` = vector('character', length = 4),
  `Path distance` = vector('integer', length = 4)
)

forayCountUnbanded_blank <- data_frame(
  `Species` = vector('character', length = 4),
  `Count` = vector('integer', length = 4)
)

techResights_blank <- data_frame(
  `Observer` = vector('character', length = 5), 
  `Time` = vector('character', length = 5), 
  `Foray #` = vector('integer', length = 5),
  `Band #` = vector('character', length = 5),
  `Longitude (dd.dddd)` = vector('numeric', length = 5),
  `Latitude (dd.dddd)` = vector('numeric', length = 5),
  `Resight type` = vector('character', length = 5),
  `Notes` = vector('character', length = 5)
)


# possible values ---------------------------------------------------------

choice_techs <- 
  expand.grid(
    list(LETTERS[1:26], LETTERS[1:26], LETTERS[1:26]),
    stringsAsFactors = FALSE
  ) %>%
  unite(col = combinedCol, 1:3, sep  = '') %>%
  .$combinedCol


timeOfDay <-
  data.frame(time = format(seq(
    ISOdate(2000, 1, 1), ISOdate(2000, 1, 2), by = 'min'
  ), '%H:%M')) %>%
  distinct %>%
  arrange(time) %>%
  slice(301:1321) %>%
  .$time

color_renderer <- "
  function(instance, td) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    td.style.size = 20;
  }
"


outfilename="test_table"

outdir=getwd()


# ui ----------------------------------------------------------------------

ui <-
  shinyUI(
    navbarPage(
      'nn Data Entry Interface',
      tabPanel(
        'Resight foray',
        fluidPage(
          fluidRow(
            column(
              5,
              offset = 1,
              HTML('<p><b>Resight foray, time and distance</b></p>')
            ),
            column(
              5,
              offset = 1,
              HTML('<p><b>Resight foray, observed unbanded focal species</b></p>')
            )
          ),
          fluidRow(
            column(5, offset = 1, rHandsontableOutput("hot_foray")),
            column(5, offset = 1, rHandsontableOutput("hot_countUnbanded"))
          ),
          hr(),
          fluidRow(
            column(11, offset = 1, HTML('<p><b>Resighted banded birds by technician</b></p>'))
          ),
          fluidRow(
            column(11, offset = 1, rHandsontableOutput("hot_techResights"))
          )
        )
      )))
     


# server ------------------------------------------------------------------

server <- shinyServer(function(input, output) {
  
  #------------------------------------------------------------------------*
  # ---- reactive values ----
  #------------------------------------------------------------------------*
  
  values <- reactiveValues()
  
  # Foray metadata:
  
  observe({
    if (!is.null(input$hot_foray)) {
      foray_blank <- hot_to_r(input$hot_foray)
    } else {
      if(is.null(values[['foray_blank']]))
        foray_blank <- foray_blank
      else
        foray_blank <- values[["foray_blank"]]
    }
    values[['foray_blank']] <- foray_blank
  })
  
  # Foray count unbanded:
  
  observe({
    if (!is.null(input$hot_foray)) {
      foray_blank <- hot_to_r(input$hot_foray)
    } else {
      if(is.null(values[['hot_countUnbanded']]))
        foray_blank <- foray_blank
      else
        foray_blank <- values[["hot_countUnbanded"]]
    }
    values[['hot_countUnbanded']] <- foray_blank
  })
  
  # Foray resights:
  
  observe({
    if (!is.null(input$hot_foray)) {
      foray_blank <- hot_to_r(input$hot_foray)
    } else {
      if(is.null(values[['hot_countUnbanded']]))
        foray_blank <- foray_blank
      else
        foray_blank <- values[["hot_countUnbanded"]]
    }
    values[['hot_countUnbanded']] <- foray_blank
  })
  
  #------------------------------------------------------------------------*
  # ---- outputs ----
  #------------------------------------------------------------------------*
  
  output$hot_foray <- renderRHandsontable({
    foray_blank <- values[['foray_blank']]
    if(!is.null(foray_blank)){
      rhandsontable(foray_blank) %>%
        hot_table(highlightRow = TRUE) %>%
        hot_cols(colWidths = c(100,50, 100, 100, 100),
                 halign = 'htCenter',
                 valign = 'htMiddle',
                 renderer = color_renderer) %>%
        hot_col('Start time (hh:mm)', type = "dropdown", source = timeOfDay) %>%
        hot_col('End time (hh:mm)', type = "dropdown", source = timeOfDay) %>%
        hot_validate_numeric('Path distance', min = 0, max = 10000) %>%
        hot_validate_character('Observer', choices = choice_techs) %>%
        hot_rows(rowHeights = 40)
    }
    })
  
  output$hot_countUnbanded <- renderRHandsontable({
  })
  

  # output$hot_foray = renderRHandsontable({
  #   if (!is.null(input$hot_foray)) {
  #     foray_blank <- hot_to_r(input$hot_foray)
  #   } else {
  #     if(is.null(values[['foray_blank']]))
  #       foray_blank <- foray_blank
  #     else
  #       foray_blank <- values[["foray_blank"]]
  #     }
  #     values[['foray_blank']] <- foray_blank
  #     rhandsontable(foray_blank) %>%
  #       hot_table(highlightRow = TRUE) %>%
  #       hot_cols(colWidths = c(100,50, 100, 100, 100),
  #                halign = 'htCenter',
  #                valign = 'htMiddle',
  #                renderer = color_renderer) %>%
  #       hot_col('Start time (hh:mm)', type = "dropdown", source = timeOfDay) %>%
  #       hot_col('End time (hh:mm)', type = "dropdown", source = timeOfDay) %>%
  #       hot_validate_numeric('Path distance', min = 0, max = 10000) %>%
  #       hot_validate_character('Observer', choices = choice_techs) %>%
  #       hot_rows(rowHeights = 40)
  # })
  
  # output$hot_countUnbanded = renderRHandsontable({
  #   if (!is.null(input$hot_countUnbanded)) {
  #     forayCountUnbanded_blank <- hot_to_r(input$hot_countUnbanded)
  #   } else {
  #     if(is.null(values[['forayCountUnbanded_blank']]))
  #       forayCountUnbanded_blank <- forayCountUnbanded_blank
  #     else
  #       forayCountUnbanded_blank <- values[["forayCountUnbanded_blank"]]
  #   }
  #   values[['forayCountUnbanded_blank']] <- forayCountUnbanded_blank
  #   rhandsontable(forayCountUnbanded_blank) %>%
  #     hot_table(highlightRow = TRUE) %>%
  #     hot_cols(colWidths = c(100,100),
  #              halign = 'htCenter',
  #              valign = 'htMiddle',
  #              renderer = color_renderer) %>%
  #     hot_validate_numeric('Count', min = 0, max = 100) %>%
  #     hot_col('Species', type = "dropdown", source = choice_techs[1:5]) %>%
  #     hot_rows(rowHeights = 40)
  # })
  
  # output$hot_techResights = renderRHandsontable({
  #   if (!is.null(input$hot_techResights)) {
  #     techResights_blank <- hot_to_r(input$hot_techResights)
  #   } else {
  #     if(is.null(values[['techResights_blank']]))
  #       techResights_blank <- techResights_blank
  #     else
  #       techResights_blank <- values[["techResights_blank"]]
  #   }
  #   values[['techResights_blank']] <- techResights_blank
  #   rhandsontable(techResights_blank) %>%
  #     hot_table(highlightRow = TRUE) %>%
  #     hot_cols(
  #       colWidths = c(100,100, 50, 180, 180, 180, 70, 280),
  #       halign = 'htCenter',
  #       valign = 'htMiddle',
  #       renderer = color_renderer) %>%
  #     hot_col('Observer', validator = "
  #          function (value, callback) {
  #             setTimeout(function(){
  #             callback( != 0);
  #             }, 1000)
  #           }",
  #          allowInvalid = FALSE) %>%
  #     # hot_validate_numeric('Count', min = 0, max = 100) %>%
  #     # hot_col('Species', type = "dropdown", source = choice_techs[1:5]) %>%
  #     hot_rows(rowHeights = 40)
  # })
  
  ###
  
  # observe({
  #   if (!is.null(input$hot_foray)) {
  #     DF <- hot_to_r(input$hot_foray)
  #   } else {
  #     if (is.null(values[["DF"]]))
  #       DF <- foray_blank
  #     else
  #       DF <- values[["DF"]]
  #   }
  #   values[["DF"]] <- DF
  # })
  # 
  # output$hot_foray <- renderRHandsontable(
  #   DF <- values[["DF"]]
  #   if (!is.null(DF))
  #     rhandsontable(DF)
  # )
  # 
  ## Save 
  # observeEvent(input$save, {
  #   finalDF <- isolate(values[["DF"]])
  #   saveRDS(finalDF, file=file.path(outdir, sprintf("%s.rds", outfilename)))
  # })
  
})



## run app 
runApp(list(ui=ui, server=server))

