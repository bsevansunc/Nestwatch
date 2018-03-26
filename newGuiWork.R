library(rhandsontable)
library(shiny)
library(stringi)

visit1 <- data_frame(
  `SITE:` = vector('character', length = 1),
  `LONGITUDE:` = vector('numeric', length = 1),
  `LATITUDE:` = vector('numeric', length = 1),
  `ACCURACY:` = vector('integer', length = 1)
)

visit2 <- data_frame(
  `DATE:` = vector('character', length = 1),
  `OBSERVER(S):` = vector('numeric', length = 1),
  `ENG:` = vector('numeric', length = 1),
  `ENCOUNTERED BIRDS:` = vector('integer', length = 1)
)

netEffort1 <- data_frame(
  `Total time, 6 m (minutes)` = vector('integer', length = 1),
  `Total time, 12 m (minutes)` = vector('integer', length = 1)
)

netEffort2 <- data_frame(
  `Total time, 9 m (minutes)` = vector('integer', length = 1),
  `Total time, 18 m (minutes)` = vector('integer', length = 1)
)

visitNotes <- data_frame(
  `Visit notes:` = vector('character', length = 1)
)

foray <- data_frame(
  `Observer` = vector('character', length = 4),
  `Foray #` = vector('integer', length = 4),
  `Start time` = vector('character', length = 4),
  `End time` = vector('character', length = 4),
  `Path dist` = vector('integer', length = 4)
)

countUnbanded <- data_frame(
  `Species` = vector('character', length = 4),
  `Count` = vector('integer', length = 4)
)

techResights <- data_frame(
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

nnSpecies <- 
  c(
    'AMRO', 'BCCH','BRTH', 'CACH', 'CARW', 'GRCA', 'HOWR', 'NOCA',
    'NOMO', 'SOSP'
  )

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
      'Neighborhood Nestwatch Data Entry Interface',
      tabPanel(
        'Visit and capture information',
        fluidRow(
          column(10, offset = 1,
                 rHandsontableOutput("hot_visit1")),
          column(1, '')
        ),
        fluidRow(
          column(10, offset = 1,
                 rHandsontableOutput("hot_visit2")),
          column(1, '')
        ),
        fluidRow(
          column(
            5,
            offset = 1,
            HTML('<h3><b>Net effort:</b></h3>')
          ),
          column(
            5,
            offset = 1,
            HTML('<h3><b>Visit notes:</b></h3>')
          )),
          fluidRow(
            column(5, offset = 1,
                   rHandsontableOutput('hot_netEffort')),
            column(5, offset = 1,
                   rHandsontableOutput('hot_visitNotes'))
          )
        ),
      tabPanel(
        'Resight foray',
        fluidPage(
          fluidRow(
            column(
              5,
              offset = 1,
              HTML('<h3><b>Resight foray, time and distance</b></h3>')
            ),
            column(
              5,
              offset = 1,
              HTML('<h3><b>Resight foray, unbanded focal species</b></h3>')
            )
          ),
          fluidRow(
            column(5, offset = 1, rHandsontableOutput("hot_foray")),
            column(5, offset = 1, rHandsontableOutput("hot_countUnbanded"))
          ),
          br(),
          fluidRow(
            column(5, offset = 1, actionButton('submit_foray',
                                               'Submit foray time and distance',
                                               class = 'btn-primary')),
            column(5, offset = 1, actionButton('submit_countUnbanded',
                                               'Submit observations of unbanded focal species',
                                               class = 'btn-primary'))
          ),
          hr(),
          fluidRow(
            column(11, offset = 1, HTML('<h3><b>Resighted banded birds by technician</b></h3>'))
          ),
          fluidRow(
            column(11, offset = 1, rHandsontableOutput("hot_techResights"))
          ),
          br(),
          fluidRow(
            column(11, offset = 1, actionButton('submit_techResights',
                                                'Submit technician resights',
                                                class = 'btn-primary'))
          )
        )
      )))
     


# server ------------------------------------------------------------------

server <- shinyServer(function(input, output) {
  
  #------------------------------------------------------------------------*
  # ---- reactive values ----
  #------------------------------------------------------------------------*
  
  values <- reactiveValues()
  
  # Visit metadata, first line:
  
  observe({
    if (!is.null(input$hot_visit1)) {
      visit1 <- hot_to_r(input$hot_visit1)
    } else {
      if(is.null(values[['visit1']]))
        visit1 <- visit1
      else
        visit1 <- values[["visit1"]]
    }
    values[['visit1']] <- visit1
  })
  
  # Visit metadata, second line:
  
  observe({
    if (!is.null(input$hot_visit2)) {
      visit2 <- hot_to_r(input$hot_visit2)
    } else {
      if(is.null(values[['visit2']]))
        visit2 <- visit2
      else
        visit2 <- values[["visit2"]]
    }
    values[['visit2']] <- visit2
  })
  
  # Foray metadata:
  
  observe({
    if (!is.null(input$hot_foray)) {
      foray <- hot_to_r(input$hot_foray)
    } else {
      if(is.null(values[['foray']]))
        foray <- foray
      else
        foray <- values[["foray"]]
    }
    values[['foray']] <- foray
  })
  
  # Foray count unbanded:
  
  observe({
    if (!is.null(input$hot_countUnbanded)) {
      countUnbanded <- hot_to_r(input$hot_countUnbanded)
    } else {
      if(is.null(values[['countUnbanded']]))
        countUnbanded <- countUnbanded
      else
        countUnbanded <- values[["countUnbanded"]]
    }
    values[['countUnbanded']] <- countUnbanded
  })
  
  # Foray resights:
  
  observe({
    if (!is.null(input$hot_techResights)) {
      techResights <- hot_to_r(input$hot_techResights)
    } else {
      if(is.null(values[['techResights']]))
        techResights <- techResights
      else
        techResights <- values[["techResights"]]
    }
    values[['techResights']] <- techResights
  })
  
  #------------------------------------------------------------------------*
  # ---- outputs ----
  #------------------------------------------------------------------------*
  
  # Visit, first line -----------------------------------------------------
  
  output$hot_visit1 <- renderRHandsontable({
    visit1 <- values[['visit1']]
    if(!is.null(visit1)){
      rhandsontable(visit1, rowHeaders = NULL) %>%
        hot_table(highlightRow = TRUE) %>%
        hot_cols(colWidths = c(300,200,200,200),
                 halign = 'htCenter',
                 valign = 'htMiddle',
                 renderer = color_renderer) %>%
        hot_rows(rowHeights = 40)
    }
  })
  
  # Visit, second line ----------------------------------------------------
  
  output$hot_visit2 <- renderRHandsontable({
    visit2 <- values[['visit2']]
    if(!is.null(visit2)){
      rhandsontable(visit2, rowHeaders = NULL) %>%
        hot_table(highlightRow = TRUE) %>%
        hot_cols(colWidths = c(300,200,200,200),
                 halign = 'htCenter',
                 valign = 'htMiddle',
                 renderer = color_renderer) %>%
        hot_rows(rowHeights = 40)
    }
  })
  
  # Foray -----------------------------------------------------------------
  
  output$hot_foray <- renderRHandsontable({
    foray <- values[['foray']]
    if(!is.null(foray)){
      rhandsontable(foray) %>%
        hot_table(highlightRow = TRUE) %>%
        hot_cols(colWidths = c(80,60, 100, 100, 100),
                 halign = 'htCenter',
                 valign = 'htMiddle',
                 renderer = color_renderer) %>%
        hot_col('Start time', type = "dropdown", source = timeOfDay) %>%
        hot_col('End time', type = "dropdown", source = timeOfDay) %>%
        hot_validate_numeric('Path dist', min = 0, max = 10000) %>%
        hot_rows(rowHeights = 40)
    }
    })
  
  # Count unbanded --------------------------------------------------------
  
  output$hot_countUnbanded <- renderRHandsontable({
    countUnbanded <- values[['countUnbanded']]
    if(!is.null(countUnbanded)){
      rhandsontable(countUnbanded) %>%
        hot_table(highlightRow = TRUE) %>%
        hot_cols(colWidths = c(100,100),
                 halign = 'htCenter',
                 valign = 'htMiddle',
                 renderer = color_renderer) %>%
        hot_validate_numeric('Count', min = 0, max = 100) %>%
        hot_col('Species', type = "dropdown", source = nnSpecies) %>%
        hot_rows(rowHeights = 40)
    }
  })
  
  # Tech resights ---------------------------------------------------------
  
  output$hot_techResights <- renderRHandsontable({
    techResights <- values[['techResights']]
    if(!is.null(techResights)){
      rhandsontable(techResights) %>%
          hot_table(highlightRow = TRUE) %>%
          hot_cols(
            colWidths = c(80, 100, 50, 150, 120, 120, 70, 280),
            halign = 'htCenter',
            valign = 'htMiddle',
            renderer = color_renderer) %>%
          # hot_col('Observer', validator = "
          #      function (value, callback) {
          #         setTimeout(function(){
          #         callback(!= 0);
          #         }, 1000)
          #       }",
          #      allowInvalid = FALSE) %>%
          # hot_validate_numeric('Count', min = 0, max = 100) %>%
          # hot_col('Observer', type = "dropdown", source = choice_techs) %>%
          hot_rows(rowHeights = 40)
    }
  })

  # output$hot_foray = renderRHandsontable({
  #   if (!is.null(input$hot_foray)) {
  #     foray <- hot_to_r(input$hot_foray)
  #   } else {
  #     if(is.null(values[['foray']]))
  #       foray <- foray
  #     else
  #       foray <- values[["foray"]]
  #     }
  #     values[['foray']] <- foray
  #     rhandsontable(foray) %>%
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
  #     countUnbanded <- hot_to_r(input$hot_countUnbanded)
  #   } else {
  #     if(is.null(values[['countUnbanded']]))
  #       countUnbanded <- countUnbanded
  #     else
  #       countUnbanded <- values[["countUnbanded"]]
  #   }
  #   values[['countUnbanded']] <- countUnbanded
  #   rhandsontable(countUnbanded) %>%
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
  #     techResights <- hot_to_r(input$hot_techResights)
  #   } else {
  #     if(is.null(values[['techResights']]))
  #       techResights <- techResights
  #     else
  #       techResights <- values[["techResights"]]
  #   }
  #   values[['techResights']] <- techResights
  #   rhandsontable(techResights) %>%
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
  #       DF <- foray
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

