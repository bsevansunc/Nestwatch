library(rhandsontable)
library(shiny)
library(stringi)


# seed data frames --------------------------------------------------------

visit1 <- data_frame(
  `<b>Site:</b>` = vector('character', length = 1),
  `<b>Longitude:</b>` = vector('numeric', length = 1),
  `<b>Latitude:</b>` = vector('numeric', length = 1),
  `<b>Accuracy:</b>` = vector('integer', length = 1)
)

visit2 <- data_frame(
  `<b>Date:</b>` = vector('character', length = 1),
  `<b>Observer(s):</b>` = vector('character', length = 1),
  `<b>Engagement:</b>` = vector('integer', length = 1),
  `<b>Encountered birds?</b>` = vector('character', length = 1)
)

netEffort <- data_frame(
  `<b>Net length:</b>` = c('6 m', '9 m', '12 m', '18 m'),
  `<b>Total time open (minutes):</b>` = vector('integer', length = 4)
)

visitNotes <- data_frame(
  `<b>Visit notes:</b>` = vector('character', length = 1)
)

capture <- data_frame(
  `<b>time</b>` = vector('character', length = 8),
  `<b>obs</b>` = vector('character', length = 8),
  `<b>enc</b>` = vector('character', length = 8),
  `<b>spp</b>` = vector('character', length = 8),
  `<b>prefix</b>` = vector('character', length = 8),
  `<b>suffix</b>` = vector('character', length = 8),
  `<b>cLeft</b>` = vector('character', length = 8),
  `<b>cRight</b>` = vector('character', length = 8),
  `<b>mass</b>` = vector('numeric', length = 8),
  `<b>wing</b>` = vector('numeric', length = 8),
  `<b>tail</b>` = vector('numeric', length = 8),
  `<b>age</b>` = vector('character', length = 8),
  `<b>sex</b>` = vector('character', length = 8),
  `<b>cp/bp</b>` = vector('character', length = 8),
  `<b>fat</b>` = vector('character', length = 8),
  `<b>sample IDs</b>` = vector('character', length = 8),
  `<b>notes</b>` = vector('character', length = 8)
)

capture0 <- data_frame(
  `<b>time</b>` = vector('character', length = 0),
  `<b>obs</b>` = vector('character', length = 0),
  `<b>enc</b>` = vector('character', length = 0),
  `<b>spp</b>` = vector('character', length = 0),
  `<b>bandPre</b>` = vector('character', length = 0),
  `<b>bandSuff</b>` = vector('character', length = 0),
  `<b>left</b>` = vector('character', length = 0),
  `<b>right</b>` = vector('character', length = 0),
  `<b>mass</b>` = vector('numeric', length = 0),
  `<b>wing</b>` = vector('numeric', length = 0),
  `<b>tail</b>` = vector('numeric', length = 0),
  `<b>age</b>` = vector('character', length = 0),
  `<b>sex</b>` = vector('character', length = 0),
  `<b>cp/bp</b>` = vector('character', length = 0),
  `<b>fat</b>` = vector('character', length = 0),
  `<b>sample IDs</b>` = vector('character', length = 0),
  `<b>notes</b>` = vector('character', length = 0)
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
  `prefix` = vector('character', length = 5),
  `suffix` = vector('character', length = 5),
  `Longitude (dd.dddd)` = vector('numeric', length = 5),
  `Latitude (dd.dddd)` = vector('numeric', length = 5),
  `Resight type` = vector('character', length = 5),
  `Notes` = vector('character', length = 5)
)

pCountMeta <- data_frame(
  `Site` = vector('character', length = 1),
  `Observer` = vector('character', length = 1),
  `Date` = vector('character', length = 1),
  `Start time` = vector('character', length = 1)
)

pCount <- data_frame(
  `Interval` = rep(1, 15),
  `Species` = vector('character', length = 15),
  `Dist < 10` = vector('integer', length = 15),
  `Dist 10-20` = vector('integer', length = 15),
  `Dist 20-30` = vector('integer', length = 15),
  `Dist 30-40` = vector('integer', length = 15),
  `Dist 40-50` = vector('integer', length = 15),
  `Detection` = vector('character', length = 15)
)


# possible values ---------------------------------------------------------

nnSpecies <- 
  c(
    'AMRO', 'BCCH','BRTH', 'CACH', 'CARW', 'GRCA', 'HOWR', 'NOCA',
    'NOMO', 'SOSP'
  )

choiceSpecies <- nnSpecies

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
      
      # Visit data --------------------------------------------------------
      
      tabPanel(
        'Visit and capture information',
        fixedPage(
          fixedRow(
            column(10, offset = 1,
                   HTML('<h3><b>Visit table</b></h3>')),
            column(1, '')
          ),
          fixedRow(
            column(10, offset = 1,
                   rHandsontableOutput("hot_visit1")),
            column(1, '')
          ),
          fixedRow(
            column(10, offset = 1,
                   rHandsontableOutput("hot_visit2")),
            column(1, '')
          ),
          br(),
          fixedRow(
            column(4, offset = 1, rHandsontableOutput('hot_netEffort'), style = 'padding-right: 0; margin-right: 0;'),
            column(7, rHandsontableOutput('hot_visitNotes'),style = 'padding-left: 0; margin-left: 0;'),
            column(1, '')
          ),
          br(),
          fixedRow(
            column(5, offset = 1, actionButton('submit_visit',
                                               'Submit visit information',
                                               class = 'btn-primary')),
            column(6, '')
          ),
          hr(),
          fixedRow(
            column(10, offset = 1,
                   HTML('<h3><b>Capture table</b></h3>')),
            column(1, '')
          ),
          fixedRow(
            column(10, offset = 1, rHandsontableOutput('hot_capture')),
            column(1, '')
          ),
          br(),
          fixedRow(
            column(5, offset = 1, actionButton('submit_capture',
                                               'Submit capture information',
                                               class = 'btn-primary')),
            column(6, '')
          ),
          br()
        )),
      
      # Resight foray data --------------------------------------------------
      
      tabPanel(
        'Resight foray',
        fixedPage(
          fixedRow(
            column(
              7,
              offset = 1,
              style = 'padding-right: 0; margin-right: 0;',
              HTML('<h3><b>Time and distance</b></h3>')
            ),
            column(
              4,
              offset = 0,
              style = 'padding-left: 15; margin-left: 15;',
              HTML('<h3><b>Unbanded focal species</b></h3>')
            )
          ),
          fixedRow(
            column(7, offset = 1, rHandsontableOutput("hot_foray"), style = 'padding-right: 0; margin-right: 0;'),
            column(4, offset = 0, rHandsontableOutput("hot_countUnbanded"), style = 'padding-left: 15; margin-left: 15;')
          ),
          br(),
          fixedRow(
            column(7, offset = 1, actionButton('submit_foray',
                                               'Submit foray time and distance',
                                               style = 'margin-right: 0;',
                                               class = 'btn-primary')),
            column(4, offset = 0, actionButton('submit_countUnbanded',
                                               ' Submit observations of unbanded focal species',
                                               style = 'margin-left: 15;',
                                               class = 'btn-primary'))
          ),
          hr(),
          fixedRow(
            column(11, offset = 1, HTML('<h3><b>Resighted banded birds by technician</b></h3>'))
          ),
          fixedRow(
            column(11, offset = 1, rHandsontableOutput("hot_techResights"))
          ),
          br(),
          fixedRow(
            column(11, offset = 1, actionButton('submit_techResights',
                                                'Submit technician resights',
                                                class = 'btn-primary'))
          ),
          br()
        )
      ),
      
      # Point counts ------------------------------------------------------
      
      tabPanel('Point count',
               fixedPage(
                   fixedRow(
                     column(10, offset = 1,
                            HTML('<h3><b>Point count metadata</b></h3>')),
                     column(1, '')
                   ),
                   fixedRow(
                     column(10, offset = 1,
                            rHandsontableOutput("hot_pCountMeta")),
                     column(1, '')
                   ),
                   br(),
                   fixedRow(
                     column(11, offset = 1, actionButton('submit_pointCountMeta',
                                                         'Submit point count metadata',
                                                         class = 'btn-primary'))
                   ),
                   hr(),
                   fixedRow(
                     column(10, offset = 1,
                            HTML('<h3><b>Point count</b></h3>')),
                     column(1, '')
                   ),
                   fixedRow(
                     column(10, offset = 1,
                            rHandsontableOutput("hot_pCount")),
                     column(1, '')
                   ),
                   br(),
                   fixedRow(
                     column(11, offset = 1, actionButton('submit_pointCount',
                                                         'Submit point count',
                                                         class = 'btn-primary'))
                   ),
                   br()
               )
               )
      ))



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
  
  # Net effort:
  
  observe({
    if (!is.null(input$hot_netEffort)) {
      netEffort <- hot_to_r(input$hot_netEffort)
    } else {
      if(is.null(values[['netEffort']]))
        netEffort <- netEffort
      else
        netEffort <- values[["netEffort"]]
    }
    values[['netEffort']] <- netEffort
  })
  
  # Visit notes:
  
  observe({
    if (!is.null(input$hot_visitNotes)) {
      visitNotes <- hot_to_r(input$hot_visitNotes)
    } else {
      if(is.null(values[['visitNotes']]))
        visitNotes <- visitNotes
      else
        visitNotes <- values[["visitNotes"]]
    }
    values[['visitNotes']] <- visitNotes
  })
  
  # Captures:
  
  observe({
    if (!is.null(input$hot_capture)) {
      capture <- hot_to_r(input$hot_capture)
    } else {
      if(is.null(values[['capture']]))
        capture <- capture
      else
        capture <- values[["capture"]]
    }
    values[['capture']] <- capture
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
  
  # Point count metadata:
  
  observe({
    if (!is.null(input$hot_pCountMeta)) {
      pCountMeta <- hot_to_r(input$hot_pCountMeta)
    } else {
      if(is.null(values[['pCountMeta']]))
        pCountMeta <- pCountMeta
      else
        pCountMeta <- values[["pCountMeta"]]
    }
    values[['pCountMeta']] <- pCountMeta
  })
  
  # Point count:
  
  observe({
    if (!is.null(input$hot_pCount)) {
      pCount <- hot_to_r(input$hot_pCount)
    } else {
      if(is.null(values[['pCount']]))
        pCount <- pCount
      else
        pCount <- values[["pCount"]]
    }
    values[['pCount']] <- pCount
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
        hot_cols(colWidths = c(360,200,200,180),
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
        hot_cols(colWidths = c(360,200,200,180),
                 halign = 'htCenter',
                 valign = 'htMiddle',
                 renderer = color_renderer) %>%
        hot_rows(rowHeights = 40)# %>%
        # hot_col(`Encountered Birds?`, type = "dropdown", source = c('Y', 'N'))
    }
  })
  
  # Net effort ------------------------------------------------------------
  
  output$hot_netEffort <- renderRHandsontable({
    netEffort <- values[['netEffort']]
    if(!is.null(netEffort)){
      rhandsontable(netEffort, rowHeaders = NULL) %>%
        hot_table(highlightRow = TRUE) %>%
        hot_cols(colWidths = c(100,270),
                 halign = 'htCenter',
                 valign = 'htMiddle',
                 renderer = color_renderer) %>%
        hot_rows(rowHeights = 40)
    }
  })
  
  # Visit notes -----------------------------------------------------------
  
  output$hot_visitNotes <- renderRHandsontable({
    visitNotes <- values[['visitNotes']]
    if(!is.null(visitNotes)){
      rhandsontable(visitNotes, rowHeaders = NULL) %>%
        hot_table(highlightRow = TRUE) %>%
        hot_cols(colWidths = 565,
                 halign = 'htCenter',
                 valign = 'htMiddle',
                 renderer = color_renderer) %>%
        hot_rows(rowHeights = 160)
    }
  })
  
  # Capture ---------------------------------------------------------------
  
  output$hot_capture <- renderRHandsontable({
    capture <- values[['capture']]
    if(!is.null(capture)){
      rhandsontable(capture, rowHeaders = NULL) %>%
        hot_table(highlightRow = TRUE) %>%
        hot_cols(colWidths = c(50,40,35,75,   # ... - spp
                               50,80,50,50,   # prefix - cRight
                               50,50,50,      # mass - tail
                               40,40,50, 30,  # age - fat
                               100, 100),     # sample IDs - ...
                 halign = 'htCenter',
                 valign = 'htMiddle',
                 renderer = color_renderer) %>%
        hot_rows(rowHeights = 40) # %>%
        # hot_col('sex', type = "dropdown", source = c('F', 'M','U'))
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
          colWidths = c(80, 100, 50,50, 75, 120, 120, 70, 295),
          halign = 'htCenter',
          valign = 'htMiddle',
          renderer = color_renderer) %>%
        hot_rows(rowHeights = 40) %>%
        hot_col('Resight type', type = "dropdown", source = c('F', 'I'))
    }
  })
  
  # Point count metadata --------------------------------------------------
  
  output$hot_pCountMeta <- renderRHandsontable({
    pCountMeta <- values[['pCountMeta']]
    if(!is.null(pCountMeta)){
      rhandsontable(pCountMeta, rowHeaders = NULL) %>%
        hot_table(highlightRow = TRUE) %>%
        hot_cols(
          colWidths = c(250,200,250,200),
          halign = 'htCenter',
          valign = 'htMiddle',
          renderer = color_renderer) %>%
        hot_rows(rowHeights = 40)
    }
  })
  
  # Point count -----------------------------------------------------------
  
  output$hot_pCount <- renderRHandsontable({
    pCount <- values[['pCount']]
    if(!is.null(pCount)){
      rhandsontable(pCount, rowHeaders = NULL) %>%
        hot_table(highlightRow = TRUE) %>%
        hot_cols(
          colWidths = c(100,100,120,120,120,120,120,100),
          halign = 'htCenter',
          valign = 'htMiddle',
          renderer = color_renderer) %>%
        hot_col('Interval', type = "dropdown", source = 1:3) %>%
        hot_col('Detection', type = "dropdown", source = c('V', 'A', 'B')) %>%
        hot_col('Species', type = "dropdown", source = choiceSpecies) %>%
        hot_rows(rowHeights = 40)
    }
  })
  
  ###

  ## Save 
  # observeEvent(input$save, {
  #   finalDF <- isolate(values[["DF"]])
  #   saveRDS(finalDF, file=file.path(outdir, sprintf("%s.rds", outfilename)))
  # })
  
})



## run app 
runApp(list(ui=ui, server=server))

