DetailByIterationCard <- R6::R6Class(
  classname = "DetailByIterationCard",
  inherit = Component,
  public = list(
    copyInput = NULL,
    
    initialize = function() {
      self$copyInput <- CopyInput$new()
    },
    
    ui = function(inputId) {
      ns <- NS(inputId)
      
      bs4Card(
        title = strong("Details by Iteration"),
        collapsible = FALSE,
        closable = FALSE,
        width = 12,
        enable_sidebar = TRUE,
        sidebar = bs4CardSidebar(
          tagList(
            pickerInput(
              inputId = ns("iterations"),
              label = "Iterations",
              choices = c(),
              width = "100%"
            )
          )
        ),
        fluidRow(
          style = "padding: 20px;",
          column(
            width = 12,
            self$copyInput$ui(
              inputId = ns("copy_best_so_far"),
              label = "Copy Best So Far Table"
            ),
            h5(strong("Description of the best-so-far Configuration")),
            htmlOutput(outputId = ns("summaryBestSoFar")),
            br(),
            DTOutput(outputId = ns("bestSoFarTable")),
            br(),
            hr(),
            self$copyInput$ui(
              inputId = ns("copy_elite_config"),
              label = "Copy Elite Configurations Table"
            ),
            h5(strong("Elite configurations")),
            DTOutput(outputId = ns("eliteConfigurationTable")),
            br()
          )
        )
      )
    },
    
    server = function(input, output, session, store) {
      copy_best_so_far <- self$copyInput$call(id = "copy_best_so_far", store = store)
      copy_elite_config <- self$copyInput$call(id = "copy_elite_config", store = store)
      
      observeEvent(copy_best_so_far$action, {
        req(store$iraceResults)
        req(copy_best_so_far$section)
        req(store$currentExecution)
        report <- store$currentExecution$get_report()
        store$copy$id <- report$get_id(copy_best_so_far$section)
        store$copy$table <- "bestSoFarTable"
      })
      
      observeEvent(copy_elite_config$action, {
        req(store$iraceResults)
        req(copy_elite_config$section)
        req(store$currentExecution)
        report <- store$currentExecution$get_report()
        store$copy$id <- report$get_id(copy_elite_config$section)
        store$copy$table <- "eliteConfigurationTable"
      })
      
      observeEvent(store$iraceResults, {
        if (is.null(store$iraceResults)) {
          updatePickerInput(
            session = session,
            inputId = "iterations",
            choices = ""
          )
        } else {
          updatePickerInput(
            session = session,
            inputId = "iterations",
            choices = 1:store$iraceResults$state$nbIterations
          )
        }
      }, ignoreNULL = FALSE)
      
      output$summaryBestSoFar <- renderUI({
        shiny::validate(
          need(
            !is.null(store$iraceResults), ""
          )
        )
        
        shiny::validate(
          need(
            nrow(store$iraceResults$allConfigurations) != 0, ""
          )
        )

        req(input$iterations, cancelOutput = TRUE)

        bestConfigurations <- store$iraceResults$allElites[as.integer(input$iterations)]
        bestConfigurationID <- bestConfigurations[[1]][1]
        detailsBestConfiguration <- getConfigurationById(store$iraceResults, ids = bestConfigurationID)
        
        names(detailsBestConfiguration)[names(detailsBestConfiguration) == ".ID."] <- "ID"
        names(detailsBestConfiguration)[names(detailsBestConfiguration) == ".PARENT."] <- "PARENT"
        
        meanValue <- colMeans(
          store$iraceResults$experiments[, store$iraceResults$iterationElites[as.integer(input$iterations)], drop = FALSE],
          na.rm = TRUE
        )
        
        tagList(
          HTML(paste(strong("Best so far Configuration:"), "&nbsp;", bestConfigurationID)),
          br(),
          HTML(paste(strong("Mean value:"), "&nbsp;", meanValue))
        )
      })
      
      output$bestSoFarTable <- renderDT({
        shiny::validate(
          need(
            !is.null(store$iraceResults),
            "Provide a Rdata file (logFile) to display information."
          )
        )
        
        shiny::validate(
          need(
            nrow(store$iraceResults$allConfigurations) != 0,
            "ERROR: The number of candidate configurations is 0."
          )
        )
        
        req(input$iterations, cancelOutput = TRUE)
        
        bestConfigurations <- store$iraceResults$allElites[as.integer(input$iterations)]
        bestConfigurationID <- bestConfigurations[[1]][1]
        detailsBestConfiguration <- getConfigurationById(store$iraceResults, ids = bestConfigurationID)
        
        names(detailsBestConfiguration)[names(detailsBestConfiguration) == ".ID."] <- "ID"
        names(detailsBestConfiguration)[names(detailsBestConfiguration) == ".PARENT."] <- "PARENT"
        
        pkg$reportStore$bestSoFarTable <- detailsBestConfiguration
        
        datatable(
          detailsBestConfiguration,
          escape = FALSE,
          rownames = FALSE,
          style = "bootstrap4",
          class = "table-condensed table-striped cell-border",
          selection = "none",
          extensions = "FixedColumns",
          options = list(
            scrollX = TRUE,
            searching = FALSE,
            paging = FALSE,
            fixedColumns = list(leftColumns = 1, rightColumns = 0),
            dom = "t"
          )
        )
      })
      
      output$eliteConfigurationTable <- renderDT({
        shiny::validate(
          need(
            !is.null(store$iraceResults),
            "Provide a Rdata file (logFile) to display information."
          )
        )
        
        shiny::validate(
          need(
            nrow(store$iraceResults$allConfigurations) != 0,
            "ERROR: The number of candidate configurations is 0."
          )
        )
        
        req(input$iterations)
        
        bestConfigurations <- store$iraceResults$allElites[as.integer(input$iterations)]
        bestConfigurationID <- bestConfigurations[[1]]
        
        elitesConfig <- getConfigurationById(store$iraceResults, ids = bestConfigurationID)
        
        names(elitesConfig)[names(elitesConfig) == ".ID."] <- "ID"
        names(elitesConfig)[names(elitesConfig) == ".PARENT."] <- "PARENT"
        
        pkg$reportStore$eliteConfigurationTable <- elitesConfig
        
        datatable(
          elitesConfig,
          escape = FALSE,
          rownames = FALSE,
          style = "bootstrap4",
          class = "table-condensed table-striped cell-border",
          selection = "none",
          extensions = c("FixedColumns"),
          options = list(
            scrollX = TRUE,
            searching = FALSE,
            paging = FALSE,
            fixedColumns = list(leftColumns = 1, rightColumns = 0),
            dom = "t"
          )
        )
      })
    }
  )
)