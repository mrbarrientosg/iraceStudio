BestConfigurationCard <- R6::R6Class(
  classname = "BestConfigurationCard",
  inherit = Component,
  public = list(
    copyInput = NULL,
    
    initialize = function() {
      self$copyInput <- CopyInput$new()
    },
    
    ui = function(inputId) {
      ns <- NS(inputId)
      
      bs4Card(
        title = strong("Best Configuration"),
        collapsible = FALSE,
        closable = FALSE,
        width = 12,
        fluidRow(
          column(
            width = 6,
            htmlOutput(
              outputId = ns("best_so_far"),
              style = "overflow-y:scroll; max-height:650px; width:100%;"
            )
          ),
          column(
            width = 6,
            self$copyInput$ui(inputId = ns("copy"), label = "Copy Box Plot"),
            plotOutput(outputId = ns("box_plot"), width = 550, height = 550)
          )
        )
      )
    },
    
    server = function(input, output, session, store) {
      copy <- self$copyInput$call(id = "copy", store = store)
      
      observeEvent(copy$action, {
        req(store$iraceResults)
        req(copy$section)
        req(store$currentExecution)
        report <- store$currentExecution$get_report()
        store$copy$id <- report$get_id(copy$section)
        store$copy$plot <- "bestConfigBoxPlot"
      })
      
      output$best_so_far <- renderUI({
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
        
        last <- length(store$iraceResults$iterationElites)
        id <- store$iraceResults$iterationElites[last]
        bestConfiguration <- getConfigurationById(
          iraceResults = store$iraceResults,
          ids = id
        )
        
        meanValue <- colMeans(
          store$iraceResults$experiments[, store$iraceResults$iterationElites[last], drop = FALSE],
          na.rm = TRUE
        )[[1]]
        
        nbIterationsElites <- length(store$iraceResults$iterationElites)
        totalInstances <- sum(!is.na(store$iraceResults$experiments[, store$iraceResults$iterationElites[nbIterationsElites]]))
        
        tagList(
          h6(strong("Best so far:")),
          tags$ul(
            style = "list-style-type:none;",
            tags$li(HTML(paste(strong("Configuration:"), "&nbsp;", bestConfiguration[[1]]))),
            tags$li(HTML(paste(strong("Mean value:"), "&nbsp;", meanValue))),
            tags$li(HTML(paste(strong("Parent:"), "&nbsp;", bestConfiguration[[length(bestConfiguration)]]))),
            tags$li(HTML(paste(strong("Total instances tested:"), "&nbsp;", totalInstances)))
          ),
          br(),
          h6(strong("Description of the 'Best so far' configuration:")),
          tags$ul(
            style = "list-style-type:none;",
            self$list_configurations(bestConfiguration)
          )
        )
      })
      
      output$box_plot <- renderPlot({
        shiny::validate(
          need(
            !is.null(store$iraceResults),
            "Provide a Rdata file (logFile) to display information."
          )
        )
        
        shiny::validate(
          need(
            nrow(store$iraceResults$allConfigurations) != 0,
            "ERROR: Cannot plot because IRACE did not finish. The number of candidate configurations is 0."
          )
        )
        
        last <- length(store$iraceResults$iterationElites)
        id <- paste0(store$iraceResults$iterationElites[last])
        results <- subset(store$iraceResults$experiments, select = id)
        
        plot <- configurationsBoxplot(
          title = "Best Configuration Box Plot",
          experiments = results,
          ylab = "Solution cost"
        )
        
        pkg$reportStore$bestConfigBoxPlot <- save_plot_as_base64()
        
        plot
      }, )
    },
    
    list_configurations = function(configurations) {
      formatedData <- NULL
      
      for (i in 2:(length(configurations) - 1)) {
        formatedData <- c(
          formatedData,
          paste(
            "<li>", strong(paste0(colnames(configurations[i]), ":")), "&nbsp;",
            configurations[i], "</li>"
          )
        )
      }
      
      return(HTML(paste(formatedData, collapse = "\n")))
    }
  )
)