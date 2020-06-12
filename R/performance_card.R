PerformanceCard <- R6::R6Class(
  classname = "PerformanceCard",
  inherit = Component,
  public = list(
    copyInput = NULL,
    
    initialize = function() {
      self$copyInput <- CopyInput$new()
    },
    
    ui = function(inputId) {
      ns <- NS(inputId)
      
      bs4Card(
        title = strong("Performance"),
        collapsible = FALSE,
        closable = FALSE,
        width = 12,
        fluidRow(
          column(
            width = 6,
            self$copyInput$ui(
              inputId = ns("copy_converge"),
              label = "Copy Convergence Plot"
            ),
            plotOutput(outputId = ns("converge_plot"), height = 650)
          ),
          column(
            width = 6,
            self$copyInput$ui(inputId = ns("copy_box"), label = "Copy Box Plot"),
            pickerInput(
              inputId = ns("iterations"),
              label = "Iterations",
              choices = c(),
              width = "100%"
            ),
            plotOutput(outputId = ns("performance_box_plot"), height = 550)
          )
        )
      )
    },
    
    server = function(input, output, session, store) {
      copy_converge <- self$copyInput$call(id = "copy_converge", store = store)
      copy_box <- self$copyInput$call(id = "copy_box", store = store)
      
      observeEvent(copy_converge$action, {
        req(store$iraceResults)
        req(copy_converge$section)
        req(store$currentExecution)
        report <- store$currentExecution$get_report()
        store$copy$id <- report$get_id(copy_converge$section)
        store$copy$plot <- "convergencePlot"
      })
      
      observeEvent(copy_box$action, {
        req(store$iraceResults)
        req(copy_box$section)
        req(store$currentExecution)
        report <- store$currentExecution$get_report()
        store$copy$id <- report$get_id(copy_box$section)
        store$copy$plot <- "performanceBoxPlot"
      })
      
      observe({
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
      })
      
      output$performance_box_plot <- renderPlot({
        shiny::validate(
          need(
            !is.null(store$iraceResults),
            "Provide a Rdata file (logFile) to display information."
          ),
          need(input$iterations, "")
        )
        shiny::validate(
          need(
            nrow(store$iraceResults$allConfigurations) != 0,
            "ERROR: Cannot plot because IRACE did not finish. The number of candidate configurations is 0."
          )
        )
        
        iteration <- as.integer(input$iterations)
        
        future({
          configurationPerIteration <- convert_vector_to_string(store$iraceResults$allElites[iteration][[1]])
          results <- store$iraceResults$experiments
          intersectedColumns <- self$format_col_data(results, configurationPerIteration)
          results <- subset(store$iraceResults$experiments, select = intersectedColumns)
          conf <- gl(ncol(results), nrow(results), labels = colnames(results))
          pairwise.wilcox.test(as.vector(results), conf, paired = TRUE, p.adj = "bonf")
          
          na.omit(results)
        }) %...>% {
          plot <- configurationsBoxplot(., ylab = "Solution cost", title = "Box Plot")
          pkg$reportStore$performanceBoxPlot <- save_plot_as_base64()
          plot
        }
      })
      
      output$converge_plot <- renderPlot({
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
        
        fes <- cumsum(table(iters))
        fes <- fes[!names(fes) == "0"]
        elites <- as.character(store$iraceResults$iterationElites)
        
        shiny::validate(
          need(
            dim(store$iraceResults$experiments[, elites]) != 0,
            "ERROR: Cannot plot because IRACE did not finish. Must be an array of two dimensions."
          )
        )
        
        values <- colMeans(store$iraceResults$experiments[, elites])
        
        data <- data.frame(x = fes, y = values, e = elites)
        data <- na.omit(data)
        
        p <- plot(
          x = data$x,
          y = data$y,
          type = "s",
          xlab = "Number of runs of the target algorithm",
          ylab = "Mean value over testing set",
          main = "Convergence Plot",
        )
        points(x = data$x, y = data$y)
        text(x = data$x, y = data$y, labels = data$e, pos = 1)
        
        pkg$reportStore$convergencePlot <- save_plot_as_base64()
        
        p
      })
    },
    
    format_col_data = function(resultsData, iterationData) {
      vectorColNames <- colnames(resultsData)
      formatedData <- Reduce(intersect, list(vectorColNames, iterationData))
      return(formatedData)
    }
  )
)