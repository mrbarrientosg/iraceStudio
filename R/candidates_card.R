CandidatesCard <- R6::R6Class(
  classname = "CandidatesCard",
  inherit = Component,
  public = list(
    copyInput = NULL,
    
    initialize = function() {
      self$copyInput <- CopyInput$new()
    },
    
    ui = function(inputId) {
      ns <- NS(inputId)
      
      bs4Card(
        title = strong("Candidates"),
        collapsible = FALSE,
        closable = FALSE,
        width = 12,
        sidebar_width = "30%",
        enable_sidebar = TRUE,
        height = "800px",
        inlineCSS(".direct-chat-contacts { z-index: 0 !important; }"),
        sidebar = bs4Dash::bs4CardSidebar(
          tagList(
            sliderInput(
              inputId = ns("iterations"),
              label = "Iterations", min = 0, max = 1,
              value = c(0, 0),
              width = "100%"
            ),
            multiInput(
              inputId = ns("parameters"),
              label = "Parameters",
              width = "100%",
              choices = "",
              selected = "",
              options = list(
                enable_search = TRUE,
                non_selected_header = "All options",
                selected_header = "Selected options"
              )
            ),
            inlineCSS(".multi-wrapper .non-selected-wrapper,
            .multi-wrapper .selected-wrapper { height: 500px; }"),
            actionButton(inputId = ns("update"), label = "Update", class = "btn-primary")
          )
        ),
        fluidRow(
          style = "padding: 20px;",
          column(
            width = 6,
            self$copyInput$ui(
              inputId = ns("copy_parallel"),
              label = "Copy Parallel Coordinates"
            ),
            uiOutput(
              outputId = ns("parallel_plot"),
              style = "overflow-y:scroll; max-height:650px; width:100%; margin-bottom:10px;"
            )
          ),
          column(
            width = 6,
            self$copyInput$ui(
              inputId = ns("copy_freq"),
              label = "Copy Frequency Plot"
            ),
            uiOutput(
              outputId = ns("freq_plot"),
              style = "overflow-y:scroll; max-height:650px; width:100%; margin-bottom:10px;"
            )
          )
        )
      )
    },
    
    server = function(input, output, session, store) {
      copy_parallel <- self$copyInput$call(id = "copy_parallel", store = store)
      copy_freq <- self$copyInput$call(id = "copy_freq", store = store)
      
      observeEvent(copy_parallel$action, {
        req(store$iraceResults)
        req(copy_parallel$section)
        req(store$currentExecution)
        report <- store$currentExecution$get_report()
        store$copy$id <- report$get_id(copy_parallel$section)
        store$copy$plot <- "parallelCoordinates"
      })
      
      observeEvent(copy_freq$action, {
        req(store$iraceResults)
        req(copy_freq$section)
        req(store$currentExecution)
        report <- store$currentExecution$get_report()
        store$copy$id <- report$get_id(copy_freq$section)
        store$copy$plot <- "parameterFreq"
      })
      
      observeEvent(store$iraceResults, {
        if (is.null(store$iraceResults)) {
          disable(id = "update")
          
          updateSliderInput(
            session = session,
            inputId = "iterations",
            min = 0,
            max = 0,
            value = 0,
            step = 1
          )
          
          updateMultiInput(
            session = session,
            inputId = "parameters",
            choices = ""
          )
          
        } else {
          updateSliderInput(
            session = session,
            inputId = "iterations",
            min = 1,
            max = store$iraceResults$state$nbIterations,
            value = c(store$iraceResults$state$nbIterations - 1, store$iraceResults$state$nbIterations),
            step = 1
          )
          
          updateMultiInput(
            session = session,
            inputId = "parameters",
            choices = store$iraceResults$parameters$names,
            selected = store$iraceResults$parameters$names
          )
          
          delay(1000, {
            enable(id = "update")
            click(id = "update")
            disable(id = "update")
          })
        }
      }, ignoreNULL = TRUE)
      
      nb_plots_freq <- reactiveVal(value = 0)
      nb_plots_parall <- reactiveVal(value = 0)
      
      #### Frequency render plot
      
      freq_plot_render <- eventReactive(input$update, {
        shiny::validate(
          need(
            length(input$parameters) >= 2,
            "ERROR: At least two parameters are necessary to generate the plots."
          )
        )
        
        disable(id = "update")
        
        createHiddenDirectory(".Fimages")
        
        nb_plots_freq(0)
        
        progress <- AsyncProgress$new(
          message = "Plotting: Frequency Plot",
          detail = "This may take a while...",
          value = 0
        )
        
        progress$set(0.1)
        
        configurations <- getConfigurationByIteration(
          iraceResults = store$iraceResults,
          iterations = input$iterations[1]:input$iterations[2]
        )
        
        parameters <- input$parameters
        progress$set(0.8, detail = "Making Plots...")
        
        future({
          max <- 12
          limit <- 1
          numberOfParameters <- ceiling(length(parameters) / max)
          
          for (i in seq_len(numberOfParameters)) {
            k <- 1
            params <- NULL
            
            for (j in limit:(max * i)) {
              if (length(parameters) >= j) {
                params[k] <- parameters[j]
                k <- k + 1
              }
            }
            
            progress$inc(0.05, detail = "Making Plots...")
            
            fixFormat <- store$iraceResults$parameters
            fixFormat$names <- params
            
            png(filename = sprintf(".Fimages/Rplot%03d.png", i), width = 550, height = 550, res = 80)
            parameterFrequency(configurations = configurations, parameters = fixFormat)
            dev.off()
            
            limit <- (max * i) + 1
          }
          
          files <- list.files(path = ".Fimages", pattern = ".*[.]png", full.names = TRUE)
          
          lapply(files, function(image) {
            image_uri(image)
          })
        }) %...>% {
          progress$set(1.0, detail = "Finishing...")
          files <- .
          pkg$reportStore$parameterFreq <- files
          unlink(".Fimages", recursive = TRUE, force = TRUE)
          progress$close()
        }
      })
      
      output$freq_plot <- renderUI({
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
        
        freq_plot_render() %...>% {
          nb_plots_freq(length(pkg$reportStore$parameterFreq))
          
          imageList <- lapply(seq_len(nb_plots_freq()), function(i) {
            imageName <- session$ns(paste0("image", i))
            htmlOutput(outputId = imageName, style = "display: flex; justify-content: center;")
          })
          
          do.call(tagList, imageList)
        }
      })
      
      observe({
        if (nb_plots_freq() == 0) {
          return(invisible())
        }
        
        for (i in seq_len(nb_plots_freq())) {
          local({
            my_i <- i
            data <- pkg$reportStore$parameterFreq[my_i]
            imageName <- paste0("image", my_i)
            
            output[[imageName]] <- renderUI({
              tags$img(src = data, height = "auto")
            })
          })
        }
        
        enable(id = "update")
      })
      
      ### Parallel Coordinates
      parallel_cord_render <- eventReactive(input$update, {
        conf <- getConfigurationByIteration(
          iraceResults = store$iraceResults,
          iterations = input$iterations[1]:input$iterations[2]
        )
        
        shiny::validate(
          need(
            length(input$parameters) >= 2,
            "ERROR: At least two parameters are necessary to generate the plots."
          ),
          need(
            nrow(conf) != 0,
            "ERROR: Cannot plot because IRACE did not finish. The amount of rows is 0."
          ),
          need(
            nrow(store$iraceResults$allConfigurations) != 0,
            "ERROR: Cannot plot because IRACE did not finish. The number of candidate configurations is 0."
          )
        )
        
        progress <- AsyncProgress$new(
          message = "Plotting: Parallel Coordinates",
          detail = "This may take a while...",
          value = 0
        )
        
        disable(id = "update")
        
        createHiddenDirectory(".Pimages")
        
        nb_plots_parall(0)
        parameters <- input$parameters
        
        future({
          max <- 12
          limit <- 1
          numberOfParameters <- ceiling(length(parameters) / max)
          
          for (i in seq_len(numberOfParameters)) {
            k <- 1
            params <- c()
            
            for (j in limit:(max * i)) {
              if (length(parameters) >= j) {
                params[k] <- parameters[j]
                k <- k + 1
              }
            }
            
            progress$inc(0.05, detail = "Making Plots...")
            png(
              filename = sprintf(".Pimages/Rplot%03d.png", i),
              width = 550,
              height = 550,
              res = 80
            )
            
            parallelCoordinatesPlot(
              configurations = conf,
              parameters = store$iraceResults$parameters,
              param_names = params,
              hierarchy = FALSE
            )
            dev.off()
            
            limit <- (max * i) + 1
          }
          
          files <- list.files(path = ".Pimages", pattern = ".*[.]png", full.names = TRUE)
          
          images <- lapply(sort(files), function(image) {
            image_uri(image)
          })
        }) %...>% {
          progress$inc(1.0, detail = "Finishing...")
          files <- .
          pkg$reportStore$parallelCoordinates <- files
          unlink(".Pimages", recursive = TRUE, force = TRUE)
          progress$close()
        }
      })
      
      output$parallel_plot <- renderUI({
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
        
        
        if (input$iterations[1] == 0) {
          return(invisible())
        }
        
        parallel_cord_render() %...>% {
          nb_plots_parall(length(pkg$reportStore$parallelCoordinates))
          
          imageList <- lapply(seq_len(nb_plots_parall()), function(i) {
            imageName <- session$ns(paste0("image-", i))
            htmlOutput(outputId = imageName, style = "display: flex; justify-content: center;")
          })
          
          do.call(tagList, imageList)
        }
      })
      
      observe({
        if (nb_plots_parall() == 0) {
          return(invisible())
        }
        
        for (i in seq_len(nb_plots_parall())) {
          local({
            my_i <- i
            data <- pkg$reportStore$parallelCoordinates[my_i]
            imageName <- paste0("image-", my_i)
            
            output[[imageName]] <- renderUI({
              tags$img(src = data, height = "auto")
            })
          })
        }
        
        enable(id = "update")
      })
    }
  )
)