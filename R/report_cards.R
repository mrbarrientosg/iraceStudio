SummaryCard <- R6::R6Class(
  classname = "SummaryCard",
  inherit = Component,
  public = list(
    ui = function(inputId) {
      ns <- NS(inputId)

      bs4Card(
        title = strong("Summary"),
        collapsible = FALSE,
        closable = FALSE,
        width = 12,
        htmlOutput(outputId = ns("summary_content"))
      )
    },

    server = function(input, output, session, store) {
      output$summary_content <- renderUI({
        shiny::validate(
          need(
            !is.null(store$iraceResults),
            "Provide a Rdata file (logFile) to display information."
          )
        )

        tags$ul(
          style = "list-style-type:none;",
          tags$li(
            HTML(
              paste(strong("IRACE version:"), "&nbsp;", store$iraceResults$irace.version)
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("Number of candidate configurations:"), "&nbsp;",
                nrow(store$iraceResults$allConfigurations)
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("Number of target executions:"), "&nbsp;",
                store$iraceResults$state$experimentsUsedSoFar
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("Elitist new instances:"), "&nbsp;",
                store$iraceResults$scenario$elitistNewInstances
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("Elitist limit:"), "&nbsp;",
                store$iraceResults$scenario$elitistLimit
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("nbIterations:"), "&nbsp;",
                store$iraceResults$state$nbIterations
              )
            )
          ),

          tags$li(
            HTML(
              paste(
                strong("minNbSurvival:"), "&nbsp;",
                store$iraceResults$state$minSurvival
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("nbParameters:"), "&nbsp;",
                store$iraceResults$parameters$nbParameters
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("seed:"), "&nbsp;",
                store$iraceResults$scenario$seed
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("confidence level:"), "&nbsp;",
                store$iraceResults$scenario$confidence
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("budget:"), "&nbsp;",
                store$iraceResults$scenario$budgetEstimation
              )
            )
          ),
          tags$li(HTML(paste(strong("mu:"), "&nbsp;", store$iraceResults$scenario$mu))),
          tags$li(
            HTML(
              paste(
                strong("deterministic:"), "&nbsp;",
                store$iraceResults$scenario$deterministic
              )
            )
          )
        )
      })
    }
  )
)

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
      },)
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

        createHiddenDirectory(file.path(gui$optionsPath, ".Fimages"))

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

            fixFormat <- isolate(store$iraceResults$parameters)
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

        createHiddenDirectory(file.path(gui$optionsPath, ".Pimages"))

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
              parameters = isolate(store$iraceResults$parameters),
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

        configurationPerIteration <- convert_vector_to_string(isolate(store$iraceResults$allElites[iteration][[1]]))
        results <- isolate(store$iraceResults$experiments)
        intersectedColumns <- self$format_col_data(results, configurationPerIteration)
        results <- subset(isolate(store$iraceResults$experiments), select = intersectedColumns)
        results <- na.omit(results)

        plot <- irace::configurationsBoxplot(results, ylab = "Solution cost", title = "Box Plot")
        pkg$reportStore$performanceBoxPlot <- save_plot_as_base64()
        plot
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

        fes <- cumsum(table(store$iraceResults$experimentLog[, "iteration"]))
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
            DT::dataTableOutput(outputId = ns("bestSoFarTable")),
            br(),
            hr(),
            self$copyInput$ui(
              inputId = ns("copy_elite_config"),
              label = "Copy Elite Configurations Table"
            ),
            h5(strong("Elite configurations")),
            DT::dataTableOutput(outputId = ns("eliteConfigurationTable")),
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

      output$bestSoFarTable <- DT::renderDataTable({
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

      output$eliteConfigurationTable <- DT::renderDataTable({
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