SummaryCard <- R6::R6Class( # nolint
  classname = "SummaryCard",
  inherit = Component,
  public = list(
    ui = function(input_id) {
      ns <- NS(input_id)

      box(
        title = strong("Summary"),
        collapsible = FALSE,
        closable = FALSE,
        width = 12,
        htmlOutput(outputId = ns("summary_content"))
      )
    },

    server = function(input, output, session, store, events) {
      output$summary_content <- renderUI({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            "Execute irace first to display information."
          )
        )

        tags$ul(
          style = "list-style-type:none;",
          tags$li(
            HTML(
              paste(strong("IRACE version:"), "&nbsp;", store$irace_results$irace.version)
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("Number of candidate configurations:"), "&nbsp;",
                nrow(store$irace_results$allConfigurations)
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("Number of target executions:"), "&nbsp;",
                store$irace_results$state$experimentsUsedSoFar
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("Elitist new instances:"), "&nbsp;",
                store$irace_results$scenario$elitistNewInstances
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("Elitist limit:"), "&nbsp;",
                store$irace_results$scenario$elitistLimit
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("nbIterations:"), "&nbsp;",
                store$irace_results$state$nbIterations
              )
            )
          ),

          tags$li(
            HTML(
              paste(
                strong("minNbSurvival:"), "&nbsp;",
                store$irace_results$state$minSurvival
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("nbParameters:"), "&nbsp;",
                store$irace_results$parameters$nbParameters
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("seed:"), "&nbsp;",
                store$irace_results$scenario$seed
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("confidence level:"), "&nbsp;",
                store$irace_results$scenario$confidence
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("budget:"), "&nbsp;",
                store$irace_results$scenario$budgetEstimation
              )
            )
          ),
          tags$li(HTML(paste(strong("mu:"), "&nbsp;", store$irace_results$scenario$mu))),
          tags$li(
            HTML(
              paste(
                strong("deterministic:"), "&nbsp;",
                store$irace_results$scenario$deterministic
              )
            )
          )
        )
      })
    }
  )
)

BestConfigurationCard <- R6::R6Class( # nolint
  classname = "BestConfigurationCard",
  inherit = Component,
  public = list(
    copy_input = NULL,

    initialize = function() {
      self$copy_input <- CopyInput$new()
    },

    ui = function(input_id) {
      ns <- NS(input_id)

      box(
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
            self$copy_input$ui(input_id = ns("copy"), label = "Copy Box Plot"),
            plotOutput(outputId = ns("box_plot"))
          )
        )
      )
    },

    server = function(input, output, session, store, events) {
      copy <- self$copy_input$call(id = "copy", store = store, events = events)

      observeEvent(copy$action, {
        req(store$irace_results)
        req(copy$section)
        req(store$current_execution)
        report <- store$current_execution$get_report()
        events$copy$id <- report$get_id(copy$section)
        events$copy$plot <- "best_config_boxplot"
      })

      output$best_so_far <- renderUI({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            "Execute irace first to display information."
          )
        )

        shiny::validate(
          need(
            nrow(store$irace_results$allConfigurations) != 0,
            "The number of candidate configurations is 0."
          )
        )

        last <- length(store$irace_results$iterationElites)
        id <- store$irace_results$iterationElites[last]
        best_configuration <- getConfigurationById(
          iraceResults = store$irace_results,
          ids = id
        )

        mean_value <- colMeans(
          store$irace_results$experiments[, store$irace_results$iterationElites[last], drop = FALSE],
          na.rm = TRUE
        )[[1]]

        nb_iterations_elites <- length(store$irace_results$iterationElites)
        total_instances <- sum(!is.na(store$irace_results$experiments[, store$irace_results$iterationElites[nb_iterations_elites]]))

        tagList(
          h6(strong("Best so far:")),
          tags$ul(
            style = "list-style-type:none;",
            tags$li(HTML(paste(strong("Configuration:"), "&nbsp;", best_configuration[[1]]))),
            tags$li(HTML(paste(strong("Mean value:"), "&nbsp;", mean_value))),
            tags$li(HTML(paste(strong("Parent:"), "&nbsp;", best_configuration[[length(best_configuration)]]))),
            tags$li(HTML(paste(strong("Total instances tested:"), "&nbsp;", total_instances)))
          ),
          br(),
          h6(strong("Description of the 'Best so far' configuration:")),
          tags$ul(
            style = "list-style-type:none;",
            self$list_configurations(best_configuration)
          )
        )
      })

      output$box_plot <- renderPlot({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            "Execute irace first to display information."
          )
        )

        shiny::validate(
          need(
            nrow(store$irace_results$allConfigurations) != 0,
            "Cannot plot because IRACE did not finish. The number of candidate configurations is 0."
          )
        )

        last <- length(store$irace_results$iterationElites)
        id <- paste0(store$irace_results$iterationElites[last])
        results <- subset(store$irace_results$experiments, select = id)

        plot <- configurationsBoxplot(
          title = "Best Configuration Box Plot",
          experiments = results,
          ylab = "Solution cost"
        )

        pkg$report_store$best_config_boxplot <- save_plot_as_base64()

        plot
      }, )
    },

    list_configurations = function(configurations) {
      formated_data <- NULL

      for (i in 2:(length(configurations) - 1)) {
        formated_data <- c(
          formated_data,
          paste(
            "<li>", strong(paste0(colnames(configurations[i]), ":")), "&nbsp;",
            configurations[i], "</li>"
          )
        )
      }

      return(HTML(paste(formated_data, collapse = "\n")))
    }
  )
)

CandidatesCard <- R6::R6Class( # nolint
  classname = "CandidatesCard",
  inherit = Component,
  public = list(
    copy_input = NULL,

    initialize = function() {
      self$copy_input <- CopyInput$new()
    },

    ui = function(input_id) {
      ns <- NS(input_id)

      box(
        title = strong("Candidates"),
        collapsible = FALSE,
        closable = FALSE,
        width = 12,
        height = "800px",
        inlineCSS(".direct-chat-contacts { z-index: 0 !important; }"),
        sidebar = boxSidebar(
          id = ns("optionsSidebar"),
          startOpen = FALSE,
          width = 35,
          background = "#595959",
          tagList(
            sliderInput(
              inputId = ns("iterations"),
              label = "Iterations", min = 0, max = 1,
              value = c(0, 0)
            ),
            multiInput(
              inputId = ns("parameters"),
              label = "Parameters",
              choices = "",
              selected = "",
              options = list(
                enable_search = TRUE,
                non_selected_header = "All options",
                selected_header = "Selected options"
              )
            ),
            inlineCSS(".multi-wrapper .non-selected-wrapper,
            .multi-wrapper .selected-wrapper { height: 350px; }"),
            bs4Dash::actionButton(inputId = ns("update"), label = "Update", status = "primary")
          )
        ),
        fluidRow(
          style = "padding: 20px;",
          column(
            width = 6,
            self$copy_input$ui(
              input_id = ns("copy_parallel"),
              label = "Copy Parallel Coordinates"
            ),
            uiOutput(
              outputId = ns("parallel_plot"),
              style = "overflow-y:scroll; max-height:650px; width:100%; margin-bottom:10px;"
            )
          ),
          column(
            width = 6,
            self$copy_input$ui(
              input_id = ns("copy_freq"),
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

    server = function(input, output, session, store, events) {
      copy_parallel <- self$copy_input$call(id = "copy_parallel", store = store, events = events)
      copy_freq <- self$copy_input$call(id = "copy_freq", store = store, events = events)

      observeEvent(copy_parallel$action, {
        req(store$irace_results)
        req(copy_parallel$section)
        req(store$current_execution)
        report <- store$current_execution$get_report()
        events$copy$id <- report$get_id(copy_parallel$section)
        events$copy$plot <- "parallel_coordinates"
      })

      observeEvent(copy_freq$action, {
        req(store$irace_results)
        req(copy_freq$section)
        req(store$current_execution)
        report <- store$current_execution$get_report()
        events$copy$id <- report$get_id(copy_freq$section)
        events$copy$plot <- "parameter_freq"
      })

      observeEvent(store$irace_results,
        {
          if (is.null(store$irace_results)) {
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
              max = store$irace_results$state$nbIterations,
              value = c(store$irace_results$state$nbIterations - 1, store$irace_results$state$nbIterations),
              step = 1
            )

            updateMultiInput(
              session = session,
              inputId = "parameters",
              choices = store$irace_results$parameters$names,
              selected = store$irace_results$parameters$names
            )

            delay(1000, {
              enable(id = "update")
              click(id = "update")
              disable(id = "update")
            })
          }
        },
        ignoreNULL = FALSE
      )

      nb_plots_freq <- reactiveVal(value = 0)
      nb_plots_parall <- reactiveVal(value = 0)

      #### Frequency render plot

      freq_plot_render <- eventReactive(input$update, {
        shiny::validate(
          need(
            length(input$parameters) >= 2,
            "At least two parameters are necessary to generate the plots."
          )
        )

        disable(id = "update")

        path <- file.path(store$gui$options_path, ".Fimages")
        create_hidden_directory(path)

        nb_plots_freq(0)

        progress <- AsyncProgress$new(
          message = "Plotting: Frequency Plot",
          detail = "This may take a while...",
          value = 0
        )

        progress$set(0.1)

        configurations <- getConfigurationByIteration(
          iraceResults = store$irace_results,
          iterations = input$iterations[1]:input$iterations[2]
        )

        parameters <- input$parameters
        progress$set(0.8, detail = "Making Plots...")

        future({
          max <- 12
          limit <- 1
          nb_parameters <- ceiling(length(parameters) / max)

          for (i in seq_len(nb_parameters)) {
            k <- 1
            params <- NULL

            for (j in limit:(max * i)) {
              if (length(parameters) >= j) {
                params[k] <- parameters[j]
                k <- k + 1
              }
            }

            progress$inc(0.05, detail = "Making Plots...")

            fix_format <- isolate(store$irace_results$parameters)
            fix_format$names <- params

            image <- paste0(path, "/Rplot%03d.png")
            png(filename = sprintf(image, i), width = 550, height = 550, res = 80)
            parameterFrequency(configurations = configurations, parameters = fix_format)
            dev.off()

            limit <- (max * i) + 1
          }

          files <- list.files(path = path, pattern = ".*[.]png", full.names = TRUE)

          lapply(files, function(image) {
            knitr::image_uri(image)
          })
        }) %...>% {
          progress$set(1.0, detail = "Finishing...")
          files <- .
          pkg$report_store$parameter_freq <- files
          unlink(path, recursive = TRUE, force = TRUE)
          progress$close()
        }
      })

      output$freq_plot <- renderUI({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            "Execute irace first to display information."
          )
        )

        shiny::validate(
          need(
            nrow(store$irace_results$allConfigurations) != 0,
            "The number of candidate configurations is 0."
          )
        )

        freq_plot_render() %...>% {
          nb_plots_freq(length(pkg$report_store$parameter_freq))

          image_list <- lapply(seq_len(nb_plots_freq()), function(i) {
            image_name <- session$ns(paste0("image", i))
            htmlOutput(outputId = image_name, style = "display: flex; justify-content: center;")
          })

          do.call(tagList, image_list)
        }
      })

      observe({
        if (nb_plots_freq() == 0) {
          return(invisible())
        }

        for (i in seq_len(nb_plots_freq())) {
          local({
            my_i <- i
            data <- pkg$report_store$parameter_freq[my_i]
            image_name <- paste0("image", my_i)

            output[[image_name]] <- renderUI({
              tags$img(src = data, height = "auto")
            })
          })
        }

        enable(id = "update")
      })

      ### Parallel Coordinates
      parallel_cord_render <- eventReactive(input$update, {
        conf <- getConfigurationByIteration(
          iraceResults = store$irace_results,
          iterations = input$iterations[1]:input$iterations[2]
        )

        shiny::validate(
          need(
            length(input$parameters) >= 2,
            "At least two parameters are necessary to generate the plots."
          ),
          need(
            nrow(conf) != 0,
            "Cannot plot because IRACE did not finish. The amount of rows is 0."
          ),
          need(
            nrow(store$irace_results$allConfigurations) != 0,
            "Cannot plot because IRACE did not finish. The number of candidate configurations is 0."
          )
        )

        progress <- AsyncProgress$new(
          message = "Plotting: Parallel Coordinates",
          detail = "This may take a while...",
          value = 0
        )

        disable(id = "update")

        path <- file.path(store$gui$options_path, ".Pimages")
        create_hidden_directory(path)

        nb_plots_parall(0)
        parameters <- input$parameters

        future({
          max <- 12
          limit <- 1
          nb_parameters <- ceiling(length(parameters) / max)

          for (i in seq_len(nb_parameters)) {
            k <- 1
            params <- c()

            for (j in limit:(max * i)) {
              if (length(parameters) >= j) {
                params[k] <- parameters[j]
                k <- k + 1
              }
            }

            progress$inc(0.05, detail = "Making Plots...")

            image <- paste0(path, "/Rplot%03d.png")
            png(
              filename = sprintf(image, i),
              width = 550,
              height = 550,
              res = 80
            )

            parallelCoordinatesPlot(
              configurations = conf,
              parameters = isolate(store$irace_results$parameters),
              param_names = params,
              hierarchy = FALSE
            )
            dev.off()

            limit <- (max * i) + 1
          }

          files <- list.files(path = path, pattern = ".*[.]png", full.names = TRUE)

          images <- lapply(sort(files), function(image) {
            knitr::image_uri(image)
          })
        }) %...>% {
          progress$inc(1.0, detail = "Finishing...")
          files <- .
          pkg$report_store$parallel_coordinates <- files
          unlink(path, recursive = TRUE, force = TRUE)
          progress$close()
        }
      })

      output$parallel_plot <- renderUI({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            "Execute irace first to display information."
          )
        )

        shiny::validate(
          need(
            nrow(store$irace_results$allConfigurations) != 0,
            "The number of candidate configurations is 0."
          )
        )


        if (input$iterations[1] == 0) {
          return(invisible())
        }

        parallel_cord_render() %...>% {
          nb_plots_parall(length(pkg$report_store$parallel_coordinates))

          image_list <- lapply(seq_len(nb_plots_parall()), function(i) {
            image_name <- session$ns(paste0("image-", i))
            htmlOutput(outputId = image_name, style = "display: flex; justify-content: center;")
          })

          do.call(tagList, image_list)
        }
      })

      observe({
        if (nb_plots_parall() == 0) {
          return(invisible())
        }

        for (i in seq_len(nb_plots_parall())) {
          local({
            my_i <- i
            data <- pkg$report_store$parallel_coordinates[my_i]
            image_name <- paste0("image-", my_i)

            output[[image_name]] <- renderUI({
              tags$img(src = data, height = "auto")
            })
          })
        }

        enable(id = "update")
      })
    }
  )
)

PerformanceCard <- R6::R6Class( # nolint
  classname = "PerformanceCard",
  inherit = Component,
  public = list(
    copy_input = NULL,

    initialize = function() {
      self$copy_input <- CopyInput$new()
    },

    ui = function(input_id) {
      ns <- NS(input_id)

      box(
        title = strong("Performance"),
        collapsible = FALSE,
        closable = FALSE,
        width = 12,
        fluidRow(
          column(
            width = 6,
            self$copy_input$ui(
              input_id = ns("copy_converge"),
              label = "Copy Convergence Plot"
            ),
            plotOutput(outputId = ns("converge_plot"), height = 650)
          ),
          column(
            width = 6,
            self$copy_input$ui(input_id = ns("copy_box"), label = "Copy Box Plot"),
            pickerInput(
              inputId = ns("iterations"),
              label = "Iterations",
              choices = c(),
              width = "100%",
              options = list(
                size = 8
              )
            ),
            plotOutput(outputId = ns("performance_box_plot"), height = 550)
          )
        )
      )
    },

    server = function(input, output, session, store, events) {
      copy_converge <- self$copy_input$call(id = "copy_converge", store = store, events = events)
      copy_box <- self$copy_input$call(id = "copy_box", store = store, events = events)

      observeEvent(copy_converge$action, {
        req(store$irace_results)
        req(copy_converge$section)
        req(store$current_execution)
        report <- store$current_execution$get_report()
        events$copy$id <- report$get_id(copy_converge$section)
        events$copy$plot <- "convergence_plot"
      })

      observeEvent(copy_box$action, {
        req(store$irace_results)
        req(copy_box$section)
        req(store$current_execution)
        report <- store$current_execution$get_report()
        events$copy$id <- report$get_id(copy_box$section)
        events$copy$plot <- "performance_boxplot"
      })

      observeEvent(store$irace_results,
        {
          if (is.null(store$irace_results)) {
            updatePickerInput(
              session = session,
              inputId = "iterations",
              choices = ""
            )
          } else {
            updatePickerInput(
              session = session,
              inputId = "iterations",
              choices = 1:store$irace_results$state$nbIterations
            )
          }
        },
        ignoreNULL = FALSE
      )

      output$performance_box_plot <- renderPlot({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            "Execute irace first to display information."
          ),
          need(input$iterations, "")
        )
        shiny::validate(
          need(
            nrow(store$irace_results$allConfigurations) != 0,
            "Cannot plot because IRACE did not finish. The number of candidate configurations is 0."
          )
        )

        iteration <- as.integer(input$iterations)

        configuration_per_iteration <- convert_vector_to_string(isolate(store$irace_results$allElites[iteration][[1]]))
        results <- isolate(store$irace_results$experiments)
        intersected_columns <- self$format_col_data(results, configuration_per_iteration)
        results <- subset(isolate(store$irace_results$experiments), select = intersected_columns)
        results <- na.omit(results)

        plot <- irace::configurationsBoxplot(results, ylab = "Solution cost", title = "Box Plot")
        pkg$report_store$performance_boxplot <- save_plot_as_base64()
        plot
      })

      output$converge_plot <- renderPlot({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            "Execute irace first to display information."
          )
        )
        shiny::validate(
          need(
            nrow(store$irace_results$allConfigurations) != 0,
            "Cannot plot because IRACE did not finish. The number of candidate configurations is 0."
          )
        )

        fes <- cumsum(table(store$irace_results$experimentLog[, "iteration"]))
        fes <- fes[!names(fes) == "0"]
        elites <- as.character(store$irace_results$iterationElites)

        shiny::validate(
          need(
            dim(store$irace_results$experiments[, elites]) != 0,
            "Cannot plot because IRACE did not finish. Must be an array of two dimensions."
          )
        )

        values <- colMeans(store$irace_results$experiments[, elites])

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

        pkg$report_store$convergence_plot <- save_plot_as_base64()

        p
      })
    },

    format_col_data = function(results_data, iteration_data) {
      vector_col_names <- colnames(results_data)
      formated_data <- Reduce(intersect, list(vector_col_names, iteration_data))
      return(formated_data)
    }
  )
)

DetailByIterationCard <- R6::R6Class( # nolint
  classname = "DetailByIterationCard",
  inherit = Component,
  public = list(
    copy_input = NULL,

    initialize = function() {
      self$copy_input <- CopyInput$new()
    },

    ui = function(input_id) {
      ns <- NS(input_id)

      box(
        title = strong("Details by Iteration"),
        collapsible = FALSE,
        closable = FALSE,
        width = 12,
        sidebar = boxSidebar(
          id = ns("optionsSidebar"),
          startOpen = FALSE,
          width = 35,
          background = "#595959",
          pickerInput(
            inputId = ns("iterations"),
            label = "Iterations",
            choices = c()
          )
        ),
        fluidRow(
          style = "padding: 20px;",
          column(
            width = 12,
            self$copy_input$ui(
              input_id = ns("copy_best_so_far"),
              label = "Copy Best So Far Table"
            ),
            h5(strong("Description of the best-so-far Configuration")),
            htmlOutput(outputId = ns("summary_best_so_far")),
            br(),
            DT::dataTableOutput(outputId = ns("best_so_far_table")),
            br(),
            hr(),
            self$copy_input$ui(
              input_id = ns("copy_elite_config"),
              label = "Copy Elite Configurations Table"
            ),
            h5(strong("Elite configurations")),
            DT::dataTableOutput(outputId = ns("elite_configuration_table")),
            br()
          )
        )
      )
    },

    server = function(input, output, session, store, events) {
      copy_best_so_far <- self$copy_input$call(id = "copy_best_so_far", store = store, events = events)
      copy_elite_config <- self$copy_input$call(id = "copy_elite_config", store = store, events = events)

      observeEvent(copy_best_so_far$action, {
        req(store$irace_results)
        req(copy_best_so_far$section)
        req(store$current_execution)
        report <- store$current_execution$get_report()
        events$copy$id <- report$get_id(copy_best_so_far$section)
        events$copy$table <- "best_so_far_table"
      })

      observeEvent(copy_elite_config$action, {
        req(store$irace_results)
        req(copy_elite_config$section)
        req(store$current_execution)
        report <- store$current_execution$get_report()
        events$copy$id <- report$get_id(copy_elite_config$section)
        events$copy$table <- "elite_configuration_table"
      })

      observeEvent(store$irace_results,
        {
          if (is.null(store$irace_results)) {
            updatePickerInput(
              session = session,
              inputId = "iterations",
              choices = ""
            )
          } else {
            updatePickerInput(
              session = session,
              inputId = "iterations",
              choices = 1:store$irace_results$state$nbIterations
            )
          }
        },
        ignoreNULL = FALSE
      )

      output$summary_best_so_far <- renderUI({
        shiny::validate(
          need(
            !is.null(store$irace_results), ""
          )
        )

        shiny::validate(
          need(
            nrow(store$irace_results$allConfigurations) != 0, ""
          )
        )

        req(input$iterations, cancelOutput = TRUE)

        best_configurations <- store$irace_results$allElites[as.integer(input$iterations)]
        best_configuration_id <- best_configurations[[1]][1]
        details_best_configuration <- getConfigurationById(store$irace_results, ids = best_configuration_id)

        names(details_best_configuration)[names(details_best_configuration) == ".ID."] <- "ID"
        names(details_best_configuration)[names(details_best_configuration) == ".PARENT."] <- "PARENT"

        mean_value <- colMeans(
          store$irace_results$experiments[, store$irace_results$iterationElites[as.integer(input$iterations)], drop = FALSE], # nolint
          na.rm = TRUE
        )

        tagList(
          HTML(paste(strong("Best so far Configuration:"), "&nbsp;", best_configuration_id)),
          br(),
          HTML(paste(strong("Mean value:"), "&nbsp;", mean_value))
        )
      })

      output$best_so_far_table <- DT::renderDataTable({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            "Execute irace first to display information."
          )
        )

        shiny::validate(
          need(
            nrow(store$irace_results$allConfigurations) != 0,
            "The number of candidate configurations is 0."
          )
        )

        req(input$iterations, cancelOutput = TRUE)

        best_configurations <- store$irace_results$allElites[as.integer(input$iterations)]
        best_configuration_id <- best_configurations[[1]][1]
        details_best_configuration <- getConfigurationById(store$irace_results, ids = best_configuration_id)

        names(details_best_configuration)[names(details_best_configuration) == ".ID."] <- "ID"
        names(details_best_configuration)[names(details_best_configuration) == ".PARENT."] <- "PARENT"

        pkg$report_store$best_so_far_table <- details_best_configuration

        datatable(
          details_best_configuration,
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

      output$elite_configuration_table <- DT::renderDataTable({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            "Execute irace first to display information."
          )
        )

        shiny::validate(
          need(
            nrow(store$irace_results$allConfigurations) != 0,
            "The number of candidate configurations is 0."
          )
        )

        req(input$iterations)

        best_configurations <- store$irace_results$allElites[as.integer(input$iterations)]
        best_configuration_id <- best_configurations[[1]]

        elites_config <- getConfigurationById(store$irace_results, ids = best_configuration_id)

        names(elites_config)[names(elites_config) == ".ID."] <- "ID"
        names(elites_config)[names(elites_config) == ".PARENT."] <- "PARENT"

        pkg$report_store$elite_configuration_table <- elites_config

        datatable(
          elites_config,
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
