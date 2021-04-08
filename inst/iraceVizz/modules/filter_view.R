FilterView <- R6::R6Class( # nolint
  classname = "FilterView",
  inherit = View,
  public = list(
    configuration_filter = NULL,

    initialize = function(id) {
      super$initialize(id)
      self$configuration_filter <- ParameterCondition$new()
    },

    ui = function() {
      ns <- NS(self$id)
      tagList(
        fluidRow(
          column(
            width = 12,
            h2("Filter"),
            p("(Development) Search and add new configurations to the current sandbox."),
            HTML("<ul>
                  <li>search for configurations in the filter section. Once you have set all conditions, click in the filter button below</li>
                  <li>select configurations in the configuration section</li>
                  <li>see the list of selected configurations in the sanbox section</li>
                 </ul>") # nolint
          )
        ),
        fluidRow(
          box(
            title = strong("Filter Options"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            p("Add conditions to search for configurations, you may search them by:"),
            HTML("<ul>
                  <li>status (elite/non-elites)</li>
                  <li>iteration</li>
                  <li>parameter values</li>
                  <li>configuration ID</li>
                  <li>...</li>
                  </ul>"),
            checkboxInput(
              inputId = ns("elites"),
              label = "Elites",
              value = FALSE
            ),
            sliderInput(
              inputId = ns("iterations"),
              label = "Iterations",
              value = c(0, 0),
              min = 0, max = 1, step = 1
            ),
            hr(),
            self$configuration_filter$ui(input_id = ns("filter")),
            hr(),
            multiInput(
              inputId = ns("idSelect"),
              label = "IDs",
              choices = "",
              selected = "",
              options = list(
                enable_search = TRUE,
                non_selected_header = "All options",
                selected_header = "Selected options"
              )
            ),
            hr(),
            fluidRow(
              column(
                width = 12,
                pickerInput(
                  inputId = ns("descentId"),
                  label = "Descent Configurations",
                  choices = c(),
                  options = list(
                    size = 8
                  )
                )
              )
            ),
            hr(),
            pickerInput(
              inputId = ns("trajectoryId"),
              label = "Trajectory Configuration",
              choices = c(),
              options = list(
                size = 8
              )
            ),
            footer = bs4Dash::actionButton(
              inputId = ns("filter"),
              label = "Filter",
              status = "primary"
            )
          ),
          box(
            title = strong("Configurations"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            column(
              width = 12,
              style = "margin-bottom: 15px; padding-left: 0px !important;",
              bs4Dash::actionButton(
                inputId = ns("addSandBox"),
                label = "Add to SandBox"
              ),
              bs4Dash::actionButton(
                inputId = ns("selectAllConfigs"),
                label = "Select All"
              ),
              bs4Dash::actionButton(
                inputId = ns("deselectAllConfigs"),
                label = "Deselect All"
              )
            ),
            DT::dataTableOutput(outputId = ns("configurations_table"), width = "100%"),
            br()
          ),
          box(
            title = strong("SandBox"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            column(
              width = 12,
              style = "margin-bottom: 15px; padding-left: 0px !important;",
              bs4Dash::actionButton(
                inputId = ns("selectAllSandBox"),
                label = "Select All"
              ),
              bs4Dash::actionButton(
                inputId = ns("deselectAllSandBox"),
                label = "Deselect All"
              ),
              bs4Dash::actionButton(
                inputId = ns("deleteSandBox"),
                label = "Delete"
              ),
            ),
            DT::dataTableOutput(outputId = ns("sandbox_table"), width = "100%"),
            br()
          )
        )
      )
    },

    server = function(input, output, session, store, events) {
      ns <- session$ns

      values <- reactiveValues(
        configurations = NULL,
        sandbox = NULL,
        expressions = data.frame(),
        types = NULL,
        domain = NULL
      )

      self$configuration_filter$call(id = "filter", store = store, parent = values)

      observe(
        {
          req(store$sandbox)
          store$sandbox$set_descent_id(input$descentId)
          store$sandbox$set_trajectory_id(input$trajectoryId)
          store$sandbox$set_elites(input$elites)
          store$sandbox$set_iterations(input$iterations)
          store$sandbox$set_ids(input$idSelect)
        },
        suspended = TRUE
      )

      observeEvent(
        c(
          store$irace_results
        ),
        {

          values$sandbox <- data.frame()
          values$configurations <- data.frame()

          self$setup_inputs(session, store)
          self$configuration_filter$setup_inputs(store$irace_results$parameters$names)
          values$configurations <- store$irace_results$allConfigurations[0, ]
          values$types <- store$irace_results$parameters$types
          values$domain <- store$irace_results$parameters$domain
          values$sandbox <- values$configurations
        },
        ignoreNULL = FALSE
      )

      observeEvent(values$expressions, {
        req(store$sandbox)
        store$sandbox$set_filters(isolate(values$expressions))
      })

      output$configurations_table <- DT::renderDataTable({
        datatable(
          values$configurations,
          escape = FALSE,
          selection = "multiple",
          rownames = FALSE,
          style = "bootstrap4",
          class = "table-condensed table-striped cell-border",
          extensions = c("FixedColumns"),
          options = list(
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 1, rightColumns = 0)
          )
        )
      })

      config_proxy <- dataTableProxy(outputId = "configurations_table")

      observeEvent(input$filter, {
        req(store$irace_results)
        req(store$sandbox)

        iterations <- seq.int(
          from = input$iterations[1],
          to = input$iterations[2],
          by = 1
        )

        configurations_iter <- getConfigurationByIteration(
          iraceResults = store$irace_results,
          iterations = iterations
        )

        if (input$elites) {
          elites <- c()
          for (i in iterations) {
            elites <- c(elites, store$irace_results$allElites[[i]])
          }
          configurations_iter <- configurations_iter[configurations_iter$.ID. %in% elites, ]
        }

        if (nrow(store$sandbox$get_filters()) > 0) {
          expression <- parse(text = paste(store$sandbox$get_filters()$condition, collapse = " & "))
          configurations_iter <- subset(configurations_iter, eval(expression))
        }

        if (!is.null(input$idSelect)) {
          ids <- store$irace_results$allConfigurations[store$irace_results$allConfigurations$.ID. %in% input$idSelect, ]
          configurations_iter <- rbind(configurations_iter, ids)
        }

        if (!is.null(input$descentId) && input$descentId != "none") {
          data <- descentConfigurationTree(store$irace_results, as.integer(input$descentId))
          if (nrow(data) > 0) {
            ids <- c(as.integer(input$descentId), data$to)
            configs <- store$irace_results$allConfigurations[store$irace_results$allConfigurations$.ID. %in% ids, ]
            configurations_iter <- rbind(configurations_iter, configs)
          }
        }

        if (!is.null(input$trajectoryId) && input$trajectoryId != "none") {
          data <- configurationTrajectory(store$irace_results, as.integer(input$trajectoryId))
          if (nrow(data) > 0) {
            ids <- c(as.integer(input$trajectoryId), data$to)
            configs <- store$irace_results$allConfigurations[store$irace_results$allConfigurations$.ID. %in% ids, ]
            configurations_iter <- rbind(configurations_iter, configs)
          }
        }

        values$configurations <- unique(configurations_iter)
      })

      observeEvent(values$configurations,
        {
          names(values$configurations)[names(values$configurations) == ".ID."] <- "ID"
          names(values$configurations)[names(values$configurations) == ".PARENT."] <- "PARENT"
        },
        ignoreInit = TRUE
      )

      observeEvent(input$addSandBox, {
        rows <- values$configurations[input$configurations_table_rows_selected, ]
        sandbox <- unique(rbind(store$sandbox$get_configurations(), rows))
        store$sandbox$set_configurations(sandbox)
        values$sandbox <- store$sandbox$get_configurations()
      })

      observeEvent(c(input$configurations_table_rows_selected, values$configurations),
        {
          condition <- !is.null(input$configurations_table_rows_selected) & nrow(values$configurations) > 0
          toggleState(id = "addSandBox", condition = condition)
          toggleState(id = "deselectAllConfigs", condition = condition)
          toggleState(id = "selectAllConfigs", condition = nrow(values$configurations) > 0)
        },
        ignoreNULL = FALSE
      )

      observeEvent(input$selectAllConfigs, {
        config_proxy %>% selectRows(input$configurations_table_rows_all)
      })

      observeEvent(input$deselectAllConfigs, {
        config_proxy %>% selectRows(NULL)
      })

      ## SANDBOX
      output$sandbox_table <- DT::renderDataTable({
        datatable(
          data = values$sandbox,
          escape = FALSE,
          selection = "multiple",
          rownames = FALSE,
          style = "bootstrap4",
          class = "table-condensed table-striped cell-border",
          extensions = c("FixedColumns"),
          options = list(
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 1, rightColumns = 0)
          )
        )
      })

      sandbox_proxy <- dataTableProxy(outputId = "sandbox_table")

      observe({
        req(values$sandbox)

        names(values$sandbox)[names(values$sandbox) == ".ID."] <- "ID"
        names(values$sandbox)[names(values$sandbox) == ".PARENT."] <- "PARENT"

        isolate(events$update_sandbox <- update_reactive_counter(events$update_sandbox))
        config_proxy %>% selectRows(NULL)
        sandbox_proxy %>% selectRows(NULL)
      })

      observeEvent(input$deleteSandBox, {
        store$sandbox$remove_configuration(input$sandbox_table_rows_selected)
        values$sandbox <- store$sandbox$get_configurations()
      })

      observe({
        condition <- !is.null(input$sandbox_table_rows_selected) & nrow(values$sandbox) > 0
        toggleState(id = "deleteSandBox", condition = condition)
        toggleState(id = "deselectAllSandBox", condition = condition)
        toggleState(id = "selectAllSandBox", condition = nrow(values$sandbox) > 0)
      })

      observeEvent(input$selectAllSandBox, {
        sandbox_proxy %>% selectRows(input$sandbox_table_rows_all)
      })

      observeEvent(input$deselectAllSandBox, {
        sandbox_proxy %>% selectRows(NULL)
      })
    },

    setup_inputs = function(session, store) {
      updateCheckboxInput(
        session = session,
        inputId = "elites",
        value = TRUE
      )

      updateSliderInput(
        session = session,
        inputId = "iterations",
        min = 1,
        max = store$irace_results$state$nbIterations,
        value = 1,
        step = 1
      )

      updateMultiInput(
        session = session,
        inputId = "idSelect",
        choices = store$irace_results$allConfigurations$.ID.
      )

      updatePickerInput(
        session = session,
        inputId = "descentId",
        choices = c("none", store$irace_results$allConfigurations$.ID.)
      )

      updatePickerInput(
        session = session,
        inputId = "trajectoryId",
        choices = c("none", store$irace_results$allConfigurations$.ID.)
      )
    },

    clear_inputs = function(session) {
      updateCheckboxInput(
        session = session,
        inputId = "elites",
        value = F
      )

      updateSliderInput(
        session = session,
        inputId = "iterations",
        min = 0,
        max = 0,
        value = c(0, 0),
        step = 1
      )

      updateMultiInput(
        session = session,
        inputId = "idSelect",
        choices = c("")
      )

      updatePickerInput(
        session = session,
        inputId = "descentId",
        choices = c("none"),
        selected = NULL
      )

      updatePickerInput(
        session = session,
        inputId = "trajectoryId",
        choices = c("none"),
        selected = NULL
      )
    }
  )
)
