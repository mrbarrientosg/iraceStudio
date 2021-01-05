FilterView <- R6::R6Class(
  classname = "FilterView",
  inherit = View,
  public = list(
    executionSelect = NULL,
    sandboxSelect = NULL,
    configurationFilter = NULL,

    initialize = function(id) {
      super$initialize(id)
      self$executionSelect <- ExecutionSelect$new()
      self$sandboxSelect <- SandboxSelect$new()
      self$configurationFilter <- ParameterCondition$new()
    },

    ui = function() {
      ns <- NS(self$id)
      tagList(
        fluidRow(
          column(
            width = 4,
            h2("Filter"),
            p("(Development) Search and add new configurations to the current sandbox."),
            HTML("<ul>
                  <li>search for configurations in the filter section. Once you have set all conditions, click in the filter button below</li>
                  <li>select configurations in the configuration section</li>
                  <li>see the list of selected configurations in the sanbox section</li>
                 </ul>")
          ),
          column(
            width = 8,
            class = "d-flex align-items-center justify-content-end",
            self$executionSelect$ui(inputId = ns("executions")),
            div(style = "padding: 8px;"),
            self$sandboxSelect$ui(inputId = ns("sandboxes"))
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
            self$configurationFilter$ui(inputId = ns("filter")),
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
            footer = iraceStudio::actionButton(inputId = ns("filter"), label = "Filter", class = "btn-primary")
          ),
          box(
            title = strong("Configurations"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            column(
              width = 12,
              style = "margin-bottom: 15px; padding-left: 0px !important;",
              iraceStudio::actionButton(
                inputId = ns("addSandBox"),
                label = "Add to SandBox"
              ),
              iraceStudio::actionButton(
                inputId = ns("selectAllConfigs"),
                label = "Select All"
              ),
              iraceStudio::actionButton(
                inputId = ns("deselectAllConfigs"),
                label = "Deselect All"
              )
            ),
            DT::dataTableOutput(outputId = ns("configurationsTable"), width = "100%"),
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
              iraceStudio::actionButton(
                inputId = ns("selectAllSandBox"),
                label = "Select All"
              ),
              iraceStudio::actionButton(
                inputId = ns("deselectAllSandBox"),
                label = "Deselect All"
              ),
              iraceStudio::actionButton(
                inputId = ns("deleteSandBox"),
                label = "Delete"
              ),
            ),
            DT::dataTableOutput(outputId = ns("sandboxTable"), width = "100%"),
            br()
          )
        )
      )
    },

    server = function(input, output, session, store) {
      ns <- session$ns

      self$executionSelect$call(
        id = "executions",
        store = store
      )

      self$sandboxSelect$call(
        id = "sandboxes",
        store = store
      )

      values <- reactiveValues(configurations = NULL,
                               sandbox = NULL,
                               expressions = data.frame(),
                               types = NULL,
                               domain = NULL)

      self$configurationFilter$call(id = "filter", store = store, parent = values)

      updateValue <- observe({
        req(store$sandbox)
        store$sandbox$setDescentId(input$descentId)
        store$sandbox$setTrajectoryId(input$trajectoryId)
        store$sandbox$setElites(input$elites)
        store$sandbox$setIterations(input$iterations)
        store$sandbox$setIds(input$idSelect)
      }, suspended = TRUE)

      observeEvent(
        c(store$sandbox,
          playground_emitter$value(playground_events$current_scenario)),  {
        updateValue$suspend()

        values$sandbox <- data.frame()
        values$configurations <- data.frame()

        if (!is.null(store$sandbox)) {
          if (!is.null(store$iraceResults)) {
            self$setupInputs(session, store)
            self$configurationFilter$setupInputs(store$iraceResults$parameters$names)
            values$configurations <- store$iraceResults$allConfigurations[0, ]
            values$types <- store$iraceResults$parameters$types
            values$domain <- store$iraceResults$parameters$domain
          }

          values$expressions <- store$sandbox$getFilters()

          if (nrow(store$sandbox$getConfigurations()) == 0)
            values$sandbox <- values$configurations
          else
            values$sandbox <- store$sandbox$getConfigurations()

        } else {
          self$configurationFilter$clearInputs()
          self$clearInputs(session)
        }

        updateValue$resume()
      }, ignoreNULL = FALSE)

      observeEvent(values$expressions, {
        req(store$sandbox)
        store$sandbox$setFilters(isolate(values$expressions))
      })

      output$configurationsTable <- DT::renderDataTable({
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

      configProxy <- dataTableProxy(outputId = "configurationsTable")

      observeEvent(input$filter, {
        req(store$iraceResults)
        req(store$sandbox)

        iterations <- seq.int(
          from = input$iterations[1],
          to = input$iterations[2],
          by = 1
        )

        configurationsIter <- getConfigurationByIteration(
          iraceResults = store$iraceResults,
          iterations = iterations
        )

        if (input$elites) {
          elites <- c()
          for (i in iterations) {
            elites <- c(elites, store$iraceResults$allElites[[i]])
          }
          configurationsIter <- configurationsIter[configurationsIter$.ID. %in% elites, ]
        }

        if (nrow(store$sandbox$getFilters()) > 0) {
          expression <- parse(text = paste(store$sandbox$getFilters()$condition, collapse = " & "))
          configurationsIter <- subset(configurationsIter, eval(expression))
        }

        if (!is.null(input$idSelect)) {
          ids <- store$iraceResults$allConfigurations[store$iraceResults$allConfigurations$.ID. %in% input$idSelect, ]
          configurationsIter <- rbind(configurationsIter, ids)
        }

        if (!is.null(input$descentId) && input$descentId != "none") {
          data <- descentConfigurationTree(store$iraceResults, as.integer(input$descentId))
          if (nrow(data) > 0) {
            ids <- c(as.integer(input$descentId), data$to)
            configs <- store$iraceResults$allConfigurations[store$iraceResults$allConfigurations$.ID. %in% ids, ]
            configurationsIter <- rbind(configurationsIter, configs)
          }
        }

        if (!is.null(input$trajectoryId) && input$trajectoryId != "none") {
          data <- configurationTrajectory(store$iraceResults, as.integer(input$trajectoryId))
          if (nrow(data) > 0) {
            ids <- c(as.integer(input$trajectoryId), data$to)
            configs <- store$iraceResults$allConfigurations[store$iraceResults$allConfigurations$.ID. %in% ids, ]
            configurationsIter <- rbind(configurationsIter, configs)
          }
        }

        values$configurations <- unique(configurationsIter)
      })

      observeEvent(values$configurations, {
        names(values$configurations)[names(values$configurations) == ".ID."] <- "ID"
        names(values$configurations)[names(values$configurations) == ".PARENT."] <- "PARENT"
      }, ignoreInit = TRUE)

      observeEvent(input$addSandBox, {
        rows <- values$configurations[input$configurationsTable_rows_selected, ]
        sandBox <- unique(rbind(store$sandbox$getConfigurations(), rows))
        store$sandbox$setConfigurations(sandBox)
        values$sandbox <- store$sandbox$getConfigurations()
      })

      observeEvent(c(input$configurationsTable_rows_selected, values$configurations), {
        condition <- !is.null(input$configurationsTable_rows_selected) & nrow(values$configurations) > 0
        toggleState(id = "addSandBox", condition = condition)
        toggleState(id = "deselectAllConfigs", condition = condition)
        toggleState(id = "selectAllConfigs", condition = nrow(values$configurations) > 0)
      }, ignoreNULL = FALSE)

      observeEvent(input$selectAllConfigs, {
        configProxy %>% selectRows(input$configurationsTable_rows_all)
      })

      observeEvent(input$deselectAllConfigs, {
        configProxy %>% selectRows(NULL)
      })

      ## SANDBOX
      output$sandboxTable <- DT::renderDataTable({
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

      sandboxProxy <- dataTableProxy(outputId = "sandboxTable")

      observe({
        req(values$sandbox)

        names(values$sandbox)[names(values$sandbox) == ".ID."] <- "ID"
        names(values$sandbox)[names(values$sandbox) == ".PARENT."] <- "PARENT"

        store$updateSandbox <- isolate(store$updateSandbox + 1)
        configProxy %>% selectRows(NULL)
        sandboxProxy %>% selectRows(NULL)
      })

      observeEvent(input$deleteSandBox, {
        store$sandbox$removeConfiguration(input$sandboxTable_rows_selected)
        values$sandbox <- store$sandbox$getConfigurations()
      })

      observe({
        condition <- !is.null(input$sandboxTable_rows_selected) & nrow(values$sandbox) > 0
        toggleState(id = "deleteSandBox", condition = condition)
        toggleState(id = "deselectAllSandBox", condition = condition)
        toggleState(id = "selectAllSandBox", condition = nrow(values$sandbox) > 0)
      })

      observeEvent(input$selectAllSandBox, {
        sandboxProxy %>% selectRows(input$sandboxTable_rows_all)
      })

      observeEvent(input$deselectAllSandBox, {
        sandboxProxy %>% selectRows(NULL)
      })
    },

    setupInputs = function(session, store) {
      updateCheckboxInput(
        session = session,
        inputId = "elites",
        value = store$sandbox$getElites()
      )

      updateSliderInput(
        session = session,
        inputId = "iterations",
        min = 1,
        max = store$iraceResults$state$nbIterations,
        value = store$sandbox$getIterations(),
        step = 1
      )

      updateMultiInput(
        session = session,
        inputId = "idSelect",
        choices = store$iraceResults$allConfigurations$.ID.,
        selected = store$sandbox$getIds()
      )

      updatePickerInput(
        session = session,
        inputId = "descentId",
        choices = c("none", store$iraceResults$allConfigurations$.ID.),
        selected = store$sandbox$getDescentId()
      )

      updatePickerInput(
        session = session,
        inputId = "trajectoryId",
        choices = c("none", store$iraceResults$allConfigurations$.ID.),
        selected = store$sandbox$getTrajectoryId()
      )
    },

    clearInputs = function(session) {
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
