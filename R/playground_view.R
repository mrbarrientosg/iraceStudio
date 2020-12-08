PlaygroundView <- R6::R6Class(
  classname = "PlaygroundView",
  inherit = View,
  private = list(
    scenario = NULL
  ),
  public = list(
    ui = function() {
      ns <- NS(self$id)

      tagList(
        div(class = "sub-header",
          h2("Playground"),
          p("The playground contains a set of scenarios that will be availabe within an Irace Studio session.
            Check the name of your playground in the center of the upper bar."),
          p("Add the scenarios you would like to have access in your playground:"),
          HTML("<ul>
               <li> to create a new scenario, click in the add button</li>
               <li> to import an scenario from an irace Rdata file, click the import button</li>
               </ul>"),
          p("Use the selector in the upper right corner to select the active scenario.")
        ),
        fluidRow(
          tabBox(
            id = ns("playground_options"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            type = "tabs",
            status = "gray",
            tabPanel(
              "Scenarios",
              fluidRow(
                column(
                  width = 8,
                  style = "padding-left: 0px !important;",
                  actionButton(
                    inputId = ns("add"),
                    label = "Add",
                    icon = icon("plus")
                  ),
                  disabled(
                    actionButton(
                      inputId = ns("edit"),
                      label = "Edit",
                      icon = icon("edit")
                    ),
                    actionButton(
                      inputId = ns("delete"),
                      label = "Delete",
                      icon = icon("minus")
                    )
                  )
                ),
                column(
                  width = 4,
                  class = "d-flex align-items-end justify-content-end",
                  importButton(inputId = ns("load"))
                )
              ),
              br(),
              DT::dataTableOutput(outputId = ns("scenarios"))
            ),
            tabPanel(
              "Options",
              disabled(
                textInput(
                  inputId = ns("playgroundName"),
                  label = "Playground Name"
                )
              ),
              textAreaInput(
                inputId = ns("playgroundDescription"),
                label = "Description"
              )
            )
          )
        )
      )
    },

    server = function(input, output, session, store) {
      ns <- session$ns

      data <- reactiveValues(scenarios = data.frame())

      observeEvent(store$pg, {
        data$scenarios <- self$scenarios_as_data_frame(store)
      })

      #volumes <- getVolumes()()
      volumes <- c("Home"=path.expand('~'), getVolumes()())

      shinyFileChoose(input, "load", roots = volumes)

      observeEvent(input$load, {
        if (!is.integer(input$load)) {
          file <- tryCatch({
            parseFilePaths(roots = volumes, input$load)
          }, error = function(err) {
            log_error("{err}")
            return(NULL)
          })

          if (is.null(file)) {
            alert.error("Can't load scenario file, check if the file format is correct.")
            return(invisible())
          }

          shinyalert(
            title = "Scenario name",
            text = "Give a name to identify the scenario after.",
            type = "input",
            inputType = "text",
            showCancelButton = TRUE,
            closeOnEsc = FALSE,
            callbackR = function(name) {
              if (is.logical(name) && !name) {
                return(invisible())
              }

              if (is.null(name) || name == "") {
                alert.error("Scenario name is empty.")
                return(invisible())
              }

              private$scenario <- scenario$new(name = name)

              result <- importScenario(file$name, file$datapath, private$scenario)

              if (result) {
                shinyalert(title = "Warning",
                          text = "Cannot be import all options from the scenario.",
                          type = "warning")
                store$pg$add_scenario(private$scenario)
                data$scenarios <- self$scenarios_as_data_frame(store)
                private$scenario <- NULL
              } else {
                alert.error("Can't load scenario file, check if the file format is correct.")
              }
            }
          )
        }
      })

      observeEvent(store$pg, {
        updateTextInput(
          session = session,
          inputId = "playgroundName",
          value = store$pg$get_name()
        )

        updateTextInput(
          session = session,
          inputId = "playgroundDescription",
          value = store$pg$get_description()
        )
      })

      output$scenarios <- DT::renderDataTable(
        datatable(
          data = data$scenarios,
          escape = FALSE,
          selection = "single",
          colnames = c("ID", "Name", "Description", "Creation Date"),
          rownames = FALSE,
          style = "bootstrap4",
          class = "table-condensed table-striped cell-border",
          options = list(
            searching = FALSE,
            paging = FALSE,
            scrollY = "200px",
            dom = "t",
            sort = FALSE
          )
        )
      )

      observeEvent(input$add, {
        showModal(
          modalDialog(
            title = "Add a new scenario",
            easyClose = TRUE,
            textInput(inputId = ns("scenario_name"), label = "Name"),
            textAreaInput(inputId = ns("scenario_description"), label = "Description"),
            footer = tagList(
              actionButton(inputId = ns("add_scenario"), label = "Add", class = "btn-primary"),
              modalButton(label = "Cancel")
            )
          )
        )
      })

      observeEvent(input$add_scenario, {
        if (is.null(input$scenario_name) || input$scenario_name == "") {
          alert.error("Scenario name is empty.")
          return(invisible())
        }

        scenario <- scenario$new(
          name = input$scenario_name,
          description = input$scenario_description
        )

        store$pg$add_scenario(scenario)

        data$scenarios <- self$scenarios_as_data_frame(store)

        removeModal()
      })

      observeEvent(input$edit, {
        req(input$scenarios_rows_selected)

        scenario <- data$scenarios[input$scenarios_rows_selected, ]

        showModal(
          modalDialog(
            title = "Edit scenario",
            easyClose = TRUE,
            textInput(inputId = ns("scenario_name"), label = "Name", value = scenario$name),
            textAreaInput(inputId = ns("scenario_description"), label = "Description", value = scenario$description),
            footer = tagList(
              actionButton(inputId = ns("update_scenario"), label = "Save", class = "btn-primary"),
              modalButton(label = "Cancel")
            )
          )
        )
      })

      observeEvent(input$update_scenario, {
        scenario <- data$scenarios[input$scenarios_rows_selected, ]
        scenario <- store$pg$get_scenario(scenario$id)

        if (scenario$get_name() != input$scenario_name) {
          playgroundPath <- file.path(store$gui$workspacePath, store$pg$get_name())
          old <- file.path(playgroundPath, scenario$get_name())

          if (dir.exists(old)) {
            new <- file.path(playgroundPath, input$scenario_name)

            if (!dir.create(new)) {
              dir.create(new)
            }

            file.rename(old, new)
          }
        }

        scenario$set_name(input$scenario_name)
        scenario$set_description(input$scenario_description)
        playground_emitter$emit(playground_events$update_scenarios)

        data$scenarios <- self$scenarios_as_data_frame(store)

        removeModal()
      })

      observeEvent(input$delete, {
        req(input$scenarios_rows_selected)

        scenario <- data$scenarios[input$scenarios_rows_selected, ]

        showModal(
          modalDialog(
            title = "Warning",
            HTML(
              paste(
                "Are you sure to delete", tags$b(scenario$name), "?"
              )
            ),
            footer = tagList(
              actionButton(inputId = ns("confirm_delete"), label = "Yes", class = "btn-danger"),
              modalButton(label = "Cancel")
            ),
            easyClose = TRUE
          )
        )
      })

      observeEvent(input$confirm_delete, {
        scenario <- data$scenarios[input$scenarios_rows_selected, ]

        store$pg$remove_scenario(scenario$id)

        data$scenarios <- self$scenarios_as_data_frame(store)

        removeModal()
      })

      observeEvent(input$scenarios_rows_selected,{
        if (store$startIrace) {
          return(invisible())
        }

        if (is.null(input$scenarios_rows_selected)) {
          disable(id = "edit")
          disable(id = "delete")
        } else {
          enable(id = "edit")
          enable(id = "delete")
        }
      }, ignoreNULL = FALSE)

      observeEvent(store$startIrace, {
        if (store$startIrace) {
          disable(id = "add")
          disable(id = "edit")
          disable(id = "delete")
        } else {
          enable(id = "add")
          enable(id = "edit")
          enable(id = "delete")
        }
      })

      observeEvent(input$playgroundName, {
        store$pg$set_name(input$playgroundName)
        store$playgroundName <- input$playgroundName
      }, ignoreInit = TRUE)

      observeEvent(input$playgroundDescription,
       store$pg$set_description(input$playgroundDescription), ignoreInit = TRUE)
    },

    scenarios_as_data_frame = function(store) {
      data <- data.frame(stringsAsFactors = FALSE)
      pg <- isolate(store$pg)

      if (is.null(pg))
        return(data)

      scenarios <- pg$get_scenarios()

      for (name in names(scenarios)) {
        scenario <- scenarios[[name]]
        data_row <- data.frame(
          id = scenario$get_id(),
          name = scenario$get_name(),
          description = scenario$get_description(),
          creation_date = scenario$get_creation_date(),
          stringsAsFactors = FALSE
        )
        data <- rbind(data, data_row)
      }

      return(data)
    }
  )
)