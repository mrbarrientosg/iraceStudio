SandboxView <- R6::R6Class( # nolint
  classname = "SandboxView",
  inherit = View,
  public = list(
    execution_select = NULL,

    initialize = function(id) {
      super$initialize(id)
      self$execution_select <- ExecutionSelect$new()
    },

    ui = function() {
      ns <- NS(self$id)

      tagList(
        fluidRow(
          column(
            width = 8,
            h2("Sandbox"),
            p("A sandbox is a set of configurations selected for visualization. "),
            HTML("<ul>
                  <li>use the execution selector (right) to select the active irace execution</li>
                  <li>to create a new sandbox, click on add</li>
                  <li>by default, a new sandbox contains the final elite configurations</li>
                  <li>to add more configurations, go to the Filter menu</li>
                 </ul>")
          ),
          column(
            width = 4,
            class = "d-flex align-items-center justify-content-end",
            self$execution_select$ui(input_id = ns("executions"))
          )
        ),
        fluidRow(
          box(
            id = ns("sandbox_options"),
            title = strong("Boxes"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            fluidRow(
              column(
                width = 12,
                style = "padding-left: 0px !important;",
                disabled(
                  bs4Dash::actionButton(
                    inputId = ns("add"),
                    label = "Add",
                    icon = icon("plus")
                  ),
                  bs4Dash::actionButton(
                    inputId = ns("edit"),
                    label = "Edit",
                    icon = icon("edit")
                  ),
                  bs4Dash::actionButton(
                    inputId = ns("delete"),
                    label = "Delete",
                    icon = icon("minus")
                  )
                )
              ),
              br("\n"),
              DT::dataTableOutput(outputId = ns("boxes"))
            )
          )
        )
      )
    },

    server = function(input, output, session, store, events) {
      ns <- session$ns

      data <- reactiveValues(sandbox = data.frame())

      self$execution_select$call(id = "executions", store = store, events = events)

      output$boxes <- DT::renderDataTable(
        datatable(
          data = data$sandbox,
          escape = FALSE,
          selection = "single",
          colnames = c("ID", "Name", "Description"),
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

      observeEvent(
        c(
          events$update_scenarios,
          events$update_executions,
          events$update_sandboxes
        ),
        {
          if (!is.null(store$current_execution)) {
            data$sandbox <- self$sandbox_as_data_frame(store$current_execution)
          } else {
            data$sandbox <- data.frame()
          }
        },
        ignoreInit = TRUE
      )

      observeEvent(input$add, {
        showModal(
          modalDialog(
            title = "Add a new scenario",
            easyClose = TRUE,
            textInput(inputId = ns("sandbox_name"), label = "Name"),
            textAreaInput(inputId = ns("sandbox_description"), label = "Description"),
            footer = tagList(
              bs4Dash::actionButton(inputId = ns("add_sandbox"), label = "Add", status = "primary"),
              modalButton(label = "Cancel")
            )
          )
        )
      })

      observeEvent(input$add_sandbox, {
        if (is.null(input$sandbox_name) || input$sandbox_name == "") {
          alert_error("Scenario name is empty.")
          return(invisible())
        }

        if (!is.null(store$current_execution)) {
          sandbox <- Sandbox$new(name = input$sandbox_name, description = input$sandbox_description)
          store$current_execution$add_sandbox(sandbox)
          update_reactive_counter(events$update_sandboxes)
          data$sandbox <- self$sandbox_as_data_frame(store$current_execution)
        }

        removeModal()
      })

      observeEvent(input$edit, {
        req(input$boxes_rows_selected)

        sandbox <- data$sandbox[input$boxes_rows_selected, ]
        showModal(
          modalDialog(
            title = "Edit scenario",
            easyClose = TRUE,
            textInput(inputId = ns("sandbox_name"), label = "Name", value = sandbox$name),
            textAreaInput(inputId = ns("sandbox_description"), label = "Description", value = sandbox$description),
            footer = tagList(
              bs4Dash::actionButton(inputId = ns("update_sandbox"), label = "Save", status = "primary"),
              modalButton(label = "Cancel")
            )
          )
        )
      })

      observeEvent(input$update_sandbox, {
        if (!is.null(store$current_execution)) {
          sandbox <- data$sandbox[input$boxes_rows_selected, ]
          sandbox <- store$current_execution$get_sandbox(sandbox$id)

          sandbox$set_name(input$sandbox_name)
          sandbox$set_description(input$sandbox_description)
          update_reactive_counter(events$update_sandboxes)

          data$sandbox <- self$sandbox_as_data_frame(store$current_execution)
        }

        removeModal()
      })

      observeEvent(input$delete, {
        req(input$boxes_rows_selected)

        sandbox <- data$sandbox[input$boxes_rows_selected, ]

        showModal(
          modalDialog(
            title = "Warning",
            HTML(
              paste(
                "Are you sure to delete", tags$b(sandbox$name), "?"
              )
            ),
            footer = tagList(
              bs4Dash::actionButton(inputId = ns("confirm_delete"), label = "Yes", status = "danger"),
              modalButton(label = "Cancel")
            ),
            easyClose = TRUE
          )
        )
      })

      observeEvent(input$confirm_delete, {
        if (!is.null(store$current_execution)) {
          sandbox <- data$sandbox[input$boxes_rows_selected, ]
          store$current_execution$remove_sandbox(sandbox$id)
          update_reactive_counter(events$update_sandboxes)
          data$sandbox <- self$sandbox_as_data_frame(store$current_execution)
        }

        removeModal()
      })

      observeEvent(store$current_execution,
        {
          if (is.null(store$current_execution)) {
            disable(id = "add")
            data$sandbox <- data.frame()
          } else {
            enable(id = "add")
            data$sandbox <- self$sandbox_as_data_frame(store$current_execution)
          }
        },
        ignoreNULL = FALSE,
        ignoreInit = TRUE
      )

      observeEvent(input$boxes_rows_selected,
        {
          if (is.null(input$boxes_rows_selected)) {
            disable(id = "edit")
            disable(id = "delete")
          } else {
            enable(id = "edit")
            enable(id = "delete")
          }
        },
        ignoreNULL = FALSE
      )
    },

    sandbox_as_data_frame = function(execution) {
      data <- data.frame(stringsAsFactors = FALSE)
      sandboxes <- execution$get_sandboxes()$get_boxes()

      for (name in names(sandboxes)) {
        sandbox <- sandboxes[[name]]
        data_row <- data.frame(
          id = sandbox$get_id(),
          name = sandbox$get_name(),
          description = sandbox$get_description(),
          stringsAsFactors = FALSE
        )

        data <- rbind(data, data_row)
      }

      return(data)
    }
  )
)
