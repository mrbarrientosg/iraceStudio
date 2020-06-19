SandboxView <- R6::R6Class(
  classname = "SandboxView",
  inherit = View,
  public = list(
    executionSelect = NULL,

    initialize = function(id) {
      super$initialize(id)
      self$executionSelect <- ExecutionSelect$new()
    },

    ui = function() {
      ns <- NS(self$id)

      tagList(
        fluidRow(
          column(
            width = 8,
            h2("Sandbox")
          ),
          column(
            width = 4,
            class = "d-flex align-items-center justify-content-end",
            self$executionSelect$ui(inputId = ns("executions"))
          )
        ),
        fluidRow(
          bs4Card(
            id = ns("sandbox_options"),
            title = "Boxes",
            collapsible = FALSE,
            closable = FALSE,
            side = "left",
            width = 12,
            fluidRow(
              column(
                width = 12,
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
              br("\n"),
              DTOutput(outputId = ns("boxes"))
            )
          )
        )
      )
    },

    server = function(input, output, session, store) {
      ns <- session$ns

      data <- reactiveValues(sandbox = data.frame())

      self$executionSelect$call(id = "executions", store = store)

      output$boxes <- renderDT(
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

      observeEvent(c(
        playground_emitter$value(playground_events$update_scenarios),
        playground_emitter$value(playground_events$update_executions),
        playground_emitter$value(playground_events$update_sandboxes)
      ), {
        if (!is.null(store$currentExecution)) {
          data$sandbox <- self$sandbox_as_data_frame(store$currentExecution)
        }
      })

      observeEvent(input$add, {
        showModal(
          modalDialog(
            title = "Add a new scenario",
            easyClose = TRUE,
            textInput(inputId = ns("sandbox_name"), label = "Name"),
            textAreaInput(inputId = ns("sandbox_description"), label = "Description"),
            footer = tagList(
              actionButton(inputId = ns("add_sandbox"), label = "Add", class = "btn-primary"),
              modalButton(label = "Cancel")
            )
          )
        )
      })

      observeEvent(input$add_sandbox, {
        if (is.null(input$sandbox_name) || input$sandbox_name == "") {
          alert.error("Scenario name is empty.")
          return(invisible())
        }

        if (!is.null(store$currentExecution)) {
          sandbox <- Sandbox$new(name = input$sandbox_name, description = input$sandbox_description)
          store$currentExecution$add_sandbox(sandbox)
          data$sandbox <- self$sandbox_as_data_frame(store$currentExecution)
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
              actionButton(inputId = ns("update_sandbox"), label = "Save", class = "btn-primary"),
              modalButton(label = "Cancel")
            )
          )
        )
      })

      observeEvent(input$update_sandbox, {
        if (!is.null(store$currentExecution)) {
          sandbox <- data$sandbox[input$boxes_rows_selected, ]
          sandbox <- store$currentExecution$get_sandbox(sandbox$id)

          sandbox$setName(input$sandbox_name)
          sandbox$setDescription(input$sandbox_description)
          playground_emitter$emit(playground_events$update_sandboxes)

          data$sandbox <- self$sandbox_as_data_frame(store$currentExecution)
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
              actionButton(inputId = ns("confirm_delete"), label = "Yes", class = "btn-danger"),
              modalButton(label = "Cancel")
            ),
            easyClose = TRUE
          )
        )
      })

      observeEvent(input$confirm_delete, {
        if (!is.null(store$currentExecution)) {
          sandbox <- data$sandbox[input$boxes_rows_selected, ]
          store$currentExecution$remove_sandbox(sandbox$id)
          data$sandbox <- self$sandbox_as_data_frame(store$currentExecution)
        }

        removeModal()
      })

      observeEvent(store$currentExecution,  {
        if (is.null(store$currentExecution)) {
          disable(id = "add")
          data$sandbox <- data.frame()
        } else {
          enable(id = "add")
          data$sandbox <- self$sandbox_as_data_frame(store$currentExecution)
        }
      }, ignoreNULL = FALSE)

      observeEvent(input$boxes_rows_selected,{
        if (is.null(input$boxes_rows_selected)) {
          disable(id = "edit")
          disable(id = "delete")
        } else {
          enable(id = "edit")
          enable(id = "delete")
        }
      }, ignoreNULL = FALSE)
    },

    sandbox_as_data_frame = function(execution) {
      data <- data.frame(stringsAsFactors = FALSE)
      sandboxes <- execution$get_sandboxes()$get_boxes()

      for (name in names(sandboxes)) {
        sandbox <- sandboxes[[name]]
        data_row <- data.frame(
          id = sandbox$getId(),
          name = sandbox$getName(),
          description = sandbox$getDescription(),
          stringsAsFactors = FALSE
        )

        data <- rbind(data, data_row)
      }

      return(data)
    }
  )
)