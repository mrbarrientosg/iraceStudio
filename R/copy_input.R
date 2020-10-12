CopyInput <- R6::R6Class(
  classname = "CopyInput",
  inherit = Component,
  public = list(
    ui = function(inputId, label, choices = c()) {
      ns <- NS(inputId)

      fluidRow(
        column(
          width = 10,
          pickerInput(
            inputId = ns("select_input"),
            label = label,
            choices = choices,
            width = "100%",
            options = list(
                size = 8
            )
          )
        ),
        column(
          width = 2,
          actionButton(
            inputId = ns("action"),
            label = "Copy",
            style = "margin-top:32px; text-align:center;",
            width = "100%",
            class = "btn-primary"
          )
        )
      )
    },

    server = function(input, output, session, store) {
      values <- reactiveValues()
      observeEvent(input$select_input, values$section <- input$select_input)
      observeEvent(input$action, values$action <- input$action)

      observeEvent(c(store$currentExecution, playground_emitter$value(playground_events$update_report)), {
        if (is.null(store$currentExecution)) {
          updatePickerInput(
            session = session,
            inputId = "select_input",
            choices = c(""),
            selected = NULL
          )
        } else {
          report <- store$currentExecution$get_report()

          data <- report$get_data()

          if (length(data) == 0) {
            data <- c("")
          } else {
            data <- unlist(lapply(data, function(d) d$title), use.names = FALSE)
          }

          updatePickerInput(
            session = session,
            inputId = "select_input",
            choices = data,
            selected = NULL
          )
        }
      }, ignoreNULL = FALSE, ignoreInit = TRUE)

      return(values)
    }
  )
)