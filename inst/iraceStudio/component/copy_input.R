#' @export
CopyInput <- R6::R6Class( # nolint
  classname = "CopyInput",
  inherit = Component,
  public = list(
    ui = function(input_id, label, choices = c()) {
      ns <- NS(input_id)

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
          bs4Dash::actionButton(
            inputId = ns("action"),
            label = "Copy",
            style = "margin-top:32px; text-align:center;",
            width = "100%",
            status = "primary"
          )
        )
      )
    },

    server = function(input, output, session, store, events) {
      values <- reactiveValues()
      observeEvent(input$select_input, values$section <- input$select_input)
      observeEvent(input$action, values$action <- input$action)

      observeEvent(c(store$current_execution, events$update_report),
        {
          if (is.null(store$current_execution)) {
            updatePickerInput(
              session = session,
              inputId = "select_input",
              choices = c(""),
              selected = NULL
            )
          } else {
            report <- store$current_execution$get_report()

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
        },
        ignoreNULL = FALSE,
        ignoreInit = TRUE
      )

      return(values)
    }
  )
)
