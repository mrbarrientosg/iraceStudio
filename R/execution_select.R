#' @export
ExecutionSelect <- R6::R6Class( # nolint
  classname = "ExecutionSelect",
  inherit = Component,
  public = list(
    ui = function(input_id, ...) {
      ns <- NS(input_id)

      pickerInput(
        inputId = ns("options"),
        label = "Execution",
        choices = "",
        options = list(
          size = 8
        ),
        ...
      )
    },

    server = function(input, output, session, store, events) {
      values <- reactiveValues()

      observeEvent(c(
        events$change_scenario,
        events$update_executions
      ),
      {
        if (length(store$pg$get_executions()) == 0) {
          store$irace_results <- NULL
          store$current_execution <- NULL
          updatePickerInput(
            session = session,
            inputId = "options",
            choices = c("")
          )
          return(invisible(NULL))
        }

        executions <- lapply(store$pg$get_executions(), function(execution) execution$get_name())
        executions_id <- lapply(store$pg$get_executions(), function(execution) execution$get_id())

        if (length(executions) == 0) {
          store$irace_results <- NULL
          store$current_execution <- NULL
          executions_id <- ""
        } else {
          names(executions_id) <- unlist(executions, use.names = FALSE)
          exe <- store$pg$get_execution(executions_id[[1]])
          store$current_execution <- exe
          store$irace_results <- exe$get_irace_results()
        }

        updatePickerInput(
          session = session,
          inputId = "options",
          choices = executions_id
        )
      },
      ignoreInit = TRUE
      )

      observeEvent(input$options, values$option <- input$options)

      observeEvent(c(store$pg, input$options),
        {
          req(input$options != "")
          exe <- store$pg$get_execution(input$options)
          store$current_execution <- exe
          if (!is.null(exe)) {
            store$irace_results <- exe$get_irace_results()
          }
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      observeEvent(store$current_execution,
        {
          updatePickerInput(
            session = session,
            inputId = "options",
            selected = store$current_execution$get_id()
          )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      return(values)
    }
  )
)
