ExecutionSelect <- R6::R6Class(
  classname = "ExecutionSelect",
  inherit = Component,
  public = list(
    ui = function(inputId, ...) {
      ns <- NS(inputId)

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

    server = function(input, output, session, store) {
      values <- reactiveValues()

      observeEvent(c(playground_emitter$value(playground_events$current_scenario),
        playground_emitter$value(playground_events$update_executions)), {

        if (length(store$pg$get_executions()) == 0) {
          return()
        }

        executions <- lapply(store$pg$get_executions(), function(execution) execution$get_name())
        executions_id <- lapply(store$pg$get_executions(), function(execution) execution$get_id())

        if (length(executions) == 0) {
          store$iraceResults <- NULL
          store$currentExecution <- NULL
          executions_id <- ""
        } else {
          names(executions_id) <- unlist(executions, use.names = FALSE)
          exe <- store$pg$get_execution(executions_id[[1]])
          store$currentExecution <- exe
          store$iraceResults <- exe$get_irace_results()
        }

        updatePickerInput(
          session = session,
          inputId = "options",
          choices = executions_id
        )
      })

      observeEvent(input$options, values$option <- input$options)

      observeEvent(c(store$pg, input$options), {
        req(input$options != "")
        exe <- store$pg$get_execution(input$options)
        store$currentExecution <- exe
        if (!is.null(exe))
          store$iraceResults <- exe$get_irace_results()
      })

      observeEvent(store$currentExecution, {
        updatePickerInput(
          session = session,
          inputId = "options",
          selected = store$currentExecution$get_id()
        )
      })

      return(values)
    }
  )
)