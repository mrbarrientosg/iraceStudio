ExecutionSelect <- R6::R6Class(
  classname = "ExecutionSelect",
  inherit = Component,
  public = list(
    ui = function(inputId, ...) {
      ns <- NS(inputId)

      pickerInput(
        inputId = ns("options"),
        label = "Executions",
        choices = "",
        ...
      )
    },
    
    server = function(input, output, session, store) {
      values <- reactiveValues()
      
      observe({
        change_scenario <- store$pg$get_change_current()
        change_scenario()
        
        count <- store$pg$get_executions_count()
        count()
        
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
          store$sandbox <- exe$getSandbox()
        }

        updatePickerInput(
          session = session,
          inputId = "options",
          choices = executions_id
        )
      })
      
      observe(values$option <- input$options)
      
      return(values)
    }
  )
)