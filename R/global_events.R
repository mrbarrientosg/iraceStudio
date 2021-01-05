EventEmitter <- R6::R6Class(
  classname = "EventEmitter",
  private = list(
    values = NULL
  ),
  public = list(
    initialize = function() {
      private$values <- reactiveValues()
    },

    value = function(name) {
      return(private$values[[name]])
    },

    emit = function(name) {
      if (is.null(isolate(private$values[[name]]))) {
        isolate(private$values[[name]] <- 0)
      }

      value <- isolate(private$values[[name]] + 1)
      private$values[[name]] <- value
      return(invisible(NULL))
    }
  )
)

#' @export
global_events <- list(
  update_scenarios = "update_scenarios",
  current_scenario = "current_scenario",
  update_executions = "update_executions",
  update_parameters = "update_parameters",
  update_sandboxes = "update_sandboxes",
  update_report = "update_report"
)

#' @export
global_emitter <- EventEmitter$new()