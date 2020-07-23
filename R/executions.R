execution <- R6::R6Class(
  classname = "execution",
  cloneable = FALSE,
  private = list(
    id = NA,
    name = "",
    irace_results = NULL,
    output_log = NULL,
    report = NULL,
    sandboxes = NULL
  ),
  public = list(
    initialize = function(name = "", execution = NULL) {
      private$name <- name
      if (!is.null(execution)) {
        private$id <- execution$id
        private$name <- execution$name
        private$irace_results <- execution$irace_results
        private$output_log <- execution$output_log
        private$report <- report$new(report = execution$report)
        private$sandboxes <- SandBoxes$new(data = execution$sandboxes)
      } else {
        private$report <- report$new()
        private$sandboxes <- SandBoxes$new()
      }
    },

    add_sandbox = function(sandbox) {
      private$sandboxes$add_sandbox(sandbox)
    },

    get_sandbox = function(id) {
      return(private$sandboxes$get_box(id))
    },

    remove_sandbox = function(id) {
      private$sandboxes$remove_sandbox(id)
    },

    set_id = function(id) {
      if (private$name == "") {
        private$name <- id
      }
      private$id <- id
    },
    
    set_irace_results = function(irace_results) {
      private$irace_results <- irace_results
    },
    
    set_output_log = function(output_log) {
      private$output_log <- output_log
    },
    
    get_id = function() private$id,
    get_output_log = function() private$output_log,
    get_irace_results = function() private$irace_results,
    get_report = function() private$report,
    get_name = function() private$name,
    get_sandboxes = function() private$sandboxes,
    
    as_list = function() {
      data <- list()
      data$id <- private$id
      data$name <- private$name
      data$irace_results <- private$irace_results
      data$output_log <- private$output_log
      data$report <- private$report$as_list()
      data$sandboxes <- private$sandboxes$as_list()
      return(data)
    }
  )
)

executions <- R6::R6Class(
  classname = "executions",
  cloneable = FALSE,
  private = list(
    executions = NULL,
    last_insert = NULL
  ),
  public = list(
    initialize = function(data = NULL) {
      private$executions <- list()
      private$last_insert <- 0
      if (!is.null(data)) {
        for (name in names(data$executions)) {
          private$executions[[name]] <- execution$new(execution = data$executions[[name]])
        }
        private$last_insert <- data$last_insert
      }
    },
    
    add_execution = function(id, execution) {
      private$last_insert <- private$last_insert + 1
      new_id <- paste0("execution-", id, "-", private$last_insert)
      execution$set_id(new_id)
      private$executions[[new_id]] <- execution
      playground_emitter$emit(playground_events$update_executions)
    },
    
    add_irace_results = function(id, irace_results) {
      private$executions[[id]]$set_irace_results(irace_results)
    },
    
    add_output_log = function(id, output_log) {
      private$executions[[id]]$set_output_log(output_log)
    },
    
    add_report = function(id, report) {
      private$executions[[id]]$set_report(report)
    },
    
    remove_all = function() {
      private$executions <- NULL
      private$executions <- list()
      private$last_insert <- 0
      playground_emitter$emit(playground_events$update_scenarios)
    },
    
    is_empty = function() length(private$executions) == 0,
    get_executions = function() private$executions,
    get_execution = function(id) {
      return(private$executions[[id]])
    },
    get_count = function() length(private$executions),
    
    as_list = function() {
      data <- list()
      
      data$executions <- list()
      for (name in names(private$executions)) {
        data$executions[[name]] <- private$executions[[name]]$as_list()
      }
      
      data$last_insert <- private$last_insert
      
      return(data)
    }
  )
)