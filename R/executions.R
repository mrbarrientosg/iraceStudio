execution <- R6::R6Class(
  classname = "execution",
  cloneable = FALSE,
  private = list(
    id = NA,
    name = "",
    irace_results = NULL,
    output_log = NULL,
    report = NULL,
    sandbox = NULL
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
        private$sandbox <- Sandbox$new(sandbox = execution$sandbox)
      } else {
        private$report <- report$new()
        private$sandbox <- Sandbox$new()
      }
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
    getSandbox = function() private$sandbox,
    
    as_list = function() {
      data <- list()
      data$id <- private$id
      data$name <- private$name
      data$irace_results <- private$irace_results
      data$output_log <- private$output_log
      data$report <- private$report$as_list()
      data$sandbox <- private$sandbox$asList()
      return(data)
    }
  )
)

executions <- R6::R6Class(
  classname = "executions",
  cloneable = FALSE,
  private = list(
    executions = NULL,
    count = NULL
  ),
  public = list(
    initialize = function(data = NULL) {
      private$executions <- list()
      private$count <- reactiveVal(value = 0)
      if (!is.null(data)) {
        for (name in names(data$executions)) {
          private$executions[[name]] <- execution$new(execution = data$executions[[name]])
        }
        private$count(data$count)
      }
    },
    
    add_execution = function(id, execution) {
      new_id <- paste0("execution-", id, "-", isolate(private$count() + 1))
      execution$set_id(new_id)
      private$executions[[new_id]] <- execution
      private$count(isolate(private$count()) + 1)
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
      private$count(0)
    },
    
    is_empty = function() isolate(private$count()) == 0,
    size = function() isolate(private$count()),
    get_executions = function() private$executions,
    get_execution = function(id) {
      print(id)
      return(private$executions[[id]])
    },
    get_count = function() private$count,
    
    as_list = function() {
      data <- list()
      
      data$executions <- list()
      for (name in names(private$executions)) {
        data$executions[[name]] <- private$executions[[name]]$as_list()
      }
      
      data$count <- isolate(private$count())
      
      return(data)
    }
  )
)