scenario <- R6::R6Class(
  classname ="scenario",
  cloneable = FALSE,
  private = list(
    id = NA,
    name = "",
    description = "",
    creation_date = NULL,
    irace_options = list(),
    parameters = NULL,
    target_runner = "",
    target_evaluator = "",
    forbidden = "",
    train_instances = "",
    test_instances = "",
    configurations = NULL,
    executions = NULL
  ),
  public = list(
    initialize = function(name = "", description = "", scenario = NULL) {
      private$name <- name
      private$description <- description
      private$creation_date <- Sys.time()
      if (!is.null(scenario)) {
        private$id <- scenario$id
        private$name <- scenario$name
        private$description <- scenario$description
        private$creation_date <- scenario$creation_date
        private$irace_options <- scenario$irace_options
        private$parameters <- parameters$new(scenario$parameters)
        private$target_runner <- scenario$target_runner
        private$target_evaluator <- scenario$target_evaluator
        private$forbidden <- scenario$forbidden
        private$train_instances <- scenario$train_instances
        private$test_instances <- scenario$test_instances
        private$configurations <- configurations$new(configurations = scenario$configurations)
        private$executions <- executions$new(scenario$executions)
      } else {
        private$executions <- executions$new()
        private$parameters <- parameters$new()
        private$configurations <- configurations$new(paramNames = c())
      }
    },
  
  # Se retorna TRUE en caso que hayan ejecucion y se este cambiando el valor de algo
    add_irace_option = function(option, value) {
      private$irace_options[[option]] <- value
    },
    
    add_parameter = function(new_parameter) {
      v <- private$parameters$add_parameter(new_parameter)
      
      if (v)
        private$configurations$addColumn(new_parameter$names)
    },
    
    update_parameter = function(row, new_parameter) {
      old <- private$parameters$update_parameter(row, new_parameter)
      
      if (!is.null(old))
        private$configurations$updateColumn(old$names, new_parameter$names)
    },
    
    remove_parameter = function(row) {
      r <- private$parameters$remove_parameter(row)
      private$configurations$removeColumn(r$names)
    },
    
    add_configuration = function(configuration) {
      private$configurations$add_configuration(configuration)
    },
    
    update_configuration = function(row, new_configuration) {
      private$configurations$update_configuration(row, new_configuration)
    },
    
    remove_configuration = function(row) {
      private$configurations$remove_configuration(row)
    },
    
    add_execution = function(execution) {
      private$executions$add_execution(private$id, execution)
    },
    
    remove_executions = function() {
      private$executions$remove_all()
    },
    
    set_target_runner = function(code) {
      private$target_runner <- code
    },
    
    set_target_evaluator = function(code) {
      private$target_evaluator <- code
    },
    
    set_forbidden = function(code) {
      private$forbidden <- code
    },
    
    add_irace_results = function(id, irace_results) {
      private$executions$add_irace_results(id, irace_results)
    },
    
    add_report = function(id, report) {
      private$executions$add_report(id, report)
    },
    
    add_output_log = function(id, output_log) {
      private$executions$add_output_log(id, output_log)
    },
    
    set_id = function(id) {
      private$id <- id
    },
    
    get_id = function() private$id,
    
    set_name = function(name) private$name <- name,
    get_name = function() private$name,
    
    get_description = function() private$description,
    set_description = function(description) private$description <- description,
    
    get_creation_date = function() private$creation_date,
    
    get_parameters = function() private$parameters$get_parameters(),
    clear_parameters = function() {
      private$parameters$clear_parameters()
      private$configurations$clear_configurations(T)
    },
    get_parameter = function(row) private$parameters$get_parameter(row),
    
    get_configurations = function() private$configurations$get_configurations(),
    clear_configurations = function() private$configurations$clear_configurations(),
    get_configuration = function(row) private$configurations$get_configuration(row),
    
    get_train_instances = function() private$train_instances,
    set_train_instances = function(train_instances) private$train_instances <- train_instances,
    
    set_test_instances = function(test_instances) private$test_instances <- test_instances,
    get_test_instances = function() private$test_instances,
    
    get_target_runner = function() private$target_runner,
    get_target_evaluator = function() private$target_evaluator,
    
    get_forbidden = function() private$forbidden,
    
    get_irace_options = function() private$irace_options,
    get_irace_option = function(option) private$irace_options[[option]],
    
    get_executions = function() private$executions$get_executions(),
    get_execution = function(name) private$executions$get_execution(name),

    as_list = function() {
      data <- list()
      data$id <- private$id
      data$name <- private$name
      data$description <- private$description
      data$creation_date <- private$creation_date
      data$irace_options <- private$irace_options
      data$parameters <- private$parameters$get_parameters()
      data$target_runner <- private$target_runner
      data$target_evaluator <- private$target_evaluator
      data$forbidden <- private$forbidden
      data$train_instances <- private$train_instances
      data$test_instances <- private$test_instances
      data$configurations <- private$configurations$get_configurations()
      data$executions <- private$executions$as_list()
      return(data)
    }
  )
)