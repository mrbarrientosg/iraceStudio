playground <- R6::R6Class(
  classname = "playground",
  cloneable = FALSE,
  private = list(
    name = "",
    description = "",
    scenarios = NULL,
    last_scenario = NULL,
    current_scenario = NULL,
    count = 0
  ),
  public = list(
    initialize = function(name = "", playground = NULL) {
      private$name <- name
      private$scenarios <- list()
      private$count <- 0
      if (!is.null(playground)) {
        private$name <- playground$name
        private$description <- playground$description
        for (name in names(playground$scenarios)) {
          private$scenarios[[name]] <- scenario$new(scenario = playground$scenarios[[name]])
          private$current_scenario <- private$scenarios[[name]]
        }
        private$last_scenario <- playground$last_scenario
        private$count <- playground$count
      } else {
        self$add_scenario(scenario$new(name = "scenario-1"))
      }
    },
    
    add_scenario = function(new_scenario) {
      private$count <- private$count + 1
      id <- as.character(private$count)
      
      if (is.null(private$scenarios)) {
        private$scenarios <- list()
      }
      
      new_scenario$set_id(id)
      private$scenarios[[id]] <- new_scenario
      playground_emitter$emit(playground_events$update_scenarios)
      
      if (is.null(private$current_scenario)) {
        private$current_scenario <- new_scenario
        playground_emitter$emit(playground_events$current_scenario)
      }
    },
    
    remove_scenario = function(id) {
      private$count <- private$count - 1
      private$scenarios[[id]] <- NULL
      
      if (is.null(private$scenarios) || length(private$scenarios) == 0) {
        self$add_scenario(scenario$new(name = "scenario-1"))
      } else {
        playground_emitter$emit(playground_events$update_scenarios)
      }
    },
    
    add_irace_option = function(option, value) {
      private$current_scenario$add_irace_option(option, value)
    },
    
    add_execution = function(execution) {
      private$current_scenario$add_execution(execution)
    },
    
    add_parameter = function(parameter) {
      private$current_scenario$add_parameter(parameter)
    },
    
    update_parameter = function(row, new_parameter) {
      private$current_scenario$update_parameter(row, new_parameter)
    },
    
    remove_parameter = function(row) {
      private$current_scenario$remove_parameter(row)
    },
    
    add_configuration = function(configuration) {
      private$current_scenario$add_configuration(configuration)
    },
    
    update_configuration = function(row, new_configuration) {
      private$current_scenario$update_configuration(row, new_configuration)
    },
    
    remove_configuration = function(row) {
      private$current_scenario$remove_configuration(row)
    },
    
    add_target_runner = function(code) {
      private$current_scenario$set_target_runner(code)
    },
    
    add_target_evaluator = function(code) {
      private$current_scenario$set_target_evaluator(code)
    },
    
    add_forbidden = function(code) {
      private$current_scenario$set_forbidden(code)
    },
    
    add_irace_results = function(id, irace_results) {
      private$current_scenario$add_irace_results(id, irace_results)
    },
    
    add_output_log = function(id, output_log) {
      private$current_scenario$add_output_log(id, output_log)
    },
    
    add_report = function(id, report) {
      private$current_scenario$add_report(id, report)
    },
    
    set_name = function(name) private$name <- name,
    get_name = function() private$name,
    
    set_description = function(description) private$description <- description,
    get_description = function() private$description,
    
    set_scenario_name = function(scenario_name) private$current_scenario$set_name(scenario_name),
    get_scenario_name = function() private$current_scenario$get_name(),
    
    get_parameters = function() private$current_scenario$get_parameters(),
    clear_parameters = function() private$current_scenario$clear_parameters(),
    get_parameter = function(row) private$current_scenario$get_parameter(row),
    
    get_configurations = function() private$current_scenario$get_configurations(),
    clear_configurations = function() private$current_scenario$clear_configurations(),
    get_configuration = function(row) private$current_scenario$get_configuration(row),
    
    set_train_instances = function(train_instances) private$current_scenario$set_train_instances(train_instances),
    get_train_instances = function() private$current_scenario$get_train_instances(),
    
    set_test_instances = function(test_instances) private$current_scenario$set_test_instances(test_instances),
    get_test_instances = function() private$current_scenario$get_test_instances(),
    
    get_target_runner = function() private$current_scenario$get_target_runner(),
    get_target_evaluator = function() private$current_scenario$get_target_evaluator(),
    
    get_forbidden = function() private$current_scenario$get_forbidden(),
    
    get_irace_options = function() private$current_scenario$get_irace_options(),
    get_irace_option = function(option) private$current_scenario$get_irace_option(option),
    
    get_scenarios = function() private$scenarios,
    get_scenario = function(id) private$scenarios[[as.character(id)]],

    get_executions = function() private$current_scenario$get_executions(),
    get_execution = function(name) private$current_scenario$get_execution(name),

    get_last_scenario = function() private$last_scenario,
    set_last_scenario = function(value) private$last_scenario <- value,
    
    change_current_scenario = function(id) {
      scenario <- private$scenarios[[id]]
      private$current_scenario <- scenario
      playground_emitter$emit(playground_events$current_scenario)
    },
    
    clear_scenario_temp = function() {
      private$current_scenario$clear_scenario_temp()
    },
    
    save = function(path) {
      playground <- list()
      playground$name <- private$name
      playground$description <- private$description
      playground$last_scenario <- private$current_scenario$get_id()
      playground$scenarios <- list()
      for (name in names(private$scenarios)) {
        playground$scenarios[[name]] <- private$scenarios[[name]]$as_list()
      }
      playground$count <- private$count
      saveRDS(playground, file = path)
    }
  )
)