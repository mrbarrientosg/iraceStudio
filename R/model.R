report <- R6::R6Class(
  classname = "report",
  private = list(
    data = NULL,
    count = 0,
    update = NULL
  ),
  public = list(
    initialize = function(report = NULL) {
      private$data <- list()
      private$update <- 0
      
      if (!is.null(report)) {
        private$data <- report$data
        private$count <- report$count
      }
    },
    
    add_title = function(title) {
      private$count <- private$count + 1
      private$data[[as.character(private$count)]]$title <- title
      playground_emitter$emit(playground_events$update_report)
    },
    
    set_content = function(id, content) {
      private$data[[as.character(id)]]$content <- content
    },
    
    set_title = function(id, title) {
      private$data[[as.character(id)]]$title <- title
    },
    
    remove_data = function(id) {
      private$data[[as.character(id)]] <- NULL
      private$count <- private$count - 1
      playground_emitter$emit(playground_events$update_report)
    },
    
    get_id = function(section) {
      idx <- sapply(private$data, function(data) any(data$title %in% section))
      key <- names(private$data)[idx]
      return(key)
    },
    
    get_data = function() private$data,
    get_count = function() private$count,
    
    as_list = function() {
      return(list(data = private$data, count = private$count))
    }
  )
)

Sandbox <- R6::R6Class(
  classname = "Sandbox",
  cloneable = FALSE,
  private = list(
    id = NA,
    name = "",
    description = "",
    elites = FALSE,
    iterations = NULL,
    filters = NULL,
    ids = NULL,
    descentId = "",
    trajectoryId = "",
    configurations = NULL
  ),
  
  public = list(
    initialize = function(name = "", description = "", sandbox = NULL) {
      private$name <- name
      private$description <- description
      private$filters <- data.frame()
      private$configurations <- data.frame()
      private$iterations <- c(0, 0)
      
      if (!is.null(sandbox)) {
        private$id <- sandbox$id
        private$name <- sandbox$name
        private$description <- sandbox$description
        private$elites <- sandbox$elites
        private$iterations <- sandbox$iterations
        private$filters <- sandbox$filters
        private$ids <- sandbox$ids
        private$descentId <- sandbox$descentId
        private$trajectoryId <- sandbox$trajectoryId
        private$configurations <- sandbox$configurations
      }
    },
    
    addFilter = function(filter) {
      private$filters <- rbind(private$filters, filter)
    },
    
    removeFilter = function(row) {
      private$filters <- private$filters[-row, ,drop = FALSE]
    },
    
    removeConfiguration = function(row) {
      private$configurations <- private$configurations[-row, ]
    },
    
    setId = function(id) private$id <- id,
    setName = function(name) private$name <- name,
    setDescription = function(desc) private$description <- desc,
    setElites = function(elites) private$elites <- elites,
    setIterations = function(iterations) private$iterations <- iterations,
    setIds = function(ids) private$ids <- ids,
    setDescentId = function(descentId) private$descentId <- descentId,
    setTrajectoryId = function(trajectoryId) private$trajectoryId <- trajectoryId,
    setConfigurations = function(configurations) private$configurations <- configurations,
    
    # GETTER
    getId = function() return(private$id),
    getName = function() return(private$name),
    getDescription = function() return(private$description),
    getElites = function() return(private$elites),
    getIterations = function() return(private$iterations),
    getFilters = function() return(private$filters),
    getIds = function() return(private$ids),
    getDescentId = function() return(private$descentId),
    getTrajectoryId = function() return(private$trajectoryId),
    getConfigurations = function() return(private$configurations),
    
    asList = function() {
      data <- list()
      data$id <- private$id
      data$name <- private$name
      data$description <- private$description
      data$elites <- private$elites
      data$iterations <- private$iterations
      data$filters <- private$filters
      data$ids <- private$ids
      data$descentId <- private$descentId
      data$trajectoryId <- private$trajectoryId
      data$configurations <- private$configurations
      return(data)
    }
  )
)

SandBoxes <- R6::R6Class(
  classname = "SandBoxes",
  private = list(
    boxes = NULL,
    count = 0
  ),
  public = list(
    initialize = function(data = NULL) {
      private$boxes <- list()
      private$count <- 0
      if (!is.null(data)) {
        for (name in names(data$sandboxes)) {
          private$boxes[[name]] <- Sandbox$new(sandbox = data$sandboxes[[name]])
        }
        private$count <- data$count
      } else {
        self$add_sandbox(Sandbox$new(name = "sandbox-1"))
      }
    },
    
    add_sandbox = function(sandbox) {
      private$count <- private$count + 1
      sandbox$setId(private$count)
      private$boxes[[as.character(private$count)]] <- sandbox
      playground_emitter$emit(playground_events$update_sandboxes)
    },
    
    remove_sandbox = function(id) {
      private$boxes[[id]] <- NULL
      private$count <- private$count - 1
      playground_emitter$emit(playground_events$update_sandboxes)
    },
    
    get_boxes = function() return(private$boxes),
    get_box = function (id) return(private$boxes[[id]]),
    
    as_list = function() {
      data <- list()
      
      data$sandboxes <- list()
      for (name in names(private$boxes)) {
        data$sandboxes[[name]] <- private$boxes[[name]]$asList()
      }
      
      data$count <- private$count
      
      return(data)
    }
  )
)

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

configurations <- R6::R6Class(
  classname ="configurations",
  cloneable = FALSE,
  private = list(
    data = NULL
  ),
  
  public = list(
    initialize = function(configurations = NULL, paramNames = NULL) {
      if (is.null(configurations)) {
        private$data <- data.frame(matrix(ncol = length(paramNames), nrow = 0))
        colnames(private$data) <- paramNames
      } else {
        private$data <- configurations
      }
    },
    
    add_configuration = function(configuration) {
      private$data <- rbind(private$data, configuration)
    },
    
    update_configuration = function(row, new_configuration) {
      private$data[row, ] <- new_configuration[1, ]
    },
    
    remove_configuration = function(row) {
      private$data <- private$data[-row, ]
    },
    
    addColumn = function(name) {
      l <- lapply(name, function(x) NA)
      names(l) <- name
      private$data <- tibble::add_column(private$data, as.data.frame(l))
    },
    
    updateColumn = function(oldName, newName) {
      private$data <- dplyr::rename(private$data, oldName = newName)
    },
    
    removeColumn = function(name) {
      private$data <- private$data[, !(names(private$data) %in% name), drop = F]
    },
    
    get_configurations = function() private$data,
    get_configuration = function(row) private$data[row, ],
    clear_configurations = function(removeAll = F) {
      if (removeAll) {
        private$data <- data.frame()
      } else {
        private$data <- private$data[0, , drop = F]
      }
    }
  )
)

parameters <- R6::R6Class(
  classname = "parameters",
  cloneable = FALSE,
  private = list(
    data = NULL,
    
    check_parameter_repeat = function(parameter) {
      if (nrow(private$data) > 0) {
        condition <- subset(
          private$data,
          names == parameter$names ||
            switches == parameter$switches,
          select = c("names", "switches")
        )
        
        if (nrow(condition) > 0) {
          return(TRUE)
        }
      }
      
      return(FALSE)
    },
    
    check_parameter_value_repeat = function(parameter) {
      result <- dplyr::semi_join(private$data, parameter)
      
      if (nrow(result) > 1) {
        return(TRUE)
      }
      
      return(FALSE)
    }
  ),
  
  public = list(
    initialize = function(parameters = NULL) {
      if (is.null(parameters))
        private$data <- data.frame(
          names = character(0),
          switches = character(0),
          types = character(0),
          domain = character(0),
          conditions = character(0),
          stringsAsFactors = FALSE
        )
      else
        private$data <- parameters
    },
    
    add_parameter = function(new_parameter) {
      if (private$check_parameter_repeat(new_parameter)) {
        stop("The parameter name or flag is repeated")
        return(FALSE)
      }
      
      private$data <- rbind(private$data, new_parameter)
      return(TRUE)
    },
    
    update_parameter = function(row, new_parameter) {
      if (private$check_parameter_value_repeat(new_parameter)) {
        return(NULL)
      }
      
      old <- private$data[row, ]
      private$data[row, ] <- new_parameter[1, ]
      return(old)
    },
    
    remove_parameter = function(row) {
      value <- private$data[row, ]
      private$data <- private$data[-row, ]
      return(value)
    },
    
    get_parameters = function() private$data,
    get_parameter = function(row) private$data[row, ],
    clear_parameters = function() {
      private$data <- data.frame(
        names = character(0),
        switches = character(0),
        types = character(0),
        domain = character(0),
        conditions = character(0),
        stringsAsFactors = FALSE
      )
    }
  )
)

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
    
    clear_scenario_temp = function() {
      self$add_irace_option("parameterFile", "")
      self$add_irace_option("trainInstancesFile", "")
      self$add_irace_option("trainInstancesDir", "")
      self$add_irace_option("scenarioFile", "")
      self$add_irace_option("execDir", "")
      self$add_irace_option("logFile", "")
      self$add_irace_option("configurationsFile", "")
      self$add_irace_option("forbiddenFile", "")
      self$add_irace_option("testInstancesFile", "")
      self$add_irace_option("testInstancesDir", "")
      self$add_irace_option("targetEvaluator", "")
      self$add_irace_option("targetRunner", "")
    },
    
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

playground <- R6::R6Class(
  classname = "playground",
  cloneable = FALSE,
  private = list(
    name = "",
    description = "",
    scenarios = NULL,
    last_scenario = NULL,
    current_scenario = NULL,
    last_insert = 0
  ),
  public = list(
    initialize = function(name = "", playground = NULL) {
      private$name <- name
      private$scenarios <- list()
      private$last_insert <- 0
      if (!is.null(playground)) {
        private$name <- playground$name
        private$description <- playground$description
        for (name in names(playground$scenarios)) {
          private$scenarios[[name]] <- scenario$new(scenario = playground$scenarios[[name]])
          private$current_scenario <- private$scenarios[[name]]
        }
        private$last_scenario <- playground$last_scenario
        private$last_insert <- playground$last_insert
      } else {
        self$add_scenario(scenario$new(name = "scenario-1"))
      }
    },
    
    add_scenario = function(new_scenario) {
      private$last_insert <- private$last_insert + 1
      id <- as.character(private$last_insert)
      
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
      playground$last_insert <- private$last_insert
      saveRDS(playground, file = path)
    }
  )
)