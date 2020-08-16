run_irace <- function(store, executionName = "") {

  createFolders <- function(store) {
    workspacePath <- store$app$createWorkspaceDirectory()
    playgroundPath <- file.path(workspacePath, store$pg$get_name())

    if (!dir.exists(playgroundPath)) {
      dir.create(playgroundPath)
    }

    scenarioPath <- file.path(playgroundPath, store$pg$get_scenario_name())

    if (!dir.exists(scenarioPath)) {
      dir.create(scenarioPath)
    }

    pkg$tempFolder <- file.path(scenarioPath, ".data")
    pkg$executionFolder <- file.path(scenarioPath, "executions")

    if (!dir.exists(pkg$executionFolder)) {
      dir.create(pkg$executionFolder)
    }

    createHiddenDirectory(pkg$tempFolder)
  }

  createFolders(store)

  store$pg$add_irace_option(
    option = "parameterFile",
    value = dQuote(file.path(pkg$tempFolder, "parameter.txt"), FALSE)
  )

  store$pg$add_irace_option(
    option = "trainInstancesFile",
    value = dQuote(file.path(pkg$tempFolder, "instances.txt"), FALSE)
  )

  store$pg$add_irace_option(
    option = "scenarioFile",
    value = dQuote(file.path(pkg$tempFolder, "scenario.txt"), FALSE)
  )

  store$pg$add_irace_option(
    option = "execDir",
    value = dQuote(file.path(pkg$tempFolder), FALSE)
  )

  logFileName <- sprintf("irace-%s.Rdata", executionName)

  store$pg$add_irace_option(
    option = "logFile",
    value = dQuote(file.path(pkg$executionFolder, logFileName), FALSE)
  )

  pkg$outputLog <- sprintf("output-%s.log", executionName)
  pkg$outputLog <- file.path(pkg$executionFolder, pkg$outputLog)

  if (nrow(store$pg$get_parameters()) == 0) {
    alert.error("Provide parameters to run irace.")
    return(invisible())
  }

  if (is.null(store$pg$get_train_instances()) || store$pg$get_train_instances() == "" ||
    length(store$pg$get_train_instances()) == 0) {
    alert.error("Provide train instances to run irace.")
    return(invisible())
  }

  if (is.null(store$pg$get_target_runner()) || store$pg$get_target_runner() == "" ||
    length(store$pg$get_target_runner()) == 0) {
    alert.error("Provide a target runner to run irace.")
    return(invisible())
  }

  create_scenario_file(pkg$tempFolder, store$pg)
  create_parameter_file(pkg$tempFolder, store$pg)
  create_instances_file(pkg$tempFolder, store$pg)
  create_target_runner_file(pkg$tempFolder, store$pg)

  if (nrow(store$pg$get_configurations()) > 0) {
    store$pg$add_irace_option(
      option = "configurationsFile",
      value = dQuote(file.path(pkg$tempFolder, "configurations.txt"), FALSE)
    )

    create_initial_config_file(pkg$tempFolder, store$pg)
  }

  if (!is.null(store$pg$get_forbidden()) && store$pg$get_forbidden() != "" &&
    length(store$pg$get_forbidden()) != 0) {
    store$pg$add_irace_option(
      option = "forbiddenFile",
      value = dQuote(file.path(pkg$tempFolder, "forbidden.txt"), FALSE)
    )

    create_forbidden_file(pkg$tempFolder, store$pg)
  }

  if (!is.null(store$pg$get_test_instances()) && store$pg$get_test_instances() != "" &&
    length(store$pg$get_test_instances()) != 0) {
    store$pg$add_irace_option(
      option = "testInstancesFile",
      value = dQuote(file.path(pkg$tempFolder, "test-instances.txt"), FALSE)
    )

    create_test_instances_file(pkg$tempFolder, store$pg)
  }

  if (!is.null(store$pg$get_target_evaluator()) && store$pg$get_target_evaluator() != "" &&
    length(store$pg$get_target_evaluator()) != 0) {
    create_target_evaluator_file(pkg$tempFolder, store$pg)

    store$pg$add_irace_option(
      option = "targetEvaluator",
      value = "./target-evaluator"
    )
  }

  store$pg$add_irace_option(
    option = "targetRunner",
    value = "./target-runner"
  )

  store$pg$add_irace_option(option = "trainInstancesDir", value = '""')
  store$pg$add_irace_option(option = "testInstancesDir", value = '""')

  store$iraceProcess <- process$new(
    command = "Rscript",
    args = c(
      system.file("inst/app/script/run_irace.R", package = packageName()),
      store$gui$iracePath,
      pkg$tempFolder,
      pkg$outputLog
    ),
    stdout = "|", stderr = "|"
  )

  #print(store$iraceProcess$poll_io(5000))
  #print(store$iraceProcess$read_output_lines())
  #print(store$iraceProcess$read_error_lines())

  store$startIrace <- TRUE
  shinyalert(title = "IRACE is now running", type = "success", timer = 1500)
}