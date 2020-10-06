run_irace <- function(store, executionName = "") {

  createFolders <- function(store) {
    store$gui$createWorkspaceDirectory()

    workspacePath <- store$gui$workspacePath
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

    log_trace("Data folder {pkg$tempFolder}")

    if (!dir.exists(pkg$executionFolder)) {
      dir.create(pkg$executionFolder)
    }

    createHiddenDirectory(pkg$tempFolder)
  }

  createFolders(store)

  path <- file.path(pkg$tempFolder, "parameter.txt")
  log_trace("1. Set parameter file {path}")
  store$pg$add_irace_option(
    option = "parameterFile",
    value = dQuote(path, FALSE)
  )

  path <- file.path(pkg$tempFolder, "instances.txt")
  log_trace("2. Set train instances file {path}")
  store$pg$add_irace_option(
    option = "trainInstancesFile",
    value = dQuote(path, FALSE)
  )

  path <- file.path(pkg$tempFolder, "scenario.txt")
  log_trace("3. Set scenario file {path}")
  store$pg$add_irace_option(
    option = "scenarioFile",
    value = dQuote(path, FALSE)
  )

  path <- file.path(pkg$tempFolder)
  log_trace("4. Set execDir {path}")
  store$pg$add_irace_option(
    option = "execDir",
    value = dQuote(path, FALSE)
  )

  logFileName <- sprintf("irace-%s.Rdata", executionName)
  path <- file.path(pkg$executionFolder, logFileName)
  log_trace("5. Set log file {path}")
  store$pg$add_irace_option(
    option = "logFile",
    value = dQuote(path, FALSE)
  )

  pkg$outputLog <- sprintf("output-%s.log", executionName)
  pkg$outputLog <- file.path(pkg$executionFolder, pkg$outputLog)

  if (nrow(store$pg$get_parameters()) == 0) {
    log_error("No parameters")
    alert.error("Provide parameters to run irace.")
    return(invisible())
  }

  if (is.null(store$pg$get_train_instances()) || store$pg$get_train_instances() == "" ||
    length(store$pg$get_train_instances()) == 0) {
    log_error("No train instances")
    alert.error("Provide train instances to run irace.")
    return(invisible())
  }

  if (is.null(store$pg$get_target_runner()) || store$pg$get_target_runner() == "" ||
    length(store$pg$get_target_runner()) == 0) {
    log_error("No target runner")
    alert.error("Provide a target runner to run irace.")
    return(invisible())
  }

  log_trace("6. Create all data files")
  create_parameter_file(pkg$tempFolder, store$pg)
  create_instances_file(pkg$tempFolder, store$pg)
  create_target_runner_file(pkg$tempFolder, store$pg)

  if (nrow(store$pg$get_configurations()) > 0) {
    path <- file.path(pkg$tempFolder, "configurations.txt")
    log_trace("7. Create initial configuration file {path}")
    store$pg$add_irace_option(
      option = "configurationsFile",
      value = dQuote(path, FALSE)
    )

    create_initial_config_file(pkg$tempFolder, store$pg)
  }

  if (!is.null(store$pg$get_forbidden()) && store$pg$get_forbidden() != "" &&
    length(store$pg$get_forbidden()) != 0) {
    path <- file.path(pkg$tempFolder, "forbidden.txt")
    log_trace("8. Create forbidden file {path}")
    store$pg$add_irace_option(
      option = "forbiddenFile",
      value = dQuote(path, FALSE)
    )

    create_forbidden_file(pkg$tempFolder, store$pg)
  }

  if (!is.null(store$pg$get_test_instances()) && store$pg$get_test_instances() != "" &&
    length(store$pg$get_test_instances()) != 0) {
    path <- file.path(pkg$tempFolder, "test-instances.txt")
    log_trace("9. Create test instances file {path}")
    store$pg$add_irace_option(
      option = "testInstancesFile",
      value = dQuote(path, FALSE)
    )

    create_test_instances_file(pkg$tempFolder, store$pg)
  }

  if (!is.null(store$pg$get_target_evaluator()) && store$pg$get_target_evaluator() != "" &&
    length(store$pg$get_target_evaluator()) != 0) {
    log_trace("10. Set target evaluator ./target-evaluator")

    targetEvaluator <- "target-evaluator"

    if (.Platform$OS.type == "windows") {
      targetEvaluator <- paste0(targetEvaluator,  ".bat")
    } else {
      targetEvaluator <- paste0("./", targetEvaluator)
    }

    store$pg$add_irace_option(
      option = "targetEvaluator",
      value = dQuote(targetEvaluator, FALSE)
    )

    create_target_evaluator_file(pkg$tempFolder, store$pg)
  }

  log_trace("11. Set target runner")

  targetRunner <- "target-runner"

  if (.Platform$OS.type == "windows") {
    targetRunner <- paste0(targetRunner,  ".bat")
  } else {
    targetRunner <- paste0("./", targetRunner)
  }

  store$pg$add_irace_option(
    option = "targetRunner",
    value = dQuote(targetRunner, FALSE)
  )

  store$pg$add_irace_option(option = "trainInstancesDir", value = '""')
  store$pg$add_irace_option(option = "testInstancesDir", value = '""')

  log_trace("12. Create scenario file")
  create_scenario_file(pkg$tempFolder, store$pg)

  log_trace("13. Run irace script")
  log_debug("Script run with: {store$pg$get_irace_path()}\n{pkg$tempFolder}\n{pkg$outputLog}")
  store$iraceProcess <- process$new(
    command = "Rscript",
    args = c(
      system.file("inst/app/script/run_irace.R", package = packageName()),
      store$pg$get_irace_path(),
      pkg$tempFolder,
      pkg$outputLog
    ),
    stdout = "|", stderr = "|"
  )

  if (get_option("debug", FALSE)) {
    store$iraceProcess$poll_io(3000)
    log_info("stdout: {store$iraceProcess$read_output_lines()}")
    log_info("stderr: {store$iraceProcess$read_error_lines()}")
  }


  store$startIrace <- TRUE
  shinyalert(title = "IRACE is now running", type = "success", timer = 1500)
}