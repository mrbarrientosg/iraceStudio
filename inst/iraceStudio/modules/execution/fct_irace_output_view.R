run_irace <- function(store, events, execution_name = "") {
  create_folders <- function(store) {
    store$gui$createWorkspaceDirectory()

    workspace_path <- store$gui$workspace_path
    playground_path <- file.path(workspace_path, store$pg$get_name())

    if (!dir.exists(playground_path)) {
      dir.create(playground_path)
    }

    scenario_path <- file.path(playground_path, store$pg$get_scenario_name())

    if (!dir.exists(scenario_path)) {
      dir.create(scenario_path)
    }

    pkg$temp_folder <- file.path(scenario_path, ".data")
    pkg$execution_folder <- file.path(scenario_path, "executions")

    log_trace("Data folder {pkg$temp_folder}")

    if (!dir.exists(pkg$execution_folder)) {
      dir.create(pkg$execution_folder)
    }

    if (dir.exists(pkg$temp_folder)) {
      unlink(pkg$temp_folder, recursive = TRUE, force = TRUE)
    }

    create_hidden_directory(pkg$temp_folder)
  }

  create_folders(store)

  path <- file.path(pkg$temp_folder, "parameter.txt")
  log_trace("1. Set parameter file {path}")
  store$pg$add_irace_option(
    option = "parameterFile",
    value = dQuote(path, FALSE)
  )

  path <- file.path(pkg$temp_folder, "instances.txt")
  log_trace("2. Set train instances file {path}")
  store$pg$add_irace_option(
    option = "trainInstancesFile",
    value = dQuote(path, FALSE)
  )

  path <- file.path(pkg$temp_folder, "scenario.txt")
  log_trace("3. Set scenario file {path}")
  store$pg$add_irace_option(
    option = "scenarioFile",
    value = dQuote(path, FALSE)
  )

  path <- file.path(pkg$temp_folder)
  log_trace("4. Set execDir {path}")
  store$pg$add_irace_option(
    option = "execDir",
    value = dQuote(path, FALSE)
  )

  log_file_name <- sprintf("irace-%s.Rdata", execution_name)
  path <- file.path(pkg$execution_folder, log_file_name)
  log_trace("5. Set log file {path}")
  store$pg$add_irace_option(
    option = "logFile",
    value = dQuote(path, FALSE)
  )

  pkg$output_log <- sprintf("output-%s.log", execution_name)
  pkg$output_log <- file.path(pkg$execution_folder, pkg$output_log)

  if (nrow(store$pg$get_parameters()) == 0) {
    log_error("No parameters")
    alert_error("Provide parameters to run irace.")
    return(invisible())
  }

  if (is.null(store$pg$get_train_instances()) || store$pg$get_train_instances() == "" ||
    length(store$pg$get_train_instances()) == 0) {
    log_error("No train instances")
    alert_error("Provide train instances to run irace.")
    return(invisible())
  }

  if (is.null(store$pg$get_target_runner()) || store$pg$get_target_runner() == "" ||
    length(store$pg$get_target_runner()) == 0) {
    log_error("No target runner")
    alert_error("Provide a target runner to run irace.")
    return(invisible())
  }

  log_trace("6. Create all data files")
  create_parameter_file(pkg$temp_folder, store$pg)
  create_instances_file(pkg$temp_folder, store$pg)
  create_target_runner_file(pkg$temp_folder, store$pg)

  if (nrow(store$pg$get_configurations()) > 0) {
    path <- file.path(pkg$temp_folder, "configurations.txt")
    log_trace("7. Create initial configuration file {path}")
    store$pg$add_irace_option(
      option = "configurationsFile",
      value = dQuote(path, FALSE)
    )

    create_initial_config_file(pkg$temp_folder, store$pg)
  }

  if (!is.null(store$pg$get_forbidden()) && store$pg$get_forbidden() != "" &&
    length(store$pg$get_forbidden()) != 0) {
    path <- file.path(pkg$temp_folder, "forbidden.txt")
    log_trace("8. Create forbidden file {path}")
    store$pg$add_irace_option(
      option = "forbiddenFile",
      value = dQuote(path, FALSE)
    )

    create_forbidden_file(pkg$temp_folder, store$pg)
  }

  if (!is.null(store$pg$get_test_instances()) && store$pg$get_test_instances() != "" &&
    length(store$pg$get_test_instances()) != 0) {
    path <- file.path(pkg$temp_folder, "test-instances.txt")
    log_trace("9. Create test instances file {path}")
    store$pg$add_irace_option(
      option = "testInstancesFile",
      value = dQuote(path, FALSE)
    )

    create_test_instances_file(pkg$temp_folder, store$pg)
  }

  if (!is.null(store$pg$get_target_evaluator()) && store$pg$get_target_evaluator() != "" &&
    length(store$pg$get_target_evaluator()) != 0) {
    log_trace("10. Set target evaluator ./target-evaluator")

    target_evaluator <- "target-evaluator"

    if (.Platform$OS.type == "windows") {
      target_evaluator <- paste0(target_evaluator, ".bat")
    } else {
      target_evaluator <- paste0("./", target_evaluator)
    }

    store$pg$add_irace_option(
      option = "targetEvaluator",
      value = dQuote(target_evaluator, FALSE)
    )

    create_target_evaluator_file(pkg$temp_folder, store$pg)
  }

  log_trace("11. Set target runner")

  target_runner <- "target-runner"

  if (.Platform$OS.type == "windows") {
    target_runner <- paste0(target_runner, ".bat")
  } else {
    target_runner <- paste0("./", target_runner)
  }

  store$pg$add_irace_option(
    option = "targetRunner",
    value = dQuote(target_runner, FALSE)
  )

  store$pg$add_irace_option(option = "trainInstancesDir", value = '""')
  store$pg$add_irace_option(option = "testInstancesDir", value = '""')

  log_trace("12. Create scenario file")
  create_scenario_file(pkg$temp_folder, store$pg)

  log_trace("13. Create run script")
  script_path <- file.path(pkg$temp_folder, "run_irace.R")
  file.copy(system.file("app/script/run_irace.R", package = "iraceStudio"), script_path)

  log_trace("14. Run irace script")
  log_debug("Script run with: {store$pg$get_irace_path()}\n{pkg$temp_folder}\n{pkg$output_log}")
  store$irace_process <- process$new(
    command = "Rscript",
    args = c(
      script_path,
      store$pg$get_irace_path(),
      pkg$temp_folder,
      pkg$output_log
    ),
    stdout = "|", stderr = "|"
  )

  # if (get_option("debug", FALSE)) {
  #  store$irace_process$poll_io(3000)
  #  log_info("stdout: {store$irace_process$read_output_lines()}")
  #  log_info("stderr: {store$irace_process$read_error_lines()}")
  # }

  events$is_irace_running <- TRUE
  shinyalert(title = "IRACE is now running", type = "success", timer = 1500)
}
