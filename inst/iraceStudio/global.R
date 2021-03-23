suppressMessages(library(iraceStudio))
suppressMessages(library(shiny))
suppressMessages(library(DT))
suppressMessages(library(glue))
suppressMessages(library(bs4Dash))
suppressMessages(library(shinyjs))
suppressMessages(library(shinyalert))
suppressMessages(library(shinyAce))
suppressMessages(library(data.table))
suppressMessages(library(processx))
suppressMessages(library(irace))
suppressMessages(library(promises))
suppressMessages(library(future))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shinyFiles))
suppressMessages(library(ipc))
suppressMessages(library(fs))
suppressMessages(library(logger))
suppressMessages(library(R6))
suppressMessages(library(knitr))
suppressMessages(library(dplyr))
suppressMessages(library(shinybusy))
suppressMessages(library(plotly))
suppressMessages(library(shinycssloaders))
suppressMessages(library(sortable))
suppressMessages(library(shinyhelper))

plan("future::sequential")

for (f in list.files("component", recursive = T, pattern = ".R", full.names = T)) {
  source(f)
}

for (f in list.files("modules", recursive = T, pattern = ".R", full.names = T)) {
  source(f)
}

app <- App$new()
app$setup()

pkg <- new.env(parent = emptyenv())
pkg$report_store <- list()

scenario_options <- jsonlite::fromJSON(
  system.file("app/static/scenario_options.json", package = "iraceStudio"),
  simplifyDataFrame = TRUE,
  flatten = TRUE
)

parameters_as_irace <- function(parameters) {
  params <- capture.output(
    write.table(
      parameters,
      row.names = FALSE,
      col.names = FALSE,
      sep = "\t",
      quote = F
    )
  )
  params <- paste0(params, collapse = "\n")

  irace::readParameters(text = params)
}

write.list <- function(x, file, export) {
  x[sapply(x, is.null)] <- NULL
  x[sapply(x, is.na)] <- NULL
  x[sapply(x, is.empty)] <- NULL
  names(x)[names(x) == "onlyTest"] <- ".onlytest"

  lines <- lapply(names(x), function(name) {
    paste(name, "=", if (is.logical(x[[name]])) as.integer(x[[name]]) else x[[name]])
  })

  data <- ""

  if (export) {
    data <- "# The value of these parameter must be set manually"
    data <- c(
      data,
      'parameterFile = ""',
      'trainInstancesFile = ""',
      'trainInstancesDir = ""',
      'scenarioFile = ""',
      'execDir = ""',
      'logFile = ""',
      'configurationsFile = ""',
      'forbiddenFile = ""',
      'testInstancesFile = ""',
      'testInstancesDir = ""'
    )
  }

  data <- c(data, "\n", lines)

  write(x = paste0(data), sep = "\n", file = file)
}

save_plot_as_base64 <- function(width = 550, height = 550) {
  filePlot <- tempfile(fileext = ".png")
  plot <- recordPlot()
  png(filePlot, width = width, height = height)
  plot.new()
  print(plot)
  dev.off()
  on.exit(file.remove(filePlot))
  return(knitr::image_uri(filePlot))
}

create_scenario_file <- function(path, pg, name = "scenario.txt", export = FALSE) {
  if (!is.null(name) && name != "") {
    path <- file.path(path, name)
  }

  if (export) {
    pg$clear_scenario_temp()
  }

  write.list(pg$get_irace_options(), file = path, export)
}

create_parameter_file <- function(path, pg, name = "parameters.txt") {
  if (!is.null(name) && name != "") {
    path <- file.path(path, name)
  }
  parameters <- data.table(pg$get_parameters())
  write.table(parameters, path, row.names = FALSE, col.names = FALSE, sep = "\t", quote = F)
}

create_instances_file <- function(path, pg, name = "instances.txt") {
  if (!is.null(name) && name != "") {
    path <- file.path(path, name)
  }

  write(paste(pg$get_train_instances(), collapse = "\n"), file = path)
}

create_target_runner_file <- function(path, pg, name = "target-runner") {
  if (!is.null(name) && name != "") {
    path <- file.path(path, name)

    if (.Platform$OS.type == "windows") {
      path <- paste0(path, ".bat")
    }
  }

  write(paste(pg$get_target_runner(), collapse = "\n"), file = path)

  if (.Platform$OS.type == "unix") {
    Sys.chmod(path, mode = "0771")
  }
}

create_target_evaluator_file <- function(path, pg, name = "target-evaluator") {
  if (!is.null(name) && name != "") {
    path <- file.path(path, name)

    if (.Platform$OS.type == "windows") {
      path <- paste0(path, ".bat")
    }
  }

  write(paste(pg$get_target_evaluator(), collapse = "\n"), file = path)

  if (.Platform$OS.type == "unix") {
    Sys.chmod(path, mode = "0771")
  }
}

create_initial_config_file <- function(path, pg, name = "configurations.txt") {
  if (!is.null(name) && name != "") {
    path <- file.path(path, name)
  }

  if (nrow(pg$get_configurations()) > 0) {
    configurations <- data.table(pg$get_configurations(), stringsAsFactors = FALSE)
    write.table(configurations, path, row.names = FALSE, col.names = TRUE, sep = "\t")
  } else {
    cat("", file = path)
  }
}

create_forbidden_file <- function(path, pg, name = "forbidden.txt") {
  if (!is.null(name) && name != "") {
    path <- file.path(path, name)
  }
  write(paste(pg$get_forbidden(), collapse = "\n"), file = path)
}

create_test_instances_file <- function(path, pg, name = "test-instances.txt") {
  if (!is.null(name) && name != "") {
    path <- file.path(path, name)
  }
  write(paste(pg$get_test_instances(), collapse = "\n"), file = path)
}

extract_parameters <- function(parameters) {
  log_info("Extracting paremeters from irace format to data frame")
  types <- c()
  switches <- c()
  domain <- c()
  conditions <- c()
  for (name in parameters$names) {
    types <- c(types, parameters$type[[name]])
    switches <- c(switches, paste0("\"", parameters$switches[[name]], "\""))
    domain <- c(domain, paste0("(", paste(parameters$domain[[name]], collapse = ", "), ")"))
    if (is.expression(parameters$conditions[[name]])) {
      conditions <- c(conditions, paste("|", parameters$conditions[[name]], collapse = ", "))
    } else {
      conditions <- c(conditions, "")
    }
  }

  df <- data.frame(list(names = parameters$names, switches = switches, types = types, domain = domain, conditions = conditions), stringsAsFactors = FALSE) # nolint

  return(df)
}

convert_vector_to_string <- function(vector) {
  new_vector <- c()
  for (i in 1:length(vector)) {
    new_vector[i] <- paste0(vector[i])
  }
  return(new_vector)
}

check_path <- function(path) {
  return(!is.null(path) && fs::is_absolute_path(path) && file.exists(path))
}

import_scenario <- function(name, path, scenario, events, only_options = FALSE) {
  log_info("Load scenario {path}")
  .scenario <- if (grepl(".Rdata", name, fixed = TRUE)) {
    load(path)
    scenario$add_parameter(extract_parameters(iraceResults$parameters))
    if (nrow(iraceResults$allConfigurations) != 0) {
      exe <- Execution$new(name = "execution-1")
      exe$set_irace_results(iraceResults)
      scenario$add_execution(exe)
      update_reactive_counter(events$update_executions)
    }
    aux <- iraceResults$scenario
    rm(iraceResults)
    aux
  } else {
    tryCatch(
      {
        irace::readScenario(filename = path)
      },
      error = function(err) {
        log_error("{err}")
        return(NULL)
      }
    )
  }

  if (!only_options) {
    if (is.null(.scenario)) {
      log_error("Can't load scenario file")
      return(FALSE)
    }

    path <- scenario$targetRunner
    if (check_path(path)) {
      log_info("Add target runner from {path}")
      scenario$set_target_runner(paste(readLines(path), collapse = "\n"))
    }

    path <- scenario$targetEvaluator
    if (check_path(path)) {
      log_info("Add target evaluator from {path}")
      scenario$set_target_evaluator(paste(readLines(path), collapse = "\n"))
    }

    path <- scenario$parameterFile
    parameters <- NULL
    if (check_path(path) && nrow(scenario$get_parameters()) == 0) {
      log_info("Add parameters from {path}")
      parameters <- tryCatch(
        {
          data <- irace::readParameters(file = path)
          scenario$add_parameter(extract_parameters(data))
          data
        },
        error = function(err) {
          log_error("{err}")
        }
      )
    }

    path <- scenario$forbiddenFile
    if (check_path(path)) {
      log_info("Add forbidden file from {path}")
      tryCatch(
        {
          irace:::readForbiddenFile(path)
          source <- readLines(path)
          scenario$add_forbidden(source)
        },
        error = function(err) {
          log_error("{err}")
        }
      )
    }

    path <- scenario$configurationsFile
    if (check_path(path)) {
      log_info("Add initial configurations {path}")

      tryCatch(
        {
          if (is.null(parameters)) {
            parameters <- parameters_as_irace(scenario$get_parameters())
          }
          config <- irace::readConfigurationsFile(filename = path, parameters = parameters)
          scenario$add_configuration(config)
        },
        error = function(err) {
          log_error("{err}")
        }
      )
    }
  }

  for (opt in names(.scenario)) {
    if (!available_option(opt)) {
      next
    }

    if (is.function(.scenario[[opt]])) {
      next
    }

    value <- as.character(.scenario[[opt]])

    value <- if (!grepl("\\D", value)) {
      as.numeric(value)
    } else {
      .scenario[[opt]]
    }

    log_info("{opt}: {as.character(.scenario[[opt]])}")
    scenario$add_irace_option(opt, value)
  }

  scenario$clear_scenario_temp()

  return(TRUE)
}

available_option <- function(option) {
  if (option %in% scenario_options$id) {
    return(TRUE)
  }
  return(FALSE)
}
