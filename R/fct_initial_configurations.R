export_initial_configurations <- function(file, store) {
  log_debug("Exporting configurations file to {file$datapath}")
  create_initial_config_file(path = file$datapath, pg = store$pg, name = NULL)
  log_debug("Configurations file exported successfully")
  shinyalert(
    title = "Exported",
    text = "The file is exported successfully.",
    type = "success"
  )
}

import_initial_configurations <- function(input, store) {
  if (nrow(store$pg$get_parameters()) == 0) {
    alert.error(
      message = "There are no parameters. First add a parameter in the parameter section."
    )
    return(invisible())
  }

  parameters <- data.table(store$pg$get_parameters())
  parameters <- capture.output(
    write.table(
      parameters,
      row.names = FALSE,
      col.names = FALSE,
      sep = "\t",
      quote = F
    )
  )
  parameters <- paste0(parameters, collapse = "\n")

  parameters <- tryCatch(readParameters(text = parameters),
    error = function(err) {
      log_error("{err}")
      alert.error(as.character(err))
      return(NULL)
    }
  )

  file <- parseFilePaths(roots = getVolumes()(), input$load)

  if (!is.null(data)) {
    config <- tryCatch(irace::readConfigurationsFile(
      filename = file$datapath,
      parameters = parameters
    ),
      error = function(err) {
        log_error("{err}")
        alert.error(as.character(err))
        return(NULL)
      }
    )

    if (!is.null(config))
      store$pg$add_configuration(config)
  }
}

create_initial_modal_content <- function(ns, configuration, store) {
  inputs <- list()

  for (row in seq_len(nrow(store$pg$get_parameters()))) {
    param <- store$pg$get_parameter(row)
    inputs[[row]] <- create_initial_modal_input(param, ns, configuration)
  }

  return(tagList(inputs))
}

create_initial_modal_input <- function(param, ns, configuration) {
  result <- gsub("[\\s+\\)\\(]", "", as.character(param$domain), perl = TRUE)
  values <- strsplit(result, ",", perl = TRUE)
  default <- if (is.null(configuration)) {
    values[[1]][1]
  } else {
    configuration[[as.character(param$names)]]
  }

  input <- NULL
  label <- ""

  if (param$type == "c" || param$type == "o") {
    input <- pickerInput(
      inputId = ns(param$names),
      label = param$names,
      choices = values[[1]],
      selected = default,
      options = list(
        size = 8
      )
    )
  } else if (param$type == "i") {
    input <- sliderInput(
      inputId = ns(param$names),
      label = param$names,
      min = as.integer(values[[1]][1]),
      max = as.integer(values[[1]][2]), step = 1, value = default
    )
    label <- sprintf(
      fmt = "%s: [%d, %d]",
      param$type,
      as.integer(values[[1]][1]),
      as.integer(values[[1]][2])
    )
  } else if (param$type == "r") {
    input <- numericInput(
      inputId = ns(param$names),
      label = param$names,
      min = as.integer(values[[1]][1]),
      max = as.integer(values[[1]][2]), value = default
    )
    label <- sprintf(
      fmt = "%s: [%d, %d]",
      param$type,
      as.integer(values[[1]][1]),
      as.integer(values[[1]][2])
    )
  } else {
    input <- numericInput(
      inputId = ns(param$names),
      label = param$names,
      min = as.integer(values[[1]][1]),
      max = as.integer(values[[1]][2]), value = default
    )
    label <- sprintf(
      fmt = "%s: [%d, %d]",
      param$type,
      as.integer(values[[1]][1]),
      as.integer(values[[1]][2])
    )
  }

  fluidRow(
    column(
      width = 8,
      input
    ),
    column(
      width = 3,
      class = "d-flex align-items-center",
      style = "margin-top: 12px",
      HTML(paste(strong(label)))
    )
  )
}