#' @export
actionButton <- function(inputId, label, icon = NULL, width = NULL, ...) {
  btn <- shiny::actionButton(inputId, label, icon, width, ...)
  opts <- list(...)

  if (!is.null(opts$class)) {
    btn$attribs$class <- gsub("btn-default", "", btn$attribs$class)
  }

  return(btn)
}

#' @export
exportButton <- function(inputId, filename, style = NULL, size = "default") {
  size <- match.arg(
    arg = size,
    choices = c("large", "default", "small")
  )

  class <- "btn btn-primary"

  if (size != "default") {
    if (size == "large") {
      size <- "lg"
    } else {
      size <- "sm"
    }

    class <- paste(class, paste0("btn-", size))
  }

  btn <- shinySaveButton(
    id = inputId,
    label = "Export",
    title = "Select a folder",
    filename = filename,
    style = style,
    class = class
  )

  btn[[2]]$attribs$class <- gsub("btn-default", "", btn[[2]]$attribs$class)

  return(btn)
}

#' @export
importButton <- function(inputId, label = "Import", style = NULL, size = "default") {
  size <- match.arg(
    arg = size,
    choices = c("large", "default", "small")
  )

  class <- "btn-outline-primary"

  if (size != "default") {
    if (size == "large") {
      size <- "lg"
    } else {
      size <- "sm"
    }

    class <- paste(class, paste0("btn-", size))
  }

  btn <- shinyFilesButton(
    id = inputId,
    label = label,
    title = "Select a file",
    multiple = FALSE,
    style = style,
    class = class
  )

  btn[[2]]$attribs$class <- gsub("btn-default", "", btn[[2]]$attribs$class)
  return(btn)
}

#' @export
directoryInput <- function(idButton, idInput, label, title, buttonText = "Browse" , width = NULL) {
  input <- textInput(
    inputId = idInput,
    label = label,
    width = width
  ) %>% tagAppendChild(
    div(
      class = "input-group-append",
      shinyDirButton(
        id = idButton,
        label = buttonText,
        title = title,
        buttonType = "primary"
      )
    )
  )

  label <- input$children[[1]]
  input$children[[1]] <- NULL

  input$attribs$class <- gsub("form-group", "input-group", input$attribs$class)

  return(div(label, input))
}