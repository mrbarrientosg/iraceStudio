#' @noRd
actionButton <- function(inputId, label, icon = NULL, width = NULL, ...) {
  btn <- shiny::actionButton(inputId, label, icon, width, ...)
  opts <- list(...)

  if (!is.null(opts$class)) {
    btn$attribs$class <- paste("btn action-button", opts$class)
  }

  return(btn)
}

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

importButton <- function(inputId, style = NULL, size = "default") {
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
    label = "Import",
    title = "Select a file",
    multiple = FALSE,
    style = style,
    class = class
  )
  
  btn[[2]]$attribs$class <- gsub("btn-default", "", btn[[2]]$attribs$class)
  return(btn)
}

