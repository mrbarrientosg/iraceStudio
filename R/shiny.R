#' @export
export_button <- function(input_id, filename, style = NULL, size = "default") {
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
    id = input_id,
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
import_button <- function(input_id, label = "Import", style = NULL, size = "default") {
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
    id = input_id,
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
directory_input <- function(id_button, id_input, label, title, button_text = "Browse", width = NULL) {
  input <- textInput(
    inputId = id_input,
    label = label,
    width = width
  ) %>% tagAppendChild(
    div(
      class = "input-group-append",
      shinyDirButton(
        id = id_button,
        label = button_text,
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

#' @export
update_reactive_counter <- function(counter) {
  return(counter + 1)
}
