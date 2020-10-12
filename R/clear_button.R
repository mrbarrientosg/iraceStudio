clear_button <- function(inputId, label = "Clear", ...) {
  ns <- NS(inputId)

  actionButton(inputId = ns("action"), label = label, icon = icon("trash"), class = "btn-danger", ...)
}

clear_button_sv <- function (input, output, session, message) {
  values <- reactiveValues(action = NULL)

  observeEvent(input$action, {
    shinyalert::shinyalert(
      title = "Clear",
      text = message,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      closeOnEsc = FALSE,
      confirmButtonText = "Yes",
      callbackR = function(event) {
        if (event) {
          values$action <- input$action
        }
      }
    )
  })

  return(values)
}