#' @export
clear_button <- function(input_id, label = "Clear", ...) {
  ns <- shiny::NS(input_id)

  bs4Dash::actionButton(inputId = ns("action"), label = label, icon = icon("trash"), status = "danger", ...) # nolint
}

#' @export
clear_button_sv <- function(input, output, session, message) {
  values <- shiny::reactiveValues(action = NULL)

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
