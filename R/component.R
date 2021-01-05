#' @export
Component <- R6::R6Class(
  classname = "Component",
  public = list(
    ui = function(id) {
      stop("Method not implemented")
    },

    server = function(input, output, session, store) {
      stop("Method not implemented")
    },

    call = function(input, output, session, id, ...) {
      shiny::callModule(module = self$server, id = id, ...)
    }
  )
)