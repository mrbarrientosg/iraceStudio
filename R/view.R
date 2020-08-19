View <- R6::R6Class(
  classname = "View",
  public = list(
    id = NULL,

    initialize = function(id) {
      self$id <- id
    },

    ui = function() {
      stop("Method not implemented")
    },

    server = function(input, output, session, store) {
      stop("Method not implemented")
    },

    call = function(input, output, session, store, ...) {
      shiny::callModule(module = self$server, id = self$id, store = store, ...)
    }
  )
)