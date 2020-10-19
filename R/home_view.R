HomeView <- R6::R6Class(
  classname = "HomeView",
  inherit = View,
  public = list(
    ui = function() {
      ns <- NS(self$id)

      tagList(
        div(class = "sub-header", h2("Home")),
        fluidRow(
          bs4Card(
            title = NULL,
            collapsible = FALSE,
            closable = FALSE,
            width = 12
            # TODO: Ver que poner aqui uwu
          )
        )
      )
    },

    server = function(input, output, session, store) {


    }
  )
)