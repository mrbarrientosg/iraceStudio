plan(sequential)

pkg <- new.env(parent = emptyenv())
pkg$reportStore <- list()

scenarioOptions <- jsonlite::fromJSON(
  system.file("app/www/scenarioOptions.json", package = packageName()),
  simplifyDataFrame = TRUE,
  flatten = TRUE
)

#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function (...) {
  app <- App$new()

  with_golem_options(
    app = shinyApp(
      ui = app$ui,
      server = app$server,
      onStart = function() {
        app$setup()
    
        onStop(function() {
          app$destroy()
        })
      },
      options = list(
        port = 4350
      )
    ),
    golem_opts = list(...)
  )
}
