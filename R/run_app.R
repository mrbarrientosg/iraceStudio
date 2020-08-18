plan("future::sequential")

pkg <- new.env(parent = emptyenv())
pkg$reportStore <- list()

scenarioOptions <- jsonlite::fromJSON(
  system.file("app/www/scenarioOptions.json", package = packageName()),
  simplifyDataFrame = TRUE,
  flatten = TRUE
)

#' Run Irace Studio
#'
#' @param port A port number that Irace Studio will listen on.
#'
#' @export
runIraceStudio <- function(port = 4350) {
  app <- App$new()

  shinyApp(
    ui = app$ui,
    server = app$server,
    onStart = function() {
      app$setup()
    },
    options = list(
      port = port,
      launch.browser = TRUE,
      minified = TRUE,
      deprecation.message = FALSE
    )
  )
}