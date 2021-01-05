#' Run Irace Studio
#'
#' @param port A port number that Irace Studio will listen on.
#'
#' @export
runIraceStudio <- function(port = 4350, ...) {
  options(golem.app.prod = TRUE)
  run_app(port, ...)
}

#' @export
runIraceVizz <- function(port = 4350, ...) {
  appDir <- appSys("iraceVizz")

  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `Irace Studio`.", call. = FALSE)
  }

  with_golem_options(
    app = shiny::runApp(appDir, display.mode = "normal", port = port, launch.browser = T),
    golem_opts = list(...)
  )
}

run_app <- function(port = 4350, ...) {
  appDir <- appSys("iraceStudio")

  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `Irace Studio`.", call. = FALSE)
  }

  with_golem_options(
    app = shiny::runApp(appDir, display.mode = "normal", port = port, launch.browser = T),
    golem_opts = list(...)
  )
}