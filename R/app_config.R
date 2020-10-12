#' Access files in the current app
#'
#' @param ... Character vector specifying directory and or file to
#'     point to inside the current package.
#'
#' @noRd
appSys <- function(...) {
  system.file(..., package = packageName())
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
addExternalResources <- function() {
  add_resource_path(
    "www", appSys("app/www")
  )

  plotlyResize <- "
  shinyjs.resizePlotly = function(params) {
    Plotly.Plots.resize(params[0]);
  }
  "

  tags$head(
    bundle_resources(
      path = appSys("app/www"),
      app_title = "Irace Studio",
      name = "irace_studio",
      version = "1.0.6.9000"
    ),
    shinyalert::useShinyalert(),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = plotlyResize, functions = c("resizePlotly"))
  )
}