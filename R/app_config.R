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
#' @export
iraceStudioResources <- function() {
  add_resource_path(
    "www", appSys("app/www")
  )

  plotlyResize <- "
  shinyjs.resizePlotly = function(params) {
    Plotly.Plots.resize(params[0]);
  }
  "
  tags$head(
      htmltools::htmlDependency(
        name = "summernote",
        version = "1.0.0",
        src = appSys("app/www"),
        script = c("summernote-bs4.js", "summernote_binding.js"),
        stylesheet = "summernote-bs4.css",
        all_files = FALSE
      ),
      tags$link(rel = "stylesheet", href = file.path("www", "app-styles.css")),
      shinyalert::useShinyalert(),
      shinyjs::useShinyjs(),
      useGridstack(),
      shinyjs::extendShinyjs(text = plotlyResize, functions = c("resizePlotly"))
    )
}

#' @export
iraceVizzResources <- function() {
  add_resource_path(
    "www", appSys("app/www")
  )

  plotlyResize <- "
  shinyjs.resizePlotly = function(params) {
    Plotly.Plots.resize(params[0]);
  }
  "
  tags$head(
      tags$link(rel = "stylesheet", href = file.path("www", "app-styles.css")),
      shinyalert::useShinyalert(),
      shinyjs::useShinyjs(),
      useGridstack(),
      shinyjs::extendShinyjs(text = plotlyResize, functions = c("resizePlotly"))
    )
}