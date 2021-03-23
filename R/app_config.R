#' Access files in the current app
#'
#' @param ... Character vector specifying directory and or file to
#'     point to inside the current package.
#'
#' @export
app_sys <- function(...) {
  system.file(..., package = packageName())
}


#' @export
irace_studio_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  plotly_resize <- "
  shinyjs.resizePlotly = function(params) {
    Plotly.Plots.resize(params[0]);
  }
  "
  tags$head(
    htmltools::htmlDependency(
      name = "summernote",
      version = "1.0.0",
      src = app_sys("app/www"),
      script = c("summernote-bs4.js", "summernote_binding.js"),
      stylesheet = "summernote-bs4.css",
      all_files = FALSE
    ),
    tags$link(rel = "stylesheet", href = file.path("www", "app-styles.css")),
    shinyalert::useShinyalert(),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = plotly_resize, functions = c("resizePlotly"))
  )
}

#' @export
irace_vizz_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  plotly_resize <- "
  shinyjs.resizePlotly = function(params) {
    Plotly.Plots.resize(params[0]);
  }
  "
  tags$head(
    tags$link(rel = "stylesheet", href = file.path("www", "app-styles.css")),
    shinyalert::useShinyalert(),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = plotly_resize, functions = c("resizePlotly"))
  )
}
