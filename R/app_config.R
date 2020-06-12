#' Access files in the current app
#'
#' @param ... Character vector specifying directory and or file to
#'     point to inside the current package.
#'
#' @noRd
app_sys <- function(...){
  system.file(..., package = packageName())
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config R_CONFIG_ACTIVE value.
#' @param use_parent Logical, scan the parent directory for config file.
#'
#' @importFrom config get
#'
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv("R_CONFIG_ACTIVE", "default"),
  use_parent = TRUE
){
  config::get(
    value = value,
    config = config,
    # Modify this if your config file is somewhere else:
    file = app_sys("golem-config.yml"),
    use_parent = use_parent
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
add_external_resources <- function() {

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Irace Studio'
    ),
    shinyalert::useShinyalert(),
    shinyjs::useShinyjs(),
    tags$script(src = "www/summernote_binding.js"),
    tags$script(src = "www/summernote-bs4.js"),
    tags$link(href = "www/summernote-bs4.css", rel = "stylesheet", type = "text/css"),
    tags$link(href = "www/app-styles.css", rel = "stylesheet", type = "text/css")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

