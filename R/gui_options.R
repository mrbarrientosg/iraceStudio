#' @export
GUIOptions <- R6::R6Class( # nolint
  classname = "GUIOptions",
  public = list(
    workspace_path = file.path(fs::path_home(), "workspace-irace"),
    options_path = file.path(fs::path_home(), ".irace_studio"),

    initialize = function() {
      if (!dir.exists(self$options_path)) {
        create_hidden_directory(self$options_path)
      }

      file <- file.path(self$options_path, "settings.yaml")
      if (file.exists(file)) {
        settings <- yaml::read_yaml(file)
        self$workspace_path <- settings$workspace_path
      }
    },

    save = function() {
      if (is.null(self$options_path)) {
        self$options_path <- file.path(fs::path_home(), ".irace_studio")
      }

      if (!dir.exists(self$options_path)) {
        create_hidden_directory(self$options_path)
      }

      data <- list()
      data$workspace_path <- self$workspace_path

      file <- file.path(self$options_path, "settings.yaml")

      yaml::write_yaml(data, file = file)
    },

    createWorkspaceDirectory = function(path) {
      if (missing(path)) {
        path <- self$workspace_path
      }

      if (is.null(path) || path == "") {
        path <- file.path(fs::path_home(), "workspace-irace")
      }

      if (!dir.exists(path)) {
        dir.create(path)
        file.create(file.path(path, "irace_studio.irs"))
        return(TRUE)
      }

      files <- list.files(path = path, pattern = "irace_studio.irs", all.files = TRUE)

      if (length(files) == 0) {
        return(FALSE)
      }

      return(TRUE)
    }
  )
)
