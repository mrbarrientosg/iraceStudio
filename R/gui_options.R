#' @export
GUIOptions <- R6::R6Class(
  classname = "GUIOptions",
  public = list(
    workspacePath = file.path(fs::path_home(), "workspace-irace"),
    optionsPath = file.path(fs::path_home(), ".irace_studio"),

    initialize = function () {
      if (!dir.exists(self$optionsPath))
        createHiddenDirectory(self$optionsPath)

      file <- file.path(self$optionsPath, "settings.yaml")
      if (file.exists(file)) {
        settings <- yaml::read_yaml(file)
        self$workspacePath <- settings$workspacePath
      }
    },

    save = function() {
      if (is.null(self$optionsPath))
        optionsPath <- file.path(fs::path_home(), ".irace_studio")

      if (!dir.exists(self$optionsPath))
        createHiddenDirectory(self$optionsPath)

      data <- list()
      data$workspacePath <- self$workspacePath

      file <- file.path(self$optionsPath, "settings.yaml")

      yaml::write_yaml(data, file = file)
    },

    createWorkspaceDirectory = function(path) {
      if (missing(path))
        path <- self$workspacePath

      if (is.null(path) || path == "") {
        path <- file.path(fs::path_home(), "workspace-irace")
      }

      if (!dir.exists(path)) {
        dir.create(path)
        file.create(file.path(path, ".irace_studio.irs"))
        return(TRUE)
      }

      files <- list.files(path = path, pattern = ".irace_studio.irs", all.files = TRUE)

      if (length(files) == 0)
        return(FALSE)

      return(TRUE)
    }
  )
)