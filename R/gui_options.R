GUIOptions <- R6::R6Class(
  classname = "GUIOptions",
  public = list(
    iracePath = .libPaths()[1],
    workspacePath = character(0),
  
    initialize = function (path) {
      workspacePath <- file.path(path, "workspace")
    }
  )
)