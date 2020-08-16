GUIOptions <- R6::R6Class(
  classname = "GUIOptions",
  public = list(
    iracePath = .libPaths()[1],
    workspacePath = file.path(fs::path_home(), "workspace")
  )
)