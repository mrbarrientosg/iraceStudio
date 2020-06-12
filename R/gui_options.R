GUIOptions <- R6::R6Class(
  classname = "GUIOptions",
  public = list(
    iracePath = character(0),
    workspacePath = file.path(getwd(), "workspace")
  )
)