AppState <- R6::R6Class(
  classname = "AppState",
  inherit = State,
  public = list(
    playground = NULL,
    scenarioState = NULL,
    
    initialize = function(playground, scenarioState) {
      super$initialize()
      self$playground <- playground
      self$scenarioState <- scenarioState
    }
  )
)

ScenarioState <- R6::R6Class(
  classname = "ScenarioState",
  inherit = State,
  public = list(
    scenario = NULL,
    
    initialize = function() {
      super$initialize()
      self$scenario <- scenario$new()
    }
  )
)