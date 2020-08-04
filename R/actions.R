actionTypes <- list(
  UPDATE_SCENARIO = "UPDATE_SCENARIO",
  CHANGE_SCENARIO = "CHANGE_SCENARIO"
)

scenarioActions <- list(
  updateScenario = function(scenario) return(action(type = actionTypes$UPDATE_SCENARIO, scenario = scenario)),
  changeScenario = function(id) return(action(type = actionTypes$CHANGE_SCENARIO, id = id))
)