appReducer <- function(state, action) {
  return(AppState$new(
    playground = checkNull(state)$playground,
    scenarioState = scenarioReducer(checkNull(state)$scenarioState, action)
  ))
}

scenarioReducer <- function(state, action) {
  if (is.null(state))
    return(ScenarioState$new())
  
  newState <- state
  
  if (action$type == actionTypes$UPDATE_SCENARIO) {
    newState <- newState$clone(TRUE)
    newState$scenario <- action$scenario
  }
  
  return(newState)
}
