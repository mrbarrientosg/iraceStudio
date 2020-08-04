changeScenarioMiddleware <- function(state, action, store, nextM) {
  
  if (action$type == actionTypes$CHANGE_SCENARIO) {
    state$playground$change_current_scenario(action$id)
    store$dispatch(scenarioActions$updateScenario(state$playground$get_current()))
  } else if (action$type == "@@INIT") {
    return(store$dispatch(scenarioActions$updateScenario(state$playground$get_current())))
  }
  
  return (nextM(state, action, store))
}