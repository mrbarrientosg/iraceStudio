State <- R6::R6Class(
  classname = "State",
  cloneable = FALSE,
  public = list(
    id = NULL,
    
    initialize = function() {
      self$id <- ids::random_id(bytes = 8)
    }
  ),
  private = list(
    deep_clone = function(name, value) {
      if (name == "id")
        ids::random_id(bytes = 8)
      else
        value
    }
  )
)

State$equal <- function(state1, state2) {
  if (is.null(state1) || is.null(state2))
    return(FALSE)
  
  return(state1$id == state2$id)
}

Store <- R6::R6Class(
  classname = "Store",
  private = list(
    initialState = NULL,
    reducer = NULL,
    subscriptions = NULL,
    middlewares = NULL,
    current = NULL,
    isDispatching = FALSE,
    
    applyMiddleware = function(state, action) {
      return(private$nextMidd(1)(state, action, self))
    },
    
    nextMidd = function(idx) {
      if (idx > length(private$middlewares) || length(private$middlewares) == 0) {
        return (function(state, action, store) return(action))
      }
      
      return (function(state, action, store) 
        return(private$middlewares[[idx]](state, action, store, private$nextMidd(idx + 1)))
      )
    }
  ),
  public = list(
    state = NULL,
    
    initialize = function(reducer, initialState = NULL, middlewares = list()) {
      if (!is.null(initialState) && (!R6::is.R6(initialState) || !checkClass(initialState, "State"))) 
        stop("State may be a R6 class")
      
      private$current <- reactiveValues(state = initialState$id)
      self$state <- initialState
      
      private$middlewares <- if (is.list(middlewares)) middlewares else list(middlewares)
      private$reducer <- reducer
      private$subscriptions <- list()
      
      self$dispatch(action("@@INIT"))
    },
    
    dispatch = function(action) {
      if (!is.list(action)) {
        stop("Actions must be list")
      }
      
      if (is.null(action$type)) {
        stop("Actions may not have an undefined 'type' property")
      }
      
      if (private$isDispatching) {
        stop("Reducers may not dispatch actions")
      }
      
      newAction <- private$applyMiddleware(self$state, action)
      
      private$isDispatching <- TRUE
      newState <- private$reducer(self$state, newAction)
      private$isDispatching <- FALSE
      
      if (State$equal(newState, self$state)) {
        return()
      }
      
      oldState <- self$state
      private$current$state <- newState$id
      self$state <- newState
      
      rm(oldState)
      
      return(action)
    },
    
    subscribeTo = function(select, subscription) {
      env <- new.env(hash = FALSE)
      env$currentState <- NULL
      
      handleChange <- function(state) {
        nextState <- select(state)
        
        if (!State$equal(nextState, env$currentState)) {
          env$currentState <- nextState
          subscription(env$currentState)
        }
      }
      
      self$subscribe(handleChange)
    },
    
    subscribe = function(subscription) {
      obs <- shiny::observeEvent(private$current$state, {
        subscription(self$state)
      }, ignoreInit = FALSE, ignoreNULL = TRUE)
      
      private$subscriptions <- append(private$subscriptions, obs)
    }
  )
)

action <- function(type, ...) {
  return (list(type = type, ...))
}

checkClass <- function(x, name) {
  return (!is.null(x) && R6::is.R6(x) && name %in% class(x))
}

checkNull <- function(x) {
  if (is.null(x)) 
    NULL 
  else 
    x
}