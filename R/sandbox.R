Sandbox <- R6::R6Class(
  classname = "Sandbox",
  cloneable = FALSE,
  private = list(
    name = "",
    elites = FALSE,
    iterations = NULL,
    filters = NULL,
    ids = NULL,
    descentId = "",
    trajectoryId = "",
    configurations = NULL
  ),

  public = list(
    initialize = function(name = "", sandbox = NULL) {
      private$name <- name
      private$filters <- data.frame()
      private$configurations <- data.frame()

      if (!is.null(sandbox)) {
        private$name <- sandbox$name
        private$elites <- sandbox$elites
        private$iterations <- sandbox$iterations
        private$filters <- sandbox$filters
        private$ids <- sandbox$ids
        private$descentId <- sandbox$descentId
        private$trajectoryId <- sandbox$trajectoryId
        private$configurations <- sandbox$configurations
      }
    },

    addFilter = function(filter) {
      private$filters <- rbind(private$filters, filter)
    },

    removeFilter = function(row) {
      private$filters <- private$filters[-row, ,drop = FALSE]
    },

    removeConfiguration = function(row) {
      private$configurations <- private$configurations[-row, ]
    },

    setElites = function(elites) private$elites <- elites,
    setIterations = function(iterations) private$iterations <- iterations,
    setIds = function(ids) private$ids <- ids,
    setDescentId = function(descentId) private$descentId <- descentId,
    setTrajectoryId = function(trajectoryId) private$trajectoryId <- trajectoryId,
    setConfigurations = function(configurations) private$configurations <- configurations,

    # GETTER
    getElites = function() return(private$elites),
    getIterations = function() return(private$iterations),
    getFilters = function() return(private$filters),
    getIds = function() return(private$ids),
    getDescentId = function() return(private$descentId),
    getTrajectoryId = function() return(private$trajectoryId),
    getConfigurations = function() return(private$configurations),

    asList = function() {
      data <- list()
      data$name <- private$name
      data$elites <- private$elites
      data$iterations <- private$iterations
      data$filters <- private$filters
      data$ids <- private$ids
      data$descentId <- private$descentId
      data$trajectoryId <- private$trajectoryId
      data$configurations <- private$configurations
      return(data)
    }
  )
)