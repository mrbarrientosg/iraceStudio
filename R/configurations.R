configurations <- R6::R6Class(
  classname ="configurations",
  cloneable = FALSE,
  private = list(
    data = NULL
  ),
  
  public = list(
    initialize = function(configurations = NULL, paramNames = NULL) {
      if (is.null(configurations)) {
        private$data <- data.frame(matrix(ncol = length(paramNames), nrow = 0))
        colnames(private$data) <- paramNames
      } else {
        private$data <- configurations
      }
    },
    
    add_configuration = function(configuration) {
      private$data <- rbind(private$data, configuration)
    },
    
    update_configuration = function(row, new_configuration) {
      private$data[row, ] <- new_configuration[1, ]
    },
    
    remove_configuration = function(row) {
      private$data <- private$data[-row, ]
    },
    
    addColumn = function(name) {
      l <- lapply(name, function(x) NA)
      names(l) <- name
      private$data <- tibble::add_column(private$data, as.data.frame(l))
    },
    
    updateColumn = function(oldName, newName) {
      private$data <- dplyr::rename(private$data, oldName = newName)
    },
    
    removeColumn = function(name) {
      private$data <- private$data[, !(names(private$data) %in% name), drop = F]
    },
    
    get_configurations = function() private$data,
    get_configuration = function(row) private$data[row, ],
    clear_configurations = function(removeAll = F) {
      if (removeAll) {
        private$data <- data.frame()
      } else {
        private$data <- private$data[0, , drop = F]
      }
    }
  )
)