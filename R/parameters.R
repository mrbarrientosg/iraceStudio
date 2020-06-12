parameters <- R6::R6Class(
  classname = "parameters",
  cloneable = FALSE,
  private = list(
    data = NULL,
    
    check_parameter_repeat = function(parameter) {
      if (nrow(private$data) > 0) {
        condition <- subset(
          private$data,
          names == parameter$names ||
            switches == parameter$switches,
          select = c("names", "switches")
        )
        
        if (nrow(condition) > 0) {
          return(TRUE)
        }
      }
      
      return(FALSE)
    },
    
    check_parameter_value_repeat = function(parameter) {
      result <- dplyr::semi_join(private$data, parameter)
      
      if (nrow(result) > 1) {
        return(TRUE)
      }
      
      return(FALSE)
    }
  ),
  
  public = list(
    initialize = function(parameters = NULL) {
      if (is.null(parameters))
        private$data <- data.frame(
          names = character(0),
          switches = character(0),
          types = character(0),
          domain = character(0),
          conditions = character(0),
          stringsAsFactors = FALSE
        )
      else
        private$data <- parameters
    },
    
    add_parameter = function(new_parameter) {
      if (private$check_parameter_repeat(new_parameter)) {
        stop("The parameter name or flag is repeated")
        return(FALSE)
      }
      
      private$data <- rbind(private$data, new_parameter)
      return(TRUE)
    },
    
    update_parameter = function(row, new_parameter) {
      if (private$check_parameter_value_repeat(new_parameter)) {
        return(NULL)
      }
      
      old <- private$data[row, ]
      private$data[row, ] <- new_parameter[1, ]
      return(old)
    },
    
    remove_parameter = function(row) {
      value <- private$data[row, ]
      private$data <- private$data[-row, ]
      return(value)
    },
    
    get_parameters = function() private$data,
    get_parameter = function(row) private$data[row, ],
    clear_parameters = function() {
      private$data <- data.frame(
        names = character(0),
        switches = character(0),
        types = character(0),
        domain = character(0),
        conditions = character(0),
        stringsAsFactors = FALSE
      )
    }
  )
)