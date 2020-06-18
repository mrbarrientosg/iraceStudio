report <- R6::R6Class(
  classname = "report",
  private = list(
    data = NULL,
    count = 0,
    update = NULL
  ),
  public = list(
    initialize = function(report = NULL) {
      private$data <- list()
      private$update <- 0
      
      if (!is.null(report)) {
        private$data <- report$data
        private$count <- report$count
      }
    },
    
    add_title = function(title) {
      private$count <- private$count + 1
      private$data[[as.character(private$count)]]$title <- title
      playground_emitter$emit(playground_events$update_report)
    },
    
    set_content = function(id, content) {
      private$data[[as.character(id)]]$content <- content
    },
    
    set_title = function(id, title) {
      private$data[[as.character(id)]]$title <- title
    },
    
    remove_data = function(id) {
      private$data[[as.character(id)]] <- NULL
      private$count <- private$count - 1
      playground_emitter$emit(playground_events$update_report)
    },
    
    get_id = function(section) {
      idx <- sapply(private$data, function(data) any(data$title %in% section))
      key <- names(private$data)[idx]
      return(key)
    },
    
    get_data = function() private$data,
    get_count = function() private$count,

    as_list = function() {
      return(list(data = private$data, count = private$count))
    }
  )
)