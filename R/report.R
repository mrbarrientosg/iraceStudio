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
      private$update <- reactiveVal(value = 0)
      
      if (!is.null(report)) {
        private$data <- report$data
        private$count <- report$count
      }
    },
    
    add_title = function(title) {
      private$count <- private$count + 1
      private$update(isolate(private$update() + 1))
      private$data[[as.character(private$count)]]$title <- title
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
      private$update(isolate(private$update() + 1))
    },
    
    get_id = function(section) {
      idx <- sapply(private$data, function(data) any(data$title %in% section))
      key <- names(private$data)[idx]
      return(key)
    },
    
    get_data = function() private$data,
    get_count = function() private$count,
    get_update = function() private$update,
    
    as_list = function() {
      return(list(data = private$data, count = private$count))
    }
  )
)