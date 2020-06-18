SandBoxes <- R6::R6Class(
  classname = "SandBoxes",
  private = list(
    boxes = NULL,
    count = 0
  ),
  public = list(
    initialize = function(data = NULL) {
      private$boxes <- list()
      private$count <- 0
      if (!is.null(data)) {
        for (name in names(data$sandboxes)) {
          private$boxes[[name]] <- Sandbox$new(sandbox = data$sandboxes[[name]])
        }
        private$count <- data$count
      } else {
        self$add_sandbox(Sandbox$new(name = "sandbox-1"))
      }
    },

    add_sandbox = function(sandbox) {
      private$count <- private$count + 1
      sandbox$setId(private$count)
      private$boxes[[as.character(private$count)]] <- sandbox
      playground_emitter$emit(playground_events$update_sandboxes)
    },

    remove_sandbox = function(id) {
      private$boxes[[id]] <- NULL
      private$count <- private$count - 1
      playground_emitter$emit(playground_events$update_sandboxes)
    },

    get_boxes = function() return(private$boxes),
    get_box = function (id) return(private$boxes[[id]]),

    as_list = function() {
      data <- list()

      data$sandboxes <- list()
      for (name in names(private$boxes)) {
        data$sandboxes[[name]] <- private$boxes[[name]]$asList()
      }

      data$count <- private$count

      return(data)
    }
  )
)