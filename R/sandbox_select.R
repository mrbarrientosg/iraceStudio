SandboxSelect <- R6::R6Class(
  classname = "SandboxSelect",
  inherit = Component,
  public = list(
    ui = function(inputId, ...) {
      ns <- NS(inputId)

      pickerInput(
        inputId = ns("options"),
        label = "Sandbox",
        choices = "",
        ...
      )
    },

    server = function(input, output, session, store) {
      values <- reactiveValues()

      observe({
        playground_emitter$value(playground_events$current_scenario)
        playground_emitter$value(playground_events$update_executions)
        playground_emitter$value(playground_events$update_sandboxes)

        if (is.null(store$currentExecution)) {
          return(invisible())
        }

        sandboxes <- store$currentExecution$get_sandboxes()

        boxes <- lapply(sandboxes$get_boxes(), function(box) box$getName())
        boxes_id <- lapply(sandboxes$get_boxes(), function(box) box$getId())

        if (length(boxes) == 0) {
          store$sandbox <- NULL
          boxes_id <- ""
        } else {
          names(boxes_id) <- unlist(boxes, use.names = FALSE)
          sandbox <- sandboxes$get_box(boxes_id[[1]])
          store$sandbox <- sandbox
        }

        updatePickerInput(
          session = session,
          inputId = "options",
          choices = boxes_id
        )
      })

      observe(values$option <- input$options)

      return(values)
    }
  )
)