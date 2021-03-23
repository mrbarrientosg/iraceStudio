#' @export
SandboxSelect <- R6::R6Class( # nolint
  classname = "SandboxSelect",
  inherit = Component,
  public = list(
    ui = function(input_id, ...) {
      ns <- NS(input_id)

      pickerInput(
        inputId = ns("options"),
        label = "Sandbox",
        choices = "",
        options = list(
          size = 8
        ),
        ...
      )
    },

    server = function(input, output, session, store, events) {
      values <- reactiveValues()

      observeEvent(c(
        events$change_scenario,
        events$update_executions,
        events$update_sandboxes,
        store$current_execution
      ),
      {
        boxes_id <- NULL

        if (!is.null(store$current_execution)) {
          sandboxes <- store$current_execution$get_sandboxes()

          boxes <- lapply(sandboxes$get_boxes(), function(box) box$get_name())
          boxes_id <- lapply(sandboxes$get_boxes(), function(box) box$get_id())

          if (length(boxes) == 0) {
            store$sandbox <- NULL
            boxes_id <- ""
          } else {
            names(boxes_id) <- unlist(boxes, use.names = FALSE)
            sandbox <- sandboxes$get_box(boxes_id[[1]])
            store$sandbox <- sandbox
          }
        } else {
          store$sandbox <- NULL
          boxes_id <- ""
        }

        updatePickerInput(
          session = session,
          inputId = "options",
          choices = boxes_id
        )
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
      )

      observeEvent(input$options, values$option <- input$options)

      observeEvent(store$current_execution,
        {
          req(input$options != "")
          sandboxes <- store$current_execution$get_sandboxes()
          store$sandbox <- sandboxes$get_box(input$options)
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      observeEvent(store$sandbox,
        {
          updatePickerInput(
            session = session,
            inputId = "options",
            selected = store$sandbox$get_id()
          )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      return(values)
    }
  )
)