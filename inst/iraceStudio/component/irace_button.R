IraceButton <- R6::R6Class( # nolint
  classname = "IraceButton",
  inherit = Component,
  public = list(
    state = FALSE,

    ui = function(input_id, label = " Start Irace", ...) {
      ns <- NS(input_id)

      bs4Dash::actionButton(
        inputId = ns("action"),
        label = label,
        icon = icon("play"),
        status = "primary",
        size = "lg",
        ...
      )
    },

    server = function(input, output, session, store, events) {
      values <- reactiveValues()

      observeEvent(input$action,
        {
          if (events$is_irace_running) {
            shinyalert(
              title = "Stop Irace",
              text = "Are you sure stop irace?.",
              type = "warning",
              showConfirmButton = TRUE,
              showCancelButton = TRUE,
              closeOnEsc = FALSE,
              confirmButtonText = "Yes",
              callbackR = function(event) {
                if (event) {
                  log_info("Stop irace")
                  values$action <- input$action
                }
              }
            )
            return(invisible())
          }

          values$action <- input$action
        },
        ignoreInit = TRUE
      )

      observeEvent(events$is_irace_running, self$changeState(session), ignoreInit = TRUE)

      return(values)
    },

    changeState = function(session) {
      if (self$state) {
        addClass(id = "action", class = "btn-primary")
        removeClass(id = "action", class = "btn-danger")
        updateActionButton(
          session = session,
          inputId = "action",
          label = " Start Irace",
          icon = icon("play")
        )
      } else {
        addClass(id = "action", class = "btn-danger")
        removeClass(id = "action", class = "btn-primary")
        updateActionButton(
          session = session,
          inputId = "action",
          label = " Stop Irace",
          icon = icon("stop")
        )
      }
      self$state <- !self$state
    }
  )
)
