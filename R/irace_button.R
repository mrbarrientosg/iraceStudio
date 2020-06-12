IraceButton <- R6::R6Class(
  classname = "IraceButton",
  inherit = Component,
  public = list(
    state = FALSE,
    
    ui = function(inputId, label = " Start Irace", ...) {
      ns <- NS(inputId)
      
      actionButton(
        inputId = ns("action"),
        label = label,
        icon = icon("play"),
        class = "btn-primary btn-lg",
        ...
      )
    },
    
    server = function(input, output, session, store) {
      values <- reactiveValues()
      
      observeEvent(input$action,
        {
        if (store$startIrace) {
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
      
      observeEvent(store$startIrace, self$changeState(session), ignoreInit = TRUE)
      
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