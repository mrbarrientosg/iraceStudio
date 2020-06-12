CopyInput <- R6::R6Class(
  classname = "CopyInput",
  inherit = Component,
  public = list(
    ui = function(inputId, label, choices = c()) {
      ns <- NS(inputId)
      
      fluidRow(
        column(
          width = 10,
          pickerInput(
            inputId = ns("select_input"),
            label = label,
            choices = choices,
            width = "100%"
          )
        ),
        column(
          width = 2,
          actionButton(
            inputId = ns("action"),
            label = "Copy",
            style = "margin-top:32px; text-align:center;",
            width = "100%",
            class = "btn-primary"
          )
        )
      )
    },
    
    server = function(input, output, session, store) {
      values <- reactiveValues()
      observe(values$section <- input$select_input)
      observe(values$action <- input$action)
      
      observe({
        if (is.null(store$currentExecution)) {
          updatePickerInput(
            session = session,
            inputId = "select_input",
            choices = c(""),
            selected = NULL
          )
        } else {
          report <- store$currentExecution$get_report()
          
          update <- report$get_update()
          update()
          
          data <- report$get_data()
          
          if (length(data) == 0) {
            data <- c("")
          } else {
            data <- unlist(lapply(data, function(d) d$title), use.names = FALSE)
          }
          
          updatePickerInput(
            session = session,
            inputId = "select_input",
            choices = data,
            selected = NULL
          )
        }
      })
      
      return(values)
    }
  )
)