ExecutionsHistoryView <- R6::R6Class(
  classname = "ExecutionsHistoryView",
  inherit = View,
  public = list(
    executionSelect = NULL,

    initialize = function(id) {
      super$initialize(id)
      self$executionSelect <- ExecutionSelect$new()
    },

    ui = function() {
      ns <- NS(self$id)

      tagList(
        fluidRow(
          column(
            width = 8,
            h2("History"),
            p("View the output of previous irace executions. 
              Use the execution selector (right) to select the execution to display.")
          ),
          column(
            width = 4,
            class = "d-flex align-items-center justify-content-end",
            self$executionSelect$ui(inputId = ns("executions"))
          )
        ),
        fluidRow(
          bs4Card(
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            verbatimTextOutput(outputId = ns("irace_output"))
          )
        )
      )
    },

    server = function(input, output, session, store) {
      executions <- self$executionSelect$call(id = "executions", store = store)

      output$irace_output <- renderText({
        shiny::validate(
          need(executions$option != "", message = "")
        )

        store$pg$get_execution(executions$option)$get_output_log()
      })
    }
  )
)