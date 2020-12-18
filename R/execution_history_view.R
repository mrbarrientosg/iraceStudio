ExecutionsHistoryView <- R6::R6Class(
  classname = "ExecutionsHistoryView",
  inherit = View,
  public = list(
    initialize = function(id) {
      super$initialize(id)
    },

    ui = function() {
      ns <- NS(self$id)

      tagList(
        fluidRow(
          column(
            width = 12,
            h2("History"),
            p("View the output of previous irace executions.
              Use the execution selector (right) to select the execution to display.")
          )
        ),
        fluidRow(
          box(
            title = strong("Output"),
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