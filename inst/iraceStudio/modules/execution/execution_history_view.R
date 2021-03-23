ExecutionsHistoryView <- R6::R6Class( # nolint
  classname = "ExecutionsHistoryView",
  inherit = View,
  public = list(
    execution_select = NULL,

    initialize = function(id) {
      super$initialize(id)
      self$execution_select <- ExecutionSelect$new()
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

    server = function(input, output, session, store, events) {
      executions <- self$execution_select$call(id = "executions", store = store, events = events)

      output$irace_output <- renderText({
        shiny::validate(
          need(executions$option != "", message = "")
        )

        store$pg$get_execution(executions$option)$get_output_log()
      })
    }
  )
)
