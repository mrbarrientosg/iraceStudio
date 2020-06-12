TargetScriptsView <- R6::R6Class(
  classname = "TargetScriptsView",
  inherit = View,
  public = list(
    targetRunner = NULL,
    targetEvaluator = NULL,
    
    initialize = function(id) {
      super$initialize(id)
      self$targetRunner <- TargetRunnerTab$new()
      self$targetEvaluator <- TargetEvaluatorTab$new()
    },
    
    ui = function() {
      ns <- NS(self$id)
      
      tagList(
        div(class = "sub-header", h2("Target Scripts")),
        fluidRow(
          bs4TabCard(
            id = ns("scripts"),
            title = "",
            collapsible = FALSE,
            closable = FALSE,
            side = "left",
            width = 12,
            bs4TabPanel(
              tabName = "Target Runner",
              self$targetRunner$ui(inputId = ns("runner"))
            ),
            bs4TabPanel(
              tabName = "Target Evaluator",
              self$targetEvaluator$ui(inputId = ns("evaluator"))
            )
          )
        )
      )
    },
    
    server = function(input, output, session, store) {
      self$targetRunner$call(id = "runner", store = store)
      self$targetEvaluator$call(id = "evaluator", store = store)
    }
  )
)