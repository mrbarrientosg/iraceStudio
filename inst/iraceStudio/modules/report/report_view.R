ReportView <- R6::R6Class(
  classname = "ReportView",
  inherit = View,
  public = list(
    summaryCard = NULL,
    bestConfigurationCard = NULL,
    candidatesCard = NULL,
    performanceCard = NULL,
    detailByIterationCard = NULL,

    initialize = function(id) {
      super$initialize(id)
      self$summaryCard <- SummaryCard$new()
      self$bestConfigurationCard <- BestConfigurationCard$new()
      self$candidatesCard <- CandidatesCard$new()
      self$performanceCard <- PerformanceCard$new()
      self$detailByIterationCard <- DetailByIterationCard$new()
    },

    ui = function() {
      ns <- NS(self$id)

      tagList(
        fluidRow(
          class = "justify-content-between",
          style = "height: 90px;",
          column(
            width = 12,
            h2("Report"),
            p("(Development) View here the irace execution report")
          )
        ),
        fluidRow(
          self$summaryCard$ui(inputId = ns("summary")),
          self$bestConfigurationCard$ui(inputId = ns("best_config")),
          self$candidatesCard$ui(inputId = ns("candidates")),
          self$performanceCard$ui(inputId = ns("performance")),
          self$detailByIterationCard$ui(inputId = ns("detail_by_iteration"))
        )
      )
    },

    server = function(input, output, session, store) {
      self$summaryCard$call(id = "summary", store = store)
      self$bestConfigurationCard$call(id = "best_config", store = store)
      self$candidatesCard$call(id = "candidates", store = store)
      self$performanceCard$call(id = "performance", store = store)
      self$detailByIterationCard$call(id = "detail_by_iteration", store = store)
    }
  )
)
