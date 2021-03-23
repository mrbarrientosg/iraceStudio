ReportView <- R6::R6Class( # nolint
  classname = "ReportView",
  inherit = View,
  public = list(
    summary_card = NULL,
    best_configuration_card = NULL,
    candidates_card = NULL,
    performance_card = NULL,
    detail_by_iteration_card = NULL,

    initialize = function(id) {
      super$initialize(id)
      self$summary_card <- SummaryCard$new()
      self$best_configuration_card <- BestConfigurationCard$new()
      self$candidates_card <- CandidatesCard$new()
      self$performance_card <- PerformanceCard$new()
      self$detail_by_iteration_card <- DetailByIterationCard$new()
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
          self$summary_card$ui(input_id = ns("summary")),
          self$best_configuration_card$ui(input_id = ns("best_config")),
          self$candidates_card$ui(input_id = ns("candidates")),
          self$performance_card$ui(input_id = ns("performance")),
          self$detail_by_iteration_card$ui(input_id = ns("detail_by_iteration"))
        )
      )
    },

    server = function(input, output, session, store, events) {
      self$summary_card$call(id = "summary", store = store, events = events)
      self$best_configuration_card$call(id = "best_config", store = store, events = events)
      self$candidates_card$call(id = "candidates", store = store, events = events)
      self$performance_card$call(id = "performance", store = store, events = events)
      self$detail_by_iteration_card$call(id = "detail_by_iteration", store = store, events = events)
    }
  )
)
