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
            width = 8,
            h2("Report"),
            p("(Development) View here the irace execution report")
          ),
          column(
            width = 4,
            class = "d-flex align-items-center justify-content-end",
            bs4Dash::actionButton(inputId = ns("irace_vizz"), "Advance Vizz", status = "primary")
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

      observeEvent(input$irace_vizz, {
        file <- file.path(store$gui$options_path, "iraceResults.Rdata")
        iraceResults <- isolate(store$irace_results)
        save(iraceResults, file = file)

        script_path <- file.path(store$gui$options_path, "run_vizz.R")

        file.copy(system.file("app/script/run_vizz.R", package = "iraceStudio"), script_path)

        a <- process$new(
          command = "Rscript",
          args = c(
            script_path,
            file
          ),
          stdout = "|", stderr = "|"
        )

        a$poll_io(5000)
        log_info("stdout: {a$read_output_lines()}")
        log_info("stderr: {a$read_error_lines()}")
      })
    }
  )
)