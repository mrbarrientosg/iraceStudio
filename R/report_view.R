ReportView <- R6::R6Class(
  classname = "ReportView",
  inherit = View,
  public = list(
    summaryCard = NULL,
    bestConfigurationCard = NULL,
    candidatesCard = NULL,
    performanceCard = NULL,
    detailByIterationCard = NULL,
    executionSelect = NULL,

    initialize = function(id) {
      super$initialize(id)
      self$summaryCard <- SummaryCard$new()
      self$bestConfigurationCard <- BestConfigurationCard$new()
      self$candidatesCard <- CandidatesCard$new()
      self$performanceCard <- PerformanceCard$new()
      self$detailByIterationCard <- DetailByIterationCard$new()
      self$executionSelect <- ExecutionSelect$new()
    },

    ui = function() {
      ns <- NS(self$id)

      tagList(
        fluidRow(
          class = "justify-content-between",
          style = "height: 90px;",
          column(
            width = 2,
            h2("Report"),
            textOutput(outputId = ns("importLabel"))
          ),
          column(
            width = 7,
            class = "d-flex align-items-center justify-content-end",
            self$executionSelect$ui(inputId = ns("executions")),
            disabled(
              importButton(
                inputId = ns("load"),
                style = "margin-left: 5px; margin-top: 15px;",
                size = "default"
              )
            ),
            shinyDirButton(
              id = ns("pdf"),
              title = "Select a directory to save PDF Report",
              label = "Save as PDF",
              style = "margin-left: 5px; margin-top: 15px;",
              buttonType = "primary"
            )
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

      executions <- self$executionSelect$call(id = "executions", store = store)

      volum <- c(root = path_home())

      shinyFileChoose(input, "load", roots = volum, filetypes = "Rdata")
      shinyDirChoose(input, "pdf", roots = volum, filetypes = "pdf")

      observeEvent(input$load, {
        if (!is.integer(input$load)) {
          file <- parseFilePaths(roots = volum, input$load)
          load(file = file$datapath)
          store$iraceResults <- iraceResults
          rm(iraceResults)
          store$currentExecution <- NULL
        }
      })

      output$importLabel <- renderText({
        shiny::validate(
          need(store$iraceResults, message = ""),
          need(is.null(store$currentExecution), message = "")
        )

        return("An external log file will not be saved in the playground.")
      })

      observeEvent(input$pdf, {
        req(store$iraceResults)

        if (!is.integer(input$pdf)) {
          make_pdf_report(store, input, volum)
        }
      })

      observeEvent(store$iraceResults, {
        if (is.null(store$iraceResults)) {
          disable(id = "pdf")
        } else {
          enable(id = "pdf")
        }
      }, ignoreNULL = FALSE)
    }
  )
)

