PerformanceConfigView <- R6::R6Class(
  classname = "PerformanceConfigView",
  inherit = View,
  public = list(
    executionSelect = NULL,
    sandboxSelect = NULL,

    initialize = function(id) {
      super$initialize(id)
      self$executionSelect <- ExecutionSelect$new()
      self$sandboxSelect <- SandboxSelect$new()
    },

    ui = function() {
      ns <- NS(self$id)

      tagList(
        fluidRow(
          column(
            width = 4,
            h2("Performance by Configuration")
          ),
          column(
            width = 8,
            class = "d-flex align-items-center justify-content-end",
            self$executionSelect$ui(inputId = ns("executions")),
            div(style = "padding: 8px;"),
            self$sandboxSelect$ui(inputId = ns("sandboxes"))
          )
        ),
        fluidRow(
          bs4Card(
            title = strong("Configuration Performance"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            plotlyOutput(outputId = ns("solutionCostConfig")) %>%
              shinycssloaders::withSpinner(type = 6)
          )
        )
      )
    },

    server = function(input, output, session, store) {

      self$executionSelect$call(
        id = "executions",
        store = store
      )

      self$sandboxSelect$call(
        id = "sandboxes",
        store = store
      )

      config_data <- eventReactive(store$iraceResults, {
        future(self$configurationByPerformance(isolate(store$iraceResults)))
      })

      filtered_config_data <- reactive({
        req(store$updateSandbox)
        req(store$sandbox)

        config <- store$sandbox$getConfigurations()$ID

        if (length(config) == 0)
          config <- isolate(store$iraceResults$allElites[[length(store$iraceResults$allElites)]])

        config_data() %...>% subset(id %in% config)
      })

      output$solutionCostConfig <- renderPlotly({
        shiny::validate(
          need(store$sandbox, ""),
          need(store$iraceResults, "")
        )

        legend <- list(
          title = list(text = "<b>Configuration</b>")
        )

        filtered_config_data() %...>% {
          data <- .

          data %>%
            plot_ly() %>%
            add_boxplot(
              x = ~id,
              y = ~value,
              color = ~id,
              boxpoints = FALSE,
              hoverinfo = "none",
              hoveron = "boxes",
              legendgroup = ~id,
              showlegend = TRUE
            ) %>%
            add_markers(
              x = ~jitter,
              y = ~value,
              marker = list(size = 5),
              color = ~id,
              customdata = ~id,
              hovertemplate = "<b>y:</b> %{y:.3f} <br><b>ID: %{customdata}</b><extra></extra>",
              legendgroup = ~id,
              showlegend = FALSE
            ) %>%
            layout(
              title = "Configuration vs Performance Raw",
              xaxis = list(title = "Configuration ID", tickvals = ~id, ticktext = ~id, fixedrange = T),
              yaxis = list(title = "Performance Raw", type = "linear", fixedrange = T),
              legend = legend,
              hovermode = "closest",
              showlegend = TRUE
            )
        }
      })

      observeEvent(session$userData$sidebar(), {
        sidebar <- session$userData$sidebar()
        if (sidebar == "visualization_by_config") {
          js$resizePlotly(session$ns("solutionCostConfig"))
        }
      })
    },

    configurationByPerformance = function(iraceResults) {
      experiments <- iraceResults$experiments
      data.labels <- colnames(experiments)
      if (is.null(data.labels)) data.labels <- seq_len(ncol(experiments))

      data <- data.frame()

      for (i in seq_len(ncol(experiments))) {
        values <- as.vector(experiments[,i])
        values <- values[!is.na(values)]
        config <- rep(data.labels[i], length(values))
        data <- rbind(data, data.frame(id = config, value = values))
      }

      data$jitter <- jitter(as.numeric(data$id))
      data$id <- factor(data$id)
      data <- data[order(data$id), ]

      return(data)
    }
  )
)