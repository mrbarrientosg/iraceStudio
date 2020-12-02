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
            h2("Configuration performance"),
            p("Visualize training performace by configuration. Select the active execution and sandbox in the selectors."),
            HTML("<ul>
                 <li>default sandbox includes final elite configurations</li>
                 <li>to add configurations in the current sandbox, go to the Filter menu</li>
                 <li>to create a new sandbox, go to the Sandbox menu</li>
                 </ul>")
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

      values <- reactiveValues(data = NULL)

      observeEvent(store$iraceResults, {
        future({
          self$configurationByPerformance(isolate(store$iraceResults))
        }) %...>% {
          values$data <- .
        }
      })

      filtered_config_data <- eventReactive(c(values$data, store$updateSandbox, store$sandbox), {
        req(store$updateSandbox)
        req(store$sandbox)

        id <- store$sandbox$getConfigurations()$ID

        if (length(id) == 0)
          id <- isolate(store$iraceResults$allElites[[length(store$iraceResults$allElites)]])

        values$data %>%
          filter(configuration %in% id)
      })

      output$solutionCostConfig <- renderPlotly({
        shiny::validate(
          need(store$sandbox, ""),
          need(store$iraceResults, "")
        )

        legend <- list(
          title = list(text = "<b>Configuration</b>")
        )

        data <- filtered_config_data()

        plot_ly(data) %>%
          add_boxplot(
            x = ~as.factor(configuration),
            y = ~performance,
            color = ~as.factor(configuration),
            showlegend = TRUE,
            hoverinfo = "y",
            boxpoints = "all",
            jitter = 1.0,
            pointpos = 0.0
          ) %>%
          layout(
            title = "Configuration vs Performance Raw",
            xaxis = list(title = "Configuration ID", type = "category", fixedrange = T),
            yaxis = list(title = "Performance Raw", type = "linear", fixedrange = T, tickformat = ".03e"),
            hovermode = "closest",
            legend = legend,
            showlegend = TRUE
          )
      })

      observeEvent(session$userData$sidebar(), {
        sidebar <- session$userData$sidebar()
        if (sidebar == "visualization_by_config") {
          js$resizePlotly(session$ns("solutionCostConfig"))
        }
      })
    },

    configurationByPerformance = function(iraceResults) {
      exp <- iraceResults$experiments
      performance <- c(exp)
      configuration <- as.numeric(colnames(exp)[col(exp)])

      df <- data.frame(performance = performance, configuration = configuration)
      df <- df[complete.cases(df),]

      return(df)
    }
  )
)