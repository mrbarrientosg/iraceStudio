PerformanceConfigView <- R6::R6Class( # nolint
  classname = "PerformanceConfigView",
  inherit = View,
  public = list(
    ui = function() {
      ns <- NS(self$id)

      tagList(
        fluidRow(
          column(
            width = 12,
            h2("Configuration performance"),
            p("Visualize training performace by configuration. Select the active execution and sandbox in the selectors."), # nolint
            HTML("<ul>
                 <li>default sandbox includes final elite configurations</li>
                 <li>to add configurations in the current sandbox, go to the Filter menu</li>
                 <li>to create a new sandbox, go to the Sandbox menu</li>
                 </ul>")
          )
        ),
        fluidRow(
          box(
            title = strong("Configuration Performance"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            plotlyOutput(outputId = ns("solution_cost_config")) %>%
              shinycssloaders::withSpinner(type = 6)
          )
        )
      )
    },

    server = function(input, output, session, store, events) {
      values <- reactiveValues(data = NULL)

      observeEvent(store$irace_results, {
        future({
          self$configurationByPerformance(isolate(store$irace_results))
        }) %...>% {
          values$data <- .
        }
      })

      filtered_config_data <- eventReactive(c(values$data, events$update_sandbox), {
        id <- store$sandbox$get_configurations()$ID

        if (length(id) == 0) {
          id <- isolate(store$irace_results$allElites[[length(store$irace_results$allElites)]])
        }

        values$data %>%
          filter(configuration %in% id)
      })

      output$solution_cost_config <- renderPlotly({
        shiny::validate(
          need(store$sandbox, ""),
          need(store$irace_results, "")
        )

        legend <- list(
          title = list(text = "<b>Configuration</b>")
        )

        data <- filtered_config_data()

        plot_ly(data) %>%
          add_boxplot(
            x = ~ as.factor(configuration),
            y = ~performance,
            color = ~ as.factor(configuration),
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
          js$resizePlotly(session$ns("solution_cost_config"))
        }
      })
    },

    configurationByPerformance = function(irace_results) {
      exp <- irace_results$experiments
      performance <- c(exp)
      configuration <- as.numeric(colnames(exp)[col(exp)])

      df <- data.frame(performance = performance, configuration = configuration)
      df <- df[complete.cases(df), ]

      return(df)
    }
  )
)
