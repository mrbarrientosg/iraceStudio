PerformanceInstanceView <- R6::R6Class(
  classname = "PerformanceInstanceView",
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
            h2("Performance by Instance"),
            p("Visualize training performace by instance. Select the active execution and sandbox in the selectors."), 
            HTML("<ul>
                 <li>default sandbox includes data of final elite configurations</li>
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
            title = strong("Raw performance"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            plotlyOutput(outputId = ns("configurationPlot")) %>%
              shinycssloaders::withSpinner(type = 6)
          ),
          bs4Card(
            title = strong("Distance to best"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            strong("Click a point to see more info."),
            plotlyOutput(outputId = ns("distanceBestPlot")) %>%
              shinycssloaders::withSpinner(type = 6),
            br(),
            DT::dataTableOutput(outputId = ns("selectedPointTable"))
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

      best_data <- eventReactive(c(store$iraceResults, store$updateSandbox), {
        future({
          config <- isolate(store$sandbox$getConfigurations()$ID)

          if (length(config) == 0)
            config <- isolate(store$iraceResults$allElites[[length(store$iraceResults$allElites)]])

          self$bestConfigurationByInstances(isolate(store$iraceResults), config)
        })
      })

      output$distanceBestPlot <- renderPlotly({
        shiny::validate(
          need(store$sandbox, ""),
          need(store$iraceResults, "")
        )

        legend <- list(
          title = list(text = "<b>Instance</b>")
        )

        best_data() %...>% {
          plot_ly(., source = "distanceBestPlot") %>%
            add_boxplot(
              x = ~as.factor(instance),
              y = ~distance,
              color = ~as.factor(instance),
              showlegend = TRUE,
              text = ~sprintf("<b>ID:</b> %s\n<b>Distance:</b> %g", configuration, distance),
              hoverinfo = "text",
              boxpoints = "all",
              jitter = 1.0,
              pointpos = 0.0,
              customdata = ~configuration
            ) %>%
            layout(
              title = "Distance to best performance vs Instance",
              xaxis = list(title = "Instance", type = "category", fixedrange = T),
              yaxis = list(title = "Distance to best performance", type = "linear", fixedrange = T, tickformat = ".03g"),
              legend = legend,
              hovermode = "closest",
              showlegend = TRUE
            ) %>%
            event_register("plotly_click")
        }
      })

      selected_best_data <- reactive({
        req(store$updateSandbox)
        req(store$sandbox)
        req(store$iraceResults)
        playground_emitter$value(playground_events$current_scenario)

        event <- event_data("plotly_click", "distanceBestPlot")

        best_data() %...>% {
          if (is.null(event)) {
            return(data.frame())
          }

          return(subset(., configuration %in% event$customdata))
        } %...>% {
          if (is.null(event)) {
            return(data.frame())
          }

          data <- .

          if (nrow(data) == 0) {
            return(data.frame())
          }

          repeated <- data %>%
            dplyr::group_by(instance) %>%
            summarise(
              id = unique(configuration),
              `mean distance` = mean(distance),
              min = min(distance),
              max = max(distance),
              nbExecutions = length(distance)
            )

          return(repeated)
        }
      })

      output$selectedPointTable <- DT::renderDataTable({
        selected_best_data() %...>% {
          datatable(
            data = .,
            escape = FALSE,
            selection = "single",
            rownames = FALSE,
            style = "bootstrap4",
            class = "table-condensed table-striped cell-border",
            options = list(
              searching = FALSE,
              paging = FALSE,
              scrollY = "500px",
              dom = "t",
              sort = FALSE
            )
          )
        }
      })

      config_data <- eventReactive(c(store$iraceResults, store$updateSandbox), {
        future({
          id <- isolate(store$sandbox$getConfigurations()$ID)

          if (length(id) == 0)
            id <- isolate(store$iraceResults$allElites[[length(store$iraceResults$allElites)]])

          self$configurationByIntances(isolate(store$iraceResults)) %>%
            subset(configuration %in% id)
        })
      })

      output$configurationPlot <- renderPlotly({
        shiny::validate(
          need(store$sandbox, ""),
          need(store$iraceResults, "")
        )

        legend <- list(
          title = list(text = "<b>Instance</b>")
        )

        config_data() %...>% {
          plot_ly(.) %>%
            add_boxplot(
              x = ~as.factor(instance),
              y = ~performance,
              color = ~as.factor(instance),
              showlegend = TRUE,
              text = ~sprintf("<b>ID:</b> %s\n<b>Performance:</b> %g", configuration, performance),
              hoverinfo = "text",
              boxpoints = "all",
              jitter = 1.0,
              pointpos = 0.0
            ) %>%
            layout(
              title = "Performance Raw vs Instance",
              xaxis = list(title = "Instance", type = "category", fixedrange = T),
              yaxis = list(title = "Performance Raw", type = "linear", fixedrange = T, tickformat = ".03e"),
              hovermode = "closest",
              legend = legend,
              showlegend = TRUE
            )
        }
      })

      observeEvent(session$userData$sidebar(), {
        sidebar <- session$userData$sidebar()
        if (sidebar == "visualization_by_instance") {
          js$resizePlotly(session$ns("configurationPlot"))
          js$resizePlotly(session$ns("distanceBestPlot"))
        }
      })
    },

    bestConfigurationByInstances = function(iraceResults, configurations = iraceResults$allConfigurations$.ID.) {
      exp <- iraceResults$experiments[, as.character(configurations), drop = FALSE]

      min <- apply(exp, 1, function(row) {
        if (all(is.na(row)))
          return(NA)
        else
          min(row, na.rm = T)
      })

      exp <- 100 * (min[row(exp)] - exp) / min[row(exp)]
      exp[is.nan(exp)] <- 0.0 # Replace nan values with 0

      instances <- iraceResults$state$.irace$instancesList[rownames(exp), "instance"]
      rownames(exp) <- sort(instances)

      distance <- c(exp)
      configuration <- as.numeric(colnames(exp)[col(exp)])
      instance <- as.numeric(rownames(exp)[row(exp)])

      df <- data.frame(distance = distance, configuration = configuration, instance = instance)
      df <- df[complete.cases(df),]

      return(df)
    },

    configurationByIntances = function(iraceResults) {
      exp <- iraceResults$experiments
      instances <- iraceResults$state$.irace$instancesList[rownames(exp), "instance"]
      rownames(exp) <- sort(instances)

      performance <- c(exp)
      configuration <- as.numeric(colnames(exp)[col(exp)])
      instance <- as.numeric(rownames(exp)[row(exp)])

      df <- data.frame(performance = performance, configuration = configuration, instance = instance)
      df <- df[complete.cases(df),]

      return(df)
    }
  )
)
