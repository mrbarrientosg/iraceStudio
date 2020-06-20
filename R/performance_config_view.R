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
            title = strong("Distance to best"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            plotlyOutput(outputId = ns("distanceBestPlot")),
            br(),
            DTOutput(outputId = ns("selectedPointTable"))
          ),
          bs4Card(
            title = strong("Configuration"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            plotlyOutput(outputId = ns("configurationPlot"))
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

      best_data <- eventReactive(store$iraceResults, {
        future({
          config <- store$iraceResults$allConfigurations$.ID.

          self$bestConfigurationByInstances(store$iraceResults, config)
        })
      })

      filtered_best_data <- reactive({
        req(store$updateSandbox)
        req(store$sandbox)

        config <- store$sandbox$getConfigurations()$ID

        if (length(config) == 0)
          config <- store$iraceResults$allConfigurations$.ID.

        best_data() %...>% subset(conf %in% config)
      })

      output$distanceBestPlot <- renderPlotly({
        shiny::validate(
          need(store$sandbox, ""),
          need(store$iraceResults, "")
        )

        legend <- list(
          title = list(text = "<b>Instance</b>")
        )

        filtered_best_data() %...>% {
          data <- .

          data %>%
            plot_ly(source = "distanceBestPlot") %>%
            add_boxplot(
              x = ~instance,
              y = ~value,
              color = ~instance,
              boxpoints = FALSE,
              hoverinfo = "none",
              hoveron = "boxes",
              legendgroup = ~instance,
              showlegend = TRUE
            ) %>%
            add_markers(
              x = ~jitter,
              y = ~value,
              marker = list(size = 5),
              color = ~instance,
              customdata = ~conf,
              hovertemplate = "<b>y:</b> %{y:.3f} <br><b>ID: %{customdata}</b><extra></extra>",
              legendgroup = ~instance,
              showlegend = FALSE
            ) %>%
            layout(
              title = "Distance to best vs Instance",
              xaxis = list(title = "Instance", tickvals = ~instance, ticktext = ~instance, fixedrange = T),
              yaxis = list(title = "Distance to best", type = "linear", fixedrange = T),
              legend = legend,
              hovermode = "closest",
              showlegend = TRUE
            ) %>%
            event_register("plotly_click")
        }
      })

      selected_best_data <- reactive({
        req(store$updateSandbox)
        playground_emitter$value(playground_events$current_scenario)

        event <- event_data("plotly_click", "distanceBestPlot")

        filtered_best_data() %...>% {
          if (is.null(event)) {
            return(data.frame())
          }

          return(subset(., conf %in% event$customdata))
        } %...>% {
          if (is.null(event)) {
            return(data.frame())
          }

          data <- .

          if (nrow(data) == 0) {
            return(data.frame())
          }

          data <- data[, !(colnames(data) %in% "jitter"), drop = F]

          repeated <- data %>%
            dplyr::group_by(instance) %>%
            summarise(
              id = unique(conf),
              mean = mean(value),
              min = min(value),
              max = max(value),
              nbExecutions = length(value)
            )

          return(repeated)
        }
      })

      output$selectedPointTable <- renderDT({
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

      config_data <- eventReactive(store$iraceResults, {
        future({
          config <- store$iraceResults$allConfigurations$.ID.

          self$configurationByIntances(store$iraceResults, config)
        })
      })

      filtered_config_data <- reactive({
        req(store$updateSandbox)
        req(store$sandbox)

        config <- store$sandbox$getConfigurations()$ID

        if (length(config) == 0)
          config <- store$iraceResults$allConfigurations$.ID.

        config_data() %...>% subset(id %in% config)
      })

      output$configurationPlot <- renderPlotly({
        shiny::validate(
          need(store$sandbox, ""),
          need(store$iraceResults, "")
        )

        legend <- list(
          title = list(text = "<b>Instance</b>")
        )

        filtered_config_data() %...>% {
          data <- .

          data %>%
            plot_ly() %>%
            add_boxplot(
              x = ~instance,
              y = ~value,
              color = ~instance,
              boxpoints = FALSE,
              hoverinfo = "none",
              hoveron = "boxes",
              legendgroup = ~instance,
              showlegend = TRUE
            ) %>%
            add_markers(
              x = ~jitter,
              y = ~value,
              marker = list(size = 5),
              color = ~instance,
              customdata = ~id,
              hovertemplate = "<b>y:</b> %{y:.3f} <br><b>ID: %{customdata}</b><extra></extra>",
              legendgroup = ~instance,
              showlegend = FALSE
            ) %>%
            layout(
              title = "Configuration vs Instance",
              xaxis = list(title = "Instance", tickvals = ~instance, ticktext = ~instance, fixedrange = T),
              yaxis = list(title = "Configuration", type = "linear", fixedrange = T),
              legend = legend,
              hovermode = "closest",
              showlegend = TRUE
            )
        }
      })
    },

    bestConfigurationByInstances = function(iraceResults, configurations = iraceResults$allConfigurations$.ID.) {
      data <- data.frame()

      experiments <- iraceResults$experiments

      min <- apply(experiments, 1, function(row) {
        if (all(is.na(row)))
          return(NA)
        else
          min(row, na.rm = T)
      })

      experiments <- iraceResults$experiments[, as.character(configurations), drop = FALSE]

      for (idx in seq_along(min)) {
        if (is.na(min[idx])) {
          next
        }

        row <- experiments[idx, , drop = FALSE]
        row <- row[, which(!is.na(row)), drop = F]
        values <- 100 * ((row - min[idx]) / min[idx])
        conf <- colnames(values)
        values <- as.vector(values)
        instance <- iraceResults$state$.irace$instancesList[idx, "instance"]
        instances <- rep(instance, length(values))
        data <- rbind(data, data.frame(instance = instances, conf = conf, value = values))
      }

      data$jitter <- jitter(as.numeric(data$instance))
      data$instance <- factor(data$instance)
      data <- data[order(data$instance), ]

      return(data)
    },

    configurationByIntances = function(iraceResults, configurations = iraceResults$allConfigurations$.ID.) {
      experiments <- iraceResults$experiments[, as.character(configurations), drop = FALSE]

      id <- rownames(experiments)
      instances <- iraceResults$state$.irace$instancesList[id, "instance"]
      rownames(experiments) <- sort(instances)

      data <- as.data.frame(as.table(experiments))
      data <- na.omit(data)

      colnames(data) <- c("instance", "id", "value")

      data$jitter <- jitter(as.numeric(data$instance))
      data <- data[order(data$instance), ]

      return(data)
    }
  )
)
