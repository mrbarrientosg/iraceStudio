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
            title = strong("Box Plot"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            plotlyOutput(outputId = ns("boxPlotPerf")),
            br(),
            DTOutput(outputId = ns("selectedPointTable"))
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


      data <- eventReactive(store$iraceResults, {
        future({
          config <- store$iraceResults$allConfigurations$.ID.

          data <- self$configurationByInstances(store$iraceResults, config)
        })
      })

      filteredData <- reactive({
        req(store$updateSandbox)
        req(store$sandbox)

        config <- store$sandbox$getConfigurations()$ID

        if (length(config) == 0)
          config <- store$iraceResults$allConfigurations$.ID.

        data() %...>% subset(conf %in% config)
      })


      output$boxPlotPerf <- renderPlotly({
        shiny::validate(
          need(store$sandbox, ""),
          need(store$iraceResults, "")
        )

        legend <- list(
          title = list(text = "<b>Instance</b>")
        )

        filteredData() %...>% {
          data <- .

          data %>%
            plot_ly(source = "boxPlotPerf") %>%
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
              title = "Box plot",
              xaxis = list(title = "Instance", tickvals = ~instance, ticktext = ~instance, fixedrange = T),
              yaxis = list(title = "Distance to best", type = "linear", fixedrange = T),
              legend = legend,
              hovermode = "closest",
              showlegend = TRUE
            ) %>%
            event_register("plotly_click")
        }
      })

      selectedData <- reactive({
        req(store$updateSandbox)
        playground_emitter$value(playground_events$current_scenario)

        event <- event_data("plotly_click", "boxPlotPerf")

        filteredData() %...>% {
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
        selectedData() %...>% {
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
    },

    configurationByInstances = function(iraceResults, configurations = iraceResults$allConfigurations$.ID.) {
      data <- data.frame()

      experiments <- iraceResults$experiments[, as.character(configurations), drop = FALSE]

      min <- apply(experiments, 1, function(row) {
        if (all(is.na(row)))
          return(NA)
        else
          min(row, na.rm = T)
      })

      for (idx in seq_along(min)) {
        row <- experiments[idx, , drop = FALSE]
        values <- 100 * ((row - min[idx]) / min[idx])
        values <- as.vector(values)
        instance <- iraceResults$state$.irace$instancesList[idx, "instance"]
        na <- which(!is.na(values))
        instances <- rep(instance, length(na))
        data <- rbind(data, data.frame(instance = instances, conf = configurations[na], value = values[na]))
      }

      data$jitter <- jitter(as.numeric(data$instance))
      data$instance <- factor(data$instance)
      data <- data[order(data$instance), ]

      return(data)
    }
  )
)
