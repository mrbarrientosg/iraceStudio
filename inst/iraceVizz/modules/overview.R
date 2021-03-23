Overview <- R6::R6Class(
  classname = "Overview",
  inherit = View,
  public = list(
    ui = function() {
      ns <- NS(self$id)

      tagList(
        div(
          class = "justify-content-between sub-header",
          h2("Overview")
        ),
        fluidRow(
          column(
            width = 7,
            fluidRow(
              div(
                class = "col mini-container",
                htmlOutput(outputId = ns("configs")),
                h5(strong("Configurations"))
              ),
              div(
                class = "col mini-container",
                htmlOutput(outputId = ns("executions")),
                h5(strong("Target Executions"))
              ),
              div(
                class = "col mini-container",
                htmlOutput(outputId = ns("best")),
                h5(strong("Best Configuration"))
              )
            ),
            fluidRow(
              box(
                title = strong("Left 1"),
                collapsible = FALSE,
                closable = FALSE,
                width = 12,
                plotlyOutput(outputId = ns("left_1"))
              ),
              box(
                title = strong("Left 2"),
                collapsible = FALSE,
                closable = FALSE,
                width = 12,
                plotlyOutput(outputId = ns("left_2"))
              )
            )
          ),
          column(
            width = 5,
            fluidRow(
              box(
                title = strong("Right 1"),
                collapsible = FALSE,
                closable = FALSE,
                width = 12,
                plotlyOutput(outputId = ns("right_1"))
              ),
              box(
                title = strong("Right 2"),
                collapsible = FALSE,
                closable = FALSE,
                width = 12,
                plotlyOutput(outputId = ns("right_2"))
              )
            )
          )
        )
      )
    },

    server = function(input, output, session, store) {
      output$configs <- renderUI({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            ""
          )
        )

        h6(nrow(store$irace_results$allConfigurations))
      })

      output$executions <- renderUI({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            ""
          )
        )

        h6(store$irace_results$state$experimentsUsedSoFar)
      })

      output$best <- renderUI({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            ""
          )
        )

        last <- length(store$irace_results$iterationElites)
        id <- store$irace_results$iterationElites[last]
        bestConfiguration <- getConfigurationById(
          iraceResults = store$irace_results,
          ids = id
        )

        h6(bestConfiguration[[1]])
      })

      output$right_1 <- renderPlotly({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            ""
          )
        )

        log <- store$irace_results$experimentLog
        iterations <- unique(log[, "iteration"])
        configurations <- store$irace_results$allConfigurations
        configurations <- cbind(rep(0, times = nrow(configurations)), configurations)
        colnames(configurations)[1] <- ".IT."


        for (i in iterations) {
          conf <- unique(log[log[, "iteration"] == i, "configuration"])
          configurations[conf, ".IT."] <- i
        }

        parameters <- store$raceResults$parameters

        for (pname in parameters$names) {
          if (parameters$types[pname] %in% c("r", "i")) {
            configurations[, pname] <- as.numeric(configurations[, pname])
          }
        }

        conf <- configurations
        conf[, ".IT."] <- as.factor(conf[, ".IT."])

        p <- ggplot(conf, aes(x = beta, fill = .IT.)) +
          geom_density(alpha = 0.5) +
          scale_fill_viridis_d(guide = "none") +
          theme_bw()

        ggplotly(p)
      })

      output$right_2 <- renderPlotly({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            ""
          )
        )

        log <- store$irace_results$experimentLog
        iterations <- unique(log[, "iteration"])
        configurations <- store$irace_results$allConfigurations
        configurations <- cbind(rep(0, times = nrow(configurations)), configurations)
        colnames(configurations)[1] <- ".IT."


        for (i in iterations) {
          conf <- unique(log[log[, "iteration"] == i, "configuration"])
          configurations[conf, ".IT."] <- i
        }

        parameters <- store$raceResults$parameters

        for (pname in parameters$names) {
          if (parameters$types[pname] %in% c("r", "i")) {
            configurations[, pname] <- as.numeric(configurations[, pname])
          }
        }

        conf <- configurations
        conf[, ".IT."] <- as.factor(conf[, ".IT."])

        p <- ggplot(conf, aes(x = alpha, y = beta)) +
          geom_point() +
          # geom_point() +
          # geom_density(aes(x = alpha, y = ..density..), position = "stack") +
          # geom_density(aes(x = beta, y = ..density..), position = "stack") +
          # scale_fill_viridis(discrete = FALSE) +
          facet_wrap(~.IT.)
        theme_bw()

        ggplotly(p)
      })

      output$left_1 <- renderPlotly({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            ""
          )
        )

        experiments <- store$irace_results$experiments[, unique(store$irace_results$iterationElites), drop = F]
        # experiments <- store$irace_results$experiments

        experiments[] <- rank(experiments, na.last = "keep")

        p <- experiments %>%
          as.data.frame() %>%
          rownames_to_column("i_id") %>%
          pivot_longer(-c(i_id), names_to = "C", values_to = "ERT") %>%
          mutate(C = fct_relevel(C, as.character(1:ncol(experiments)))) %>%
          mutate(i_id = fct_relevel(i_id, as.character(1:nrow(experiments)))) %>%
          ggplot(aes(x = C, y = i_id, fill = ERT)) +
          geom_bin2d() +
          scale_fill_viridis_c()

        ggplotly(p)
      })
      # output$left_2 <- renderPlotly(
      #   plot_ly(z = ~volcano, type = "surface")
      # )
    }
  )
)
