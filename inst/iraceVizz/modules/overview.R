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
                            h4("Configurations"),
                            "132"
                            #nrow(store$iraceResults$allConfigurations)
                        ),
                        div(
                            class = "col mini-container",
                            h4("Target Executions"),
                            "132"
                            #store$iraceResults$state$experimentsUsedSoFar
                        ),
                        div(
                            class = "col mini-container",
                            h4("Best Configuration"),
                            "64"
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
        output$right_1 <- renderPlotly(
            plot_ly(z = ~volcano, type = "surface")
        )
        output$right_2 <- renderPlotly(
            plot_ly(z = ~volcano, type = "surface")
        )

        output$left_1 <- renderPlotly(
            plot_ly(z = ~volcano, type = "surface")
        )
        output$left_2 <- renderPlotly(
            plot_ly(z = ~volcano, type = "surface")
        )
    }
  )
)