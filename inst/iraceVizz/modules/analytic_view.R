AnalyticView <- R6::R6Class(
  classname = "AnalyticView",
  inherit = View,
  public = list(
    ui = function() {
        ns <- NS(self$id)

        tagList(
            fluidRow(
                class = "justify-content-between sub-header",
                column(
                    width = 8,
                    h2("Analytic"),
                ),
                column(
                    width = 4,
                    iraceStudio::actionButton(
                        inputId = ns("add"),
                        label = "Add chart",
                        class = "float-right btn-primary",
                        style = "margin-left: 5px;"
                    )
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    gridStack(
                        id = ns("grid")
                    )
                )
            )
        )
    },

    server = function(input, output, session, store) {
        values <- reactiveValues(plot = 0)
        observeEvent(input$add, {
            id <- session$ns(paste0("test-", values$plot))
            addWidgetGrid(session$ns("grid"), id)
            insertUI(
                selector = paste0("#", id),
                where = "beforeEnd",
                ui = tags$div(plot_ly(z = ~volcano, type = "surface")),
                immediate = TRUE
            )
            values$plot <- values$plot + 1
        })
    }
  )
)