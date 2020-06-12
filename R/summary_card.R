SummaryCard <- R6::R6Class(
  classname = "SummaryCard",
  inherit = Component,
  public = list(
    ui = function(inputId) {
      ns <- NS(inputId)
      
      bs4Card(
        title = strong("Summary"),
        collapsible = FALSE,
        closable = FALSE,
        width = 12,
        htmlOutput(outputId = ns("summary_content"))
      )
    },
    
    server = function(input, output, session, store) {
      output$summary_content <- renderUI({
        shiny::validate(
          need(
            !is.null(store$iraceResults),
            "Provide a Rdata file (logFile) to display information."
          )
        )
        
        tags$ul(
          style = "list-style-type:none;",
          tags$li(
            HTML(
              paste(strong("IRACE version:"), "&nbsp;", store$iraceResults$irace.version)
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("Number of candidate configurations:"), "&nbsp;",
                nrow(store$iraceResults$allConfigurations)
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("Number of target executions:"), "&nbsp;",
                store$iraceResults$state$experimentsUsedSoFar
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("Elitist new instances:"), "&nbsp;",
                store$iraceResults$scenario$elitistNewInstances
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("Elitist limit:"), "&nbsp;",
                store$iraceResults$scenario$elitistLimit
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("nbIterations:"), "&nbsp;",
                store$iraceResults$state$nbIterations
              )
            )
          ),
          
          tags$li(
            HTML(
              paste(
                strong("minNbSurvival:"), "&nbsp;",
                store$iraceResults$state$minSurvival
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("nbParameters:"), "&nbsp;",
                store$iraceResults$parameters$nbParameters
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("seed:"), "&nbsp;",
                store$iraceResults$scenario$seed
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("confidence level:"), "&nbsp;",
                store$iraceResults$scenario$confidence
              )
            )
          ),
          tags$li(
            HTML(
              paste(
                strong("budget:"), "&nbsp;",
                store$iraceResults$scenario$budgetEstimation
              )
            )
          ),
          tags$li(HTML(paste(strong("mu:"), "&nbsp;", store$iraceResults$scenario$mu))),
          tags$li(
            HTML(
              paste(
                strong("deterministic:"), "&nbsp;",
                store$iraceResults$scenario$deterministic
              )
            )
          )
        )
      })
    }
  )
)