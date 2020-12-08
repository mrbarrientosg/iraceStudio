Navbar <- R6::R6Class(
  classname = "Navbar",
  inherit = Component,
  public = list(
    ui = function(id) {
      ns <- NS(id)

      dashboardHeader(
        title = dashboardBrand(
          title = h2("Irace Studio", style = "text-align:center; margin-bottom: 0;")
        ),
        #title = h1("Irace Studio", style = "color: white;text-align:center; margin-bottom: 0;"),
        fixed = TRUE,
        rightUi = tagList(
          tags$li(
            class = "dropdown",
            style = "height: 34px;",
            selectInput(
              inputId = ns("scenarioPicker"),
              label = NULL,
              choices = "",
              width = 250
            )
          ),
          tags$li(
            class = "dropdown",
            style = "align-self: center;",
            tags$a(
              "Irace User Guide",
              class = "btn-link",
              style = "padding: 8px;",
              href = "https://cran.r-project.org/web/packages/irace/vignettes/irace-package.pdf",
              target = "_blank"
            )
          )
        ),
        h4(
          textOutput(
            outputId = ns("playgroundName")
          ),
          style = "text-align: center; flex: 1 0 auto; margin-top: 5px;"
        )
      )
    },

    server = function(input, output, session, store) {
      output$playgroundName <- renderText(store$playgroundName)

      observeEvent(input$scenarioPicker, {
        req(input$scenarioPicker)
        store$pg$change_current_scenario(input$scenarioPicker)
        pkg$outputLog <- NULL
      })

      observeEvent(store$startIrace, {
        if (store$startIrace) {
          disable(id = "scenarioPicker")
        } else {
          enable(id = "scenarioPicker")
        }
      })

      observeEvent(playground_emitter$value(playground_events$update_scenarios), {
        scenarios <- lapply(store$pg$get_scenarios(), function(scenario) scenario$get_name())
        scenarios_id <- lapply(store$pg$get_scenarios(), function(scenario) scenario$get_id())

        if (length(scenarios) == 0) {
          scenarios_id <- ""
        } else {
          names(scenarios_id) <- unlist(scenarios, use.names = FALSE)
        }

        selected <- NULL

        if (!is.null(store$pg$get_last_scenario())) {
          selected <- store$pg$get_last_scenario()
          store$pg$set_last_scenario(NULL)
        }

        updateSelectInput(
          session = session,
          inputId = "scenarioPicker",
          choices = scenarios_id,
          selected = selected
        )
      })
    }
  )
)