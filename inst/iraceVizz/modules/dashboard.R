Navbar <- R6::R6Class(
  classname = "Navbar",
  inherit = Component,
  public = list(
    ui = function(id) {
      ns <- NS(id)

      dashboardHeader(
        title = dashboardBrand(
          title = h2("Irace Vizz", style = "text-align:center; margin-bottom: 0;")
        ),
        fixed = TRUE,
        h4(
          textOutput(
            outputId = ns("playgroundName")
          ),
          style = "text-align: center; flex: 1 0 auto; margin-top: 5px;"
        )
      )
    },

    server = function(input, output, session, store) {
      output$playgroundName <- renderText(store$playground_name)
    }
  )
)

Body <- R6::R6Class(
  classname = "Body",
  public = list(
    overview = NULL,

    initialize = function() {
      self$overview <- Overview$new("overview")
    },

    ui = function() {
      dashboardBody(
        irace_vizz_resources(),
        tabItems(
          tabItem(
            tabName = "overview",
            self$overview$ui()
          )
        )
      )
    },

    setupModules = function(store) {
      self$overview$call(store = store)
    }
  )
)

ControlBar <- R6::R6Class(
  classname = "ControlBar",
  inherit = Component,
  public = list(
    ui = function(id) {
      ns <- NS(id)

      dashboardControlbar(
        id = ns("sidebar"),
        skin = "light",
        overlay = TRUE,
        collapsed = TRUE,
        fluidRow(
          box(
            title = strong("Global Options"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            pickerInput(
              inputId = ns("scenarioPicker"),
              label = "Scenario",
              choices = "",
              options = list(
                size = 8
              )
            )
          )
        )
      )
    },

    server = function(input, output, session, store) {
      observeEvent(input$scenarioPicker, {
        req(input$scenarioPicker)
        store$current_scenario <- input$scenarioPicker
        store$irace_results <- store$scenarios[[input$scenarioPicker]]
      })

      observeEvent(store$scenarios, {
        updatePickerInput(
          session = session,
          inputId = "scenarioPicker",
          choices = names(store$scenarios)
        )
      })
    }
  )
)

Sidebar <- R6::R6Class(
  classname = "Sidebar",
  public = list(
    ui = function() {
      dashboardSidebar(
        minified = FALSE,
        sidebarMenu(
          menuItem(
            text = "Overview",
            tabName = "overview",
            icon = NULL
          )
        )
      )
    }
  )
)

Footer <- R6::R6Class(
  classname = "Footer",
  inherit = Component,
  public = list(
    ui = function(id) {
      ns <- NS(id)

      dashboardFooter(
        left = div(
          class = "d-flex flex-row",
          div(
            "Current Scenario: ",
            uiOutput(
              outputId = ns("scenario"),
              inline = TRUE
            )
          )
        ),
        fixed = TRUE
      )
    },

    server = function(input, output, session, store) {
      output$scenario <- renderUI({
        store$current_scenario
      })
    }
  )
)
