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
      output$playgroundName <- renderText(store$playgroundName)
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
        iraceVizzResources(),
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
    executionSelect = NULL,
    sandboxSelect = NULL,

    initialize = function() {
      self$executionSelect <- ExecutionSelect$new()
      self$sandboxSelect <- SandboxSelect$new()
    },

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
              ),
              self$executionSelect$ui(inputId = ns("executions")),
              self$sandboxSelect$ui(inputId = ns("sandboxes"))
            )
          )
        )
    },

    server = function(input, output, session, store) {
      self$executionSelect$call(id = "executions", store = store)
      self$sandboxSelect$call(id = "sandboxes", store = store)

      observeEvent(input$scenarioPicker, {
        req(input$scenarioPicker)
        store$pg$change_current_scenario(input$scenarioPicker)
        pkg$outputLog <- NULL
      })

      observeEvent(global_emitter$value(global_events$update_scenarios), {
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

        updatePickerInput(
          session = session,
          inputId = "scenarioPicker",
          choices = scenarios_id,
          selected = selected
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
