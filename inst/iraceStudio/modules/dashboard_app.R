NavbarApp <- R6::R6Class(
  classname = "NavbarApp",
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
      # rightUi = tagList(
      #   tags$li(
      #     class = "dropdown",
      #     style = "height: 34px;",
      #     selectInput(
      #       inputId = ns("scenarioPicker"),
      #       label = NULL,
      #       choices = "",
      #       width = 250
      #     )
      #   ),
      #   tags$li(
      #     class = "dropdown",
      #     style = "align-self: center;",
          # tags$a(
          #   "Irace User Guide",
          #   class = "btn-link",
          #   style = "padding: 8px;",
          #   href = "https://cran.r-project.org/web/packages/irace/vignettes/irace-package.pdf",
          #   target = "_blank"
          # )
      #   )
      # ),
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

BodyApp <- R6::R6Class(
  classname = "BodyApp",
  public = list(
    homeView = NULL,
    uiOptionsView = NULL,
    playgroundView = NULL,
    iraceOptionsView = NULL,
    parametersView = NULL,
    trainInstancesView = NULL,
    targetScriptsView = NULL,
    initialConfigurationsView = NULL,
    forbiddenView = NULL,
    testingView = NULL,

    iraceOutputView = NULL,
    executionsHistoryView = NULL,

    sandboxView = NULL,
    filterView = NULL,
    performanceInstance = NULL,
    performanceConfig = NULL,

    reportView = NULL,
    userSectionView = NULL,

    initialize = function() {
      self$homeView <- HomeView$new("welcome")
      self$uiOptionsView <- UIOptionsView$new("ui_options")
      self$playgroundView <- PlaygroundView$new("playground")
      self$iraceOptionsView <- IraceOptionsView$new("scenario_irace_options")
      self$parametersView <- ParametersView$new("scenario_parameters")
      self$trainInstancesView <- TrainInstancesView$new("scenario_train_instances")
      self$targetScriptsView <- TargetScriptsView$new("scenario_target_scripts")
      self$initialConfigurationsView <- InitialConfigurationsView$new("scenario_initial_configurations")
      self$forbiddenView <- ForbiddenView$new("scenario_forbidden")
      self$testingView <- TestingView$new("scenario_testing")
      self$iraceOutputView <- IraceOutputView$new("execution_irace_output")
      self$executionsHistoryView <- ExecutionsHistoryView$new("execution_history")

      self$sandboxView <- SandboxView$new("visualization_sandbox")
      self$filterView <- FilterView$new("visualization_filter")
      self$performanceInstance <- PerformanceInstanceView$new("visualization_by_instance")
      self$performanceConfig <- PerformanceConfigView$new("visualization_by_config")

      self$reportView <- ReportView$new("report")
      self$userSectionView <- UserSectionView$new("report_user_section")
    },

    ui = function() {
      dashboardBody(
        iraceStudioResources(),
        tabItems(
          tabItem(
            tabName = "home",
            self$homeView$ui()
          ),
          tabItem(
            tabName = "ui_options",
            self$uiOptionsView$ui()
          ),
          tabItem(
            tabName = "playground",
            self$playgroundView$ui()
          ),
          tabItem(
            tabName = "scenario_irace_options",
            self$iraceOptionsView$ui()
          ),
          tabItem(
            tabName = "scenario_parameters",
            self$parametersView$ui()
          ),
          tabItem(
            tabName = "scenario_train_instances",
            self$trainInstancesView$ui()
          ),
          tabItem(
            tabName = "scenario_target_scripts",
            self$targetScriptsView$ui()
          ),
          tabItem(
            tabName = "scenario_initial_configurations",
            self$initialConfigurationsView$ui()
          ),
          tabItem(
            tabName = "scenario_forbidden",
            self$forbiddenView$ui()
          ),
          tabItem(
            tabName = "scenario_testing",
            self$testingView$ui()
          ),
          tabItem(
            tabName = "execution_irace_output",
            self$iraceOutputView$ui()
          ),
          tabItem(
            tabName = "execution_history",
            self$executionsHistoryView$ui()
          ),
          tabItem(
            tabName = "visualization_sandbox",
            self$sandboxView$ui()
          ),
          tabItem(
            tabName = "visualization_filter",
            self$filterView$ui()
          ),
          tabItem(
            tabName = "visualization_by_config",
            self$performanceConfig$ui()
          ),
          tabItem(
            tabName = "visualization_by_instance",
            self$performanceInstance$ui()
          ),
          tabItem(
            tabName = "report",
            self$reportView$ui()
          ),
          tabItem(
            tabName = "report_user_section",
            self$userSectionView$ui()
          )
        )
      )
    },

    setupModules = function(store) {
      self$homeView$call(store = store)
      self$uiOptionsView$call(store = store)
      self$playgroundView$call(store = store)
      self$iraceOptionsView$call(store = store)
      self$parametersView$call(store = store)
      self$trainInstancesView$call(store = store)
      self$targetScriptsView$call(store = store)
      self$initialConfigurationsView$call(store = store)
      self$forbiddenView$call(store = store)
      self$testingView$call(store = store)

      self$iraceOutputView$call(store = store)
      self$executionsHistoryView$call(store = store)

      self$sandboxView$call(store = store)
      self$filterView$call(store = store)
      self$performanceInstance$call(store = store)
      self$performanceConfig$call(store = store)

      self$reportView$call(store = store)
      self$userSectionView$call(store = store)
    }
  )
)

ControlBarApp <- R6::R6Class(
  classname = "ControlBarApp",
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
              self$sandboxSelect$ui(inputId = ns("sandboxes")),
              tags$a(
                "Irace User Guide",
                class = "btn-link",
                style = "padding: 8px;",
                href = "https://cran.r-project.org/web/packages/irace/vignettes/irace-package.pdf",
                target = "_blank"
              )
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
        store$didChangeScenario <- store$didChangeScenario + 1
        pkg$outputLog <- NULL
      })

      observeEvent(store$startIrace, {
        if (store$startIrace) {
          disable(id = "scenarioPicker")
        } else {
          enable(id = "scenarioPicker")
        }
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

SidebarApp <- R6::R6Class(
  classname = "SidebarApp",
  public = list(
    ui = function() {
      dashboardSidebar(
        minified = FALSE,
      #elevation = 0,
        id = "sidebarState",
        sidebarMenu(
          id = "sidebar",
          menuItem(
            text = "Home",
            tabName = "home",
            icon = NULL
          ),
          menuItem(
            text = "UI Options",
            tabName = "ui_options",
            icon = NULL
          ),
          menuItem(
            text = "Playground",
            tabName = "playground",
            icon = NULL
          ),
          menuItem(
            text = strong("Scenario"),
            menuSubItem(
              text = "Irace Options",
              tabName = "scenario_irace_options",
              icon = NULL
            ),
            menuSubItem(
              text = "Parameters",
              tabName = "scenario_parameters",
              icon = NULL
            ),
            menuSubItem(
              text = "Target Scripts",
              tabName = "scenario_target_scripts",
              icon = NULL
            ),
            menuSubItem(
              text = "Train Instances",
              tabName = "scenario_train_instances",
              icon = NULL
            ),
            menuSubItem(
              text = "Initial Configurations",
              tabName = "scenario_initial_configurations",
              icon = NULL
            ),
            menuSubItem(
              text = "Forbidden",
              tabName = "scenario_forbidden",
              icon = NULL
            ),
            menuSubItem(
              text = "Testing",
              tabName = "scenario_testing",
              icon = NULL
            )
          ),

          menuItem(
            text = strong("Execution"),
            menuSubItem(
              text = "Irace output",
              tabName = "execution_irace_output",
              icon = NULL
            ),
            menuSubItem(
              text = "History",
              tabName = "execution_history",
              icon = NULL
            )
          ),

          menuItem(
            text = strong("Report"),
            menuSubItem(
              text = "Summary",
              tabName = "report",
              icon = NULL
            ),
            menuSubItem(
              text = "User Notes",
              tabName = "report_user_section",
              icon = NULL
            )
          ),

          menuItem(
            text = strong("Visualization"),
            menuItem(
              text = strong("Performance"),
              menuSubItem(
                text = "Configuration",
                tabName = "visualization_by_config",
                icon = NULL
              ),
              menuSubItem(
                text = "Instance",
                tabName = "visualization_by_instance",
                icon = NULL
              )
            ),
            menuSubItem(
              text = "Sandbox",
              tabName = "visualization_sandbox",
              icon = NULL
            ),
            menuSubItem(
              text = "Filter",
              tabName = "visualization_filter",
              icon = NULL
            )
          )
        )
      )
    }
  )
)

FooterApp <- R6::R6Class(
  classname = "FooterApp",
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
          ),
          div(
            class = "ml-4",
            "Current Execution: ",
            uiOutput(
              outputId = ns("execution"),
              inline = TRUE
            )
          ),
          div(
            class = "ml-4",
            "Current Sandbox: ",
            uiOutput(
              outputId = ns("sandbox"),
              inline = TRUE
            )
          )
        ),
        right = tagList(
          "Irace:",
          uiOutput(
            outputId = ns("status"),
            inline = TRUE
          )
        ),
        fixed = TRUE
      )
    },

    server = function(input, output, session, store) {
      output$scenario <- renderUI({
        shiny::validate(
          need(global_emitter$value(global_events$current_scenario), "Nothing")
        )
        store$pg$get_current_scenario()$get_name()
      })

      output$execution <- renderUI({
        shiny::validate(
          need(store$currentExecution, "Nothing")
        )
        store$currentExecution$get_name()
      })

      output$sandbox <- renderUI({
        shiny::validate(
          need(store$sandbox, "Nothing")
        )
        store$sandbox$getName()
      })


      output$status <- renderUI({
        if (store$startIrace) {
          dashboardBadge(
            "Running",
            position = "right",
            color = "success",
            rounded = TRUE
          )
        } else {
          dashboardBadge(
            "Not running",
            position = "right",
            color = "danger",
            rounded = TRUE
          )
        }
      })

    }
  )
)