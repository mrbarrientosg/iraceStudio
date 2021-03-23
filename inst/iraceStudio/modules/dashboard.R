Navbar <- R6::R6Class( # nolint
  classname = "Navbar",
  inherit = Component,
  public = list(
    ui = function(id) {
      ns <- NS(id)

      dashboardHeader(
        title = dashboardBrand(
          title = h2("Irace Studio", style = "text-align:center; margin-bottom: 0;")
        ),
        fixed = TRUE,
        h4(
          textOutput(
            outputId = ns("playground_name")
          ),
          style = "text-align: center; flex: 1 0 auto; margin-top: 5px;"
        )
      )
    },

    server = function(input, output, session, store) {
      output$playground_name <- renderText(store$playground_name)
    }
  )
)

Body <- R6::R6Class( # nolint
  classname = "Body",
  public = list(
    home_view = NULL,
    ui_options_view = NULL,
    playground_view = NULL,
    irace_options_view = NULL,
    parameters_view = NULL,
    train_instances_view = NULL,
    target_scripts_view = NULL,
    initial_configurations_view = NULL,
    forbidden_view = NULL,
    testing_view = NULL,

    irace_output_view = NULL,
    executions_history_view = NULL,

    sandbox_view = NULL,
    filter_view = NULL,
    performance_instance = NULL,
    performance_config = NULL,

    report_view = NULL,
    user_section_view = NULL,

    initialize = function() {
      self$home_view <- HomeView$new("welcome")
      self$ui_options_view <- UIOptionsView$new("ui_options")
      self$playground_view <- PlaygroundView$new("playground")
      self$irace_options_view <- IraceOptionsView$new("scenario_irace_options")
      self$parameters_view <- ParametersView$new("scenario_parameters")
      self$train_instances_view <- TrainInstancesView$new("scenario_train_instances")
      self$target_scripts_view <- TargetScriptsView$new("scenario_target_scripts")
      self$initial_configurations_view <- InitialConfigurationsView$new("scenario_initial_configurations")
      self$forbidden_view <- ForbiddenView$new("scenario_forbidden")
      self$testing_view <- TestingView$new("scenario_testing")
      self$irace_output_view <- IraceOutputView$new("execution_irace_output")
      self$executions_history_view <- ExecutionsHistoryView$new("execution_history")

      self$sandbox_view <- SandboxView$new("visualization_sandbox")
      self$filter_view <- FilterView$new("visualization_filter")
      self$performance_instance <- PerformanceInstanceView$new("visualization_by_instance")
      self$performance_config <- PerformanceConfigView$new("visualization_by_config")

      self$report_view <- ReportView$new("report")
      self$user_section_view <- UserSectionView$new("report_user_section")
    },

    ui = function() {
      dashboardBody(
        irace_studio_resources(),
        tabItems(
          tabItem(
            tabName = "home",
            self$home_view$ui()
          ),
          tabItem(
            tabName = "ui_options",
            self$ui_options_view$ui()
          ),
          tabItem(
            tabName = "playground",
            self$playground_view$ui()
          ),
          tabItem(
            tabName = "scenario_irace_options",
            self$irace_options_view$ui()
          ),
          tabItem(
            tabName = "scenario_parameters",
            self$parameters_view$ui()
          ),
          tabItem(
            tabName = "scenario_train_instances",
            self$train_instances_view$ui()
          ),
          tabItem(
            tabName = "scenario_target_scripts",
            self$target_scripts_view$ui()
          ),
          tabItem(
            tabName = "scenario_initial_configurations",
            self$initial_configurations_view$ui()
          ),
          tabItem(
            tabName = "scenario_forbidden",
            self$forbidden_view$ui()
          ),
          tabItem(
            tabName = "scenario_testing",
            self$testing_view$ui()
          ),
          tabItem(
            tabName = "execution_irace_output",
            self$irace_output_view$ui()
          ),
          tabItem(
            tabName = "execution_history",
            self$executions_history_view$ui()
          ),
          tabItem(
            tabName = "visualization_sandbox",
            self$sandbox_view$ui()
          ),
          tabItem(
            tabName = "visualization_filter",
            self$filter_view$ui()
          ),
          tabItem(
            tabName = "visualization_by_config",
            self$performance_config$ui()
          ),
          tabItem(
            tabName = "visualization_by_instance",
            self$performance_instance$ui()
          ),
          tabItem(
            tabName = "report",
            self$report_view$ui()
          ),
          tabItem(
            tabName = "report_user_section",
            self$user_section_view$ui()
          )
        )
      )
    },

    setupModules = function(store, events) {
      self$home_view$call(store = store, events = events)
      self$ui_options_view$call(store = store, events = events)
      self$playground_view$call(store = store, events = events)
      self$irace_options_view$call(store = store, events = events)
      self$parameters_view$call(store = store, events = events)
      self$train_instances_view$call(store = store, events = events)
      self$target_scripts_view$call(store = store, events = events)
      self$initial_configurations_view$call(store = store, events = events)
      self$forbidden_view$call(store = store, events = events)
      self$testing_view$call(store = store, events = events)

      self$irace_output_view$call(store = store, events = events)
      self$executions_history_view$call(store = store, events = events)

      self$sandbox_view$call(store = store, events = events)
      self$filter_view$call(store = store, events = events)
      self$performance_instance$call(store = store, events = events)
      self$performance_config$call(store = store, events = events)

      self$report_view$call(store = store, events = events)
      self$user_section_view$call(store = store, events = events)
    }
  )
)

ControlBar <- R6::R6Class( # nolint
  classname = "ControlBar",
  inherit = Component,
  public = list(
    execution_select = NULL,
    sandbox_select = NULL,

    initialize = function() {
      self$execution_select <- ExecutionSelect$new()
      self$sandbox_select <- SandboxSelect$new()
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
            self$execution_select$ui(input_id = ns("executions")),
            self$sandbox_select$ui(input_id = ns("sandboxes")),
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

    server = function(input, output, session, store, events) {
      self$execution_select$call(
        id = "executions",
        store = store,
        events = events
      )

      self$sandbox_select$call(
        id = "sandboxes",
        store = store,
        events = events
      )

      observeEvent(input$scenarioPicker, {
        req(input$scenarioPicker)
        store$pg$change_current_scenario(input$scenarioPicker)
        update_reactive_counter(events$current_scenario)
        pkg$output_log <- NULL
      })

      observeEvent(events$is_irace_running, {
        if (events$is_irace_running) {
          disable(id = "scenarioPicker")
        } else {
          enable(id = "scenarioPicker")
        }
      })

      observeEvent(c(store$pg, events$update_scenarios),
        {
          scenarios <- lapply(store$pg$get_scenarios(), function(scenario) {
            scenario$get_name()
          })

          scenarios_id <- lapply(store$pg$get_scenarios(), function(scenario) {
            scenario$get_id()
          })

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
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )
    }
  )
)

Sidebar <- R6::R6Class( # nolint
  classname = "Sidebar",
  public = list(
    ui = function() {
      dashboardSidebar(
        minified = FALSE,
        # elevation = 0,
        id = "sidebarState",
        sidebarMenu(
          id = "sidebar",
          menuItem(
            text = "Home",
            tabName = "home",
            icon = NULL,
            selected = TRUE
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

Footer <- R6::R6Class( # nolint
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

    server = function(input, output, session, store, events) {
      output$scenario <- renderUI({
        shiny::validate(
          need(events$change_scenario, "Nothing"),
          need(store$pg, "Nothing")
        )
        store$pg$get_current_scenario()$get_name()
      })

      output$execution <- renderUI({
        shiny::validate(
          need(store$current_execution, "Nothing")
        )
        store$current_execution$get_name()
      })

      output$sandbox <- renderUI({
        shiny::validate(
          need(store$sandbox, "Nothing")
        )
        store$sandbox$get_name()
      })

      output$status <- renderUI({
        if (events$is_irace_running) {
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
