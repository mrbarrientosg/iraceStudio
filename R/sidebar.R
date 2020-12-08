Sidebar <- R6::R6Class(
  classname = "Sidebar",
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
          
          bs4Dash::bs4SidebarMenuItem(
            text = strong("Report"),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "Summary",
              tabName = "report"
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "User Notes",
              tabName = "report_user_section"
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
          ),

          menuItem(
            text = strong("Report"),
            menuSubItem(
              text = "Summary",
              tabName = "report",
              icon = NULL
            ),
            menuSubItem(
              text = "User Section",
              tabName = "report_user_section",
              icon = NULL
            )
          )
        )
      )
    }
  )
)