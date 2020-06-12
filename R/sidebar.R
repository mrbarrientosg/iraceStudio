Sidebar <- R6::R6Class(
  classname = "Sidebar",
  public = list(
    ui = function() {
      bs4Dash::bs4DashSidebar(
        title = h1("Irace Studio", style = "text-align:center; margin-bottom: 0;"),
        fixed = TRUE,
        elevation = 0,
        opacity = 0.8,
        bs4Dash::bs4SidebarMenu(
          bs4Dash::bs4SidebarMenuItem(
            text = "UI Options",
            tabName = "ui_options"
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = "Playground",
            tabName = "playground"
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = strong("Scenario"),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "Irace Options",
              tabName = "scenario_irace_options"
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "Parameters",
              tabName = "scenario_parameters"
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "Train Instances",
              tabName = "scenario_train_instances"
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "Target Scripts",
              tabName = "scenario_target_scripts"
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "Initial Configurations",
              tabName = "scenario_initial_configurations"
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "Forbidden",
              tabName = "scenario_forbidden"
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "Testing",
              tabName = "scenario_testing"
            )
          ),
  
          bs4Dash::bs4SidebarMenuItem(
            text = strong("Execution"),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "Irace output",
              tabName = "execution_irace_output"
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "History",
              tabName = "execution_history"
            )
          ),
  
          bs4Dash::bs4SidebarMenuItem(
            text = strong("Visualization"),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "Sandbox",
              tabName = "visualization_sandbox"
            ),
            bs4Dash::bs4SidebarMenuItem(
              text = strong("Performance"),
              bs4SidebarMenuSubItem(
                text = "Configuration",
                tabName = "visualization_perf_config"
              )
            )
          ),
  
          bs4Dash::bs4SidebarMenuItem(
            text = strong("Report"),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "Summary",
              tabName = "report"
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "User Section",
              tabName = "report_user_section"
            )
          )
        )
      )
    }
  )
)