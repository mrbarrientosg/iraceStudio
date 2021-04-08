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

    filter_view = NULL,
    performance_instance = NULL,
    performance_config = NULL,

    initialize = function() {
      self$overview <- Overview$new("overview")

      self$filter_view <- FilterView$new("visualization_filter")
      self$performance_instance <- PerformanceInstanceView$new("visualization_by_instance")
      self$performance_config <- PerformanceConfigView$new("visualization_by_config")
    },

    ui = function() {
      dashboardBody(
        irace_vizz_resources(),
        tabItems(
          tabItem(
            tabName = "overview",
            self$overview$ui()
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
          )
        )
      )
    },

    setupModules = function(store, events) {
      self$overview$call(store = store)

      self$filter_view$call(store = store, events = events)
      self$performance_instance$call(store = store, events = events)
      self$performance_config$call(store = store, events = events)
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
          ),
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
          menuItem(
            text = "Filter",
            tabName = "visualization_filter",
            icon = NULL
          )
        )
      )
    }
  )
)
