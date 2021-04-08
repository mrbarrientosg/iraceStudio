App <- R6::R6Class(
  classname = "App",
  cloneable = FALSE,
  private = list(
    navbar = NULL,
    sidebar = NULL,
    body = NULL,
    store = NULL,
    logs = NULL,
    events = NULL,
    logger_path = NULL,

    initialModal = function(input) {
      volumes <- c("Home" = path.expand("~"), getVolumes()())
      showModal(
        modalDialog(
          title = "Welcome to Irace Vizz",
          footer = tagList(
            shinyFilesButton(
              id = "select",
              label = "Select",
              title = "Select a Scenario",
              multiple = FALSE,
              buttonType = "primary"
            )
          )
        )
      )

      shinyFileChoose(
        input = input,
        id = "select",
        roots = volumes
      )
    },

    validateName = function(name, path) {
      files <- list.files(path)
      return(tolower(name) %in% tolower(files))
    },

    setupModules = function() {
      shinybusy::show_modal_spinner(text = "Loading scenario...")
      private$navbar$call(id = "navbar", store = private$store)
      private$body$setupModules(private$store, private$events)
      shinybusy::remove_modal_spinner()
    }
  ),

  public = list(
    initialize = function() {
      private$navbar <- Navbar$new()
      private$sidebar <- Sidebar$new()
      private$body <- Body$new()

      private$store <- reactiveValues(
        irace_results = NULL,
        current_scenario = NULL,
        sandbox = NULL
      )

      private$events <- reactiveValues(
        update_sandbox = 0
      )
    },

    ui = function() {
      dashboardPage(
        title = "Irace Vizz",
        dark = FALSE,
        # freshTheme = common_theme,
        header = private$navbar$ui("navbar"),
        sidebar = private$sidebar$ui(),
        body = private$body$ui()
      )
    },

    server = function(input, output, session) {
      # shinyhelper::observe_helpers(withMathJax = TRUE)

      volumes <- c("Home" = path.expand("~"), getVolumes()())

      observeEvent(input$select, {
        if (!is.integer(input$select)) {
          file <- parseFilePaths(roots = volumes, input$select)
          load(file$datapath)
          private$store$irace_results <- iraceResults
          private$store$sandbox <- Sandbox$new()
          rm(iraceResults)
          private$setupModules()
        }
      })

      if (is.null(golem::get_golem_options("data"))) {
        private$initialModal(input)
      } else {
        isolate({
          load(golem::get_golem_options("data"))
          private$store$irace_results <- iraceResults
          private$store$sandbox <- Sandbox$new()
          rm(iraceResults)
        })
        private$setupModules()
      }

      onSessionEnded(function() {
        stopApp()
      })

      session$userData$sidebar <- reactive(input$sidebar)
    },

    setup = function() {
      logger <- layout_glue_generator(format = "{level} [{format(time, \"%Y-%m-%d %H:%M:%S\")}] {msg}")
      log_layout(logger)
      if (get_option("debug", FALSE)) {
        log_threshold(TRACE)
      } else {
        log_threshold(FATAL)
      }

      # TODO: Implements logger in a correct way.
      # private$logger_path <- self$setupLogger()

      log_info("Irace Studio Start")

      # output <- file.path(logs, "output.log")
      # output <- file(output, open = "w")

      # sink(file = output, type = "message")
    }
  )
)