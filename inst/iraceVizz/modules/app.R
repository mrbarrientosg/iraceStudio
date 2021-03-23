App <- R6::R6Class(
  classname = "App",
  cloneable = FALSE,
  private = list(
    navbar = NULL,
    sidebar = NULL,
    body = NULL,
    controlbar = NULL,
    footer = NULL,
    store = NULL,
    logs = NULL,
    logger_path = NULL,

    initialModal = function(input) {
      if (length(isolate(private$store$scenarios)) == 0) {
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
      }
    },

    validateName = function(name, path) {
      files <- list.files(path)
      return(tolower(name) %in% tolower(files))
    },

    setupModules = function() {
      shinybusy::show_modal_spinner(text = "Loading scenario...")
      private$navbar$call(id = "navbar", store = private$store)
      private$controlbar$call(id = "controlbar", store = private$store)
      private$footer$call(id = "footer", store = private$store)
      private$body$setupModules(private$store)
      shinybusy::remove_modal_spinner()
    }
  ),

  public = list(
    initialize = function() {
      private$navbar <- Navbar$new()
      private$sidebar <- Sidebar$new()
      private$body <- Body$new()
      private$controlbar <- ControlBar$new()
      private$footer <- Footer$new()

      private$store <- reactiveValues(
        scenarios = list(),
        iraceResults = NULL,
        currentScenario = NULL
      )
    },

    ui = function() {
      dashboardPage(
        title = "Irace Vizz",
        dark = FALSE,
        # freshTheme = common_theme,
        header = private$navbar$ui("navbar"),
        sidebar = private$sidebar$ui(),
        body = private$body$ui(),
        controlbar = private$controlbar$ui("controlbar"),
        footer = private$footer$ui("footer")
      )
    },

    server = function(input, output, session) {
      # shinyhelper::observe_helpers(withMathJax = TRUE)

      volumes <- c("Home" = path.expand("~"), getVolumes()())

      observeEvent(input$select, {
        if (!is.integer(input$select)) {
          file <- parseFilePaths(roots = volumes, input$select)
          removeModal()
          shinyalert(
            title = "Scenario name",
            text = "Give a name to identify the scenario.",
            type = "input",
            inputType = "text",
            showCancelButton = TRUE,
            closeOnEsc = FALSE,
            callbackR = function(name) {
              if (is.logical(name) && !name) {
                private$initialModal(input)
                return(invisible())
              }

              if (is.null(name) || name == "") {
                alert_error("Scenario name is empty.")
                return(invisible())
              }

              # if (private$validateName(name, workPath)) {
              #   shinyalert(
              #     title = "Error",
              #     text = "Playground name is repeated.",
              #     closeOnEsc = FALSE,
              #     type = "error",
              #     callbackR = function() {
              #       private$initialModal(input)
              #     }
              #   )
              #   return(invisible())
              # }

              load(file$datapath)
              private$store$scenarios[[name]] <- iraceResults
              rm(iraceResults)
              private$setupModules()
            }
          )
        }
      })

      if (app_prod()) {
        private$initialModal(input)
      } else {
        isolate({
          load(app_sys("data/irace-acotsp.Rdata"))
          private$store$scenarios[["test"]] <- iraceResults
          rm(iraceResults)
        })
        private$setupModules()
      }
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
