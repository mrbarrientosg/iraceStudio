App <- R6::R6Class(
  classname = "App",
  cloneable = FALSE,

  private = list(
    navbar = NULL,
    sidebar = NULL,
    body = NULL,
    store = NULL,
    logs = NULL,
    logger_path = NULL,

    initialModal = function(input) {
      if (is.null(isolate(private$store$pg))) {
        workspaceVolume <- list(workspace = isolate(private$store$gui$workspacePath))
        importVolume <- list(root = getVolumes()())

        showModal(
          modalDialog(
            title = "Welcome to Irace Studio",
            "Create, select or import a playground in your workspace.",
            footer = tagList(
              actionButton(inputId = "new", label = "New", class = "btn-primary"),
              shinyFilesButton(
                id = "select",
                label = "Select",
                title = "Select a Playground",
                multiple = FALSE,
                buttonType = "outline-primary"
              ),
              shinyFilesButton(
                id = "import",
                label = "Import",
                title = "Import a Playground",
                multiple = FALSE,
                buttonType = "outline-primary"
              )
            )
          )
        )

        shinyFileChoose(
          input = input,
          id = "select",
          roots = workspaceVolume,
          filetypes = "rds"
        )
        shinyFileChoose(
          input = input,
          id = "import",
          roots = importVolume,
          filetypes = "rds"
        )
      }
    }
  ),

  public = list(
    initialize = function() {
      private$navbar <- Navbar$new()
      private$sidebar <- Sidebar$new()
      private$body <- Body$new()
      private$store <- reactiveValues(
        pg = NULL,
        gui = GUIOptions$new(),
        app = self
      )
    },

    ui = function() {
      bs4Dash::bs4DashPage(
        title = "Irace Studio",
        sidebar_mini = FALSE,
        navbar = private$navbar$ui("navbar"),
        sidebar = private$sidebar$ui(),
        body = private$body$ui(),
        enable_preloader = TRUE,
        loading_background = "#242939"
      )
    },

    server = function(input, output, session) {
      private$store$playgroundName <- ""
      private$store$startIrace <- FALSE
      private$store$iraceAlive <- reactiveTimer(intervalMs = 1050)
      private$store$copy <- list(id = NULL, plot = NULL, table = NULL)
      private$store$updateSandbox <- 0

      private$navbar$call(id = "navbar", store = private$store)
      private$body$setupModules(private$store)

      session$userData$sidebar <- reactive(input$sidebar)

      delay(200, {
        private$initialModal(input)
      })

      observeEvent(input$new, {
        removeModal()
        shinyalert(
          title = "Playground name",
          text = "Give a name to identify the playground.",
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
              alert.error("Playground name is empty.")
              return(invisible())
            }

            private$store$pg <- playground$new(name = name)
          }
        )
      })

      observeEvent(input$select, {
        if (!is.integer(input$select)) {
          file <- parseFilePaths(roots = workspaceVolume, input$select)
          pg <- readRDS(file = file$datapath)

          if (is.null(pg$.iraceStudio) || !pg$.iraceStudio) {
            alert.error("Bad Irace Studio playground.")
          }

          private$store$pg <- playground$new(playground = pg)

          removeModal()
        }
      })

      observeEvent(input$import, {
        if (!is.integer(input$import)) {
          file <- parseFilePaths(roots = workspaceVolume, input$import)
          pg <- readRDS(file = file$datapath)

          if (is.null(pg$.iraceStudio) || !pg$.iraceStudio) {
            alert.error("Bad Irace Studio playground.")
          }

          private$store$pg <- playground$new(playground = pg)

          removeModal()
        }
      })

      # Javascript code: Before the user closes the browser tab, a warning
      # alert will prompt indicating if he wants close all irace studio and
      # irace, if this is still running.
      runjs(code = '
          window.addEventListener("beforeunload", (event) => {
            event.preventDefault();

            event.returnValue = "If Irace is running, will stop the execution.";

            return "If Irace is running, will stop the execution.";
          })
      ')

      onSessionEnded(function() {
        self$destroy()
        stopApp()
      })
    },

    setupLogger = function() {
      # log_layout(layout_json())
      gui <- isolate(private$store$gui)

      log_threshold(TRACE)

      time <- format(Sys.time(), "%d%m%Y%H%M%S")

      path <- file.path(gui$optionsPath, "logs")

      if (!dir.exists(path)) {
        dir.create(path)
      }

      path <- sprintf("%s/log-%s.log", path, time)
      log_appender(appender_file(file = path))

      return(path)
    },

    setup = function() {
      log_threshold(FATAL)

      gui <- isolate(private$store$gui)
      pg <- isolate(private$store$pg)

      gui$createWorkspaceDirectory()

      # TODO: Implements logger in a correct way.
      #private$logger_path <- self$setupLogger()

      log_info("Shiny app start")

      # output <- file.path(logs, "output.log")
      # output <- file(output, open = "w")

      # sink(file = output, type = "message")
    },

    destroy = function() {
      gui <- isolate(private$store$gui)
      pg <- isolate(private$store$pg)

      # sink(NULL, type = "message")

      # output <- file.path(private$logs, "output.log")

      # if (file.exists(output)) {
      #  cat(
      #    paste(readLines(output), collapse = "\n"),
      #    file = private$logger_path,
      #    append = TRUE,
      #    fill = TRUE
      #  )
      #  file.remove(output)
      # }

      gui$save()

      if (!is.null(pg)) {
        path <- file.path(gui$workspacePath, pg$get_name())

        if (!dir.exists(path)) {
          dir.create(path)
        }

        path <- file.path(path, paste0(pg$get_name(), ".rds"))

        if (file.exists(path)) {
          file.remove(path)
        }

        pg$save(path)
      }

      iraceProcess <- isolate(private$store$iraceProcess)

      if (!is.null(iraceProcess)) {
        iraceProcess$kill()
        iraceProcess$finalize()
      }

      unlink(file.path(gui$optionsPath, ".Fimages"), recursive = TRUE, force = TRUE)
      unlink(file.path(gui$optionsPath, ".Pimages"), recursive = TRUE, force = TRUE)
      # unlink(pkg_env$tempFolder, recursive = TRUE, force = TRUE)
    }
  )
)
