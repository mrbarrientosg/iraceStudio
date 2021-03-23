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
    events = NULL,
    logs = NULL,
    logger_path = NULL,

    initialModal = function(input) {
      if (is.null(isolate(private$store$pg))) {
        import_volume <- getVolumes()()
        workspace_volume <- c("workspace" = isolate(private$store$gui$workspace_path), import_volume)
        showModal(
          modalDialog(
            title = "Welcome to Irace Studio",
            p("To start, you must select/create playground, click on:"),
            HTML("<ul>
                 <li>Select, to open a previuously saved playground in your workspace (Rds file)</li>
                 <li>Import, to create new playground from an irace Rdata file</li>
                 <li>New, to create a new playground and start an scenario from scratch</li>
                 </ul>"),
            p("If its your first time using Irace Studio, click on New and follow the instructions in Home!"),
            footer = tagList(
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
              ),
              bs4Dash::actionButton(inputId = "new", label = "New", status = "primary")
            )
          )
        )

        shinyFileChoose(
          input = input,
          id = "select",
          roots = workspace_volume,
          filetypes = "rds"
        )
        shinyFileChoose(
          input = input,
          id = "import",
          roots = import_volume,
          filetypes = "rds"
        )
      }
    },

    validateName = function(name, path) {
      files <- list.files(path)
      return(tolower(name) %in% tolower(files))
    },

    setupModules = function() {
      shinybusy::show_modal_spinner(text = "Loading workspace...")
      private$body$setupModules(private$store, private$events)
      shinybusy::remove_modal_spinner()
    },

    create_playground = function(name = NULL, pg = NULL) {
      if (is.null(name)) {
        private$store$pg <- Playground$new(playground = pg)
      } else {
        private$store$pg <- Playground$new(name = name)
      }
      update_reactive_counter(private$events$update_scenarios)
      update_reactive_counter(private$events$change_scenario)
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
        pg = NULL,
        gui = GUIOptions$new(),
        app = self,
        playground_name = "",
        irace_results = NULL,
        current_execution = NULL,
        sandbox = NULL,
        irace_process = NULL,
        current_scenario = NULL
      )

      private$events <- reactiveValues(
        update_scenarios = 0,
        change_scenario = 0,
        update_executions = 0,
        update_parameters = 0,
        update_sandboxes = 0,
        update_report = 0,
        update_sandbox = 0,
        is_irace_running = FALSE,
        is_irace_alive = reactiveTimer(intervalMs = 1050),
        copy = list(id = NULL, plot = NULL, table = NULL)
      )
    },

    ui = function() {
      dashboardPage(
        title = "Irace Studio",
        dark = FALSE,
        # freshTheme = common_theme,
        header = private$navbar$ui("navbar"),
        sidebar = private$sidebar$ui(),
        body = private$body$ui(),
        footer = private$footer$ui("footer"),
        controlbar = private$controlbar$ui("controlbar")
      )
    },

    server = function(input, output, session) {
      shinyhelper::observe_helpers(withMathJax = TRUE)
      logger::log_shiny_input_changes(input)

      private$navbar$call(id = "navbar", store = private$store)
      private$controlbar$call(id = "controlbar", store = private$store, events = private$events)
      private$footer$call(id = "footer", store = private$store, events = private$events)

      work_path <- isolate(private$store$gui$workspace_path)
      import_volume <- getVolumes()()
      workspace_volume <- c("workspace" = work_path, import_volume)

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
              alert_error("Playground name is empty.")
              return(invisible())
            }

            if (private$validateName(name, work_path)) {
              shinyalert(
                title = "Error",
                text = "Playground name is repeated.",
                closeOnEsc = FALSE,
                type = "error",
                callbackR = function() {
                  private$initialModal(input)
                }
              )
              return(invisible())
            }

            private$setupModules()
            # FIXME: check if this is correct here
            # MATIAS: Workspace check if exist when the app initialize
            # dir.create(paste0(work_path, "/", name))
            private$create_playground(name = name)
          }
        )
      })

      observeEvent(input$select, {
        if (!is.integer(input$select)) {
          file <- parseFilePaths(roots = workspace_volume, input$select)
          pg <- readRDS(file = file$datapath)

          if (is.null(pg$irace_studio) || !pg$irace_studio) {
            alert_error("Bad Irace Studio playground.")
            return()
          }

          removeModal()

          private$setupModules()
          private$create_playground(pg = pg)
        }
      })

      observeEvent(input$import, {
        if (!is.integer(input$import)) {
          file <- parseFilePaths(roots = import_volume, input$import)
          pg <- readRDS(file = file$datapath)

          if (is.null(pg$irace_studio) || !pg$irace_studio) {
            alert_error("Bad Irace Studio playground.")
            return()
          }

          removeModal()

          private$setupModules()
          private$create_playground(pg = pg)
        }
      })

      # Javascript code: Before the user closes the browser tab, a warning
      # alert will prompt indicating if he wants close irace studio and
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
        # stopApp()
      })

      if (app_prod()) {
        private$initialModal(input)
      } else {
        private$setupModules()
        private$create_playground(name = "dev-test")
      }
      session$userData$sidebar <- reactive(input$sidebar)
    },

    setupLogger = function() {
      # log_layout(layout_json())
      gui <- isolate(private$store$gui)

      log_threshold(TRACE)

      time <- format(Sys.time(), "%d%m%Y%H%M%S")

      path <- file.path(gui$options_path, "logs")

      if (!dir.exists(path)) {
        dir.create(path)
      }

      path <- sprintf("%s/log-%s.log", path, time)
      log_appender(appender_file(file = path))

      return(path)
    },

    setup = function() {
      logger <- layout_glue_generator(format = "{level} [{format(time, \"%Y-%m-%d %H:%M:%S\")}] {msg}")
      log_layout(logger)
      # if (get_option("debug", FALSE)) {
      #   log_threshold(TRACE)
      # } else {
      #   log_threshold(FATAL)
      # }
      log_threshold(TRACE)

      gui <- isolate(private$store$gui)
      pg <- isolate(private$store$pg)

      gui$createWorkspaceDirectory()

      # TODO: Implements logger in a correct way.
      # private$logger_path <- self$setupLogger()

      log_info("Irace Studio Start")

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

      # FIXME: check what happens if the app gets closed unexpectedly. It would be better if the scenario data is saved when something changes
      # MATIAS: Saving the data every time when something changes will affect the performance, but I think to overwrite Ctrl+S or add a timer when end trigger the action to save data.
      gui$save()

      if (!is.null(pg)) {
        path <- file.path(gui$workspace_path, pg$get_name())

        if (!dir.exists(path)) {
          dir.create(path)
        }

        path <- file.path(path, paste0(pg$get_name(), ".rds"))

        if (file.exists(path)) {
          file.remove(path)
        }

        pg$save(path)
      }

      irace_process <- isolate(private$store$irace_process)

      if (!is.null(irace_process)) {
        irace_process$kill_tree()
        irace_process$finalize()
      }

      unlink(file.path(gui$options_path, ".Fimages"), recursive = TRUE, force = TRUE)
      unlink(file.path(gui$options_path, ".Pimages"), recursive = TRUE, force = TRUE)
      if (!get_option("debug", FALSE)) {
        unlink(pkg$temp_folder, recursive = TRUE, force = TRUE)
      }
    }
  )
)
