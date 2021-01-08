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
        importVolume <- getVolumes()()
        workspaceVolume <- c("workspace" = isolate(private$store$gui$workspacePath), importVolume)
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
              iraceStudio::actionButton(inputId = "new", label = "New", class = "btn-primary")
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
    },

    validateName = function(name, path) {
      files <- list.files(path)
      return(tolower(name) %in% tolower(files))
    },

    setupModules = function() {
      shinybusy::show_modal_spinner(text = "Loading workspace...")
      private$body$setupModules(private$store)
      shinybusy::remove_modal_spinner()
    }
  ),

  public = list(
    initialize = function() {
      private$navbar <- Navbar$new()
      private$sidebar <- Sidebar$new()
      private$body <- Body$new()

      private$store <- reactiveValues(
        pg = NULL
      )
    },

    ui = function() {
      dashboardPage(
        title = "Irace Vizz",
        dark = FALSE,
        #freshTheme = common_theme,
        header = private$navbar$ui("navbar"),
        sidebar = private$sidebar$ui(),
        body = private$body$ui()
      )
    },

    server = function(input, output, session) {
      shinyhelper::observe_helpers(withMathJax = TRUE)

      private$store$playgroundName <- ""
      private$store$updateSandbox <- 0

      # actions
      private$store$currentScenario <- NULL
      private$store$iraceResults <- NULL
      private$store$currentExecution <- NULL

      private$navbar$call(id = "navbar", store = private$store)

      workPath <- isolate(private$store$gui$workspacePath)
      importVolume <- getVolumes()()
      workspaceVolume <- c("workspace" = workPath, importVolume)

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

            if (private$validateName(name, workPath)) {
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
            #FIXME: check if this is correct here
            #MATIAS: Workspace check if exist when the app initialize
            #dir.create(paste0(workPath, "/", name))
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
            return()
          }

          removeModal()

          private$setupModules()
          private$store$pg <- playground$new(playground = pg)
        }
      })

      observeEvent(input$import, {
        if (!is.integer(input$import)) {
          file <- parseFilePaths(roots = importVolume, input$import)
          pg <- readRDS(file = file$datapath)

          if (is.null(pg$.iraceStudio) || !pg$.iraceStudio) {
            alert.error("Bad Irace Studio playground.")
            return()
          }

          removeModal()

          private$setupModules()
          private$store$pg <- playground$new(playground = pg)
        }
      })

      if (app_prod()) {
        private$initialModal(input)
      } else {
        private$setupModules()
        private$store$pg <- playground$new("dev-test")
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
      #private$logger_path <- self$setupLogger()

      log_info("Irace Studio Start")

      # output <- file.path(logs, "output.log")
      # output <- file(output, open = "w")

      # sink(file = output, type = "message")
    }
  )
)
