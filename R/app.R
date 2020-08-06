App <- R6::R6Class(
  classname = "App",
  cloneable = FALSE,

  private = list(
    navbar = NULL,
    sidebar = NULL,
    body = NULL,
    store = NULL,
    logs = NULL,
    logger_path = NULL
  ),

  public = list(
    initialize = function() {
      private$navbar <- Navbar$new()
      private$sidebar <- Sidebar$new()
      private$body <- Body$new()
      private$store <- reactiveValues(
        pg = playground$new(name = "dev-playground"),
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
      
      onSessionEnded(function() {
        #self$destroy()
      })
    },
  
    setupLogger = function() {
      # log_layout(layout_json())
      gui <- isolate(private$store$gui)
    
      log_threshold(TRACE)
    
      time <- format(Sys.time(), "%d%m%Y%H%M%S")
    
      path <- file.path(gui$workspacePath, "logs")
    
      if (!dir.exists(path)) {
        dir.create(path)
      }
    
      path <- sprintf("%s/log-%s.log", path, time)
      log_appender(appender_file(file = path))
    
      return(path)
    },
  
    setup = function() {
      self$createWorkspaceDirectory()
      private$logger_path <- self$setupLogger()
    
      log_info("Shiny app start")
    
      log_info("Check gui-data exists")
      if (file.exists(".gui-data.Rdata")) {
        log_info("Load gui-data.Rdata")
        # load(file = ".gui-data.Rdata")
      }
    
      log_info("Create workspace directory")
    
      gui <- isolate(private$store$gui)
      pg <- isolate(private$store$pg)
    
      # FIXME: This works only in dev mode
      path <- file.path(
        gui$workspacePath, pg$get_name(),
        paste0(pg$get_name(), ".rds")
      )
    
      if (file.exists(path)) {
        pg <- readRDS(file = path)
        private$store$pg <- playground$new(playground = pg)
      }
    
      private$logs <- file.path(gui$workspacePath, "logs")
      # output <- file.path(logs, "output.log")
      # output <- file(output, open = "w")
    
      # sink(file = output, type = "message")
    },
  
    createWorkspaceDirectory = function() {
      gui <- isolate(private$store$gui)
    
      path <- gui$workspacePath
    
      if (is.null(gui$workspacePath) ||
        gui$workspacePath == "") {
        path <- file.path(getwd(), "workspace")
      }
    
      if (!dir.exists(path)) {
        dir.create(path)
      }
    
      return(path)
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
    
      save(gui, file = ".gui-data.Rdata")
    
      path <- file.path(
        gui$workspacePath, pg$get_name()
      )
    
      if (!dir.exists(path)) {
        dir.create(path)
      }
    
      path <- file.path(path, paste0(pg$get_name(), ".rds"))
    
      if (file.exists(path)) {
        file.remove(path)
      }
    
      pg$save(path)
    
      if (exists("iraceProcess")) {
        iraceProcess$kill()
        iraceProcess$finalize()
      }
    
      reportFolder <- file.path(
        gui$workspacePath, pg$get_name(),
        pg$get_scenario_name(), "Reports"
      )
    
      unlink(file.path(reportFolder, "figure"), recursive = TRUE)
    
      if (file.exists(file.path(reportFolder, "irace_report.tex"))) {
        file.remove(file.path(reportFolder, "irace_report.tex"))
      }
    
      if (file.exists(file.path(reportFolder, "irace_report.pdf"))) {
        file.remove(file.path(reportFolder, "irace_report.pdf"))
      }
    
      unlink(".Fimages", recursive = TRUE, force = TRUE)
      unlink(".Pimages", recursive = TRUE, force = TRUE)
      # unlink(pkg_env$tempFolder, recursive = TRUE, force = TRUE)
    }
  )
)

