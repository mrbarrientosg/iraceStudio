TestingView <- R6::R6Class(
  classname = "TestingView",
  inherit = View,
  public = list(
    testingOptions = NULL,

    initialize = function(id) {
      super$initialize(id)
      self$testingOptions <- IraceOptionTab$new()
    },

    ui = function() {
      ns <- NS(self$id)

      tagList(
        div(class = "sub-header",
            h2("Testing"),
            HTML("Activate the execution of a test phase in irace for evaluating the best configurations on a different set of problem instances.<br>
                 For more information, go to the irace package <a href=\"https://cran.r-project.org/package=irace/vignettes/irace-package.pdf\" target=\"_blank\">user guide</a> ")
            ),
        fluidRow(
          box(
            inputId = ns("testingInstances"),
            title = strong("Testing Options"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            uiOutput(outputId = ns("content"))
          ),
          box(
            inputId = ns("testingInstances"),
            title = strong("Test Instance File"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            fluidRow(
              class = "sub-header",
              column(
                width = 12,
                class = "d-flex align-items-end justify-content-end",
                shinyDirButton(
                  id = ns("dir"),
                  label = "Import Dir",
                  title = "Select a directory",
                  buttonType = "outline-primary"
                ),
                importButton(
                  inputId = ns("load"),
                  label = "Import File",
                  style = "margin-left: 5px;"
                ),
                shinyDirButton(
                  id = ns("absolute"),
                  label = "Add path",
                  title = "Select a directory",
                  buttonType = "outline-primary",
                  style = "margin-left: 5px;"
                ),
                exportButton(
                  inputId = ns("export"),
                  filename = "instances.txt",
                  style = "margin-left: 5px;"
                ),
                clear_button(inputId = ns("clear"), style = "margin-left: 5px;")
              )
            ),
            tags$textarea(
              id = ns("source_instances_file"),
              style = "width: 100%; height: 500px;"
            )
          )
        )
      )
    },

    server = function(input, output, session, store) {
      ns <- session$ns

      clear <- callModule(
        module = clear_button_sv,
        id = "clear",
        message = "This action will remove all instances. Are you sure?."
      )

      obs_value <- reactiveVal(value = FALSE)

      volumes <- c("Home"=path.expand('~'), getVolumes()())

      shinyDirChoose(input, "dir", roots = volumes)
      shinyDirChoose(input, "absolute", roots = volumes)
      shinyFileSave(input = input, id = "export", roots = volumes)
      shinyFileChoose(input, "load", roots = volumes)

      observeEvent(input$dir, {
        if (!is.integer(input$dirPath)) {
          dir <- parseDirPath(roots = volumes, input$dir)
          files <- list.files(path = dir, full.names = TRUE)
          updateTextAreaInput(
            session = session,
            inputId = "source_instances_file",
            value = paste(files, collapse = "\n")
          )
        }
      })

      observeEvent(input$absolute, {
        if (!is.integer(input$absolute)) {
          dir <- parseDirPath(roots = volumes, input$absolute)
          lines <- strsplit(input$source_instances_file, "\n")
          output <- c()

          for (line in lines[[1]]) {
            .line <- line
            if (!fs::is_absolute_path(line)) {
              .line <- file.path(dir, line)
            }
            output <- c(output, .line)
          }

          updateTextAreaInput(
            session = session,
            inputId = "source_instances_file",
            value = paste0(output, collapse = "\n")
          )
        }
      })

      observeEvent(input$export, {
        if (!is.integer(input$export)) {
          file <- parseSavePath(roots = volumes, selection = input$export)
          log_debug("Exporting test instances file to {file$datapath}")
          create_test_instances_file(path = file$datapath, pg = store$pg, name = NULL)
          log_debug("Test instances file exported successfully")
          shinyalert(
            title = "Exported",
            text = "The file is exported successfully.",
            type = "success"
          )
        }
      })

      observeEvent(input$load, {
        if (!is.integer(input$load)) {
          file <- tryCatch({
            parseFilePaths(roots = volumes, input$load)
          }, error = function(err) {
            log_error("{err}")
            return(NULL)
          })

          if (is.null(file)) {
            alert.error("Can't load testing instances file, check if the file format is correct.")
            return(invisible())
          }

          log_info("Importing paremeter file from {file$datapath}")
          source <- readLines(file$datapath)
          source <- paste(source, collapse = "\n")
          updateTextAreaInput(
            session = session,
            inputId = "source_instances_file",
            value = source
          )
        }
      })

      output$content <- renderUI({
        shiny::validate(
          need(store$pg, "")
        )

        global_emitter$value(global_events$current_scenario)

        obs_value(TRUE)
        self$testingOptions$ui(inputId = ns("testing"), "Testing", store, FALSE)
      })

      observeEvent(c(global_emitter$value(global_events$current_scenario), store$pg), {
        updateTextAreaInput(
          session = session,
          inputId = "source_instances_file",
          value = store$pg$get_test_instances()
        )
      })

      observeEvent(input$source_instances_file,
                   store$pg$set_test_instances(input$source_instances_file),
                   ignoreInit = TRUE)

      observe({
        if (obs_value()) {
          self$testingOptions$call(
            id = "testing",
            store = store,
            .section = "Testing",
            update = NULL,
            isFast = FALSE
          )
          obs_value(FALSE)
        }
      })

      observeEvent(clear$action, {
        log_debug("Removing test instances")

        updateTextAreaInput(
          session = session,
          inputId = "source_instances_file",
          value = ""
        )

        reset(id = "instances_file")
        reset(id = "source_instances_file")
        reset(id = "instances_dir")
      })
    }
  )
)