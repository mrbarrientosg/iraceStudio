TrainInstancesView <- R6::R6Class(
  classname = "TrainInstancesView",
  inherit = View,
  public = list(
    ui = function() {
      ns <- NS(self$id)

      tagList(
        div(class = "sub-header", 
            h2("Train Instances"),
            p("Add training instances to perform the configuration. You can import instances from a folder, file, or add them directly in the text box.")
            ),
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
        fluidRow(
          bs4Card(
            title = strong("Instance File"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            tags$textarea(
              id = ns("source_instances_file"),
              style = "width: 100%; height: 500px;"
            )
          )
        )
      )
    },

    server = function(input, output, session, store) {
      clear <- callModule(
        module = clear_button_sv,
        id = "clear",
        message = "This action will remove all instances. Are you sure?."
      )

      volumes <- getVolumes()()

      shinyDirChoose(input, "dir", roots = volumes)
      shinyDirChoose(input, "absolute", roots = volumes)
      shinyFileSave(input = input, id = "export", roots = volumes)
      shinyFileChoose(input, "load", roots = volumes)

      observeEvent(input$dir, {
        if (!is.integer(input$dir)) {
          dir <- parseDirPath(roots = volumes, input$dir)
          log_debug("Adding trainInstancesDir {dir}")
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
          log_debug("Exporting instances file to {file$datapath}")
          create_instances_file(path = file$datapath, pg = store$pg, name = NULL)
          log_debug("Instances file exported successfully")
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
            alert.error("Can't load train instances file, check if the file format is correct.")
            return(invisible())
          }

          log_info("Importing testing instances file from {file$datapath}")
          source <- readLines(file$datapath)
          source <- paste(source, collapse = "\n")
          updateTextAreaInput(
            session = session,
            inputId = "source_instances_file",
            value = source
          )
        }
      })

      observeEvent(c(playground_emitter$value(playground_events$current_scenario), store$pg), {
        updateTextAreaInput(
          session = session,
          inputId = "source_instances_file",
          value = store$pg$get_train_instances()
        )
      })

      observeEvent(input$source_instances_file,
                   store$pg$set_train_instances(input$source_instances_file),
                   ignoreInit = TRUE)

      observeEvent(clear$action, {
        log_debug("Removing all data from instances")

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