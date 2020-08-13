TrainInstancesView <- R6::R6Class(
  classname = "TrainInstancesView",
  inherit = View,
  public = list(
    ui = function() {
      ns <- NS(self$id)
      
      tagList(
        div(class = "sub-header", h2("Train Instances")),
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
              class = "ant-input",
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
      
      volum <- c(root = path_home())
      
      shinyDirChoose(input, "dir", roots = volum)
      
      observeEvent(input$dir, {
        if (!is.integer(input$dir)) {
          dir <- parseDirPath(roots = volum, input$dir)
          log_debug("Adding trainInstancesDir {dir}")
          files <- list.files(path = dir, full.names = TRUE)
          updateTextAreaInput(
            session = session,
            inputId = "source_instances_file",
            value = paste(files, collapse = "\n")
          )
        }
      })
      
      shinyFileSave(input = input, id = "export", roots = volum)
      
      observeEvent(input$export, {
        if (!is.integer(input$export)) {
          file <- parseSavePath(roots = volum, selection = input$export)
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
      
      shinyFileChoose(input, "load", roots = volum)
      
      observeEvent(input$load, {
        if (!is.integer(input$load)) {
          file <- parseFilePaths(roots = volum, input$load)
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
      
      observeEvent(playground_emitter$value(playground_events$current_scenario), {
        updateTextAreaInput(
          session = session,
          inputId = "source_instances_file",
          value = store$pg$get_train_instances()
        )
      })
      
      observeEvent(input$source_instances_file, store$pg$set_train_instances(input$source_instances_file))
      
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