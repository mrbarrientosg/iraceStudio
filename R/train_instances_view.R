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
            width = 8,
            directoryInput(
              idButton = ns("dir_path"),
              idInput = ns("instances_dir"),
              label = "Train Instances Dir",
              title = "Train Instances Directory",
              width = "auto"
            )
          ),
          column(
            width = 4,
            class = "d-flex align-items-end justify-content-end",
            importButton(inputId = ns("load")),
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

      observeEvent(store$pg, {
        updateTextInput(
          session = session,
          inputId = "instances_dir",
          value = gsub('"', "", store$pg$get_irace_option("trainInstancesDir"))
        )
      })
      
      shinyDirChoose(input, "dir_path", roots = volum)
      
      observeEvent(input$dir_path, {
        if (!is.integer(input$dir_path)) {
          dir <- parseDirPath(roots = volum, input$dir_path)
          log_debug("Adding trainInstancesDir {dir}")
          store$pg$add_irace_option(option = "trainInstancesDir", value = paste0('"', dir, '"'))
          updateTextInput(session = session, inputId = "instances_dir", value = dir)
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
      
      shinyFileChoose(input, "load", roots = volum, filetypes = "txt")
      
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

      observeEvent(input$instances_dir, {
        value <- NULL

        if (input$instances_dir != "") {
          value <- paste0('"', input$instances_dir, '"')
        }

        store$pg$add_irace_option(
          option = "trainInstancesDir",
          value = value
        )
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