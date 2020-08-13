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
        div(class = "sub-header", h2("Testing")),
        fluidRow(
          bs4Card(
            inputId = ns("testingInstances"),
            title = strong("Testing Options"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            uiOutput(outputId = ns("content"))
          ),
          bs4Card(
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
              class = "ant-input",
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
      
      volum <- c(root = path_home())

      shinyDirChoose(input, "dir", roots = volum)
      
      observeEvent(input$dir, {
        if (!is.integer(input$dirPath)) {
          dir <- parseDirPath(roots = volum, input$dir)
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
      
      output$content <- renderUI({
        playground_emitter$value(playground_events$current_scenario)
        
        obs_value(TRUE)
        self$testingOptions$ui(inputId = ns("testing"), "testing", store)
      })
      
      observeEvent(input$source_instances_file, {
        req(input$source_instances_file != "")
        store$pg$set_test_instances(input$source_instances_file)
      })

      observeEvent(playground_emitter$value(playground_events$current_scenario), {
        updateTextAreaInput(
          session = session,
          inputId = "source_instances_file",
          value = store$pg$get_test_instances()
        )
      })
      
      observe({
        if (obs_value()) {
          self$testingOptions$call(
            id = "testing",
            store = store,
            section = "testing"
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