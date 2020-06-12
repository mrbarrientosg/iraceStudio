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
              column(
                width = 8,
                fluidRow(
                  column(
                    width = 4,
                    disabled(
                      textInput(inputId = ns("instances_dir"), label = "Test Instances Dir")
                    )
                  ),
                  column(
                    width = 2,
                    shinyDirButton(
                      id = ns("dirPath"),
                      label = "Browse",
                      title = "Test Instances Directory",
                      style = "margin-top: 30px; height: 38px;",
                      class = "ant-btn ant-btn-primary ant-btn-background-ghost"
                    )
                  )
                )
              ),
              column(
                width = 4,
                class = "d-flex align-items-center justify-content-end",
                importButton(inputId = ns("load")),
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
      
      shinyDirChoose(input, "dirPath", roots = volum)
      
      observeEvent(input$dirPath, {
        if (!is.integer(input$dirPath)) {
          dir <- parseDirPath(roots = volum, input$dirPath)
          store$pg$add_irace_option(
            option = "testInstancesDir",
            value = paste0('"', dir, '"')
          )
          updateTextInput(session = session, inputId = "instances_dir", value = dir)
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
      
      shinyFileChoose(input, "load", roots = volum, filetypes = c("txt"))
      
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
        change_scenario <- store$pg$get_change_current()
        change_scenario()
        
        obs_value(TRUE)
        self$testingOptions$ui(inputId = ns("testing"), "testing", store)
      })
      
      observeEvent(input$source_instances_file, {
        req(input$source_instances_file != "")
        store$pg$set_test_instances(input$source_instances_file)
      })
      
      observe({
        change_scenario <- store$pg$get_change_current()
        change_scenario()
        
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