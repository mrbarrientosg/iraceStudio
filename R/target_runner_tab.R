TargetRunnerTab <- R6::R6Class(
  classname = "TargetRunnerTab",
  inherit = Component,
  public = list(
    ui = function(inputId) {
      ns <- NS(inputId)
      
      tagList(
        fluidRow(
          column(
            width = 12,
            class = "d-flex align-items-center justify-content-end",
            importButton(inputId = ns("load")),
            exportButton(
              inputId = ns("export"),
              filename = "target-runner",
              style = "margin-left: 5px;"
            ),
            clear_button(inputId = ns("clear"), style = "margin-left: 5px;")
          )
        ),
        br(),
        fluidRow(
          column(
            width = 9,
            style = "border-width: 1px 0px 1px 1px; border-radius: 0px 2px 0px 2px;border-color: #d9d9d9;border-style: solid;",
            strong("Coding Editor"),
            aceEditor(
              outputId = ns("target_code"),
              theme = "textmate",
              autoComplete = "enable",
              autoScrollEditorIntoView = TRUE,
              minLines = 8,
              maxLines = 25,
            )
          ),
          column(
            width = 3,
            style = "background-color: #e9ecef;border-width: 1px 1px 1px 0px;border-radius: 0px 2px 0px 2px;border-color: #d9d9d9;border-style: solid;",
            selectInput(
              inputId = ns("ace_mode"),
              label = "Language",
              choices = getAceModes(),
              selected = "sh"
            )
          )
        )
      )
    },
    
    server = function(input, output, session, store) {
      # Don't remove this line it's used by aceEditor
      ns <- session$ns
  
      clear <- callModule(
        module = clear_button_sv,
        id = "clear",
        message = "This action will remove the target runner code. Are you sure?."
      )
      
      volum <- c(root = path_home())
      
      shinyFileSave(input = input, id = "export", roots = volum)
      
      observeEvent(input$export, {
        if (!is.integer(input$export)) {
          file <- parseSavePath(roots = volum, selection = input$export)
          log_debug("Exporting target runner file to {file$datapath}")
          create_target_runner_file(path = file$datapath, pg = store$pg, name = NULL)
          log_debug("Target runner file exported successfully")
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
          log_debug("Reading target runner file from {file$datapath}")
          source <- readLines(file$datapath)
          
          updateAceEditor(
            session = session,
            editorId = "target_code",
            value = paste(source, collapse = "\n")
          )
        }
      })
      
      observeEvent(input$ace_mode, {
        updateAceEditor(
          session = session,
          editorId = "target_code",
          mode = input$ace_mode
        )
      })
      
      observe({
        change_scenario <- store$pg$get_change_current()
        change_scenario()
        
        updateAceEditor(
          session = session,
          editorId = "target_code",
          value = store$pg$get_target_runner()
        )
      })
      
      observeEvent(input$target_code, {
        store$pg$add_target_runner(input$target_code)
      }, ignoreInit = TRUE)
      
      observeEvent(clear$action, {
        log_debug("Removing target runner code")
        updateAceEditor(
          session = session,
          editorId = "target_code",
          value = ""
        )
        reset(id = "target_file")
      })
    }
  )
)