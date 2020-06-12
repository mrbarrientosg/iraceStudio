UIOptionsView <- R6::R6Class(
  classname = "UIOptionsView",
  inherit = View,
  public = list(
    ui = function() {
      ns <- NS(self$id)
      
      tagList(
        div(class = "sub-header", h2("UI Options")),
        fluidRow(
          bs4Card(
            title = strong("Options"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            textInput(
              inputId = ns("iracePath"),
              label = "Irace Path"
            ),
            div(
              class = "input-group-append",
              textInput(
                inputId = ns("workspace"),
                label = "Workspace Path"
              ),
              shinyDirButton(
                id = ns("dirPath"),
                label = "Browse",
                title = "Workspace Directory",
                style = "margin-top: 30px; height: 38px;",
                class = "btn-primary"
              )
            )
          )
        )
      )
    },
    
    server = function(input, output, session, store) {
      volum <- list(root = path_home())
      
      shinyDirChoose(input = input, id = "dirPath", roots = volum)
  
      observeEvent(store$gui, {
        updateTextInput(
          session = session,
          inputId = "iracePath",
          value = store$gui$iracePath
        )
  
        updateTextInput(
          session = session,
          inputId = "workspace",
          value = store$gui$workspacePath
        )
      })
      
      observeEvent(input$dirPath, {
        if (!is.integer(input$dirPath)) {
          dir <- parseDirPath(roots = volum, input$dirPath)
          updateTextInput(
            session = session,
            inputId = "workspace",
            value = file.path(dir, "workspace")
          )
        }
      })
      
      observeEvent(input$workspace, {
        store$gui$workspacePath <- input$workspace
  
        if (is.null(input$workspace) || input$workspace == "") {
          store$gui$workspacePath <- file.path(getwd(), "workspace")
        }
  
        store$app$createWorkspaceDirectory()
      })
      
      observeEvent(input$iracePath, store$gui$iracePath <- input$iracePath)
    }
  )
)