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
            directoryInput(
              idButton = ns("iraceButton"),
              idInput = ns("iracePath"),
              label = "Irace Path",
              title = "Irace Library Directory",
              width = "auto"
            ),
            tags$br(),
            directoryInput(
              idButton = ns("workspaceButton"),
              idInput = ns("workspacePath"),
              label = "Workspace Path",
              title = "Workspace Directory",
              width = "auto"
            )
          )
        )
      )
    },
    
    server = function(input, output, session, store) {
      volum <- list(root = path_home())
      
      shinyDirChoose(input = input, id = "workspaceButton", roots = volum)
      shinyDirChoose(input = input, id = "iraceButton", roots = volum)

      observeEvent(store$gui, {
        shinyjs::disable(id = "workspacePath")
        shinyjs::disable(id = "iracePath")

        updateTextInput(
          session = session,
          inputId = "iracePath",
          value = store$gui$iracePath
        )
  
        updateTextInput(
          session = session,
          inputId = "workspacePath",
          value = store$gui$workspacePath
        )
      })
      
      observeEvent(input$workspaceButton, {
        if (!is.integer(input$workspaceButton)) {
          dir <- parseDirPath(roots = volum, input$workspaceButton)

          path <- private$getWorkspacePath(dir)

          updateTextInput(
            session = session,
            inputId = "workspacePath",
            value = path
          )

          store$gui$workspacePath <- path
          store$app$createWorkspaceDirectory()
        }
      })

      observeEvent(input$iraceButton, {
        if (!is.integer(input$iraceButton)) {
          dir <- parseDirPath(roots = volum, input$iraceButton)

          path <- .libPaths()[1]

          if (private$checkPath(dir)) {
            path <- dir
          }

          updateTextInput(
            session = session,
            inputId = "iracePath",
            value = path
          )

          store$gui$iracePath <- input$iracePath
        }
      })
      
    }
  ),
  private = list(
    checkPath = function(path) {
      if (is.null(path) || path == "") {
        return(false)
      }

      return(true)
    },

    getWorkspacePath = function(path) {
      if (!private$checkPath(path)) {
        return(file.path(getwd(), "workspace"))
      }

      return(file.path(path, "workspace"));
    }
  )
)