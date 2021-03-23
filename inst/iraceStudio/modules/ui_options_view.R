UIOptionsView <- R6::R6Class( # nolint
  classname = "UIOptionsView",
  inherit = View,
  public = list(
    ui = function() {
      ns <- NS(self$id)

      tagList(
        div(
          class = "sub-header",
          h2("UI Options"),
          p("Set here Irace Studio options:"),
          HTML("<ul>
                <li>define the path where irace is installed in your computer</li>
                <li>define the folder where Irace Studio data will be saved</li>
               </ul>")
        ),
        fluidRow(
          box(
            title = strong("Options"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            directory_input(
              id_button = ns("iraceButton"),
              id_input = ns("iracePath"),
              label = "Irace Path",
              title = "Irace Library Directory",
              width = "auto"
            ),
            tags$br(),
            directory_input(
              id_button = ns("workspaceButton"),
              id_input = ns("workspacePath"),
              label = "Workspace Path",
              title = "Workspace Directory",
              width = "auto"
            )
          )
        )
      )
    },

    server = function(input, output, session, store, events) {
      volumes <- c("Home" = path.expand("~"), getVolumes()())

      shinyDirChoose(input = input, id = "workspaceButton", roots = volumes)
      shinyDirChoose(input = input, id = "iraceButton", roots = volumes)

      observeEvent(c(store$gui, store$pg),
        {
          shinyjs::disable(id = "workspacePath")
          shinyjs::disable(id = "iracePath")

          if (!is.null(store$pg)) {
            updateTextInput(
              session = session,
              inputId = "iracePath",
              value = store$pg$get_irace_path()
            )
          }

          updateTextInput(
            session = session,
            inputId = "workspacePath",
            value = store$gui$workspace_path
          )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      observeEvent(input$workspaceButton, {
        if (!is.integer(input$workspaceButton)) {
          dir <- parseDirPath(roots = volumes, input$workspaceButton)

          path <- private$getWorkspacePath(dir)

          # TODO: Move all files inside of workspace to the new path and
          # validating if another workspace do not exist
          if (store$gui$createWorkspaceDirectory(path)) {
            store$gui$workspace_path <- path
            updateTextInput(
              session = session,
              inputId = "workspacePath",
              value = path
            )
          } else {
            alert_error("Can't create workspace directory, because there is an another folder called workspace.")
          }
        }
      })

      observeEvent(input$iraceButton, {
        if (!is.integer(input$iraceButton)) {
          dir <- parseDirPath(roots = volumes, input$iraceButton)

          path <- .libPaths()[1]

          if (private$check_path(dir)) {
            path <- dir
          }

          updateTextInput(
            session = session,
            inputId = "iracePath",
            value = path
          )

          store$pg$set_irace_path(path)
        }
      })
    }
  ),
  private = list(
    check_path = function(path) {
      if (is.null(path) || path == "") {
        return(FALSE)
      }

      return(TRUE)
    },

    getWorkspacePath = function(path) {
      if (!private$check_path(path)) {
        return(file.path(fs::path_home(), "workspace-irace"))
      }

      return(file.path(path, "workspace-irace"))
    }
  )
)
