ForbiddenView <- R6::R6Class(
  classname = "ForbiddenView",
  inherit = View,
  public = list(
    ui = function() {
      ns <- NS(self$id)

      tagList(
        fluidRow(
          class = "sub-header",
          column(
            width = 10,
            h2("Forbidden Configurations")
          ),
          column(
            width = 2,
            class = "d-flex align-items-center justify-content-end",
            importButton(inputId = ns("load")),
            exportButton(
              inputId = ns("export"),
              filename = "forbidden.txt",
              style = "margin-left: 5px;"
            )
          )
        ),
        fluidRow(
          bs4Card(
            title = strong("Coding Editor"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            aceEditor(
              outputId = ns("conditions"),
              theme = "textmate",
              autoComplete = "enable",
              mode = "r",
              autoScrollEditorIntoView = TRUE,
              height = 550
            )
          )
        )
      )
    },

    server = function(input, output, session, store) {
      # Don't remove this line it's used by aceEditor
      ns <- session$ns

      volum <- c(root = path_home())

      shinyFileSave(input = input, id = "export", roots = volum)

      observeEvent(input$export, {
        if (!is.integer(input$export)) {
          file <- parseSavePath(roots = volum, selection = input$export)
          log_debug("Exporting forbidden file to {file$datapath}")
          create_forbidden_file(path = file$datapath, pg = store$pg, name = NULL)
          log_debug("Forbidden file exported successfully")
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

          tryCatch({
            irace:::readForbiddenFile(file$datapath)
            source <- readLines(file$datapath)

            updateAceEditor(
              session = session,
              editorId = "conditions",
              value = paste(source, collapse = "\n")
            )
          }, error = function(err) {
            log_error("{err}")
            alert.error(err$message)
          })
        }
      })

      observeEvent(input$conditions, {
        store$pg$add_forbidden(input$conditions)
      }, ignoreInit = TRUE)

      observeEvent(playground_emitter$value(playground_events$current_scenario), {
        updateAceEditor(
          session = session,
          editorId = "conditions",
          value = store$pg$get_forbidden()
        )
      })

      observeEvent(input$clear, {
        log_debug("Removing forbidden code")

        updateAceEditor(
          session = session,
          editorId = "conditions",
          value = ""
        )

        store$pg$add_forbidden("")
      })
    }
  )
)