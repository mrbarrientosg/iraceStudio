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
            h2("Forbidden Configurations"),
            HTML("Forbid parameter settings combinations for the configuration process.<br>
                 For more information and examples, go to the irace package <a href=\"https://cran.r-project.org/package=irace/vignettes/irace-package.pdf\" target=\"_blank\">user guide</a> ")
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
          box(
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

      volumes <- c("Home"=path.expand('~'), getVolumes()())

      shinyFileSave(input = input, id = "export", roots = volumes)

      observeEvent(input$export, {
        if (!is.integer(input$export)) {
          file <- parseSavePath(roots = volumes, selection = input$export)
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

      shinyFileChoose(input, "load", roots = volumes)

      observeEvent(input$load, {
        if (!is.integer(input$load)) {
          file <- tryCatch({
            parseFilePaths(roots = volumes, input$load)
          }, error = function(err) {
            log_error("{err}")
            return(NULL)
          })

          if (is.null(file)) {
            alert.error("Can't load forbidden file, check if the file format is correct.")
            return(invisible())
          }

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

      observeEvent(c(global_emitter$value(global_events$current_scenario), store$pg), {
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