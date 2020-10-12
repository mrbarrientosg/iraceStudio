TargetScriptsView <- R6::R6Class(
  classname = "TargetScriptsView",
  inherit = View,
  public = list(
    targetTab = NULL,

    initialize = function(id) {
      super$initialize(id)
      self$targetTab <- TargetTab$new()
    },

    ui = function() {
      ns <- NS(self$id)

      tagList(
        div(class = "sub-header", h2("Target Scripts")),
        fluidRow(
          bs4TabCard(
            id = ns("scripts"),
            title = "",
            collapsible = FALSE,
            closable = FALSE,
            side = "left",
            width = 12,
            bs4TabPanel(
              tabName = "Target Runner",
              self$targetTab$ui(inputId = ns("runner"))
            ),
            bs4TabPanel(
              tabName = "Target Evaluator",
              self$targetTab$ui(inputId = ns("evaluator"))
            )
          )
        )
      )
    },

    server = function(input, output, session, store) {
      self$targetTab$call(id = "runner", store = store, isRunner = TRUE)
      self$targetTab$call(id = "evaluator", store = store, isRunner = FALSE)
    }
  )
)

TargetTab <- R6::R6Class(
  classname = "TargetTab",
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
              filename = "target-evaluator",
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
              choices = c("batchfile", "sh"),
              selected = "sh"
            )
          )
        )
      )
    },

    server = function(input, output, session, store, isRunner) {
      # Don't remove this line it's used by aceEditor
      ns <- session$ns

      clear <- callModule(
        module = clear_button_sv,
        id = "clear",
        message = "This action will remove the target code. Are you sure?."
      )

      volumes <- getVolumes()()

      shinyFileSave(input = input, id = "export", roots = volumes)

      observeEvent(input$export, {
        if (!is.integer(input$export)) {
          file <- parseSavePath(roots = volumes, selection = input$export)

          if (isRunner) {
            log_debug("Exporting target runner file to {file$datapath}")
            create_target_runner_file(path = file$datapath, pg = store$pg, name = NULL)
            log_debug("Target runner file exported successfully")
          } else {
            log_debug("Exporting target evaluator file to {file$datapath}")
            create_target_evaluator_file(path = file$datapath, pg = store$pg, name = NULL)
            log_debug("Target evaluator file exported successfully")
          }

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
            alert.error("Can't load target file, check if the file format is correct.")
            return(invisible())
          }

          log_debug("Reading target file from {file$datapath}")
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

      observeEvent(playground_emitter$value(playground_events$current_scenario), {
        if (isRunner) {
          updateAceEditor(
            session = session,
            editorId = "target_code",
            value = store$pg$get_target_runner()
          )
        } else {
          updateAceEditor(
            session = session,
            editorId = "target_code",
            value = store$pg$get_target_evaluator()
          )
        }
      })

      observeEvent(input$target_code, {
        if (isRunner) {
          store$pg$add_target_runner(input$target_code)
        } else {
          store$pg$add_target_evaluator(input$target_code)
        }
      }, ignoreInit = TRUE)

      observeEvent(clear$action, {
        log_debug("Removing target code")
        updateAceEditor(
          session = session,
          editorId = "target_code",
          value = ""
        )
        reset(id = "target_file")
      }, ignoreInit = TRUE)
    }
  )
)