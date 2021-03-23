TargetScriptsView <- R6::R6Class( # nolint
  classname = "TargetScriptsView",
  inherit = View,
  public = list(
    target_tab = NULL,

    initialize = function(id) {
      super$initialize(id)
      self$target_tab <- TargetTab$new()
    },

    ui = function() {
      ns <- NS(self$id)

      tagList(
        div(
          class = "sub-header",
          h2("Target Scripts"),
          p("Add or import target runner and target evaluator scripts."),
          HTML("<b>Target runner</b><br> The target runner script must receive the following arguments:<br>
                 <center> &lt;id.configuration&gt; &lt;id.instance&gt; &lt;seed&gt; &lt;instance&gt; [bound] &lt;configuration&gt; </center><br>
                 the script must execute your algorithm and <b>only</b> print the result found and, optionally, the execution time:
                 <center>24629&nbsp;&nbsp;&nbsp;20.4</center><br>"), # nolint
          HTML("For more information and examples, go to the irace package <a href=\"https://cran.r-project.org/package=irace/vignettes/irace-package.pdf\" target=\"_blank\">user guide</a> ") # nolint
        ),
        fluidRow(
          tabBox(
            id = ns("scripts"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            type = "tabs",
            status = "primary",
            tabPanel(
              "Target Runner",
              self$target_tab$ui(input_id = ns("runner"))
            ),
            tabPanel(
              "Target Evaluator",
              self$target_tab$ui(input_id = ns("evaluator"))
            )
          )
        )
      )
    },

    server = function(input, output, session, store, events) {
      self$target_tab$call(id = "runner", store = store, events = events, is_runner = TRUE)
      self$target_tab$call(id = "evaluator", store = store, events = events, is_runner = FALSE)
    }
  )
)

TargetTab <- R6::R6Class( # nolint
  classname = "TargetTab",
  inherit = Component,
  public = list(
    ui = function(input_id) {
      ns <- NS(input_id)

      tagList(
        fluidRow(
          column(
            width = 12,
            class = "d-flex align-items-center justify-content-end",
            import_button(input_id = ns("load")),
            export_button(
              input_id = ns("export"),
              filename = "target-evaluator",
              style = "margin-left: 5px;"
            ),
            clear_button(input_id = ns("clear"), style = "margin-left: 5px;")
          )
        ),
        br(),
        fluidRow(
          column(
            width = 9,
            style = "border-width: 1px 0px 1px 1px; border-radius: 0px 2px 0px 2px;border-color: #d9d9d9;border-style: solid;", # nolint
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
            style = "background-color: #e9ecef;border-width: 1px 1px 1px 0px;border-radius: 0px 2px 0px 2px;border-color: #d9d9d9;border-style: solid;", # nolint
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

    server = function(input, output, session, store, events, is_runner) {
      clear <- callModule(
        module = clear_button_sv,
        id = "clear",
        message = "This action will remove the target code. Are you sure?."
      )

      volumes <- c("Home" = path.expand("~"), getVolumes()())

      shinyFileSave(input = input, id = "export", roots = volumes)

      observeEvent(input$export, {
        if (!is.integer(input$export)) {
          file <- parseSavePath(roots = volumes, selection = input$export)

          if (is_runner) {
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
          file <- tryCatch(
            {
              parseFilePaths(roots = volumes, input$load)
            },
            error = function(err) {
              log_error("{err}")
              return(NULL)
            }
          )

          if (is.null(file)) {
            alert_error("Can't load target file, check if the file format is correct.")
            return(invisible())
          }

          log_debug("Reading target file from {file$datapath}")
          source <- readLines(file$datapath)

          shinyAce::updateAceEditor(
            session = session,
            editorId = "target_code",
            value = paste(source, collapse = "\n")
          )
        }
      })

      observeEvent(input$ace_mode, {
        shinyAce::updateAceEditor(
          session = session,
          editorId = "target_code",
          mode = input$ace_mode
        )
      })

      observeEvent(c(events$change_scenario, store$pg),
        {
          if (is_runner) {
            shinyAce::updateAceEditor(
              session = session,
              editorId = "target_code",
              value = store$pg$get_target_runner()
            )
          } else {
            shinyAce::updateAceEditor(
              session = session,
              editorId = "target_code",
              value = store$pg$get_target_evaluator()
            )
          }
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      observeEvent(input$target_code,
        {
          if (is_runner) {
            store$pg$add_target_runner(input$target_code)
          } else {
            store$pg$add_target_evaluator(input$target_code)
          }
        },
        ignoreInit = TRUE
      )

      observeEvent(clear$action,
        {
          log_debug("Removing target code")
          shinyAce::updateAceEditor(
            session = session,
            editorId = "target_code",
            value = ""
          )
          reset(id = "target_file")
        },
        ignoreInit = TRUE
      )
    }
  )
)
