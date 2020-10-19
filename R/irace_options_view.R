IraceOptionsView <- R6::R6Class(
  classname = "IraceOptionsView",
  inherit = View,
  public = list(
    ui = function() {
      ns <- NS(self$id)

      tagList(
        fluidRow(
          class = "sub-header",
          column(
            width = 10,
            h2("Irace Options")
          ),
          column(
            width = 2,
            class = "d-flex align-items-center justify-content-end",
            importButton(inputId = ns("load")),
            exportButton(inputId = ns("export"), filename = "scenario.txt",
              style = "margin-left: 5px;")
          )
        ),
        fluidRow(
          uiOutput(outputId = ns("content"), style = "width: 100%;")
        )
      )
    },

    server = function(input, output, session, store) {
      ns <- session$ns

      update <- reactiveValues(id = NULL, section = NULL)

      volumes <- getVolumes()()

      shinyFileChoose(input, "load", roots = volumes)
      shinyFileSave(input = input, id = "export", roots = volumes)

      observeEvent(input$load, {
        if (!is.integer(input$load)) {
          file <- tryCatch({
            parseFilePaths(roots = volumes, input$load)
          }, error = function(err) {
            log_error("{err}")
            return(NULL)
          })

          if (is.null(file)) {
            alert.error("Can't load scenario file, check if the file format is correct.")
            return(invisible())
          }

          result <- importScenario(file$name, file$datapath, store$pg$get_current_scenario(), TRUE)

          if (result) {
            shinyalert(title = "Warning",
                      text = "Cannot be import all options from the scenario.",
                      type = "warning")
            playground_emitter$emit(playground_events$current_scenario)
          } else {
            alert.error("Can't load scenario file, check if the file format is correct.")
          }
        }
      })

      observeEvent(input$export, {
        if (!is.integer(input$export)) {
          file <- parseSavePath(roots = volumes, selection = input$export)
          log_debug("Exporting scenario file to {file$datapath}")
          create_scenario_file(path = file$datapath, pg = store$pg, name = NULL, export = TRUE)
          log_debug("Scenario file exported successfully")
          shinyalert(
            title = "Exported",
            text = "The file is exported successfully.",
            type = "success"
          )
        }
      })

      output$content <- renderUI({
        shiny::validate(
          need(store$pg, "")
        )

        args <- c(
          self$create_tabs(ns, store, update),
          id = ns("tabs"),
          title = "",
          collapsible = FALSE,
          closable = FALSE,
          side = "left",
          width = 12
        )

        do.call(bs4TabCard, args)
      })
    },

    create_tabs = function(ns, store, update) {
      option <- IraceOptionTab$new()

      data <- scenarioOptions %>%
        distinct(section) %>%
        filter(section != "Testing")

      tabs <- list()

      quick_section <- "Quick Options"
      quick <- gsub(" ", "", quick_section)

      tab <- bs4TabPanel(
        tabName = quick_section,
        option$ui(inputId = ns(quick), section = quick_section, store = store, isFast = TRUE)
      )

      option$call(id = quick, store = store, .section = quick_section, update = update, isFast = TRUE)

      tabs <- c(tabs, list(tab))

      for (row in seq_len(nrow(data))) {
        section <- data[row, ]
        name <- gsub(" ", "", section)

        tab <- bs4TabPanel(
          tabName = section,
          option$ui(inputId = ns(name), section = section, store = store, isFast = FALSE)
        )

        option$call(id = name, store = store, .section = section, update = update, isFast = FALSE)

        tabs <- c(tabs, list(tab))
      }

      return(tabs)
    }
  )
)