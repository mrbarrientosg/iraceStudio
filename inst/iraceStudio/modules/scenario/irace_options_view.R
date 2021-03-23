IraceOptionsView <- R6::R6Class( # nolint
  classname = "IraceOptionsView",
  inherit = View,
  public = list(
    ui = function() {
      ns <- NS(self$id)

      tagList(
        fluidRow(
          class = "sub-header",
          column(
            width = 9,
            h2("Irace Options"),
            HTML("Set options for your scenario:<br>
                  <ul>
                  <li>you do not need to set all options, default options values are assigned</li>
                  <li>most commonly used options are listed in <b>Quick Options</b> tab </li>
                  <li>for a quick setup, just define either <b>maxExperiments</b> or <b>maxTime</b> options </li>
                  </ul>
                  Check each option information for details, or go to the irace package
                  <a href=\"https://cran.r-project.org/package=irace/vignettes/irace-package.pdf\" target=\"_blank\">user guide</a> ") # nolint
          ),
          column(
            width = 3,
            class = "d-flex align-items-center justify-content-end",
            import_button(input_id = ns("load")),
            export_button(
              input_id = ns("export"),
              filename = "scenario.txt",
              style = "margin-left: 5px;"
            )
          )
        ),
        fluidRow(
          uiOutput(outputId = ns("content"), style = "width: 100%;")
        )
      )
    },

    server = function(input, output, session, store, events) {
      ns <- session$ns

      update <- reactiveValues(id = NULL, section = NULL)

      volumes <- c("Home" = path.expand("~"), getVolumes()())

      shinyFileChoose(input, "load", roots = volumes)
      shinyFileSave(input = input, id = "export", roots = volumes)

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
            alert_error("Can't load scenario file, check if the file format is correct.")
            return(invisible())
          }

          result <- import_scenario(
            name = file$name,
            path = file$datapath,
            scenario = store$pg$get_current_scenario(),
            events = events,
            onlyOptions = TRUE
          )

          if (result) {
            shinyalert(
              title = "Warning",
              text = "Cannot be import all options from the scenario.",
              type = "warning"
            )
            update_reactive_counter(events$change_scenario)
          } else {
            alert_error("Can't load scenario file, check if the file format is correct.")
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
          self$create_tabs(ns, store, events, update),
          id = ns("tabs"),
          collapsible = FALSE,
          closable = FALSE,
          width = 12,
          type = "tabs",
          status = "primary"
        )

        do.call(tabBox, args)
      })
    },

    create_tabs = function(ns, store, events, update) {
      option <- IraceOptionTab$new()

      data <- scenario_options %>%
        distinct(section) %>%
        filter(section != "Testing")

      tabs <- list()

      quick_section <- "Quick Options"
      quick <- gsub(" ", "", quick_section)

      tab <- tabPanel(
        quick_section,
        option$ui(
          input_id = ns(quick),
          section = quick_section,
          store = store,
          is_fast = TRUE
        )
      )

      option$call(
        id = quick,
        store = store,
        events = events,
        .section = quick_section,
        update = update,
        is_fast = TRUE
      )

      tabs <- c(tabs, list(tab))

      for (row in seq_len(nrow(data))) {
        section <- data[row, ]
        name <- gsub(" ", "", section)

        tab <- tabPanel(
          section,
          option$ui(
            input_id = ns(name),
            section = section,
            store = store,
            is_fast = FALSE
          )
        )

        option$call(
          id = name,
          store = store,
          events = events,
          .section = section,
          update = update,
          is_fast = FALSE
        )

        tabs <- c(tabs, list(tab))
      }

      return(tabs)
    }
  )
)
