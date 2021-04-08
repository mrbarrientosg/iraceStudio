IraceOutputView <- R6::R6Class( # nolint
  classname = "IraceOutputView",
  inherit = View,
  public = list(
    irace_button = NULL,
    execution = NULL,

    initialize = function(id) {
      super$initialize(id)
      self$irace_button <- IraceButton$new()
    },

    ui = function() {
      ns <- NS(self$id)

      tagList(
        fluidRow(
          class = "sub-header",
          column(
            width = 10,
            h2("Irace Output"),
            p("See here the progress of the current irace execution. Click on start to launch irace."),
            p("You will be asked to assign a name to your execution,
              in this way you may execute your scenario more than once.")
          ),
          column(
            width = 2,
            class = "d-flex align-items-center justify-content-end",
            self$irace_button$ui(input_id = ns("start_irace"))
          )
        ),
        fluidRow(
          box(
            inputId = ns("target"),
            title = strong("Output"),
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            verbatimTextOutput(outputId = ns("irace_output"))
            # tags$head(
            #  tags$style(
            #    sprintf("#%s{overflow-y:scroll; max-height:550px;}", ns("irace_output"))
            #  ),
            #  tags$script(
            #    sprintf(
            #      '
            #      Shiny.addCustomMessageHandler("iraceOuputScroll", function (x) {
            #        var obj = document.getElementById("%s");
            #        obj.scrollTop = obj.scrollHeight;
            #      });
            #      ',
            #      ns("irace_output")
            #    )
            #  )
            # )
          )
        )
      )
    },

    server = function(input, output, session, store, events) {
      values <- reactiveValues(
        source = NULL,
        timer = reactiveTimer(intervalMs = 900) # Change refresh timer for running log
      )

      start <- self$irace_button$call(id = "start_irace", store = store, events = events)

      observeEvent(start$action,
      {
        if (events$is_irace_running) {
          events$is_irace_running <- FALSE
          store$irace_process$kill_tree()
          store$irace_process$finalize()
        } else {
          shinyalert(
            title = "Execution name",
            text = "Give a name to identify the execution after.",
            type = "input",
            inputType = "text",
            showCancelButton = TRUE,
            closeOnEsc = FALSE,
            callbackR = function(name) {
              if (is.logical(name) && !name) {
                return(invisible())
              }

              if (is.null(name) || name == "") {
                alert_error("Give a name.")
                return(invisible())
              }

              run_irace(store, events, name)

              if (events$is_irace_running) {
                self$execution <- Execution$new(name = name)
              }
            }
          )
        }
      })

      observe({
        if (!events$is_irace_running) {
          return(invisible())
        }

        events$is_irace_alive()

        if (!store$irace_process$is_alive()) {
          log_info("Irace end.")
          enable(id = "scenarioPicker")

          if (!get_option("debug", FALSE)) {
            unlink(pkg$temp_folder, recursive = TRUE, force = TRUE)
          }

          store$irace_process$poll_io(1500)
          error <- store$irace_process$read_error_lines()

          if (get_option("debug", FALSE)) {
            log_error("Optional: {error}")
          }

          log <- gsub('"', "", store$pg$get_irace_option("logFile"))

          if (file.exists(log)) {
            load(log)

            if (nrow(iraceResults$allConfigurations) != 0) {
              self$execution$set_irace_results(iraceResults)

              if (file.exists(pkg$output_log)) {
                self$execution$set_output_log(paste(readLines(pkg$output_log), collapse = "\n"))
              }

              store$pg$add_execution(self$execution)
              events$update_executions <- update_reactive_counter(events$update_executions)

              shinyalert(title = "The execution has ended", type = "success", timer = 1500)
            } else {
              file.remove(log)
              file.remove(pkg$output_log)

              if (!is.null(error) && error != "" && length(error) > 0) {
                log_error("Stop irace with error: {error}")
                alert_error(paste(error, collapse = "\n"))
                self$execution <- NULL
              }
            }

            rm(iraceResults)
          } else {
            if (!is.null(error) && error != "" && length(error) > 0) {
              log_error("Stop irace with error: {error}")
              alert_error(paste(error, collapse = "\n"))
              self$execution <- NULL
            }
          }

          store$pg$clear_scenario_temp()

          store$irace_process <- NULL
          events$is_irace_running <- FALSE
        }
      })

      observe({
        events$change_scenario

        if (!events$is_irace_running) {
          if (!is.null(pkg$output_log) && file.exists(pkg$output_log)) {
            values$source <- paste(readLines(pkg$output_log), collapse = "\n")
          } else {
            values$source <- ""
          }
          return(invisible())
        }

        values$timer()

        future({
          if (file.exists(pkg$output_log)) {
            paste(readLines(pkg$output_log), collapse = "\n")
          } else {
            ""
          }
        }) %...>% {
          values$source <- .
        }

        # session$sendCustomMessage(type = "iraceOuputScroll", 1)
      })

      output$irace_output <- renderText({
        shiny::validate(
          need(values$source, message = "Irace is not running yet.")
        )
        values$source
      })
    }
  )
)
