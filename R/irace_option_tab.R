IraceOptionTab <- R6::R6Class(
  classname = "IraceOptionTab",
  inherit = Component,
  public = list(
    fast_ids = c("debugLevel", "seed", "elitist", "deterministic",
                 "parallel", "maxExperiments", "maxTime", "testType", "capping"),

    ui = function(inputId, section, store, isFast) {
      private$create_tab_content(inputId, section, store, isFast)
    },

    server = function(input, output, session, store, .section, update, isFast) {
      data <- if (isFast) {
        scenarioOptions %>%
          filter(id %in% self$fast_ids)
      } else {
        scenarioOptions %>%
          filter(section == .section)
      }

      data %>%
        apply(1, function(row) {
          local({
            my_id <- row$id

            observeEvent(input[[my_id]], {
              if (my_id != "testType" && my_id != "boundType") {
                log_debug("Observing input: {my_id}, value: {input[[my_id]]}")
                store$pg$add_irace_option(option = my_id, value = input[[my_id]])
              } else {
                log_debug("Observing input: {my_id}, value: {input[[my_id]]}")
                store$pg$add_irace_option(
                  option = my_id,
                  value = paste0('"', input[[my_id]], '"')
                )
              }

              if (!is.null(update)) {
                update$id <- isolate(NULL)
                update$section <- isolate(NULL)

                update$id <- my_id
                update$section <- .section
              }
            }, ignoreInit = TRUE)
          })
        })

      observeEvent(playground_emitter$value(playground_events$current_scenario), {
        if (!isFast) {
          scenarioOptions %>%
            filter(section == .section) %>%
            apply(1, function(row) {
              private$updateInput(row, session, store)
            })
        }
      }, ignoreInit = TRUE)

      observeEvent(c(update$id, update$section), {
        req(update$id)
        req(update$section)
        if (update$section != .section) {
          scenarioOptions %>%
            filter(id == update$id) %>%
            apply(1, function(row) {
              private$updateInput(row, session, store)
            })
        }
      })

      observeEvent(input$elitist, {
        if (!input$elitist) {
          log_info("Disable elitistLimit and elitistNewInstances")
          disable(id = "elitistLimit")
          disable(id = "elitistNewInstances")
        } else {
          log_info("Enable elitistLimit and elitistNewInstances")
          enable(id = "elitistLimit")
          enable(id = "elitistNewInstances")
        }
      })
    }
  ),
  private = list(
    create_tab_content = function(inputId, .section, store, isFast) {
      content <- if (isFast) {
        scenarioOptions %>%
          dplyr::filter(id %in% self$fast_ids)
      } else {
        scenarioOptions %>%
          dplyr::filter(section == .section)
      }

      content <- content %>%
        apply(1, function(row) {
          private$create_input(inputId, row, store)
        })

      return(tagList(content))
    },

    create_input = function(inputId, data, store) {
      ns <- NS(inputId)

      default <- if (is.null(store$pg$get_irace_option(data$id)))
        data$default
      else
        store$pg$get_irace_option(data$id)

      if (is.na(default))
        default <- NULL

      input <- if (data$type == "numeric") {
        numericInput(inputId = ns(data$id), label = data$name, value = default, min = data$min, step = data$step, max = data$max)
      } else if (data$type == "bool") {
        checkboxInput(inputId = ns(data$id), label = data$name, value = as.logical(default))
      } else if (data$type == "atext") {
        textAreaInput(inputId = ns(data$id), label = data$name, value = default)
      } else {
        pickerInput(inputId = ns(data$id), label = data$name, choices = data$values, selected = default, options = list(size = 8))
      }

      input %>% shinyhelper::helper(type = "inline", title = data$name, content = data$description, icon = "info-circle")
    },

    addInputInfo = function(option, input, inputId) {
      info <- bs4Dash::bs4PopoverUI(
        actionButton(
          inputId = paste0(inputId, "-info"),
          label = NULL,
          icon = icon("info-circle"),
          class = "btn-link"
        ),
        title = option$name,
        content = option$description,
        placement = "right"
      )

      if (option$type == "numeric" || option$type == "atext" || option$type == "list") {
        input$children[[1]] <- tagAppendChildren(input$children[[1]], info)
      } else {
        input$children[[1]]$children[[1]] <- tagAppendChildren(input$children[[1]]$children[[1]], info)
      }

      input
    },

    updateInput = function(option, session, store) {
      default <- if (is.null(store$pg$get_irace_option(option$id)))
        option$default
      else
        store$pg$get_irace_option(option$id)

      if (option$type == "numeric") {
        updateNumericInput(
          session = session,
          inputId = option$id,
          value = default
        )
      } else if (option$type == "bool") {
        updateCheckboxInput(
          session = session,
          inputId = option$id,
          value = as.logical(default)
        )
      } else if (option$type == "atext") {
        updateTextAreaInput(
          session = session,
          inputId = option$id,
          value = default
        )
      } else {
        updatePickerInput(
          session = session,
          inputId = option$id,
          selected = gsub('"', "", default)
        )
      }
    }
  )
)