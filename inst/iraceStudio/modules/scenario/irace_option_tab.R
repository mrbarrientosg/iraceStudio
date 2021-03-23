IraceOptionTab <- R6::R6Class( # nolint
  classname = "IraceOptionTab",
  inherit = Component,
  public = list(
    fast_ids = c(
      "debugLevel", "seed", "elitist", "deterministic",
      "parallel", "maxExperiments", "maxTime", "testType", "capping"
    ),

    ui = function(input_id, section, store, is_fast) {
      private$create_tab_content(input_id, section, store, is_fast)
    },

    server = function(input, output, session, store, events, .section, update, is_fast) {
      data <- if (is_fast) {
        scenario_options %>%
          filter(id %in% self$fast_ids)
      } else {
        scenario_options %>%
          filter(section == .section)
      }

      data %>%
        apply(1, function(row) {
          local({
            my_id <- row$id

            observeEvent(input[[my_id]],
              {
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

                isolate({
                  if (!is.null(update) && my_id %in% self$fast_ids) {
                    update$id <- my_id
                    update$section <- .section
                  }
                })

              },
              ignoreInit = TRUE
            )
          })
        })

      observeEvent(events$change_scenario,
        {
          if (!is_fast) {
            scenario_options %>%
              filter(section == .section) %>%
              apply(1, function(row) {
                private$updateInput(row, session, store)
              })
          }
        },
        ignoreInit = TRUE
      )

      observeEvent(c(update$id, update$section), {
        req(update$id)
        req(update$section)
        if (update$section != .section) {
            scenario_options %>%
              filter(id == update$id) %>%
              apply(1, function(row) {
                freezeReactiveValue(input, update$id)
                isolate(private$updateInput(row, session, store))
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
    create_tab_content = function(input_id, .section, store, is_fast) {
      content <- if (is_fast) {
        scenario_options %>%
          dplyr::filter(id %in% self$fast_ids)
      } else {
        scenario_options %>%
          dplyr::filter(section == .section)
      }

      content <- content %>%
        apply(1, function(row) {
          private$create_input(input_id, row, store)
        })

      return(tagList(content))
    },

    create_input = function(input_id, data, store) {
      ns <- NS(input_id)

      default <- if (is.null(store$pg$get_irace_option(data$id))) {
        data$default
      } else {
        store$pg$get_irace_option(data$id)
      }

      if (is.na(default)) {
        default <- NULL
      }

      input <- if (data$type == "numeric") {
        numericInput(
          inputId = ns(data$id),
          label = data$name,
          value = default,
          min = data$min,
          step = data$step,
          max = data$max
        )
      } else if (data$type == "bool") {
        checkboxInput(
          inputId = ns(data$id),
          label = data$name,
          value = as.logical(default)
        )
      } else if (data$type == "atext") {
        textAreaInput(inputId = ns(data$id), label = data$name, value = default)
      } else {
        pickerInput(
          inputId = ns(data$id),
          label = data$name,
          choices = data$values,
          selected = default,
          options = list(size = 8)
        )
      }

      input %>%
        shinyhelper::helper(
          type = "inline",
          colour = "var(--blue)",
          title = data$name,
          content = data$description,
          icon = "info-circle"
        )
    },

    updateInput = function(option, session, store) {
      default <- if (is.null(store$pg$get_irace_option(option$id))) {
        option$default
      } else {
        store$pg$get_irace_option(option$id)
      }

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
