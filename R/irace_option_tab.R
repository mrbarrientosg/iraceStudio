IraceOptionTab <- R6::R6Class(
  classname = "IraceOptionTab",
  inherit = Component,
  public = list(
    ui = function(inputId, section, store) {
      private$create_tab_content(inputId, section, store)
    },

    server = function(input, output, session, store, section) {
      for (row in seq_len(nrow(scenarioOptions[[section]]$options))) {
        local({
          option <- scenarioOptions[[section]]$options[row, ]
          my_id <- option$id

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
          })
        })
      }

      observeEvent(playground_emitter$value(playground_events$current_scenario), {
        for (i in seq_len(nrow(scenarioOptions[[section]]$options))) {
          option <- scenarioOptions[[section]]$options[i, ]
          private$updateInput(option, session, store)
        }
      }, ignoreInit = TRUE)

      observeEvent(input$elitist, {
        if (!input$elitist) {
          log_info("Disable elitistLimit and elitistNewInstances")
          disable(id = "elitistLimit")
          disable(id = "elitistNewInstances")

          # TODO: No dejar en null, revisar cuando se cree el archivo scenario.txt
          # store$scenario$elitistLimit <- NULL
          # store$scenario$elitistNewInstances <- NULL
        } else {
          log_info("Enable elitistLimit and elitistNewInstances")
          enable(id = "elitistLimit")
          enable(id = "elitistNewInstances")
        }
      })
    }
  ),
  private = list(
    create_tab_content = function(inputId, section, store) {
      content <- list()
      data <- scenarioOptions[[section]]$options

      for (row in seq_len(nrow(data))) {
        input <- data[row, ]
        content[[row]] <- private$create_input(inputId, input, store)
      }

      return(tagList(content))
    },

    create_input = function(inputId, data, store) {
      ns <- NS(inputId)

      default <- if (is.null(store$pg$get_irace_option(data$id)))
        data$default
      else
        store$pg$get_irace_option(data$id)

      input <- if (data$type == "numeric") {
        numericInput(inputId = ns(data$id), label = data$name, value = default, min = data$min, step = data$step, max = data$max)
      } else if (data$type == "bool") {
        checkboxInput(inputId = ns(data$id), label = data$name, value = as.logical(default))
      } else if (data$type == "atext") {
        textAreaInput(inputId = ns(data$id), label = data$name, value = default)
      } else {
        pickerInput(inputId = ns(data$id), label = data$name, choices = data$values[[1]], selected = default)
      }

      private$addInputInfo(data, input, ns(data$id))
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