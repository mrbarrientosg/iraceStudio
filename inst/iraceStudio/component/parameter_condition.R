ParameterCondition <- R6::R6Class( # nolint
  classname = "ParameterCondition",
  inherit = Component,
  public = list(
    session = NULL,

    ui = function(input_id) {
      ns <- NS(input_id)

      tagList(
        fluidRow(
          column(
            width = 4,
            class = "d-flex align-items-end",
            pickerInput(
              inputId = ns("paramNames"),
              label = "Parameter Name",
              choices = c(),
              width = "100%",
              options = list(
                size = 8
              )
            )
          ),
          column(
            width = 3,
            class = "d-flex align-items-end",
            pickerInput(
              inputId = ns("conditions"),
              label = "Condition",
              choices = c(),
              width = "100%",
              options = list(
                size = 8
              )
            )
          ),
          column(
            width = 5,
            class = "d-flex align-items-center",
            uiOutput(outputId = ns("value_condition"), style = "width: 100%;"),
            bs4Dash::actionButton(
              inputId = ns("addCondition"),
              label = "Add",
              style = "margin-left: 15px; margin-top: 15px;",
              status = "primary"
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 5,
            strong("Filters")
          ),
          column(
            width = 7,
            class = "d-flex align-items-center justify-content-end",
            bs4Dash::actionButton(
              inputId = ns("deleteCondition"),
              label = "Delete",
              icon = icon("trash"),
              status = "danger"
            )
          )
        ),
        DT::dataTableOutput(outputId = ns("expression_table"), width = "100%"),
        br()
      )
    },

    server = function(input, output, session, store, parent) {
      self$session <- session
      ns <- session$ns

      observeEvent(c(parent$types, input$paramNames), {
        req(input$paramNames)

        conditions <- if (!input$paramNames %in% names(parent$types)) {
          c()
        } else {
          type <- parent$types[[input$paramNames]]
          domain <- parent$domain[[input$paramNames]]
          self$conditions_list(type)
        }

        updatePickerInput(
          session = session,
          inputId = "conditions",
          choices = conditions
        )
      })

      observeEvent(c(
        input$paramNames,
        parent$types
      ), {
        output$value_condition <- renderUI({
          shiny::validate(
            need(store$pg, "")
          )

          if (!input$paramNames %in% names(parent$types)) {
            return()
          }

          type <- isolate(parent$types[[input$paramNames]])
          domain <- isolate(parent$domain[[input$paramNames]])

          if (type == "r" || type == "r,log") {
            numericInput(
              inputId = ns("paramValue"),
              label = "Parameter value",
              value = domain[1],
              min = domain[1],
              max = domain[2],
              width = "100%"
            )
          } else if (type == "o" || type == "c") {
            pickerInput(
              inputId = ns("paramValue"),
              label = "Parameter value",
              choices = domain,
              width = "100%",
              options = list(
                size = 8
              )
            )
          } else {
            sliderInput(
              inputId = ns("paramValue"),
              label = "Parameter value",
              value = domain[1],
              min = domain[1],
              max = domain[2],
              width = "100%"
            )
          }
        })
      })

      output$expression_table <- DT::renderDataTable({
        datatable(
          data = parent$expressions,
          escape = FALSE,
          selection = "single",
          rownames = TRUE,
          colnames = c("Row", "Condition"),
          style = "bootstrap4",
          class = "table-condensed table-striped cell-border",
          options = list(
            searching = FALSE,
            paging = FALSE,
            scrollY = "200px",
            dom = "t",
            sort = FALSE
          )
        )
      })

      observeEvent(input$conditions, {
        req(input$conditions)

        if (!input$paramNames %in% names(parent$types)) {
          return()
        }

        type <- isolate(parent$types[[input$paramNames]])
        domain <- isolate(parent$domain[[input$paramNames]])

        if (type == "o" || type == "c") {
          if (input$conditions == "in" || input$conditions == "not in") {
            output$value_condition <- renderUI(
              pickerInput(
                inputId = ns("paramValue"),
                label = "Parameter value",
                choices = domain,
                width = "100%",
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  size = 8
                )
              )
            )
          } else {
            output$value_condition <- renderUI(
              pickerInput(
                inputId = ns("paramValue"),
                label = "Parameter value",
                choices = domain,
                width = "100%",
                options = list(
                  size = 8
                )
              )
            )
          }
        }
      })

      observeEvent(input$addCondition, {
        req(input$paramValue != "")

        expr <- if (input$conditions == "in") {
          values_c <- paste0(paste0('"', input$paramValue, '"'), collapse = ", ")
          paste0(input$paramNames, " %in% ", "c(", values_c, ")")
        } else if (input$conditions == "not in") {
          values_c <- paste0(paste0('"', input$paramValue, '"'), collapse = ", ")
          paste0("!(", input$paramNames, " %in% ", "c(", values_c, ")", ")")
        } else {
          paste(input$paramNames, input$conditions, input$paramValue)
        }

        if (nrow(store$sandbox$get_filters()) > 0) {
          data <- subset(
            store$sandbox$get_filters(),
            condition == expr
          )

          if (nrow(data) > 0) {
            # TODO: Error alert
            return(invisible())
          }
        }

        new_row <- data.frame(condition = expr)

        parent$expressions <- rbind(parent$expressions, new_row)
      })

      observeEvent(input$deleteCondition, {
        row <- input$expression_table_rows_selected
        parent$expressions <- parent$expressions[-row, , drop = FALSE]
      })

      observe({
        toggleState(
          id = "deleteCondition",
          condition = nrow(parent$expressions) > 0 & !is.null(input$expression_table_rows_selected)
        )
      })
    },

    conditions_list = function(type) {
      common <- c("==", "!=")
      if (type == "r" || type == "i" || type == "i,log" || type == "r,log") {
        return(c(
          ">",
          ">=",
          "<",
          "<=",
          common
        ))
      } else {
        return(c(
          "in",
          "not in",
          common
        ))
      }
    },

    setup_inputs = function(names) {
      updatePickerInput(
        session = self$session,
        inputId = "paramNames",
        choices = names
      )
    },

    clear_inputs = function() {
      updatePickerInput(
        session = self$session,
        inputId = "paramNames",
        choices = c(""),
        selected = NULL
      )

      updatePickerInput(
        session = self$session,
        inputId = "conditions",
        choices = c(""),
        selected = NULL
      )
    }
  )
)
