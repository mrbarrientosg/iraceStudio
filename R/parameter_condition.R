ParameterCondition <- R6::R6Class(
  classname = "ParameterCondition",
  inherit = Component,
  public = list(
    session = NULL,

    ui = function(inputId) {
      ns <- NS(inputId)

      tagList(
        fluidRow(
          column(
            width = 4,
            class = "d-flex align-items-end",
            pickerInput(
              inputId = ns("paramNames"),
              label = "Parameter Name",
              choices = c(),
              width = "100%"
            )
          ),
          column(
            width = 3,
            class = "d-flex align-items-end",
            pickerInput(
              inputId = ns("conditions"),
              label = "Condition",
              choices = c(),
              width = "100%"
            )
          ),
          column(
            width = 5,
            class = "d-flex align-items-center",
            uiOutput(outputId = ns("valueCondition"), style = "width: 100%;"),
            actionButton(
              inputId = ns("addCondition"),
              label = "Add",
              style = "margin-left: 15px; margin-top: 15px;",
              class = "btn-primary"
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
            actionButton(
              inputId = ns("deleteCondition"),
              label = "Delete",
              icon = icon("trash"),
              class = "btn-danger"
            )
          )
        ),
        DT::dataTableOutput(outputId = ns("expressionTable"), width = "100%"),
        br()
      )
    },

    server = function(input, output, session, store, parent) {
      self$session <- session
      ns <- session$ns

      observeEvent(c(parent$types, input$paramNames), {
        req(input$paramNames != "")

        type <- parent$types[[input$paramNames]]
        domain <- parent$domain[[input$paramNames]]
        conditions <- self$conditionsList(type)

        updatePickerInput(
          session = session,
          inputId = "conditions",
          choices = conditions
        )
      })

      observeEvent(c(input$paramNames,
                     parent$types), {
        output$valueCondition <- renderUI({
          shiny::validate(
            need(store$pg, "")
          )
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
              width = "100%"
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

      output$expressionTable <- DT::renderDataTable({
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
        req(input$conditions != "")

        type <- isolate(parent$types[[input$paramNames]])
        domain <- isolate(parent$domain[[input$paramNames]])

        if (type == "o" || type == "c") {
          if (input$conditions == "in" || input$conditions == "not in") {
            output$valueCondition <- renderUI(
              pickerInput(
                inputId = ns("paramValue"),
                label = "Parameter value",
                choices = domain,
                width = "100%",
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE
                )
              )
            )
          } else {
            output$valueCondition <- renderUI(
              pickerInput(
                inputId = ns("paramValue"),
                label = "Parameter value",
                choices = domain,
                width = "100%"
              )
            )
          }
        }
      })

      observeEvent(input$addCondition, {
        req(input$paramValue != "")

        expr <- if (input$conditions == "in") {
          valuesC <- paste0(paste0('"', input$paramValue, '"'), collapse = ", ")
          paste0(input$paramNames, " %in% ", "c(", valuesC, ")")
        } else if (input$conditions == "not in") {
          valuesC <- paste0(paste0('"', input$paramValue, '"'), collapse = ", ")
          paste0("!(", input$paramNames, " %in% ", "c(", valuesC, ")", ")")
        } else {
          paste(input$paramNames, input$conditions, input$paramValue)
        }

        if (nrow(store$sandbox$getFilters()) > 0) {
          data <- subset(
            store$sandbox$getFilters(),
            condition == expr
          )

          if (nrow(data) > 0) {
            # TODO: Error alert
            return(invisible())
          }
        }

        newRow <- data.frame(condition = expr)

        parent$expressions <- rbind(parent$expressions, newRow)
      })

      observeEvent(input$deleteCondition, {
        row <- input$expressionTable_rows_selected
        parent$expressions <- parent$expressions[-row, ,drop = FALSE]
      })

      observe({
        toggleState(
          id = "deleteCondition",
          condition = nrow(parent$expressions) > 0 & !is.null(input$expressionTable_rows_selected))
      })
    },

    conditionsList = function(type) {
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

    setupInputs = function(names) {
      updatePickerInput(
        session = self$session,
        inputId = "paramNames",
        choices = names
      )
    },

    clearInputs = function() {
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
