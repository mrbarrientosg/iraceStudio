ConfigurationFilter <- R6::R6Class(
  classname = "ConfigurationFilter",
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
        DTOutput(outputId = ns("expressionTable"), width = "100%"),
        br()
      )
    },

    server = function(input, output, session, store, parent) {
      self$session <- session
      ns <- session$ns

      values <- reactiveValues(expressions = data.frame())
      
      observeEvent(parent$updateConfig, {
        req(input$paramNames)
        req(store$iraceResults)

        print(input$paramNames)

        type <- store$iraceResults$parameters$types[[input$paramNames]]
        domain <- store$iraceResults$parameters$domain[[input$paramNames]]
        conditions <- self$conditionsList(type)
        
        updatePickerInput(
          session = session,
          inputId = "conditions",
          choices = conditions
        )
      })

      output$valueCondition <- renderUI({
        playground_emitter$value(playground_events$current_scenario)
        playground_emitter$value(playground_events$update_sandboxes)

        shiny::validate(
          need(input$paramNames, message = ""),
          need(store$iraceResults, message = "")
        )

        type <- store$iraceResults$parameters$types[[input$paramNames]]
        domain <- store$iraceResults$parameters$domain[[input$paramNames]]

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

      output$expressionTable <- renderDT({
        datatable(
          data = values$expressions,
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

      filterProxy <- dataTableProxy(outputId = "expressionTable")

      observe({
        if (is.null(store$sandbox))
          data <- data.frame()
        else
          data <- store$sandbox$getFilters()

        values$expressions <- data

        filterProxy %>%
          replaceData(
            data,
            resetPaging = FALSE,
            rownames = FALSE
          )
      })

      observeEvent(input$addCondition, {
        expr <- ""
        if (input$conditions == "in") {
          expr <- paste0(input$paramNames, " %in% ", '"', input$paramValue, '"')
        } else if (input$conditions == "not in") {
          expr <- paste0("!(", input$paramNames, " %in% ", '"', input$paramValue, '"', ")")
        } else {
          expr <- paste(input$paramNames, input$conditions, input$paramValue)
        }

        if (nrow(store$sandbox$getFilters()) > 0) {
          data <- subset(
            store$sandbox$getFilters(),
            condition == expr
          )

          if (nrow(data) > 0) {
            return(invisible())
          }
        }

        newRow <- data.frame(condition = expr)

        store$sandbox$addFilter(newRow)
        values$expressions <- store$sandbox$getFilters()

        filterProxy %>%
          replaceData(
            data = store$sandbox$getFilters(),
            resetPaging = FALSE,
            rownames = FALSE
          )
      })

      observeEvent(input$deleteCondition, {
        store$sandbox$removeFilter(input$expressionTable_rows_selected)
        values$expressions <- store$sandbox$getFilters()
        filterProxy %>%
          replaceData(
            data = store$sandbox$getFilters(),
            resetPaging = FALSE,
            rownames = FALSE
          )
      })

      observe({
        toggleState(
          id = "deleteCondition",
          condition = nrow(values$expressions) > 0 & !is.null(input$expressionTable_rows_selected))
      })
    },

    conditionsList = function(type) {
      if (type == "r" || type == "i" || type == "i,log" || type == "r,log") {
        return(c(
          ">",
          ">=",
          "<",
          "<=",
          "!=",
          "=="
        ))
      } else {
        return(c(
          "in",
          "not in"
        ))
      }
    },

    setupInputs = function(store) {
      updatePickerInput(
        session = self$session,
        inputId = "paramNames",
        choices = store$iraceResults$parameters$names
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