ParametersView <- R6::R6Class(
  classname = "ParametersView",
  inherit = View,
  public = list(
    modal = NULL,
    initialize = function(id) {
      super$initialize(id)
      self$modal <- ModalParameter$new()
    },

    ui = function() {
      ns <- NS(self$id)

      tagList(
        div(class = "sub-header", h2("Parameters")),
        fluidRow(
          column(
            width = 8,
            actionButton(inputId = ns("add"), label = "Add", icon = icon("plus")),
            disabled(
              actionButton(
                inputId = ns("edit"),
                label = "Edit",
                icon = icon("edit")
              ),
              actionButton(
                inputId = ns("delete"),
                label = "Delete",
                icon = icon("minus")
              ),
              actionButton(
                inputId = ns("check"),
                label = "Check",
                icon = icon("check")
              )
            )
          ),
          column(
            width = 4,
            class = "d-flex align-items-center justify-content-end",
            importButton(
              inputId = ns("load")
            ),
            exportButton(
              inputId = ns("export"),
              filename = "parameters.txt",
              style = "margin-left: 5px;"
            ),
            clear_button(inputId = ns("clear"), style = "margin-left: 5px;")
          )
        ),
        br(),
        fluidRow(
          bs4Card(
            collapsible = FALSE,
            closable = FALSE,
            width = 12,
            htmlOutput(outputId = ns("tip")),
            br(),
            DT::dataTableOutput(outputId = ns("parameters_table")),
            br()
          )
        )
      )
    },

    server = function(input, output, session, store) {
      ns <- session$ns

      values <- reactiveValues(parameters = NULL,
                               domainList = c(),
                               parameter = NULL)

      clear <- callModule(
        module = clear_button_sv,
        id = "clear",
        message = "This action will remove all parameters. Are you sure?."
      )

      output$parameters_table <- DT::renderDataTable({
        shiny::validate(
          need(store$pg, "")
        )

        store$pg$get_parameters()
      },
        escape = FALSE,
        selection = "single",
        rownames = FALSE,
        style = "bootstrap4",
        class = "table-condensed table-striped cell-border",
        options = list(
          language = list(
            zeroRecords = "There is no parameters to show"
          )
        )
      )

      output$tip <- renderUI(strong("* Select a row to delete or edit."))

      # Para poder modificar la tabla de parametros despues de instanciarse
      proxy <- dataTableProxy(outputId = "parameters_table")

      observeEvent(playground_emitter$value(playground_events$current_scenario), {
        values$parameters <- store$pg$get_parameters()
      })

      observe({
        playground_emitter$emit(playground_events$update_parameters)

        proxy %>%
          replaceData(
            data = values$parameters,
            resetPaging = FALSE,
            rownames = FALSE
          )
      })

      volumes <- getVolumes()()

      shinyFileChoose(input, "load", roots = volumes)

      observeEvent(input$load, {
        if (!is.integer(input$load)) {
          file <- parseFilePaths(roots = volumes, input$load)
          log_info("Importing paremeter file from {file$datapath}")

          tryCatch({
            data <- readParameters(file = file$datapath)

            store$pg$add_parameter(extract.parameters(data))

            values$parameters <- store$pg$get_parameters()
          },
          error = function(err) {
            log_error("{err}")
            alert.error(err$message)
          })
        }
      })

      shinyFileSave(input = input, id = "export", roots = volumes)

      observeEvent(input$export, {
        if (!is.integer(input$export)) {
          file <- parseSavePath(roots = volumes, selection = input$export)
          log_debug("Exporting parameter file to {file$datapath}")
          create_parameter_file(path = file$datapath, pg = store$pg, name = NULL)
          log_debug("Parameter file exported successfully")
          shinyalert(
            title = "Exported",
            text = "The file is exported successfully.",
            type = "success"
          )
        }
      })

      observe({
        condition <- !is.null(input$parameters_table_rows_selected) & nrow(values$parameters) > 0
        toggleState(id = "edit", condition)
        toggleState(id = "delete", condition)
        toggleState(id = "check", nrow(values$parameters) > 0)
        toggle(id = "tip", condition = nrow(values$parameters) > 0)
      })

      observeEvent(input$check, {
        parameters <- data.table(store$pg$get_parameters())
        parameters <- capture.output(
          write.table(
            parameters,
            row.names = FALSE,
            col.names = FALSE,
            sep = "\t",
            quote = F
          )
        )

        parameters <- paste0(parameters, collapse = "\n")

        log_debug("Checking parameters")
        tryCatch({
          irace::readParameters(text = parameters)

          log_debug("Parameters check successfully")

          shinyalert(
            title = "Check",
            text = "The check is successfully.",
            type = "success"
          )
        },
        error = function(err) {
          log_error("{err}")
          alert.error(err$message)
        })
      })

      # Show modal to add a new parameter
      observeEvent(input$add, {
        values$parameter <- NULL

        showModal(
          self$modal$ui(ns, "New parameter")
        )

        #self$modal$server(input, output, session, store = store, values)
      })

      self$modal$server(input, output, session, store = store, values)

      # show modal to edit a parameter
      observeEvent(input$edit, {
        parameter <- as.list(store$pg$get_parameter(input$parameters_table_rows_selected))
        values$parameter <- parameter

        showModal(
          self$modal$ui(ns, "Edit", parameter)
        )

        #self$modal$server(input, output, session, store = store, values, parameter)
      })

      # show modal to delete a parameter
      observeEvent(input$delete, {
        if (is.null(input$parameters_table_rows_selected) ||
            is.na(input$parameters_table_rows_selected)) {
          shinyalert(
            title = "Error",
            text = "Please select the parameter that you want to delete!",
            type = "error"
          )
        } else {
          showModal(
            modalDialog(
              title = "Warning",
              HTML(
                paste(
                  "Are you sure to delete",
                  tags$b(
                    store$pg$get_parameter(input$parameters_table_rows_selected)$names
                  ), "param?"
                )
              ),
              footer = tagList(
                actionButton(inputId = ns("confirm_delete"), label = "Yes", class = "btn-danger"),
                modalButton(label = "Cancel")
              ),
              easyClose = TRUE
            )
          )
        }
      })

      # handle to delete a parameter
      observeEvent(input$confirm_delete, {
        param <- store$pg$get_parameter(input$parameters_table_rows_selected)
        log_debug("Deleting a parameter with name: {param$names}")

        store$pg$remove_parameter(input$parameters_table_rows_selected)

        values$parameters <- store$pg$get_parameters()

        log_debug("Parameter deleted")

        removeModal()
      })

      observeEvent(clear$action, {
        log_debug("Removing all parameters from table")

        store$pg$clear_parameters()

        values$parameters <- store$pg$get_parameters()

        log_debug("All parameters removed")
      })
    }
  )
)

ModalParameter <- R6::R6Class(
  classname = "ModalParameter",
  private = list(
    removeObserver = list(),

    checkValue = function(key, values) {
      if (is.null(values))
        return(NULL)

      if (key == "domain") {
        result <- gsub("[\\s+\\)\\(]", "", as.character(values[[key]]), perl = TRUE)
        result <- strsplit(result, ",", perl = TRUE)
        return(result[[1]])
      } else if (key == "switches") {
        return(gsub('"', "", values[[key]]))
      } else if (key == "conditions") {
        return(substring(values[[key]], 2))
      }

      return(values[[key]])
    },

    clearValue = function(key, .input, ns) {
      .subset2(.input, "impl")$.values$remove(ns(key))
    }
  ),
  public = list(
    ui = function(ns, title, values = NULL) {

      types <- c(
        "Real" = "r",
        "Integer" = "i",
        "Categorical" = "c",
        "Ordered" = "o",
        "Real Log" = "r,log",
        "Integer Log" = "i,log"
      )

      modalDialog(
        title = title,
        easyClose = FALSE,
        textInput(
          inputId = ns("parameterName"),
          label = "Name",
          width = "100%",
          value = private$checkValue("names", values)
        ),
        textInput(
          inputId = ns("parameterFlag"),
          label = "Flag",
          width = "100%",
          value = private$checkValue("switches", values)
        ),
        selectInput(
          inputId = ns("parameterType"),
          label = "Type",
          choices = types,
          width = "100%",
          selected = checkNull(private$checkValue("types", values), "r")
        ),
        uiOutput(outputId = ns("domainOutput")),
        br(),
        strong("Condition (using R syntax)"),
        shinyAce::aceEditor(
          outputId = ns("parameterCondition"),
          theme = "textmate",
          mode = "r",
          autoComplete = "enable",
          autoScrollEditorIntoView = TRUE,
          minLines = 8,
          maxLines = 10,
          value = private$checkValue("conditions", values)
        ),
        footer = tagList(
          actionButton(inputId = ns("parameterSave"), label = "Save", class = "btn-primary"),
          actionButton(inputId = ns("parameterCancel"), label = "Cancel")
        )
      )
    },

    server = function(input, output, session, store, parent) {
      ns <- session$ns

      output$domainOutput <- renderUI({
        type <- input$parameterType
        param <- parent$parameter
        default <- checkNull(private$checkValue("types", param), "r")
        type <- checkNull(type, default)

        if (type == "o" || type == "c") {
          domain <- checkNull(private$checkValue("domain", param), c())
          for (value in domain) {
            local({
              myId <- value
              buttonId <- paste0(myId, "-delete")
              private$removeObserver[[myId]] <- observeEvent(input[[buttonId]], {
                parent$domainList <- parent$domainList[!(parent$domainList %in% myId)]
                private$removeObserver[[myId]] <- NULL
              }, ignoreInit = TRUE, once = TRUE)
            })
          }
          parent$domainList <- domain
          tagList(
            textInput(ns("domainName"), "Domain name"),
            uiOutput(ns("domainList")),
            br(),
            actionButton(ns("addDomain"), "Add", class = "btn-link")
          )
        } else {
          domain <- private$checkValue("domain", param)
          fluidRow(
            column(
              width = 6,
              numericInput(ns("domainMin"), "Min", checkNull(domain[1], 0))
            ),
            column(
              width = 6,
              numericInput(ns("domainMax"), "Max", checkNull(domain[2], 0))
            )
          )
        }
      })

      output$domainList <- renderUI({
        shiny::validate(
          need(length(parent$domainList) != 0, "Empty domain")
        )
        tagList(
          bs4Dash::bs4ListGroup(
            id = "sortable",
            lapply(parent$domainList, function(name) {
              bs4ListGroupItem(
                name,
                actionButton(ns(paste0(name, "-delete")), labe = NULL, icon = icon("trash"), class = "btn-danger")
              )
            })
          ),
          sortable::sortable_js(
            css_id = "sortable",
            options = sortable::sortable_options(
              onSort = sortable::sortable_js_capture_input(input_id = ns("sortable"))
            )
          )
        )
      })

      observeEvent(input$addDomain, {
        if (input$domainName %in% isolate(parent$domainList))
          return()

        id <- as.character(input$domainName)
        parent$domainList <- c(parent$domainList, input$domainName)

        local({
          myId <- id
          name <- input$domainName
          buttonId <- paste0(name, "-delete")
          private$removeObserver[[myId]] <- observeEvent(input[[buttonId]], {
            parent$domainList <- parent$domainList[!(parent$domainList %in% name)]
            private$removeObserver[[myId]] <- NULL
            private$clearValue(buttonId, input, ns)
          }, ignoreInit = TRUE, once = TRUE)
        })

        shinyjs::reset("domainName")
      })

      observeEvent(input$sortable, {
        parent$domainList <- isolate(input$sortable)
      })

      observeEvent(input$parameterSave, {
        shinyjs::disable("parameterSave")

        domain <- if (input$parameterType == "o" || input$parameterType == "c") {
          if (length(parent$domainList) == 0) {
            alert.error("Domain cannot be empty")
            return(NULL)
          }
          paste0("(", paste0(parent$domainList, collapse = ", "), ")")
        } else {
          paste0("(", input$domainMin, ", ", input$domainMax, ")")
        }

        if (is.null(domain)) {
          shinyjs::enable("parameterSave")
          return(NULL)
        }

        flag <- gsub('"', "", input$parameterFlag)
        condition <- if (is.null(input$parameterCondition) || input$parameterCondition == "")
          ""
        else
          paste("|", input$parameterCondition)

        newRow <- data.frame(
          names = input$parameterName,
          switches = paste0('"', flag, '"'),
          types = input$parameterType,
          domain = domain,
          conditions = condition,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )

        check <- capture.output(
          write.table(
            newRow,
            row.names = FALSE,
            col.names = FALSE,
            sep = "\t",
            quote = F
          )
        )

        result <- tryCatch({
          irace::readParameters(text = check)
          TRUE
        },
        error = function(err) {
          if (!grepl("A parameter definition is missing!", err$message, fixed = TRUE)) {
            log_error("{err}")
            alert.error(err$message)
            shinyjs::enable("parameterSave")
            FALSE
          } else {
            TRUE
          }
        })

        if (result) {
          log_debug(
            paste(
              "Save a new parameter with",
              "name: {input$parameter_name}, flag: {input$parameter_flag}"
            )
          )

          added <- tryCatch({
            if (is.null(parent$parameter)) {
              store$pg$add_parameter(newRow)
            } else {
              store$pg$update_parameter(
                row = input$parameters_table_rows_selected,
                new_parameter = newRow
              )
            }
          },
          error = function(err) {
            log_error("{err}")
            alert.error(err$message)
            shinyjs::enable("parameterSave")
            FALSE
          })

          if (added) {
            log_debug("Parameter saved")
            parent$parameters <- store$pg$get_parameters()

            clear()

            removeModal()
          }
        }
      }, ignoreInit = TRUE)

      observeEvent(input$parameterCancel, {
        clear()
        removeModal()
      }, ignoreInit = TRUE)

      clear <- function() {
        for (value in isolate(parent$domainList)) {
          private$clearValue(paste0(value, "-delete"), input, ns)
        }
        parent$domainList <- isolate(c())
        parent$parameter <- NULL
        lapply(private$removeObserver, function(o) o$destroy())
        private$removeObserver <- list()
      }
    }
  )
)
