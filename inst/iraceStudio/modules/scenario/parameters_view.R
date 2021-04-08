ParametersView <- R6::R6Class( # nolint
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
        div(
          class = "sub-header",
          h2("Parameters"),
          p("Add, remove or modify parameter definitions"),
          HTML("<ul>
                <li>Name: name to identify a parameter in irace (e.g. tabuSize)</li>
                <li>Switch: (optional) command line flag to pass the parameter value to the target runner (e.g. --tsize )</li>
                <li>Type: parameter type (real, integer, categorical or ordered)</li>
                <li>Domain: parameter domain (a range for numerical parameters, or a set for categorical and ordered parameters)</li>
                <li>Condition: activation condition (in R) based on the values of other parameters (e.g. searchType == \"tabu\")  </li>
                </ul>
                For more information, go to the irace package <a href=\"https://cran.r-project.org/package=irace/vignettes/irace-package.pdf\" target=\"_blank\">user guide</a>") # nolint
        ),
        fluidRow(
          column(
            width = 8,
            bs4Dash::actionButton(
              inputId = ns("add"),
              label = "Add",
              icon = icon("plus")
            ),
            disabled(
              bs4Dash::actionButton(
                inputId = ns("edit"),
                label = "Edit",
                icon = icon("edit")
              ),
              bs4Dash::actionButton(
                inputId = ns("delete"),
                label = "Delete",
                icon = icon("minus")
              ),
              bs4Dash::actionButton(
                inputId = ns("check"),
                label = "Check",
                icon = icon("check")
              )
            )
          ),
          column(
            width = 4,
            class = "d-flex align-items-center justify-content-end",
            import_button(
              input_id = ns("load")
            ),
            export_button(
              input_id = ns("export"),
              filename = "parameters.txt",
              style = "margin-left: 5px;"
            ),
            clear_button(input_id = ns("clear"), style = "margin-left: 5px;")
          )
        ),
        br(),
        fluidRow(
          box(
            title = strong("Parameters"),
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

    server = function(input, output, session, store, events) {
      ns <- session$ns

      values <- reactiveValues(
        parameters = NULL,
        domain_list = c(),
        parameter = NULL
      )

      clear <- callModule(
        module = clear_button_sv,
        id = "clear",
        message = "This action will remove all parameters. Are you sure?."
      )

      output$parameters_table <- DT::renderDataTable(
        {
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

      observeEvent(c(events$change_scenario, store$pg),
        {
          values$parameters <- store$pg$get_parameters()
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      observeEvent(values$parameters, {
        events$update_parameters <- update_reactive_counter(events$update_parameters)

        proxy %>%
          replaceData(
            data = values$parameters,
            resetPaging = FALSE,
            rownames = FALSE
          )
      })

      volumes <- c("Home" = path.expand("~"), getVolumes()())

      shinyFileChoose(input, "load", roots = volumes)

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
            alert_error("Can't load parameters file, check if the file format is correct.")
            return(invisible())
          }

          log_info("Importing paremeter file from {file$datapath}")

          tryCatch(
            {
              data <- readParameters(file = file$datapath)

              store$pg$add_parameter(extract_parameters(data))

              values$parameters <- store$pg$get_parameters()
            },
            error = function(err) {
              log_error("{err}")
              alert_error(err$message)
            }
          )
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
        tryCatch(
          {
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
            alert_error(err$message)
          }
        )
      })

      # Show modal to add a new parameter
      observeEvent(input$add, {
        values$parameter <- NULL

        showModal(
          self$modal$ui(ns, "New parameter")
        )

        # self$modal$server(input, output, session, store = store, values)
      })

      self$modal$server(input, output, session, store = store, values)

      # show modal to edit a parameter
      observeEvent(input$edit, {
        parameter <- as.list(store$pg$get_parameter(input$parameters_table_rows_selected))
        values$parameter <- parameter

        showModal(
          self$modal$ui(ns, "Edit", parameter)
        )

        # self$modal$server(input, output, session, store = store, values, parameter)
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
                bs4Dash::actionButton(inputId = ns("confirm_delete"), label = "Yes", status = "danger"),
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

ModalParameter <- R6::R6Class( # nolint
  classname = "ModalParameter",
  private = list(
    remove_observer = list(),

    checkValue = function(key, values) {
      if (is.null(values)) {
        return(NULL)
      }

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
          selected = check_null(private$checkValue("types", values), "r")
        ),
        uiOutput(outputId = ns("domain_output")),
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
          bs4Dash::actionButton(inputId = ns("parameterSave"), label = "Save", status = "primary"),
          bs4Dash::actionButton(inputId = ns("parameterCancel"), label = "Cancel")
        )
      )
    },

    server = function(input, output, session, store, parent) {
      ns <- session$ns

      output$domain_output <- renderUI({
        type <- input$parameterType
        param <- parent$parameter
        default <- check_null(private$checkValue("types", param), "r")
        type <- check_null(type, default)

        if (type == "o" || type == "c") {
          domain <- check_null(private$checkValue("domain", param), c())
          for (value in domain) {
            local({
              my_id <- value
              button_id <- paste0(my_id, "-delete")
              private$remove_observer[[my_id]] <- observeEvent(input[[button_id]],
                {
                  parent$domain_list <- parent$domain_list[!(parent$domain_list %in% my_id)]
                  private$remove_observer[[my_id]] <- NULL
                },
                ignoreInit = TRUE,
                once = TRUE
              )
            })
          }
          parent$domain_list <- domain
          tagList(
            textInput(ns("domainName"), "Domain values (press add)"),
            bs4Dash::actionButton(ns("addDomain"), "Add", class = "btn-link"),
            uiOutput(ns("domain_list"))
          )
        } else {
          domain <- private$checkValue("domain", param)
          fluidRow(
            column(
              width = 6,
              numericInput(ns("domainMin"), "Min", check_null(domain[1], 0))
            ),
            column(
              width = 6,
              numericInput(ns("domainMax"), "Max", check_null(domain[2], 0))
            )
          )
        }
      })

      output$domain_list <- renderUI({
        shiny::validate(
          need(length(parent$domain_list) != 0, "Empty domain")
        )
        tagList(
          bs4Dash::bs4ListGroup(
            id = "sortable",
            .list = lapply(parent$domain_list, function(name) {
              bs4ListGroupItem(
                name,
                bs4Dash::actionButton(
                  inputId = ns(paste0(name, "-delete")),
                  label = NULL,
                  icon = icon("trash"),
                  status = "danger"
                )
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
        if (input$domainName %in% isolate(parent$domain_list)) {
          return()
        }

        id <- as.character(input$domainName)
        parent$domain_list <- c(parent$domain_list, input$domainName)

        local({
          my_id <- id
          name <- input$domainName
          button_id <- paste0(name, "-delete")
          private$remove_observer[[my_id]] <- observeEvent(input[[button_id]],
            {
              parent$domain_list <- parent$domain_list[!(parent$domain_list %in% name)]
              private$remove_observer[[my_id]] <- NULL
              private$clearValue(button_id, input, ns)
            },
            ignoreInit = TRUE,
            once = TRUE
          )
        })

        shinyjs::reset("domainName")
      })

      observeEvent(input$sortable, {
        parent$domain_list <- isolate(input$sortable)
      })

      observeEvent(input$parameterSave,
        {
          shinyjs::disable("parameterSave")

          domain <- if (input$parameterType == "o" || input$parameterType == "c") {
            if (length(parent$domain_list) == 0) {
              alert_error("Domain cannot be empty")
              return(NULL)
            }
            paste0("(", paste0(parent$domain_list, collapse = ", "), ")")
          } else {
            paste0("(", input$domainMin, ", ", input$domainMax, ")")
          }

          if (is.null(domain)) {
            shinyjs::enable("parameterSave")
            return(NULL)
          }

          flag <- gsub('"', "", input$parameterFlag)
          condition <- if (is.null(input$parameterCondition) || input$parameterCondition == "") {
            ""
          } else {
            paste("|", input$parameterCondition)
          }

          new_row <- data.frame(
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
              new_row,
              row.names = FALSE,
              col.names = FALSE,
              sep = "\t",
              quote = F
            )
          )

          result <- tryCatch(
            {
              irace::readParameters(text = check)
              TRUE
            },
            error = function(err) {
              if (!grepl("A parameter definition is missing!", err$message, fixed = TRUE)) {
                log_error("{err}")
                alert_error(err$message)
                shinyjs::enable("parameterSave")
                FALSE
              } else {
                TRUE
              }
            }
          )

          if (result) {
            log_debug(
              paste(
                "Save a new parameter with",
                "name: {input$parameter_name}, flag: {input$parameter_flag}"
              )
            )

            added <- tryCatch(
              {
                if (is.null(parent$parameter)) {
                  store$pg$add_parameter(new_row)
                } else {
                  store$pg$update_parameter(
                    row = input$parameters_table_rows_selected,
                    new_parameter = new_row
                  )
                }
              },
              error = function(err) {
                log_error("{err}")
                alert_error(err$message)
                shinyjs::enable("parameterSave")
                FALSE
              }
            )

            if (added) {
              log_debug("Parameter saved")
              parent$parameters <- store$pg$get_parameters()

              clear()

              removeModal()
            }
          }
        },
        ignoreInit = TRUE
      )

      observeEvent(input$parameterCancel,
        {
          clear()
          removeModal()
        },
        ignoreInit = TRUE
      )

      clear <- function() {
        for (value in isolate(parent$domain_list)) {
          private$clearValue(paste0(value, "-delete"), input, ns)
        }
        parent$domain_list <- isolate(c())
        parent$parameter <- NULL
        lapply(private$remove_observer, function(o) o$destroy())
        private$remove_observer <- list()
      }
    }
  )
)
