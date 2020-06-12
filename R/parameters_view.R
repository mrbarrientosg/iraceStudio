ParametersView <- R6::R6Class(
  classname = "ParametersView",
  inherit = View,
  public = list(
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
            DTOutput(outputId = ns("parameters_table")),
            br()
          )
        )
      )
    },
    
    server = function(input, output, session, store) {
      ns <- session$ns
      
      values <- reactiveValues(parameters = NULL)
  
      clear <- callModule(
        module = clear_button_sv,
        id = "clear",
        message = "This action will remove all parameters. Are you sure?."
      )
      
      output$parameters_table <- renderDT(
        datatable(
          data = store$pg$get_parameters(),
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
      )
      
      output$tip <- renderUI(strong("* Select a row to delete or edit."))
      
      # Para poder modificar la tabla de parametros despues de instanciarse
      proxy <- dataTableProxy(outputId = "parameters_table")
      
      observe({
        change_scenario <- store$pg$get_change_current()
        change_scenario()
        
        values$parameters <- store$pg$get_parameters()
      })
      
      observe({
        store$updateConfig <- isolate(store$updateConfig + 1)
        
        proxy %>%
          replaceData(
            data = values$parameters,
            resetPaging = FALSE,
            rownames = FALSE
          )
      })
      
      volum <- c(root = path_home())
      
      shinyFileChoose(input, "load", roots = volum, filetypes = "txt")
      
      observeEvent(input$load, {
        if (!is.integer(input$load)) {
          file <- parseFilePaths(roots = volum, input$load)
          log_info("Importing paremeter file from {file$datapath}")
          
          data <- tryCatch(readParameters(file = file$datapath),
            error = function(err) {
              log_error("{err}")
              alert.error(message = as.character(err))
              return(NULL)
            }
          )
          
          if (is.null(data)) {
            return(invisible())
          }
          
          store$pg$add_parameter(extract.parameters(data))
          
          values$parameters <- store$pg$get_parameters()
        }
      })
      
      shinyFileSave(input = input, id = "export", roots = volum)
      
      observeEvent(input$export, {
        if (!is.integer(input$export)) {
          file <- parseSavePath(roots = volum, selection = input$export)
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
        data <- tryCatch(readParameters(text = parameters),
          error = function(err) {
            log_error("{err}")
            alert.error(as.character(err))
            return(NULL)
          }
        )
        
        if (!is.null(data)) {
          log_debug("Parameters check successfully")
          shinyalert(
            title = "Check",
            text = "The check is successfully.",
            type = "success"
          )
        }
      })
      
      types <- c(
        "Real" = "r",
        "Integer" = "i",
        "Cardinal" = "c",
        "Ordinal" = "o",
        "RealLog" = "r,log",
        "IntegerLog" = "i,log"
      )
      
      # Show modal to add a new parameter
      observeEvent(input$add, {
        showModal(
          modalDialog(
            title = "Add a new parameter",
            easyClose = TRUE,
            textInput(inputId = ns("parameter_name"), label = "Name"),
            textInput(inputId = ns("parameter_flag"), label = "Flag"),
            selectInput(
              inputId = ns("parameter_type"),
              label = "Type",
              choices = types
            ),
            textInput(inputId = ns("parameter_domain"), label = "Domain"),
            textAreaInput(
              inputId = ns("parameter_conditions"),
              label = "Conditions"
            ),
            footer = tagList(
              actionButton(inputId = ns("add_parameter"), label = "Add", type = "primary"),
              modalButton(label = "Cancel")
            )
          )
        )
      })
      
      # Handle the new parameter to add to table
      observeEvent(input$add_parameter, {
        log_debug(
          paste(
            "Adding a new parameter with",
            "name: {input$parameter_name}, flag: {input$parameter_flag}"
          )
        )
        
        new_row <- validate_parameters_input(input)
        
        store$pg$add_parameter(new_row)
        
        values$parameters <- store$pg$get_parameters()
        
        log_debug("Parameter added")
        
        removeModal()
      })
      
      # show mdoal to edit a parameter
      observeEvent(input$edit, {
        if (is.null(input$parameters_table_rows_selected) ||
          is.na(input$parameters_table_rows_selected)) {
          shinyalert(
            title = "Error",
            text = "Please select the parameter that you want to edit!",
            type = "error"
          )
        } else {
          parameter <- store$pg$get_parameter(input$parameters_table_rows_selected)
          
          showModal(
            modalDialog(
              title = "Edit",
              easyClose = TRUE,
              textInput(
                inputId = ns("parameter_name"),
                label = "Name",
                value = parameter$names
              ),
              textInput(
                inputId = ns("parameter_flag"),
                label = "Flag",
                value = gsub("\"", "", parameter$switches)
              ),
              selectInput(
                inputId = ns("parameter_type"),
                label = "Type",
                choices = types,
                selected = parameter$types
              ),
              textInput(
                inputId = ns("parameter_domain"),
                label = "Domain",
                value = parameter$domain
              ),
              textAreaInput(
                inputId = ns("parameter_conditions"),
                label = "Conditions",
                value = parameter$conditions
              ),
              footer = tagList(
                actionButton(inputId = ns("confirm_update"), label = "Update", type = "primary"),
                modalButton(label = "Cancel")
              )
            )
          )
        }
      })
      
      observeEvent(input$confirm_update, {
        log_debug(
          paste(
            "Editing a parameter with",
            "name: {input$parameter_name}, flag: {input$parameter_flag}"
          )
        )
        
        new_row <- validate_parameters_input(input)
        
        log_debug("Parameter edited")
        
        store$pg$update_parameter(
          row = input$parameters_table_rows_selected,
          new_parameter = new_row
        )
        
        values$parameters <- store$pg$get_parameters()
        
        removeModal()
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
                actionButton(inputId = ns("confirm_delete"), label = "Yes", type = "primary"),
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