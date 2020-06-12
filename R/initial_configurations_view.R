InitialConfigurationsView <- R6::R6Class(
  classname = "InitialConfigurationsView",
  inherit = View,
  public = list(
    ui = function() {
      ns <- NS(self$id)
      
      tagList(
        div(class = "sub-header", h2("Initial Configurations")),
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
              )
            )
          ),
          column(
            width = 4,
            class = "d-flex align-items-center justify-content-end",
            importButton(inputId = ns("load")),
            exportButton(
              inputId = ns("export"),
              filename = "configurations.txt",
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
            DTOutput(outputId = ns("initial_config_table")),
            br()
          )
        )
      )
    },
    
    server = function(input, output, session, store) {
      ns <- session$ns
      
      clear <- callModule(
        module = clear_button_sv,
        id = "clear",
        message = "This action will remove all configurations. Are you sure?."
      )
      
      values <- reactiveValues(configurations = NULL)
      
      volum <- c(root = path_home())
      
      shinyFileSave(input = input, id = "export", roots = volum)
      
      observeEvent(input$export, {
        if (!is.integer(input$export)) {
          file <- parseSavePath(roots = volum, selection = input$export)
          export_initial_configurations(file, store)
        }
      })
      
      shinyFileChoose(input, "load", roots = volum, filetypes = "txt")
      
      observeEvent(input$load, {
        if (!is.integer(input$load)) {
          import_initial_configurations(input, store)
        }
      })
      
      observe({
        change_scenario <- store$pg$get_change_current()
        change_scenario()
        
        values$configurations <- store$pg$get_configurations()
      })
      
      observeEvent(values$configurations, {
        proxy %>%
          replaceData(
            data = values$configurations,
            resetPaging = FALSE,
            rownames = FALSE
          )
      })
      
      output$initial_config_table <- renderDT({
        store$updateConfig
        
        shiny::validate(
          need(nrow(store$pg$get_parameters()) > 0, "Empty parameters")
        )
        
        datatable(
          data = store$pg$get_configurations(),
          escape = FALSE,
          rownames = FALSE,
          selection = "single",
          style = "bootstrap4",
          class = "table-condensed table-striped cell-border",
          options = list(
            scrollX = TRUE,
            language = list(
              zeroRecords = "There is no configurations to show"
            )
          )
        )
      })
  
      proxy <- dataTableProxy("initial_config_table")
  
      observe({
        condition <- !is.null(input$initial_config_table_rows_selected) & nrow(values$configurations) > 0
        toggleState(id = "edit", condition)
        toggleState(id = "delete", condition)
        toggle(id = "tip", condition = nrow(values$configurations) > 0)
      })
      
      output$tip <- renderUI(strong("* Select a row to delete or edit."))
      
      observeEvent(input$add, {
        if (nrow(store$pg$get_parameters()) == 0) {
          alert.error(
            message = "There are no parameters. First add a parameter in the parameter section."
          )
          return(invisible())
        }
        
        showModal(
          modalDialog(
            title = "Add a new configuration",
            create_initial_modal_content(ns, NULL, store),
            style = "overflow-y: scroll; max-height:650px;",
            footer = tagList(
              actionButton(inputId = ns("add_config"), label = "Add", type = "primary"),
              modalButton(label = "Cancel")
            )
          )
        )
      })
      
      observeEvent(input$add_config, {
        log_debug("Adding a new configuration")
        
        data <- list()
        
        for (row in seq_len(nrow(store$pg$get_parameters()))) {
          param <- store$pg$get_parameter(row)
          name <- as.character(param$names)
          data[[name]] <- input[[name]]
        }
        
        row <- data.frame(data, stringsAsFactors = FALSE)
        
        store$pg$add_configuration(row)
        
        values$configurations <- store$pg$get_configurations()
        
        log_debug("Configuration added")
        
        removeModal()
      })
      
      observeEvent(input$edit, {
        if (is.null(input$initial_config_table_rows_selected) ||
          is.na(input$initial_config_table_rows_selected)) {
          shinyalert(
            title = "Error",
            text = "Please select the configuration that you want to edit!",
            type = "error"
          )
        } else {
          configuration <- store$pg$get_configuration(input$initial_config_table_rows_selected)
          
          showModal(
            modalDialog(
              title = "Add a new configuration",
              create_initial_modal_content(ns, configuration, store),
              style = "overflow-y:scroll; max-height:650px;",
              footer = tagList(
                actionButton(inputId = ns("confirm_update"), label = "Update", type = "primary"),
                modalButton(label = "Cancel")
              )
            )
          )
        }
      })
      
      observeEvent(input$confirm_update, {
        log_debug("Editing a configuration")
        
        data <- list()
        
        for (row in seq_len(nrow(store$pg$get_parameters()))) {
          param <- store$pg$get_parameter(row)
          name <- as.character(param$names)
          data[[name]] <- input[[name]]
        }
        
        row <- data.frame(data, stringsAsFactors = FALSE)
        
        store$pg$update_configuration(input$initial_config_table_rows_selected, row)
        
        values$configurations <- store$pg$get_configurations()
        
        log_debug("Configuration edited")
        removeModal()
      })
      
      observeEvent(input$delete, {
        if (is.null(input$initial_config_table_rows_selected) ||
          is.na(input$initial_config_table_rows_selected)) {
          shinyalert(
            title = "Error",
            text = "Please select the configuration that you want to delete!",
            type = "error"
          )
        } else {
          showModal(
            modalDialog(
              title = "Warning",
              HTML(
                paste(
                  "Are you sure to delete",
                  tags$b(input$initial_config_table_rows_selected),
                  "configuration?"
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
        log_debug("Deleting a configuration")
        
        store$pg$remove_configuration(input$initial_config_table_rows_selected)
        
        values$configurations <- store$pg$get_configurations()
        
        log_debug("Configuration deleted")
        removeModal()
      })
      
      observeEvent(clear$action, {
        log_debug("Removing all configurations from table")
        
        store$pg$clear_configurations()
        
        values$configurations <- store$pg$get_configurations()
        
        log_debug("All configurations removed")
      })
    }
  )
)