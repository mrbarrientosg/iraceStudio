PlaygroundView <- R6::R6Class(
  classname = "PlaygroundView",
  inherit = View,
  public = list(
    ui = function() {
      ns <- NS(self$id)
      
      tagList(
        div(class = "sub-header", h2("Playground")),
        fluidRow(
          bs4TabCard(
            id = ns("playground_options"),
            title = "",
            collapsible = FALSE,
            closable = FALSE,
            side = "left",
            width = 12,
            bs4TabPanel(
              tabName = "Scenarios",
              fluidRow(
                column(
                  width = 12,
                  style = "padding-left: 0px !important;",
                  actionButton(
                    inputId = ns("add"),
                    label = "Add",
                    icon = icon("plus")
                  ),
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
                br("\n"),
                DTOutput(outputId = ns("scenarios"))
              )
            ),
            bs4TabPanel(
              tabName = "Options",
              disabled(
                textInput(
                  inputId = ns("playgroundName"),
                  label = "Playground Name"
                )
              ),
              textAreaInput(
                inputId = ns("playgroundDescription"),
                label = "Description"
              )
            )
          )
        )
      )
    },
    
    server = function(input, output, session, store) {
      ns <- session$ns
      
      data <- reactiveValues(scenarios = self$scenarios_as_data_frame(store))
  
      observeEvent(store$pg, {
        updateTextInput(
          session = session,
          inputId = "playgroundName",
          value = store$pg$get_name()
        )
  
        updateTextInput(
          session = session,
          inputId = "playgroundDescription",
          value = store$pg$get_description()
        )
      })
      
      output$scenarios <- renderDT({
        shiny::validate(
          need(store$pg$get_update_observer()() != 0, "")
        )
        
        datatable(
          data = data$scenarios,
          escape = FALSE,
          selection = "single",
          colnames = c("ID", "Name", "Description", "Creation Date"),
          rownames = FALSE,
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
      
      observeEvent(input$add, {
        showModal(
          modalDialog(
            title = "Add a new scenario",
            easyClose = TRUE,
            textInput(inputId = ns("scenario_name"), label = "Name"),
            textAreaInput(inputId = ns("scenario_description"), label = "Description"),
            footer = tagList(
              actionButton(inputId = ns("add_scenario"), label = "Add", type = "primary"),
              modalButton(label = "Cancel")
            )
          )
        )
      })
      
      observeEvent(input$add_scenario, {
        if (is.null(input$scenario_name) || input$scenario_name == "") {
          alert.error("Scenario name is empty.")
          return(invisible())
        }
        
        scenario <- scenario$new(
          name = input$scenario_name,
          description = input$scenario_description
        )
        
        store$pg$add_scenario(scenario)
        
        data$scenarios <- self$scenarios_as_data_frame(store)
        
        removeModal()
      })
      
      observeEvent(input$edit, {
        req(input$scenarios_rows_selected)
        
        scenario <- data$scenarios[input$scenarios_rows_selected, ]
        scenario <- store$pg$get_scenario(scenario$id)
        showModal(
          modalDialog(
            title = "Edit scenario",
            easyClose = TRUE,
            textInput(inputId = ns("scenario_name"), label = "Name", value = scenario$get_name()),
            textAreaInput(inputId = ns("scenario_description"), label = "Description", value = scenario$get_description()),
            footer = tagList(
              actionButton(inputId = ns("update_scenario"), label = "Save", type = "primary"),
              modalButton(label = "Cancel")
            )
          )
        )
      })
      
      observeEvent(input$update_scenario, {
        scenario <- data$scenarios[input$scenarios_rows_selected, ]
        scenario <- store$pg$get_scenario(scenario$id)
        
        if (scenario$get_name() != input$scenario_name) {
          playgroundPath <- file.path(store$gui$workspacePath, store$pg$get_name())
          old <- file.path(playgroundPath, scenario$get_name())
          
          if (dir.exists(old)) {
            new <- file.path(playgroundPath, input$scenario_name)
            
            if (!dir.create(new)) {
              dir.create(new)
            }
            
            file.rename(old, new)
          }
        }
        
        scenario$set_name(input$scenario_name)
        scenario$set_description(input$scenario_description)
        store$pg$set_update_observer(isolate(store$pg$get_update_observer()()) + 1)
        
        data$scenarios <- self$scenarios_as_data_frame(store)
        
        removeModal()
      })
      
      observeEvent(input$delete, {
        req(input$scenarios_rows_selected)
        
        showModal(
          modalDialog(
            title = "Warning",
            HTML(
              paste(
                "Are you sure to delete", tags$b(input$scenarios_rows_selected), "param?"
              )
            ),
            footer = tagList(
              actionButton(inputId = ns("confirm_delete"), label = "Yes", type = "primary"),
              modalButton(label = "Cancel")
            ),
            easyClose = TRUE
          )
        )
      })
      
      observeEvent(input$confirm_delete, {
        store$pg$remove_scenario(input$scenarios_rows_selected)
        
        data$scenarios <- self$scenarios_as_data_frame(store)
        
        removeModal()
      })
      
      observeEvent(input$scenarios_rows_selected,{
        if (store$startIrace) {
          return(invisible())
        }
        
        if (is.null(input$scenarios_rows_selected)) {
          disable(id = "edit")
          disable(id = "delete")
        } else {
          enable(id = "edit")
          enable(id = "delete")
        }
      }, ignoreNULL = FALSE)
      
      observeEvent(store$startIrace, {
        if (store$startIrace) {
          disable(id = "add")
          disable(id = "edit")
          disable(id = "delete")
        } else {
          enable(id = "add")
          enable(id = "edit")
          enable(id = "delete")
        }
      })
      
      observeEvent(input$playgroundName, {
        store$pg$set_name(input$playgroundName)
        store$playgroundName <- input$playgroundName
      })
      
      observeEvent(input$playgroundDescription, store$pg$set_description(input$playgroundDescription))
    },
    
    scenarios_as_data_frame = function(store) {
      data <- data.frame(stringsAsFactors = FALSE)
      pg <- isolate(store$pg)
      scenarios <- pg$get_scenarios()
      
      for (name in names(scenarios)) {
        scenario <- scenarios[[name]]
        data_row <- data.frame(
          id = scenario$get_id(),
          name = scenario$get_name(),
          description = scenario$get_description(),
          creation_date = scenario$get_creation_date(),
          stringsAsFactors = FALSE
        )
        data <- rbind(data, data_row)
      }
      
      return(data)
    }
  )
)