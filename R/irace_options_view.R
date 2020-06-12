IraceOptionsView <- R6::R6Class(
  classname = "IraceOptionsView",
  inherit = View,
  public = list(
    ui = function() {
      ns <- NS(self$id)
      
      tagList(
        fluidRow(
          class = "sub-header",
          column(
            width = 10,
            h2("Irace Options")
          ),
          column(
            width = 2,
            class = "d-flex align-items-center justify-content-end",
            exportButton(inputId = ns("export"), filename = "scenario.txt")
          )
        ),
        fluidRow(
          uiOutput(outputId = ns("content"), style = "width: 100%;")
        )
      )
    },
    
    server = function(input, output, session, store) {
      ns <- session$ns
      
      volum <- c(root = path_home())
      
      shinyFileSave(input = input, id = "export", roots = volum)
      
      observeEvent(input$export, {
        if (!is.integer(input$export)) {
          file <- parseSavePath(roots = volum, selection = input$export)
          log_debug("Exporting scenario file to {file$datapath}")
          create_scenario_file(path = file$datapath, pg = store$pg, name = NULL, export = TRUE)
          log_debug("Scenario file exported successfully")
          shinyalert(
            title = "Exported",
            text = "The file is exported successfully.",
            type = "success"
          )
        }
      })
      
      output$content <- renderUI({
        change_scenario <- store$pg$get_change_current()
        change_scenario()
        
        args <- c(
          self$create_tabs(ns, store),
          id = ns("tab"),
          title = "",
          collapsible = FALSE,
          closable = FALSE,
          side = "left",
          width = 12
        )
        
        do.call(bs4TabCard, args)
      })
    },
    
    create_tabs = function(ns, store) {
      tabs <- list()
      
      option <- IraceOptionTab$new()
      
      for (name in names(scenarioOptions)) {
        if (name == "testing") {
          next
        }
        
        tab <- bs4TabPanel(
          tabName = scenarioOptions[[name]]$name,
          option$ui(inputId = ns(name), section = name, store = store)
        )
        
        tabs[[name]] <- tab
        
        option$call(id = name, store = store, section = name)
      }
      
      return(tabs)
    }
  )
)