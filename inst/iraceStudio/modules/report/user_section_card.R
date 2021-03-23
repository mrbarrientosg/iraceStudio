UserSectionCard <- R6::R6Class( # nolint
  classname = "UserSectionCard",
  inherit = Component,
  public = list(
    summoner_input = NULL,

    initialize = function() {
      self$summoner_input <- SummonerInput$new()
    },

    ui = function(input_id, title, value = "") {
      ns <- NS(input_id)

      box(
        id = ns("card"),
        title = strong(title),
        collapsible = TRUE,
        closable = TRUE,
        width = 12,
        self$summoner_input$ui(input_id = ns("content"), value = value, height = 300)
      )
    },

    server = function(input, output, session, report_id, report, store, events) {
      values <- reactiveValues()

      observeEvent(input$card, {
        req(input$card)
        values$card <- input$card
      })

      observeEvent(input$content, {
        req(input$content)
        values$content <- input$content
        report$set_content(report_id, input$content)
      })

      # Plot
      observeEvent(events$copy$plot, {
        req(events$copy$id)
        req(events$copy$id == report_id)

        images <- lapply(pkg$report_store[[events$copy$plot]], function(plot) {
          paste0(tags$img(src = plot))
        })

        self$summoner_input$updateSummernoteInput(
          session$ns("content"),
          paste(images, "</br>", collapse = "</br>", sep = "</br>")
        )

        events$copy$id <- NULL
        events$copy$plot <- NULL
      })

      # Table

      observeEvent(events$copy$table, {
        req(events$copy$id)
        req(events$copy$id == report_id)

        table <- pkg$report_store[[events$copy$table]]

        header <- ""
        header_type <- 0
        if (events$copy$table == "bestSoFarTable") {
          header_type <- 1
          header <- strong("Best so far configuration:")
        } else {
          header_type <- 2
          header <- "Configuration of Final Elite %d:"
        }

        l <- list()
        for (row in seq_len(nrow(table))) {
          l[[row]] <- list()
          for (name in names(table)) {
            l[[row]] <- c(
              l[[row]],
              paste0(
                "<li>", strong(paste0(name, ":")), "&nbsp",
                table[row, name],
                "</li>"
              )
            )
          }
        }

        data <- c()
        for (row in seq_len(nrow(table))) {
          ul <- ""
          ul <- paste(ul, lapply(l[[row]], paste), collapse = "\n")
          if (header_type == 1) {
            data <- c(data, paste(header, "<ul>", ul, "</ul>", sep = "\n"))
          } else {
            data <- c(data, paste0(strong(sprintf(header, row)), "<ul>", ul, "</ul>", collapse = ""))
          }
        }

        self$summoner_input$updateSummernoteInput(
          session$ns("content"),
          paste(data, "</br>")
        )

        events$copy$id <- NULL
        events$copy$table <- NULL
      })

      observeEvent(input$add, {
        showModal(
          modalDialog(
            title = "Add a new section",
            easyClose = TRUE,
            textInput(inputId = session$ns("title"), label = "Title"),
            footer = tagList(
              bs4Dash::actionButton(inputId = session$ns("addSection"), label = "Add", status = "primary"),
              modalButton(label = "Dissmis")
            )
          )
        )
      })

      return(values)
    }
  )
)
