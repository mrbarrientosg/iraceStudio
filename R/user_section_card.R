UserSectionCard <- R6::R6Class(
  classname = "UserSectionCard",
  inherit = Component,
  public = list(
    summonerInput = NULL,

    initialize = function() {
      self$summonerInput <- SummonerInput$new()
    },

    ui = function(inputId, title, value = "") {
      ns <- NS(inputId)

      box(
        id = ns("card"),
        title = strong(title),
        collapsible = TRUE,
        closable = TRUE,
        width = 12,
        self$summonerInput$ui(inputId = ns("content"), value = value, height = 300)
      )
    },

    server = function(input, output, session, report_id, report, store) {
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
      observeEvent(store$copy$plot, {
        req(store$copy$id)
        req(store$copy$id == report_id)

        images <- lapply(pkg$reportStore[[store$copy$plot]], function(plot) {
          paste0(tags$img(src = plot))
        })

        self$summonerInput$updateSummernoteInput(
          session$ns("content"),
          paste(images, "</br>", collapse = "</br>", sep = "</br>")
        )

        store$copy$id <- NULL
        store$copy$plot <- NULL
      })

      # Table

      observeEvent(store$copy$table, {
        req(store$copy$id)
        req(store$copy$id == report_id)

        table <- pkg$reportStore[[store$copy$table]]

        header <- ""
        headerType <- 0
        if (store$copy$table == "bestSoFarTable") {
          headerType <- 1
          header <- strong("Best so far configuration:")
        } else {
          headerType <- 2
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
          if (headerType == 1) {
            data <- c(data, paste(header, "<ul>", ul, "</ul>", sep = "\n"))
          } else {
            data <- c(data, paste0(strong(sprintf(header, row)), "<ul>", ul, "</ul>", collapse = ""))
          }
        }

        self$summonerInput$updateSummernoteInput(
          session$ns("content"),
          paste(data, "</br>")
        )

        store$copy$id <- NULL
        store$copy$table <- NULL
      })

      observeEvent(input$add, {
        showModal(
          modalDialog(
            title = "Add a new section",
            easyClose = TRUE,
            textInput(inputId = session$ns("title"), label = "Title"),
            footer = tagList(
              actionButton(inputId = session$ns("addSection"), label = "Add"),
              modalButton(label = "Dissmis")
            )
          )
        )
      })

      return(values)
    }
  )
)