UserSectionView <- R6::R6Class(
  classname = "UserSectionView",
  inherit = View,
  public = list(
    cards = NULL,
    observe_delete_card = NULL,
    count = 0,
    userSectionCard = NULL,

    initialize = function(id) {
      super$initialize(id)
      self$cards <- list()
      self$observe_delete_card <- list()
      self$count <- 0
      self$userSectionCard <- UserSectionCard$new()
    },

    ui = function() {
      ns <- NS(self$id)

      tagList(
        fluidRow(
          class = "justify-content-between sub-header",
          column(
            width = 3,
            h2("User Notes"),
          ),
          column(
            width = 4,
            iraceStudio::actionButton(
              inputId = ns("add"),
              label = "Add note",
              class = "float-right btn-primary",
              style = "margin-left: 5px;"
            )
          )
        ),
        fluidRow(
          id = "userSectionContent"
        )
      )
    },

    server = function(input, output, session, store) {
      ns <- session$ns

      observeEvent(input$add, {
        if (is.null(store$currentExecution)) {
          alert.error("Select a execution or execute irace first.")
        } else {
          showModal(
            modalDialog(
              title = "Add a new note",
              easyClose = TRUE,
              textInput(inputId = ns("title"), label = "Title"),
              footer = tagList(
                iraceStudio::actionButton(inputId = ns("addSection"), label = "Add", class = "btn-primary"),
                modalButton(label = "Cancel")
              )
            )
          )
        }
      })

      observeEvent(store$currentExecution, {
        for (id in names(self$cards)) {
          self$cards[[id]] <- NULL
          self$observe_delete_card[[id]]$destroy()
          self$observe_delete_card[[id]] <- NULL
          removeUI(
            selector = paste0("div:has(> #", ns(id), "-card)"),
            immediate = TRUE
          )
          self$remove_shiny_inputs(id, input, ns)
        }

        report <- store$currentExecution$get_report()

        data <- report$get_data()

        self$cards <- list()
        self$observe_delete_card <- list()
        self$count <- report$get_count()

        for (id in names(data)) {
          insertUI(
            selector = "#userSectionContent",
            where = "beforeEnd",
            ui = self$userSectionCard$ui(
              inputId = ns(id),
              title = data[[id]]$title,
              value = data[[id]]$content
            )
          )

          self$cards[[id]] <- self$userSectionCard$call(
            id = id,
            report_id = id,
            report = report,
            store = store
          )

          local({
            myId <- id
            .report <- report
            self$observe_delete_card[[id]] <- observe({
              req(self$cards[[myId]]$card$visible != TRUE)

              self$cards[[myId]] <- NULL
              self$observe_delete_card[[myId]]$destroy()
              self$observe_delete_card[[myId]] <- NULL
              .report$remove_data(myId)
              removeUI(
                selector = paste0("div:has(> #", ns(myId), "-card)"),
                immediate = TRUE
              )
              self$remove_shiny_inputs(myId, input, ns)
            })
          })
        }
      })

      observeEvent(input$addSection, {
        removeModal()

        report <- store$currentExecution$get_report()
        report$add_title(input$title)

        self$count <- self$count + 1

        id <- as.character(self$count)

        insertUI(
          selector = "#userSectionContent",
          where = "beforeEnd",
          ui = self$userSectionCard$ui(
            inputId = ns(id),
            title = input$title
          )
        )

        self$cards[[id]] <- self$userSectionCard$call(
          id = id,
          report_id = id,
          report = report,
          store = store
        )

        local({
          myId <- id
          .report <- report
          self$observe_delete_card[[id]] <- observe({
            req(self$cards[[myId]]$card$visible != TRUE)

            self$cards[[myId]] <- NULL
            self$observe_delete_card[[myId]]$destroy()
            self$observe_delete_card[[myId]] <- NULL
            .report$remove_data(myId)
            removeUI(
              selector = paste0("div:has(> #", ns(myId), "-card)"),
              immediate = TRUE
            )
            self$remove_shiny_inputs(myId, input, ns)
          })
        })
      }, ignoreInit = TRUE)
    },

    remove_shiny_inputs = function(id, .input, ns) {
      invisible(
        lapply(grep(id, names(.input), value = TRUE), function(i) {
          .subset2(.input, "impl")$.values$remove(ns(i))
        })
      )
    }
  )
)