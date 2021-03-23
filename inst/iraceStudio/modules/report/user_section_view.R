UserSectionView <- R6::R6Class( # nolint
  classname = "UserSectionView",
  inherit = View,
  public = list(
    cards = NULL,
    observe_delete_card = NULL,
    count = 0,
    user_section_card = NULL,

    initialize = function(id) {
      super$initialize(id)
      self$cards <- list()
      self$observe_delete_card <- list()
      self$count <- 0
      self$user_section_card <- UserSectionCard$new()
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
            bs4Dash::actionButton(
              inputId = ns("add"),
              label = "Add note",
              class = "float-right",
              style = "margin-left: 5px;",
              status = "primary"
            )
          )
        ),
        fluidRow(
          id = "userSectionContent"
        )
      )
    },

    server = function(input, output, session, store, events) {
      ns <- session$ns

      observeEvent(input$add, {
        if (is.null(store$current_execution)) {
          alert_error("Select a execution or execute irace first.")
        } else {
          showModal(
            modalDialog(
              title = "Add a new note",
              easyClose = TRUE,
              textInput(inputId = ns("title"), label = "Title"),
              footer = tagList(
                bs4Dash::actionButton(inputId = ns("addSection"), label = "Add", status = "primary"),
                modalButton(label = "Cancel")
              )
            )
          )
        }
      })

      observeEvent(store$current_execution,
        {
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

          report <- store$current_execution$get_report()

          data <- report$get_data()

          self$cards <- list()
          self$observe_delete_card <- list()
          self$count <- report$get_count()

          for (id in names(data)) {
            insertUI(
              selector = "#userSectionContent",
              where = "beforeEnd",
              ui = self$user_section_card$ui(
                input_id = ns(id),
                title = data[[id]]$title,
                value = data[[id]]$content
              )
            )

            self$cards[[id]] <- self$user_section_card$call(
              id = id,
              report_id = id,
              report = report,
              store = store,
              events = events
            )

            local({
              my_id <- id
              .report <- report
              self$observe_delete_card[[id]] <- observe({
                req(self$cards[[my_id]]$card$visible != TRUE)

                self$cards[[my_id]] <- NULL
                self$observe_delete_card[[my_id]]$destroy()
                self$observe_delete_card[[my_id]] <- NULL
                .report$remove_data(my_id)
                update_reactive_counter(events$update_report)
                removeUI(
                  selector = paste0("div:has(> #", ns(my_id), "-card)"),
                  immediate = TRUE
                )
                self$remove_shiny_inputs(my_id, input, ns)
              })
            })
          }
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      observeEvent(input$addSection,
        {
          removeModal()

          report <- store$current_execution$get_report()
          report$add_title(input$title)
          update_reactive_counter(events$update_report)

          self$count <- self$count + 1

          id <- as.character(self$count)

          insertUI(
            selector = "#userSectionContent",
            where = "beforeEnd",
            ui = self$user_section_card$ui(
              input_id = ns(id),
              title = input$title
            )
          )

          self$cards[[id]] <- self$user_section_card$call(
            id = id,
            report_id = id,
            report = report,
            store = store,
            events = events
          )

          local({
            my_id <- id
            .report <- report
            self$observe_delete_card[[id]] <- observe({
              req(self$cards[[my_id]]$card$visible != TRUE)

              self$cards[[my_id]] <- NULL
              self$observe_delete_card[[my_id]]$destroy()
              self$observe_delete_card[[my_id]] <- NULL
              .report$remove_data(my_id)
              update_reactive_counter(events$update_report)
              removeUI(
                selector = paste0("div:has(> #", ns(my_id), "-card)"),
                immediate = TRUE
              )
              self$remove_shiny_inputs(my_id, input, ns)
            })
          })
        },
        ignoreInit = TRUE
      )
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
