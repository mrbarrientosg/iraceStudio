SummonerInput <- R6::R6Class(
  classname = "SummonerInput",
  inherit = Component,
  public = list(
    ui = function(inputId, label = NULL, value = "", ...) {
      tagList(
        if (!is.null(label)) tags$label(`for` = inputId, label),
        tags$div(
          class = "summernoteInput", id = inputId, style = "height: 200px;",
          `data-options` = toJSON(list(...), auto_unbox = TRUE), HTML(value)
        )
      )
    },

    updateSummernoteInput = function(inputId, value, session = getDefaultReactiveDomain()) {
      session$sendCustomMessage("updateSummernoteInput", list(inputId = inputId, value = value))
    }
  )
)