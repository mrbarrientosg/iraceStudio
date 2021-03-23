#' @export
SummonerInput <- R6::R6Class( # nolint
  classname = "SummonerInput",
  inherit = Component,
  public = list(
    ui = function(input_id, label = NULL, value = "", ...) {
      tagList(
        if (!is.null(label)) tags$label(`for` = input_id, label),
        tags$div(
          class = "summernoteInput", id = input_id, style = "height: 200px;",
          `data-options` = toJSON(list(...), auto_unbox = TRUE), HTML(value)
        )
      )
    },

    updateSummernoteInput = function(input_id, value, session = getDefaultReactiveDomain()) {
      session$sendCustomMessage("updateSummernoteInput", list(inputId = input_id, value = value))
    }
  )
)
