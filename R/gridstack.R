useGridstack <- function() {
    add_resource_path(
        "gridstack", appSys("app/www/gridstack")
    )

    singleton(
        tags$head(
            tags$script(src = file.path("gridstack", "gridstack.min.js")),
            tags$script(src = file.path("gridstack", "gridstack.jQueryUI.min.js")),
            tags$script(src = file.path("gridstack", "gridstack-binding.js")),
            tags$link(rel = "stylesheet", href = file.path("gridstack", "gridstack.min.css"))
        )
    )
}

gridStack <- function(...) {
    tags$div(class = "grid-stack", ...)
}

gridItem <- function(...) {
    tags$div(
        class = "grid-stack-item",
        "gs-x" = 0, "gs-y" = 0,
        "gs-width" = 5, "gs-height" = 5,
        tags$div(
            class = "grid-stack-item-content",
            ...
        )
    )
}

addWidgetGrid <- function(inputId, value, session = getDefaultReactiveDomain()) {
    session$sendCustomMessage("addWidgetGrid", list(inputId = inputId, value = value))
}