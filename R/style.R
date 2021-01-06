#' @export
common_theme <- fresh::create_theme(
    theme = "default",
    fresh::bs4dash_vars(
        btn_border_radius = "2px",
        btn_box_shadow = "0 2px 0 rgba(0, 0, 0, .015)",
        input_border_color = "#d9d9d9",
        input_border_radius = "2px",
        input_border_width = "1px"
    ),
    fresh::bs4dash_sidebar_dark(
        bg = "#242939",
        header_color = "#242939"
    ),
    fresh::bs4dash_status(
        primary = "#1890ff",
        danger = "#ff4d4f",
        success = "#52c41a",
        warning = "#faad14"
    ),
    fresh::bs4dash_layout(
        main_bg = "#f2f3f8"
    ),
    fresh::bs4dash_button(
        default_background_color = "#ffffff",
        default_color = "rgba(0,0,0,.85)",
        default_border_color = "#d9d9d9"
    )
)
