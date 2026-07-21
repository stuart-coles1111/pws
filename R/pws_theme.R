#' Standard theme for Playing With Statistics apps
#'
#' @return A bslib theme object
#' @export
#'
pws_theme <- function(){

    bslib::bs_theme(
        version = 5,
        bootswatch = "minty",

        primary = "#7B9ACC",
        secondary = "#CDB4DB",

        bg = "#F7F7FB",
        fg = "#2E3440",

        base_font = "Inter, Segoe UI, Roboto, Helvetica Neue, Arial, sans-serif"
    )

}
