#' Launch Statistics Lab
#'
#' Interactive coding environment for the book.
#'
#' @export

launch_stats_lab <- function(){

    app_dir <- system.file(
        "shiny/stats_lab",
        package = "pws"
    )

    shiny::runApp(app_dir)
}
