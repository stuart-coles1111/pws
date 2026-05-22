#' Run  App 4
#' Executes app for Activity 4 in Playing With Statistics
#'
#' @examples
#' activity4_run_app()
#'
#' @export
#'
activity4_run_app <- function() {

    appDir <- system.file("shiny", "appb.R", package = "pws")
    if (appDir == "") {
        stop("Could not find directory pws. Try re-installing.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}
