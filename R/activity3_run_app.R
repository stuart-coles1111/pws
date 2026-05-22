#' Run  App 3
#' Executes app for Activity 3 in Playing With Statistics
#'
#' @examples
#' activity3_run_app()
#'
#' @export
#'
activity3_run_app <- function() {

    appDir <- system.file("shiny", "app_activity3.R", package = "pws")
    if (appDir == "") {
        stop("Could not find directory pws. Try re-installing.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}
