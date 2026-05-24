#' Run  App 1
#' Executes app for Activity 1 in Playing With Statistics
#'
#' @examples
#' activity1_run_app()
#'
#' @export
#'
activity1_run_app <- function() {

    appDir <- system.file("shiny", "app_activity1.R", package = "pws")
    if (appDir == "") {
        stop("Could not find directory pws. Try re-installing.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}
