#' Run  App 7
#' Executes app for Activity 7 in Playing With Statistics
#'
#' @examples
#' activity7_run_app()
#'
#' @export
#'
activity7_run_app <- function() {

    appDir <- system.file("shiny", "app_activity7.R", package = "pws")
    if (appDir == "") {
        stop("Could not find directory pws. Try re-installing.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}
