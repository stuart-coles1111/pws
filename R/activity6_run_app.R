#' Run  App 6
#' Executes app for Activity 6 in Playing With Statistics
#'
#' @examples
#' activity6_run_app()
#'
#' @export
#'
activity6_run_app <- function() {

    appDir <- system.file("shiny", "app_activity6.R", package = "pws")
    if (appDir == "") {
        stop("Could not find directory pws. Try re-installing.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}
