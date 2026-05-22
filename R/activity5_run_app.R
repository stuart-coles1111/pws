#' Run  App 5
#' Executes app for Activity 5 in Playing With Statistics
#'
#' @examples
#' activity5_run_app()
#'
#' @export
#'
activity5_run_app <- function() {

    appDir <- system.file("shiny", "app_activity5.R", package = "pws")
    if (appDir == "") {
        stop("Could not find directory pws. Try re-installing.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}
