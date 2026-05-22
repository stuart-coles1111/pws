#' Run  App 2
#' Executes app for Activity 2 in Playing With Statistics
#'
#' @examples
#' activity2_run_app()
#'
#' @export
#'
activity2_run_app <- function() {

    appDir <- system.file("shiny", "app_activity2.R", package = "pws")
    if (appDir == "") {
        stop("Could not find directory pws. Try re-installing.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}
