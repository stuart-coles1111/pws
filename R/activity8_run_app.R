#' Run Horse Race App
#' Executes horse race app for Activity 8 in Playing With Statistics
#'
#' @examples
#' activity8_run_app()
#'
#' @export
#'
activity8_run_app <- function() {

    appDir <- system.file("shiny", "app_horse_race_game.R", package = "pws")
    if (appDir == "") {
        stop("Could not find directory pws. Try re-installing.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}
