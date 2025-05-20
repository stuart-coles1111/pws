#' Run Horse Race Simulation App
#' Executes simulation version of horse race app for Activity 8 in Playing With Statistics
#'
#' @examples
#' activity8_run_app_sim()
#'
#' @export
#'


activity8_run_app_sim <- function() {

    appDir <- system.file("shiny", "app_horse_race_game_sim.R", package = "pws")

    if (appDir == "") {
        stop("Could not find directory pws. Try re-installing.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}

