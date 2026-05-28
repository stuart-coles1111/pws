#' Run Horse Race App
#' @param mode "multiplayer" or "singleplayer"
#'
#' @export
activity8_run_app <- function(mode = c("multiplayer", "singleplayer")) {

    mode <- match.arg(mode)

    app_file <- switch(
        mode,
        multiplayer = "app_activity8.R",
        singleplayer = "app_activity8_sim.R"
    )

    appDir <- system.file("shiny", app_file, package = "pws")

    if (appDir == "") {
        stop(
            paste0("Could not find app file: ", app_file,
                   ". Try reinstalling the package."),
            call. = FALSE
        )
    }

    shiny::runApp(appDir, display.mode = "normal")
}
