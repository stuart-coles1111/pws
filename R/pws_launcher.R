#' Launch the pws Shiny application launcher
#'
#' Opens the pws Shiny launcher app, which provides access to the
#' package's Shiny applications.
#'
#' @param launch.browser Whether to open the app in the default browser.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return Invisibly returns the result of [shiny::runApp()].
#' @export
#'
#' @examples
#' \dontrun{
#' pws_launcher()
#' }
pws_launcher <- function(launch.browser = TRUE, ...) {

    app_dir <- system.file(
        "shiny",
        "launcher",
        package = "pws"
    )

    if (app_dir == "") {
        stop(
            "Could not find launcher app. ",
            "Is the package installed correctly?"
        )
    }

    shiny::runApp(
        appDir = app_dir,
        launch.browser = launch.browser,
        ...
    )
}
