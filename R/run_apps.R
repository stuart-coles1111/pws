run_lab <- function() {
    shiny::runApp(
        system.file("shiny", "stats_lab", package = "pws"),
        launch.browser = TRUE
    )
}

run_activity1 <- function() {
    shiny::runApp(
        system.file("shiny", "app_activity1", package = "pws"),
        launch.browser = TRUE
    )
}

run_activity2 <- function() {
    shiny::runApp(
        system.file("shiny", "app_activity2", package = "pws"),
        launch.browser = TRUE
    )
}

run_activity3 <- function() {
    shiny::runApp(
        system.file("shiny", "app_activity3", package = "pws"),
        launch.browser = TRUE
    )
}

run_activity4 <- function() {
    shiny::runApp(
        system.file("shiny", "app_activity4", package = "pws"),
        launch.browser = TRUE
    )
}

run_activity5 <- function() {
    shiny::runApp(
        system.file("shiny", "app_activity5", package = "pws"),
        launch.browser = TRUE
    )
}

run_activity6 <- function() {
    shiny::runApp(
        system.file("shiny", "app_activity6", package = "pws"),
        launch.browser = TRUE
    )
}

run_activity7 <- function() {
    shiny::runApp(
        system.file("shiny", "app_activity7", package = "pws"),
        launch.browser = TRUE
    )
}

run_activity8 <- function() {
    shiny::runApp(
        system.file("shiny", "app_activity8", package = "pws"),
        launch.browser = TRUE
    )
}

launch <- function() {

    shiny::runApp(
        system.file(
            "shiny",
            "launcher",
            package = "pws"
        ),
        launch.browser = TRUE
    )

}
