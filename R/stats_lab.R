#' Launch apps for Playing WIth Statistics
#'
#' Provide menu for launching apps that accompany the book Playing With Statistics
#'
#'
#' @export

stats_lab <- function() {

    apps <- c(
        "Activity 1: Picturing Randomness" =
            "app_activity1",
        "Activity 2: Who Wants to be a Danish Millionaire?" =
            "app_activity2",
        "Activity 3: Place Your Bets" =
            "app_activity3",
        "Activity 4: Quiz Time" =
            "app_activity4",
        "Activity 5: Pick a Card" =
            "app_activity5",
        "Activity 6: The World Record Ski Jump" =
            "app_activity6",
        "Activity 7: The World Cup of Dice" =
            "app_activity7",
        "Activity 8: A Day at the Races" =
            "app_activity8",
        "Statistics Lab" =
            "stats_lab",
        "Exit" = NA
    )

    repeat {

        cat("\n")
        cat(crayon::bold$blue(
            "=========================================\n"
        ))
        cat(crayon::bold$blue(
            "           STATISTICS LAB\n"
        ))
        cat(crayon::bold$blue(
            "=========================================\n\n"
        ))

        choice <- menu(
            names(apps),
            title = "Choose an activity"
        )

        # user cancelled
        if (choice == 0)
            return(invisible(NULL))

        selected <- apps[[choice]]

        # EXIT
        if (is.na(selected)) {
            cat(crayon::green("\nGoodbye!\n\n"))
            return(invisible(NULL))
        }

        app_dir <- system.file(
            "shiny",
            selected,
            package = "pws"
        )

        if (!nzchar(app_dir) || !dir.exists(app_dir)) {
            cat(crayon::red("App not found: "), selected, "\n")
            next
        }

        shiny::runApp(app_dir)
    }
}
