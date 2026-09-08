#' Launch apps for Playing WIth Statistics
#'
#' Provide menu for launching apps that accompany the book Playing With Statistics
#'
#'
#' @export

stats_lab <- function() {

    apps <- stats_lab_apps()

    repeat {

        cat("\n")

        cat(
            crayon::bold$blue(
                "=========================================\n"
            )
        )

        cat(
            crayon::bold$blue(
                "           STATISTICS LAB\n"
            )
        )

        cat(
            crayon::bold$blue(
                "=========================================\n\n"
            )
        )

        choice <- menu(
            names(apps),
            title = "Choose an option"
        )

        if (choice == 0)
            return(invisible(NULL))

        selected <- apps[[choice]]

        if (is.na(selected)) {

            cat(
                crayon::green(
                    "\nGoodbye!\n\n"
                )
            )

            return(invisible(NULL))
        }

        app_dir <- system.file(
            "shiny",
            selected,
            package = "pws"
        )

        if (!nzchar(app_dir) || !dir.exists(app_dir)) {

            cat(
                crayon::red(
                    "App not found: "
                ),
                selected,
                "\n"
            )

            next
        }

        shiny::runApp(app_dir)
    }
}
