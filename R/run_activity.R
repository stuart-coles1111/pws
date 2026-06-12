run_activity <- function(chapter) {

    is_deployed <- nzchar(Sys.getenv("SHINY_PORT"))

    if (is_deployed) {

        urls <- c(
            "1" = "https://bujx5j-stuart-coles.shinyapps.io/app-activity-1",
            "2" = "https://bujx5j-stuart-coles.shinyapps.io/app-activity-2",
            "3" = "https://bujx5j-stuart-coles.shinyapps.io/app-activity-3",
            "4" = "https://bujx5j-stuart-coles.shinyapps.io/app-activity-4"
        )

        if (as.character(chapter) %in% names(urls)) {
            return(urls[as.character(chapter)])
        } else {
            return(NULL)
        }
    }

    port <- sample(8000:9000, 1)

    logfile <- tempfile(fileext = ".log")

    callr::r_bg(
        func = function(chapter, port, logfile) {

            sink(logfile, append = TRUE, split = TRUE)
            on.exit(sink(NULL), add = TRUE)

            library(pws)

            shiny::runApp(
                system.file(
                    "shiny",
                    paste0("app_activity", chapter, "/app.R"),
                    package = "pws"
                ),
                host = "127.0.0.1",
                port = port,
                launch.browser = FALSE
            )
        },
        args = list(chapter = chapter, port = port, logfile = logfile)
    )

    paste0("http://127.0.0.1:", port)
}
