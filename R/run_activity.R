run_activity <- function(chapter) {


    is_deployed <-
        nzchar(Sys.getenv("SHINY_PORT"))

    if (is_deployed) {

        urls <- c(
            "1" = "https://bujx5j-stuart-coles.shinyapps.io/app-activity-1"
        )

        return(urls[as.character(chapter)])
    }


    port <- sample(8000:9000, 1)

    logfile <- tempfile(fileext = ".log")

    callr::r_bg(
        func = function(chapter, port, logfile) {

            sink(logfile, append = TRUE, split = TRUE)
            on.exit(sink(NULL), add = TRUE)

            cat("Starting chapter", chapter, "\n")

            library(pws)

            shiny::runApp(
                system.file(
                    "shiny",
                    paste0("app_activity", chapter, "/app.R"),
                    package = "pws"
                ),
                host = "127.0.0.1",
                port = port,
                launch.browser = FALSE,
                quiet = FALSE
            )

        },
        args = list(chapter = chapter, port = port, logfile = logfile)
    )

    Sys.sleep(1)

    paste0("http://127.0.0.1:", port)
}
