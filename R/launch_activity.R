launch_activity <- function(app_name) {

    appdir <- system.file(
        "shiny",
        app_name,
        package = "pws"
    )

    port <- httpuv::randomPort()

    p <- callr::r_bg(
        function(appdir, port) {

            shiny::runApp(
                appdir,
                host = "127.0.0.1",
                port = port,
                launch.browser = FALSE
            )

        },
        args = list(appdir, port)
    )


    url <- sprintf("http://127.0.0.1:%s", port)

    deadline <- Sys.time() + 15

    repeat {

        ready <- tryCatch(
            {
                con <- url(url)
                close(con)
                TRUE
            },
            error = function(e) FALSE
        )

        if (ready) {
            break
        }

        if (!p$is_alive()) {
            stop("App process terminated before starting.")
        }

        if (Sys.time() > deadline) {
            stop("Timed out waiting for app to start.")
        }

        Sys.sleep(0.1)
    }


    utils::browseURL(url)

    invisible(p)
}
