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

    # Wait until Shiny is actually listening
    deadline <- Sys.time() + 10

    repeat {

        con <- suppressWarnings(
            try(
                socketConnection(
                    host = "127.0.0.1",
                    port = port,
                    open = "r+",
                    blocking = TRUE,
                    timeout = 0.1
                ),
                silent = TRUE
            )
        )

        if (!inherits(con, "try-error")) {
            close(con)
            break
        }

        if (!p$is_alive()) {
            stop("App process terminated before starting.")
        }

        if (Sys.time() > deadline) {
            stop("Timed out waiting for app to start.")
        }

        Sys.sleep(0.05)
    }

    utils::browseURL(
        sprintf("http://127.0.0.1:%s", port)
    )

    invisible(p)
}
