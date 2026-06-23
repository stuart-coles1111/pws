launch_activity <- function(app_name) {

    appdir <- system.file(
        "shiny",
        app_name,
        package = "pws"
    )

    port <- httpuv::randomPort()

    p <- callr::r_bg(
        function(appdir, port) {

            message("Starting app")
            message(appdir)
            message(port)

            shiny::runApp(
                appdir,
                host = "127.0.0.1",
                port = port,
                launch.browser = FALSE
            )

        },
        args = list(appdir, port),
        stdout = "NULL",
        stderr = "NULL",
        supervise = FALSE
    )

    Sys.sleep(1)

    if (!p$is_alive()) {

        cat(
            paste(p$read_error_lines(), collapse = "\n"),
            "\n"
        )

        cat(
            paste(p$read_output_lines(), collapse = "\n"),
            "\n"
        )
    }

    url <- sprintf("http://127.0.0.1:%s", port)

    deadline <- Sys.time() + 15

    repeat {

        ready <- tryCatch({

            res <- httr::GET(
                url,
                httr::timeout(1)
            )

            httr::status_code(res) == 200

        }, error = function(e) FALSE)

        if (ready)
            break

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
