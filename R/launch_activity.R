wait_for_port <- function(port, timeout = 10) {

    start <- Sys.time()
    url <- sprintf("http://127.0.0.1:%s", port)

    repeat {

        ok <- tryCatch(
            {
                httr::GET(url, httr::timeout(0.5))
                TRUE
            },
            error = function(e) FALSE
        )

        if (ok) return(TRUE)

        if (difftime(Sys.time(), start, units = "secs") > timeout) {
            return(FALSE)
        }

        Sys.sleep(0.05)
    }
}

launch_activity <- function(app_name) {

    appdir <- system.file("shiny", app_name, package = "pws")
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
        args = list(appdir = appdir, port = port),
        stdout = NULL,
        stderr = NULL,
        supervise = TRUE
    )

    ok <- wait_for_port(port, timeout = 10)

    if (!ok) {
        stop("App failed to start (port not available).")
    }

    utils::browseURL(sprintf("http://127.0.0.1:%s", port))

    invisible(p)
}
