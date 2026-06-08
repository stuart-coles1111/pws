#' Run an activity app
#'
#' @param chapter Chapter number
#' @export
run_activity <- function(chapter) {

    port <- sample(8000:9000, 1)

    callr::r_bg(
        func = function(chapter, port) {

            library(pws)

            shiny::runApp(
                system.file(
                    "shiny",
                    paste0("app_activity", chapter, ".R"),
                    package = "pws"
                ),
                host = "127.0.0.1",
                port = port,
                launch.browser = FALSE
            )

        },
        args = list(chapter = chapter, port = port)
    )

    # return URL to main session
    paste0("http://127.0.0.1:", port)
}
