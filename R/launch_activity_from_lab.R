#' Launch activity for use from the Statistics Lab
#'
#' Starts the activity app and waits briefly for startup.
#'
#' @param chapter Chapter number
#'
#' @keywords internal
launch_activity_from_lab <- function(chapter) {

    url <- run_activity(chapter)

    Sys.sleep(1.5)

    url
}
