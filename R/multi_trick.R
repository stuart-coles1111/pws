#' Multi Trick
#'
#' Multiple runs of trick for Activity 5
#'
#' @param nrep number of repetitions
#' @param picture_value score to give picture cards
#'
#' @returns  summary statistics from multiple trick runs
#' @examples multi_trick()
#'
#' @export
#'
#'
#'

multi_trick <- function(nrep = 10000, picture_value = 10){
    trick_agree <- lapply(1:nrep, function(x, picture_value) single_trick(picture_value, show_result = FALSE), picture_value = picture_value)
    trick_agree <- trick_agree %>% unlist %>% table
    names(trick_agree) <- c("disagree", "agree")
    p <- trick_agree[1]/nrep %>% as.numeric
    names(p) <- "p"
    list(nrep = nrep, picture_value = picture_value, results=trick_agree, fail_prob=p)
}
