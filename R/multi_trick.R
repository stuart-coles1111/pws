#' Multi Trick
#'
#' Multiple runs of trick for Activity 5
#'
#' @param nrep number of repetitions
#' @param picture_value score to give picture cards
#' @param seed value of seed for random number generator
#'
#' @returns  summary statistics from multiple trick runs
#' @examples multi_trick()
#'
#' @export
#'
#'
#'

multi_trick <- function(nrep = 10000, picture_value = 10, seed = NULL){
    if(!is.null(seed)) set.seed(seed)
    trick_agree <- lapply(1:nrep, function(x, picture_value) single_trick(picture_value, show_result = FALSE), picture_value = picture_value)
    trick_agree <- trick_agree %>% unlist %>% table
    names(trick_agree) <- c("incorrect", "correct")
    p <- trick_agree[1]/nrep %>% as.numeric
    names(p) <- "p"
    list(nrep = nrep, results=trick_agree, fail_prob=p)
}
