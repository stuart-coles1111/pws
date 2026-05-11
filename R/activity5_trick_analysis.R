#' Analyse card trick performance for Activity 5
#'
#' Repeats the card trick simulation from
#' `activity5_simulate_trick()` multiple times and summarises
#' the results.
#'
#' @param nrep Number of repetitions of the simulation.
#' @param picture_value Numeric value assigned to picture cards.
#' @param seed Optional random seed for reproducibility.
#'
#' @return A list containing:
#' \describe{
#'   \item{nrep}{The number of simulation runs.}
#'   \item{results}{A table of correct and incorrect outcomes.}
#'   \item{fail_prob}{Estimated probability that the trick fails.}
#' }
#'
#' @examples
#' trick_res <- activity5_trick_analysis()
#' trick_res$fail_prob
#'
#' @export

activity5_trick_analysis <- function(
        nrep = 10000,
        picture_value = 10,
        seed = NULL
){

    if(!is.null(seed)) set.seed(seed)

    trick_agree <- replicate(
        nrep,
        activity5_simulate_trick(
            picture_value,
            show_result = FALSE
        )
    )

    trick_agree <- trick_agree %>%
        table()

    names(trick_agree) <- c("incorrect", "correct")

    p <- as.numeric(trick_agree[1] / nrep)
    names(p) <- "p"

    list(
        nrep = nrep,
        results = trick_agree,
        fail_prob = p
    )
}
