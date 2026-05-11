#' Standard error of trick failure probability
#'
#' Calculates the standard error of the estimated trick failure
#' probability from `activity5_trick_analysis()` using repeated
#' binomial resampling.
#'
#' This simulation-based approach is used for pedagogical purposes
#' to illustrate sampling variability and the concept of a standard
#' error.
#'
#' @param trick_res Output from `activity5_trick_analysis()`.
#' @param n_resample Number of resamples used in the simulation.
#' @param seed Optional random seed for reproducibility.
#'
#' @return A numeric value giving the estimated standard error.
#'
#' @examples
#' trick_res <- activity5_trick_analysis()
#' activity5_prob_se(trick_res)
#'
#' @export
activity5_prob_se <- function(trick_res, n_resample = 1000, seed = NULL){

    if(!is.null(seed)) set.seed(seed)

    resample <- rbinom(
        n_resample,
        trick_res$nrep,
        trick_res$fail_prob
    ) / trick_res$nrep

    sd(resample)
}
