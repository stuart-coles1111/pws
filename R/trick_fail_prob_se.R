#' Trick failure probability SE
#'
#' Standard error calculation of trick failure probability for Activity 5
#'
#' @param trick_res output from multi_trick
#' @param n_resample number of resamples
#' @param seed value of seed for random number generator
#'
#' @returns  value of standard error
#' @examples trick_res = multi_trick()
#'           trick_fail_prob_se(trick_res)
#'
#' @export
#'
#'
#'
#'

#trick_fail_prob_se <- function(trick_res, n_resample = 1000, seed = NULL){
#    if(!is.null(seed)) set.seed(seed)
#    resample <- rbinom(n_resample, trick_res$nrep, trick_res$fail_prob) / trick_res$nrep
#    sd(resample)
#}

trick_fail_prob_se <- function(trick_res, n_resample = 1000, seed = NULL){
    if(!is.null(seed)) set.seed(seed)
    resample <- rbinom(n_resample, trick_res$nrep, trick_res$fail_prob) / trick_res$nrep
    sd(resample)
}
