#' Poisson approximation for birthday matching probability
#'
#' Internal helper for birthday problem demonstrations.
#'
#' @keywords internal
birthday_poisson <- function(n, m){

    lambda <- n / 365

    probability_no_day_reaches_m <-
        ppois(
            m - 1,
            lambda
        )^365

    1 - probability_no_day_reaches_m

}
