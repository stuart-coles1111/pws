#' Estimate choice exercise
#'
#' Provides 2 estimates of a quantity as required in Chapter 5 of Playing With Statistics
#'
#'
#' @param xG_mod fitted xG model
#' @param theta value of parameter to be estimated
#' @param m1 mean of first estimator
#' @param sd1 standard deviation of first estimator
#' @param m2 mean of second estimator
#' @param sd2 standard deviation of second estimator
#'
#' @returns pair of estimates
#' @examples
#' estimate_choice()
#'
#' @export
#'
estimate_choice <-
    function(theta = 5,
             m1 = 5,
             sd1 = 1.5,
             m2 = 4.75,
             sd2 = .25,
             rho = 0.75) {
        Sigma <- rbind(c(sd1 ^ 2, rho * sd1 * sd2), c(rho * sd1 * sd2, sd2 ^ 2))
        estimate <-
            mvrnorm(1, mu = c(m1, m2), Sigma = Sigma) %>% round(2)
        df <-
            data.frame(estimate1 = estimate[1], estimate2 = estimate[2])
        return(df)
    }
