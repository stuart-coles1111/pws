#' Estimate choice exercise
#'
#' Provides 2 estimates of a population quantity as described in Chapter 5 of Playing With Statistics
#'
#'
#' @param theta mean of normal distribution
#' @param sd standard deviation of normal distribution
#' @param n_dat sample size
#' @param seed seed for random number generator
#'
#' @returns pair of estimates
#' @examples
#' estimate_choice()
#'
#' @export
#'
estimate_choice <-
    function(theta = 0,
             sd = 1,
             n_dat = 10,
             seed = NULL) {
        if(!is.null(seed)) set.seed(seed)
        x <- rnorm(n_dat, theta, sd) %>% exp
        estimate <- c()
        estimate[1] <- mean(x)
        y <- log(x)
        estimate[2] <- exp(mean(y) + var(y)/2)
        estimate <- estimate %>% round(3)
        df <-
            data.frame("Estimate_A" = estimate[1], "Estimate_B" = estimate[2], row.names = "")
        return(df)
    }
