#' Activity 2 data simulation
#'
#' Generates data from which your aim is to guess the parameter Theta, as described in Activity 2 of Playing With Statistics
#'
#'
#' @param n_rounds Number of rounds of simulated dice throws
#' @param plot Should histogram be drawn (TRUE/FALSE)?
#' @param n_bins Number of bins for (optional) histogram
#' @param Theta True value of Theta
#' @param seed Set seed to enable identical simulation across calls
#'
#'
#' @returns  list containing simulated scores and their mean value
#' @examples
#' activity2_data_sim()
#'
#' @export
#'
activity2_data_sim <-
    function(n_rounds = 30,
             plot = TRUE,
             n_bins = 10,
             Theta = 7.5,
             seed = NULL) {
        if(!is.null(seed)) set.seed(seed)
        f <- function(j)
            sample(1:6, 1 + rpois(1, Theta - 1), replace = T) %>% sum
        score <- sapply(1:n_rounds, f) / 3.5
        results <- list(score = score, mean_score = mean(score))
        if (plot) {
            df <- data.frame(score = results$score)
            p <-
                ggplot2::ggplot(df, ggplot2::aes(x = score, y = ggplot2::after_stat(density))) +
                ggplot2::geom_histogram(bins = n_bins, fill = "lightblue", colour = "black") +
                ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
                ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
                ggplot2::xlab("Score") +
                ggplot2::ylab("Density")
            plot(p)
        }
        results
    }
