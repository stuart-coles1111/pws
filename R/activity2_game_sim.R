#' Activity 2 game simulation
#'
#' Generates data from which your aim is to guess the parameter Theta, as described in Activity 2 of Playing With Statistics
#'
#'
#' @param n_rounds Number of rounds of simulated dice throws
#' @param plot Should histogram be drawn (TRUE/FALSE)?
#' @param n_bins Number of bins for (optional) histogram
#' @param Theta True value of Theta
#' @param rand_Theta randomise Theta (TRUE/FALSE)? if TRUE, Theta argument is ignored
#' @param show_Theta show Theta in output (TRUE/FALSE)?
#' @param seed Set seed to enable identical simulation across calls
#'
#'
#' @returns  list containing simulated scores and their mean value
#' @examples
#' activity2_game_sim()
#'
#' @export
#'
activity2_game_sim <-
    function(n_rounds = 30,
             plot = TRUE,
             n_bins = 10,
             Theta = NULL,
             rand_Theta = FALSE,
             show_Theta = FALSE,
             seed = NULL) {

        if(!is.null(seed)) set.seed(seed)
        if(is.null(Theta)) Theta <- 7.5 # default hidden value of Theta
        if(rand_Theta) Theta <- sample(seq(1, 50, by = 0.25), 1) # randomised value of Theta if required
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
        if(show_Theta) results$Theta <- Theta # include Theta in output list if requested
        results
    }
