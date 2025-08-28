#' Analyse running means from sequences of dice throws
#'
#' Shows how sample means of large samples converge based on mean score of standard dice rolls
#'
#'
#' @param n_rolls Maximum number of dice rolls
#' @param nrep Number of repetitions for each experiment
#' @param ncol Number of columns for plots
#'
#' @returns  Series of cumulative running means of dice scores in each experiment
#' @examples
#' dice_mean_series()
#'
#' @export
#'
dice_mean_series <- function(n_rolls = 1000,
                             nrep = 9,
                             ncol = 3) {
    df <- c()
    for (i in 1:nrep) {
        x <- sample(1:6, n_rolls, replace = T)
        m <- cumsum(x) / (1:n_rolls)
        df <- rbind(df, data.frame(
            run = i,
            Roll = 1:n_rolls,
            Mean = m
        ))
    }
    ggplot2::ggplot(df, ggplot2::aes(Roll, Mean)) +
        ggplot2::geom_line() +
        ggplot2::geom_abline(intercept = 3.5, slope = 0,colour = "indianred4") +
        ggplot2::ylim(1, 6)  +
        ggplot2::facet_wrap(. ~ run, labeller = ggplot2::labeller(number = labels),  ncol = ncol) +
        ggplot2::theme(strip.text.x = ggplot2::element_blank()) +
        ggplot2::xlab("Roll Number") +
        ggplot2::ylab("Rolling Mean Score")
}
