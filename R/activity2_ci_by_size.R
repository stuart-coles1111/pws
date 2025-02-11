#' Analyse Activity 2 with Different Number of Throws
#'
#' Shows effect of sample size on distribution of estimate in Activity 2 of Playing With Statistics
#'
#'
#' @param n_rounds Vector of rounds of simulated dice throws
#' @param nrep Number of repetitions for each experiment
#' @param Theta True value of Theta
#' @param alpha confidence level
#' @param width width of error bars
#' @param epsilon Determines scale of plot
#' @param seed Set seed to enable identical simulation across calls
#'
#' @returns  Confidence intervals for Theta in simulated repeats of Activity 2 with different numbers of throws of dice
#' @examples
#' activity2_ci_by_size()
#'
#' @export
#'
#'
activity2_ci_by_size <-
    function(n_rounds = c(10, 25, 50, 100, 250, 500, 1000, 2500, 5000),
             nrep = 10000,
             Theta = 7.5,
             alpha = 0.95,
             width = 0.1,
             epsilon = 3,
             seed = NULL) {
        if (!is.null(seed)) set.seed(seed)
        resample_mat <- c()
        p <- qnorm((1+alpha)/2)

        for (i in 1:length(n_rounds)) {
            scores <- activity2_data_sim(n_rounds = n_rounds[i], plot = FALSE)$score
            resample_out <-
                activity2_se(scores, nrep = nrep, plot = FALSE)
            resample_mat <-
                rbind(resample_mat, cbind(n_rounds[i], resample_out[["resample"]]))
        }
        resample_mat <- as.data.frame(resample_mat)
        colnames(resample_mat) <- c("run", "estimate")
        df <- resample_mat  %>%
            dplyr::group_by(run) %>%
            dplyr::summarise(
                s1 = mean(estimate) - p * sd(estimate),
                s2 = mean(estimate),
                s3 = mean(estimate) + p * sd(estimate),
            )
        df$run <- as.factor(df$run)
        df$dummy <- 1:length(n_rounds) %>% as.factor
        df$n_rounds <- n_rounds
        ggplot2::ggplot(df) +
            ggplot2::geom_point(ggplot2::aes(n_rounds, s2), colour = "steelblue") +
            ggplot2::geom_errorbar(ggplot2::aes(x = n_rounds, ymin = s1, ymax = s3), width = width) +
            ggplot2::coord_flip()   +
            ggplot2::scale_x_continuous(trans = 'log10') +
            ggplot2::ylim(Theta - epsilon, Theta + epsilon) +
            ggplot2::ylab(latex2exp::TeX("$\\Theta $")) +
            ggplot2::xlab("Number of throws") +
            ggplot2::geom_hline(yintercept = Theta, colour="red")


    }
