#' Analyse Activity 2 with Different Number of Throws
#'
#' Shows effect of sample size on distribution of estimate in Activity 2 of Playing With Statistics
#'
#'
#' @param n_throws Vector of numbers of simulated dice throws
#' @param nrep Number of repetitions for each experiment
#' @param Theta True value of Theta
#' @param epsilon Determines scale of plot
#' @param seed Set seed to enable identical simulation across calls
#'
#' @returns  Boxplots (as defined in Playing With Statistics) showing distribution of estimator in simulated repeats of Activity 2 with different numbers of throws of dice
#' @examples
#' activity2_boxplots_by_size()
#'
#' @export
#'
#'
activity2_boxplots_by_size <-
    function(n_throws = c(10, 25, 50, 100, 250, 500, 1000, 2500, 5000),
             nrep = 10000,
             Theta = 7.5,
             epsilon = 3,
             seed = NULL) {
        if (!is.null(seed)) set.seed(seed)
        resample_mat <- c()
        for (i in 1:length(n_throws)) {
            scores <- activity2_data_sim(n_throws = n_throws[i], plot = FALSE)$score
            resample_out <-
                activity2_resample_est(scores, nrep = nrep, plot = FALSE)
            resample_mat <-
                rbind(resample_mat, cbind(n_throws[i], resample_out[["resample"]]))
        }
        resample_mat <- as.data.frame(resample_mat)
        colnames(resample_mat) <- c("run", "estimate")
        df <- resample_mat  %>%
            dplyr::group_by(run) %>%
            dplyr::summarise(
                s1 = quantile(estimate, .025),
                s2 = quantile(estimate, .25),
                s3 = mean(estimate),
                s4 = quantile(estimate, .75),
                s5 = quantile(estimate, .975)
            )
        df$run <- as.factor(df$run)
        df$dummy <- 1:length(n_throws) %>% as.factor
        df$n_throws <- n_throws
        ggplot2::ggplot(df,  ggplot2::aes( x = n_throws ,group = n_throws, lower = s2, upper = s4 ,middle = s3, ymin = s1, ymax = s5)) +
            ggplot2::geom_boxplot(stat = "identity", fill = "lightblue",  width = 0.2) +
            ggplot2::coord_flip()   +
            ggplot2::scale_x_continuous(trans = 'log10') + ggplot2::ylim(Theta - epsilon, Theta + epsilon) +
            ggplot2::ylab("Score") + ggplot2::xlab("Number of throws")

    }
