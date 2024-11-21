#' Analyse Activity 2 with Different Number of Throws
#'
#' Shows effect of sample size on standard error of estimate in Activity 2 of Playing With Statistics
#'
#'
#' @param n_throws Vector of numbers of simulated dice throws
#' @param nrep Number of repetitions for each experiment
#' @param Theta True value of Theta
#' @param transform_sd Transform Standard Error? (True/False)
#' @param log_trans Plot both axes on log scale? (True/False)
#' @param seed Set seed to enable identical simulation across calls
#'
#' @returns Standard errors calculated by resampling of estimates of Theta in Activity 2 with different numbers of throws of dice
#' @examples
#' activity2_se_by_size()
#'
#' @export
#'
activity2_se_by_size <-
    function(n_throws = c(10, 25, 50, 100, 250, 500, 1000, 2500, 5000),
             nrep = 10000,
             Theta = 7.5,
             transform_se = F,
             log_trans = F,
             seed = NULL) {
        if (!is.null(seed))
            set.seed(seed)
        s <- c()
        for (i in 1:length(n_throws)) {
            scores <-
                activity2_data_sim(n_throws = n_throws[i],
                            Theta = Theta,
                            plot = FALSE)$score
            s[i] <-
                activity2_se(scores, nrep = nrep, plot = FALSE)[["sd"]]
        }
        df <-
            data.frame(
                n_throws = n_throws,
                se = s,
                s = 1 / s ^ 2,
                log_n_throws = log(n_throws),
                log_s = log(s)
            )

        if (!transform_se) {
            ggplot2::ggplot(data = df, ggplot2::aes(n_throws, se)) +
                ggplot2::geom_point(colour = "steelblue") +
                ggplot2::labs(x = "Number of throws", y = "Standard Error")
        }
        else{
            if (!log_trans) {
                ggplot2::ggplot(data = df, ggplot2::aes(n_throws, s)) +
                    ggplot2:: geom_point(colour = "steelblue") +
                    ggplot2::coord_trans( x = ifelse(log_trans == T, "log10", "identity"), y = ifelse(log_trans == T, "log10", "identity")) +
                    ggplot2::geom_smooth( method = "lm", formula = y ~ x - 1, se = FALSE, color = "darkslategrey", linewidth = 0.5) +
                    ggplot2::labs(x = "Number of throws", y = "Transformed standard error")
            }
            else{
                ggplot2::ggplot(data = df, ggplot2::aes(n_throws, s)) +
                    ggplot2:: geom_point(colour = "steelblue") +
                    ggplot2::coord_trans(x = ifelse(log_trans == T, "log10", "identity"), y = ifelse(log_trans == T, "log10", "identity")) +
                    ggplot2::geom_smooth(method = "glm", formula = y ~ log(x),  se = FALSE, color = "darkslategrey",  linewidth = 0.5,
                         method.args = list(family = ggplot2::gaussian(link = 'log')) +
                    ggplot2::labs(x = "Number of throws (log scale)", y = "Transformed standard error (log scale)")
                )
            }
        }
    }
