#' Analyse Activity 2 with Different Number of Throws
#'
#' Shows effect of sample size on standard error of estimate in Activity 2 of Playing With Statistics
#'
#'
#' @param n_rounds Vector of rounds of simulated dice throws
#' @param nrep Number of repetitions for each experiment
#' @param Theta True value of Theta
#' @param transform_n Transform number of rounds? (True/False)
#' @param seed Set seed to enable identical simulation across calls
#'
#' @returns Standard errors calculated by resampling of estimates of Theta in Activity 2 with different numbers of throws of dice
#' @examples
#' activity2_se_by_size()
#'
#' @export
#'
activity2_se_by_size <- function(n_rounds = c(10, 25, 50, 100, 250, 500, 1000, 2500, 5000),
                                 nrep = 10000,
                                 Theta = 7.5,
                                 transform_n = F,
                                 seed = NULL) {
    if (!is.null(seed))
        set.seed(seed)
    s <- c()
    for (i in 1:length(n_rounds)) {
        scores <-
            activity2_game_sim(n_rounds = n_rounds[i],
                               Theta = Theta,
                               plot = FALSE)$score
        s[i] <-
            activity2_se(scores, nrep = nrep, plot = FALSE, print_results = FALSE)[["sd"]]
    }
    df <-
        data.frame(
            n_rounds = n_rounds,
            se = s,
            s = 1 / s ^ 2,
            log_n_rounds = log(n_rounds),
            log_s = log(s)
        )

    if (!transform_n) {
        ggplot2::ggplot(data = df, ggplot2::aes(n_rounds, se)) +
            ggplot2::geom_point(colour = "steelblue") +
            ggplot2::labs(x = "Number of throws", y = "Standard Error")
    }
    else{
        ggplot2::ggplot(data = df, ggplot2::aes(1/sqrt(n_rounds), se)) +
            ggplot2:: geom_point(colour = "steelblue") +
            ggplot2::geom_smooth( method = "lm", formula = y ~ x - 1, se = FALSE, color = "darkslategrey", linewidth = 0.5) +
            ggplot2::labs(x = TeX("$1/\\sqrt{n}$") , y = "Standard error")

    }
}
