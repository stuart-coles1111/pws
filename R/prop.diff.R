#' Test for difference in proportions
#'
#' Tests for difference in proportions of two samples
#'
#' @param counts vector of counts in 2 groups
#' @param trials number of trials in each group
#' @param alpha level of confidence
#' @param nsim number of simulations
#' @param seed Set seed to enable identical simulation across calls
#' @param ci_lim vector of limits for plotting confidence interval
#'
#' @returns  standard error of difference and alpha-level confidence intervals (plots as side effect)
#' @examples
#' prop.diff()
#'
#' @export
#'

prop.diff <- function(counts = c(21, 9), trials = c(31, 23), alpha=.95, nsim =10000, seed = NULL, ci_lim = NULL){

    if(!is.null(seed)) set.seed(seed)

    p_1 <- counts[1] / trials[1]
    p_2 <- counts[2] / trials[2]

    s1 <- rbinom(nsim, trials[1], p_1) / trials[1]
    s2 <- rbinom(nsim, trials[2], p_2) / trials[2]

    d <- s1 - s2

    se <- sd(d)
    m <- mean(d)

    qv <- qnorm((1 + alpha) / 2)
    ci <- c(m - qv * se, m + qv*se)

    df <- data.frame(d = d)

    f1 <- ggplot2::ggplot(df, ggplot2::aes(d)) + ggplot2::geom_histogram(fill="lightblue", bins=10, colour="black", ggplot2::aes(y = ggplot2::after_stat(density))) +
        ggplot2::xlab("Proportion difference") + ggplot2::ylab("Probability density")

    l <- ci[1]
    u <- ci[2]

    df <- data.frame(x = 1,l = l,m = m,u = u)
    f2 <- ggplot2::ggplot(df) + ggplot2::geom_errorbar(ggplot2::aes(x = x, ymin = l, ymax = u), width = 0.1)  +
        ggplot2::geom_point(ggplot2::aes(x,m), size=2, colour = "steelblue")  + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                                                                                               axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                                                                                               axis.line.y = ggplot2::element_blank()) + ggplot2:: coord_flip() +
        ggplot2::ylab(latex2exp::TeX("$\\Theta$")) + ggplot2::xlim(0.5, 1.5)

    if(!is.null(ci_lim)) f2 <- f2 + ylim(ci_lim)

    gridExtra::grid.arrange(f1, f2, ncol=2)
    list(se = se, ci = ci)
}
