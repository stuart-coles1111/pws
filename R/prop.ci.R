#' Confidence interval for proportions
#'
#' PRodices confidence interval for proportion using approximate process sampling
#'
#' @param count number of successes
#' @param trials number of trials
#' @param alpha level of confidence
#' @param nsim number of simulations
#' @param tails proportion for tails (TRUE/FALSE)
#' @param seed Set seed to enable identical simulation across calls
#' @param ci_lim vector of limits for plotting confidence interval
#'
#' @returns  standard error of proportion and alpha-level confidence intervals (plots as side effect)
#' @examples
#' prop.ci()
#'
#' @export
#'


prop.ci <- function(count = 522, trials = 1000, alpha = 0.95, nsim = 10000, tails=TRUE, seed = NULL, ci_lim = NULL){

    if(!is.null(seed)) set.seed(seed)

    p_est <- count/trials

    sim_data <- lapply(1:nsim, function(x) sample(c(0, 1), trials, replace=T, prob = c(1 - p_est, p_est)) %>% mean)  %>% unlist %>% data.frame

    colnames(sim_data) <- "sim_data"

    f1 <- ggplot2::ggplot(sim_data, ggplot2::aes(sim_data)) + ggplot2::geom_histogram(fill="lightblue", bins=10, colour="black",
                                                                                       ggplot2::aes(y = ggplot2::after_stat(density))) +
        ggplot2::xlab(paste0("Proportion of ", ifelse(tails, "Tails", "Heads"))) + ggplot2::ylab("Probability density")

    m <- mean(sim_data$sim_data)
    s <- sd(sim_data$sim_data)
    qv <- qnorm((1 + alpha)/2)
    ci <- c(m - qv*s, m + qv*s)

    l <- ci[1]
    u <- ci[2]

    df <- data.frame(x = 1, l = l, m = m,u = u)
    f2 <- ggplot2::ggplot(df) + ggplot2::geom_errorbar(ggplot2::aes(x = x, ymin = l, ymax = u), width = 0.1)  +
        ggplot2::geom_point(ggplot2::aes(x,m), size=2, colour = "steelblue")  + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                                                                    axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                                                                    axis.line.y = ggplot2::element_blank()) + ggplot2::coord_flip() +
        ggplot2::ylab(latex2exp::TeX("$p$")) + ggplot2::xlim(0.5, 1.5)

    if(!is.null(ci_lim)) f2 <- f2 + ylim(ci_lim)

    gridExtra::grid.arrange(f1, f2, ncol=2)
    list(se = s, ci = ci)
}

