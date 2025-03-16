#' Binomial-Normal approximation
#'
#' Compare Binomial and Normal Distributions with identical means as a function of n
#'
#'
#' @param p in Binomial distribution
#' @param n_min smallest value of n
#' @param step step size for sequence of n
#' @param n values of n in Binomial distribution
#' @param ncol number of columns for plotting
#' @param scales plotting scales fixed or free?
#' @param xlim limit of values of x
#' @param approx include Normal approximation (TRUE/FALSE)
#'
#' @returns graphical comparison of Binomial and Poisson probability distributions
#' @examples
#' binom_norm_approx(p = 0.2)
#'
#' @export
#'
#'

binom_norm_approx <-
    function(p = 0.2,
             n_min = 5,
             step = 5,
             n = seq(n_min, n_min + 8 * step, by = step),
             ncol = 3,
             scales = "fixed",
             xlim = c(-5, 20),
             approx = TRUE) {
        labels <- paste0("n = ", n)
        names(labels) <- n
        x <- 0:xlim[2]
        x_norm <- seq(xlim[1], xlim[2], by = .01)
        f <-
            function(i)
                data.frame(n[i], x = x, density = dbinom(x, n[i], p))
        g <-
            function(i)
                data.frame(n[i], x = x_norm, density = dnorm(x_norm, n[i] * p, sqrt(n[i] *
                                                                                        p * (1 - p))))
        df <- lapply(1:length(n), f) %>% do.call(what = rbind)
        df2 <- lapply(1:length(n), g) %>% do.call(what = rbind)
        colnames(df) <- colnames(df2) <- c('n', 'x', 'Probability')
        plot <- ggplot2::ggplot(df,  ggplot2::aes(x, Probability)) +
            ggplot2::geom_bar(stat = "identity", fill = "lightblue") +
            ggplot2::facet_wrap(
                . ~ n,
                ncol = ncol,
                labeller =  ggplot2::labeller(n = labels),
                scales = scales
            ) +
            ggplot2::scale_x_continuous(breaks = scales::pretty_breaks())
        if (approx)
            plot <-
            plot +  ggplot2::geom_line(data = df2,  ggplot2::aes(x, Probability), colour = "indianred4")
        plot <- plot +  ggplot2::xlab(latex2exp::TeX("x")) +  ggplot2::theme(axis.title.x =  ggplot2::element_text(face="italic"))
        plot
    }
