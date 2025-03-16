#' Binomial-Poisson approximation
#'
#' Compare Binomial and Poisson Distributions with Identical Means as function of n
#'
#'
#' @param lambda mean of Poisson distribution
#' @param step step size for sequence of n
#' @param n values of n in Binomial distribution
#' @param ncol number of columns for plotting
#' @param scales plotting scales fixed or free?
#' @param xlim limit of values of x
#' @param approx include poisson approximation (TRUE/FALSE)
#'
#' @returns graphical comparison of Binomial and Poisson probability distributions
#' @examples
#' binom_pois_approx(lambda=5)
#'
#' @export
#'
#'
binom_pois_approx <-
    function(lambda = 5,
             step = 5,
             n = seq(lambda, lambda + 8 * step, by = step),
             ncol = 3,
             scales = "fixed",
             xlim = c(0, 3 * lambda),
             approx = TRUE) {
        labels <- paste0("n = ", n)
        names(labels) <- n
        x <- xlim[1]:xlim[2]
        p <- lambda / n
        f <-
            function(i)
                data.frame(model = 'Binomial',
                           n[i],
                           x = x,
                           density = dbinom(x, n[i], p[i]))
        df <- lapply(1:length(n), f) %>% do.call(what = rbind)
        colnames(df) <- c('Model', 'n', 'x', 'Probability')
        g <-
            function(i)
                data.frame(model = 'Poisson',
                           n[i],
                           x = x,
                           density = dpois(x, lambda))
        df2 <- lapply(1:length(n), g) %>% do.call(what = rbind)
        colnames(df2) <- c('Model', 'n', 'x', 'Probability')
        if (approx)
            df <- rbind(df, df2)
        if (approx) {
            plot <- ggplot2::ggplot(df, ggplot2::aes(x, Probability, fill = Model)) +
                ggplot2::geom_bar(stat = "identity", position = 'dodge2')
        }
        else{
            plot <- ggplot2::ggplot(df, ggplot2::aes(x, Probability)) +
                ggplot2::geom_bar(stat = "identity",
                         position = 'dodge2',
                         fill = "lightblue")  + ylab("Probability")
        }
        plot <- plot +
            ggplot2::facet_wrap(
                . ~ n,
                ncol = ncol,
                labeller = ggplot2::labeller(n = labels),
                scales = scales
            ) +
            ggplot2::scale_x_continuous(breaks = scales::pretty_breaks())
        if (!approx)
            plot <- plot +  ggplot2::theme(legend.position = "none")
        plot <- plot + ggplot2::xlab("x") + ggplot2::theme(axis.title.x=ggplot2::element_text(face="italic"))
        plot
    }
