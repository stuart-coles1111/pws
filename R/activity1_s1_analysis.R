#' Analyse Statistic 1 for Activity 1
#'
#' Analyses Statistic 1 for Activity 1 as described in Chapter 1 of Playing With Statistics
#'
#' @param data_mat Matrix containing data (H/T): one row per individual, one column per imaginary coin toss
#' @param n_throws Number of throws per individual
#' @param x_low Lower limit for plotting
#' @param x_up Upper limit for plotting
#' @param include_data include data in plots? (TRUE/FALSE)
#' @param print_summary  print summary of results?  (TRUE/FALSE)
#'
#' @returns  list containing theoretical and sample statistics for statistic 1 in activity 1
#' @examples
#' activity1_s1_analysis(activity1_data_sm)
#'
#' @export
#'
activity1_s1_analysis <- function(data_mat,
                                  n_throws = 50,
                                  x_low = 10,
                                  x_up = 40,
                                  include_data = TRUE,
                                  print_summary = TRUE) {
    counts <- apply(data_mat, 1, function(x)
        length(x[x == "H"]))
    x_range <- x_low:x_up
    freqs <-
        table(factor(counts, levels = x_range)) %>% as.vector()
    probs <- dbinom(x_low:x_up, n_throws, 0.5)
    plot_df <- data.frame(x = x_range,
                          probs = probs,
                          freqs = freqs)
    plot_df <-
        rbind(plot_df,
              cbind(
                  x = x_range,
                  probs = probs,
                  freqs = probs * n_throws
              ))
    method <-
        rep(c("observed", "theoretical"), c(length(x_range), length(x_range)))
    plot_df$method <- method
    if (include_data) {
        p <-
            ggplot2::ggplot(plot_df, ggplot2::aes(x, freqs, fill = method)) +
            ggplot2::geom_bar(stat = "identity", position = 'dodge2') +
            ggplot2::xlab("Number of Heads") + ggplot2::ylab("Frequency") +
            ggplot2::scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
            ggplot2::labs(fill = "Frequencies")
    }
    else{
        p <-
            ggplot2::ggplot(
                subset(plot_df, method == "theoretical"),
                ggplot2::aes(x, probs, fill = method)
            ) + ggplot2::geom_bar(stat = "identity", position = 'dodge2') +
            ggplot2::xlab("Number of Heads") + ggplot2::ylab("Probability") +
            ggplot2::theme(legend.position = "none") +
            ggplot2::scale_fill_manual(values = c("#00BFC4")) +
            ggplot2::labs(fill = "Probabilities")
    }
    plot(p)
    true_mean <- 0.5 * n_throws
    observed_mean <- mean(counts)
    true_sd <- sqrt(0.5 * 0.5 * n_throws)
    observed_sd <- sd(counts)
    if (print_summary)
        list(
            means = c(observed_mean = observed_mean, true_mean = true_mean) %>% round(2),
            standard_deviations = c(observed_sd = observed_sd, true_sd = true_sd) %>% round(2)
        )
}
