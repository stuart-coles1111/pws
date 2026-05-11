#' Standard error and sample size
#'
#' Illustrates how the standard error of an estimator changes
#' with sample size using repeated resampling from Poisson data.
#'
#' For each sample size, a sample is generated from a Poisson
#' distribution and bootstrap resampling is used to estimate
#' the standard error of the sample mean.
#'
#' @param lambda Mean of the Poisson distribution.
#' @param ns Numeric vector of sample sizes.
#' @param n_resample Number of bootstrap resamples.
#' @param transform_n Logical; should the x-axis be transformed
#'   to `1/sqrt(n)`? Defaults to `FALSE`.
#' @param seed Optional random seed for reproducibility.
#'
#' @return A `ggplot2` object showing estimated standard error
#' as a function of sample size.
#'
#' @examples
#' se_by_sample_size()
#'
#' @export
se_by_sample_size <- function(
        lambda = 2.5,
        ns = c(10, 50, 100, 500, 1000, 5000, 10000),
        n_resample = 10000,
        transform_n = FALSE,
        seed = NULL
){

    if(!is.null(seed)) set.seed(seed)

    est_sd <- numeric(length(ns))

    for(i in seq_along(ns)){

        x <- rpois(ns[i], lambda)

        sampled_est <- replicate(
            n_resample,
            mean(sample(x, ns[i], replace = TRUE))
        )

        est_sd[i] <- sd(sampled_est)
    }

    df <- data.frame(
        ns = ns,
        se = est_sd,
        s = 1 / est_sd^2,
        log_n = log(ns),
        log_s = log(est_sd)
    )

    if(!transform_n){

        ggplot2::ggplot(
            data = df,
            ggplot2::aes(ns, se)
        ) +
            ggplot2::geom_point(colour = "steelblue") +
            ggplot2::labs(
                x = "Sample size",
                y = "Standard error"
            )

    } else {

        ggplot2::ggplot(
            data = df,
            ggplot2::aes(1 / sqrt(ns), se)
        ) +
            ggplot2::geom_point(colour = "steelblue") +
            ggplot2::geom_smooth(
                method = "lm",
                formula = y ~ x - 1,
                se = FALSE,
                colour = "darkslategrey",
                linewidth = 0.5
            ) +
            ggplot2::labs(
                x = latex2exp::TeX("$1/\\sqrt{n}$"),
                y = "Standard error"
            )
    }
}
