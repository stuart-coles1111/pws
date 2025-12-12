#' Standard errors and sample size
#'
#' Illustration of effect of sample size on standard error
#'
#' @param lambda Poisson population mean
#' @param ns vector of sample sizes
#' @param n_resample number of resamples
#' @param transform_n (TRUE/FALSE) transform x axis of plot?
#' @param seed seed for random number generator
#'
#' @returns  plot of standard error of estimate versus sample size
#' @examples se_by_sample_size()
#'
#' @export
#'
#'


se_by_sample_size <- function(lambda = 2.5, ns = c(10, 50, 100, 500, 1000, 5000, 10000), n_resample = 10000, transform_n = FALSE, seed = NULL){
    if(!is.null(seed)) set.seed(seed)

    est_sd <- c()

    for(i in 1:length(ns)){
        x <- rpois(ns[i], lambda)
        est <- mean(x)
        sampled_est <- lapply(1:n_resample, function(j) sample(x, ns[i], replace = TRUE) %>% mean) %>% unlist
        est_sd[i] <- sd(sampled_est)
    }

    df <-
        data.frame(
            ns = ns,
            se = est_sd,
            s = 1 / est_sd ^ 2,
            log_n = log(ns),
            log_s = log(est_sd)
        )

    if (!transform_n) {
        ggplot2::ggplot(data = df, ggplot2::aes(ns, se)) +
            ggplot2::geom_point(colour = "steelblue") +
            ggplot2::labs(x = "n", y = "Standard Error")
    }

    else{
        ggplot2::ggplot(data = df, ggplot2::aes(1/sqrt(ns), se)) +
            ggplot2:: geom_point(colour = "steelblue") +
            ggplot2::geom_smooth( method = "lm", formula = y ~ x - 1, se = FALSE, color = "darkslategrey", linewidth = 0.5) +
            ggplot2::labs(x = latex2exp::TeX("$1/\\sqrt{n}$") , y = "Standard error")

    }

}





