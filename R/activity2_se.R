#' Activity 2 resampling to obtain standard error of estimates
#'
#' Generates error plot of Theta estimates based on resampling, as described in Activity 2 of Playing With Statistics
#'
#' @param dice_scores Values of original sample of scores
#' @param nrep  Number of resamples
#' @param alpha confidence level
#' @param plot Should confidence interval be drawn (TRUE/FALSE)?
#' @param width width of confidence interval error bars
#' @param seed Set seed to enable identical simulation across calls
#'
#' @returns  invisible list containing mean, standard deviation, enumeration and quantiles of estimates based on resamples
#' @examples
#'
#' estimates <- activity2_data_sim(plot = FALSE)
#' activity2_se(estimates$score)
#' @export
#'
#'
activity2_se <-
    function(dice_scores,
             nrep = 10000,
             alpha = .95,
             plot = TRUE,
             width = 0.1,
             seed = NULL) {
        if(!is.null(seed)) set.seed(seed)
        resample <- c()
        for (i in 1:nrep) {
            resample[i] <-
                sample(dice_scores, length(dice_scores), replace = TRUE) %>% mean
        }

        p <- qnorm((1+alpha)/2)
        me <- mean(resample)
        se <- sd(resample)


        df <-
            data.frame(
                x = "",
                lower = me - p * se,
                middle = me,
                upper =  me + p * se
            )
        rownames(df) <- "value"


        if (plot) {
            p <-
                ggplot2::ggplot(df) +
                ggplot2::geom_errorbar(ggplot2::aes(x = x, ymin = lower, ymax = upper), width = width) +
                ggplot2::geom_point(ggplot2::aes(x, middle), colour = "steelblue") +
                ggplot2::coord_flip() +
                ggplot2::xlab("") +
                ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
                ggplot2::ylab(latex2exp::TeX("$\\Theta $"))

            print(p)
        }

        return(list(
            mean = mean(resample),
            sd = sd(resample),
            resample = resample,
            quantiles = df[, -1]
        ) %>% invisible)
    }
