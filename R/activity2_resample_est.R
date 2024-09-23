#' Activity 2 resampling of estimates
#'
#' Generates boxplot of Theta estimates based on resampling, as described in Activity 2 of Playing With Statistics
#'
#' @param dice_scores Values of original sample of scores
#' @param nrep  Number of resamples
#' @param plot Should boxplot be drawn (TRUE/FALSE)?
#' @param seed Set seed to enable identical simulation across calls
#'
#' @returns  invisible list containing mean, standard deviation, enumeration and quantiles of estimates based on resamples
#' @examples
#'
#' estimates <- activity2_data_sim(plot = FALSE)
#' activity2_resample_est(estimates$score)
#' @export
#'
#'
activity2_resample_est <-
    function(dice_scores,
             nrep = 10000,
             plot = TRUE,
             seed = NULL) {
        if(!is.null(seed)) set.seed(seed)
        resample <- c()
        for (i in 1:nrep) {
            resample[i] <-
                sample(dice_scores, length(dice_scores), replace = TRUE) %>% mean
        }

        stats <-
            c(
                min = quantile(resample, .025),
                lower = quantile(resample, .25),
                middle = mean(resample),
                upper = quantile(resample, .75),
                max = quantile(resample, .975)
            )

        df <-
            data.frame(
                x = "",
                ymin = stats[1],
                lower = stats[2],
                middle = stats[3],
                upper = stats[4],
                ymax = stats[5]
            )
        rownames(df) <- "value"


        if (plot) {
            p <-
                ggplot2::ggplot(df, ggplot2::aes( x = x,lower = lower, upper = upper, middle = middle,  ymin = ymin, ymax = ymax)) +
                ggplot2::geom_boxplot(stat = "identity", fill = "lightblue", width = 0.25) +
                ggplot2::coord_flip() +
                ggplot2::xlab("") +
                ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
                ggplot2::ylab("Score")

            print(p)
        }

        return(list(
            mean = mean(resample),
            sd = sd(resample),
            resample = resample,
            quantiles = df[, -1]
        ) %>% invisible)
    }
