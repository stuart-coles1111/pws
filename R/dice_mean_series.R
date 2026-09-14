#' Analyse running means from sequences of dice throws
#'
#' Shows how sample means of large samples converge based on mean score of standard dice rolls
#'
#'
#' @param n_rolls Maximum number of dice rolls
#' @param nrep Number of repetitions for each experiment
#' @param ncol Number of columns for plots
#'
#' @returns  Series of cumulative running means of dice scores in each experiment
#' @examples
#' dice_mean_series()
#'
#' @export
#'
dice_mean_series <- function(
        n_rolls = 1000,
        nrep = 4
) {

    df <- data.frame()

    for (i in 1:nrep) {

        x <- sample(
            1:6,
            n_rolls,
            replace = TRUE
        )

        m <- cumsum(x) / seq_along(x)

        df <- rbind(
            df,
            data.frame(
                run = i,
                Roll = seq_along(x),
                Mean = m
            )
        )
    }

    # Choose a sensible number of columns automatically
    ncol <- ceiling(sqrt(nrep))

    ggplot2::ggplot(
        df,
        ggplot2::aes(Roll, Mean)
    ) +

        ggplot2::geom_line() +

        ggplot2::geom_hline(
            yintercept = 3.5,
            colour = "indianred4"
        ) +

        ggplot2::coord_cartesian(
            ylim = c(1, 6)
        ) +

        ggplot2::facet_wrap(
            ~run,
            ncol = ncol
        ) +

        ggplot2::theme_minimal(base_size = 13) +

        ggplot2::theme(
            strip.text = ggplot2::element_text(
                face = "bold"
            )
        ) +

        ggplot2::labs(
            x = "Roll number",
            y = "Rolling mean"
        )
}
