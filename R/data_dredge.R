#' Illustration of Effects of Data Dredging
#'
#' Data dredging example discussed in Chapter 7 of Playing With Statistics
#'
#' @param n_data number of rows of data
#' @param n_games_per_match number of variables
#' @param sig_x standard deviation of x values
#' @param  sig_x standard deviation of y values
#' @param seed seed to initialise randomisation
#'
#' @returns summary of most significant linear relationship between y and any of the x variables
#' @examples
#' data_dredge()
#'
#' @export
#'
#'
data_dredge <- function(n_data = 100, n_var = 50, sig_x = 10, sig_y = 5, seed = NULL){

    if(!is.null(seed)) set.seed(seed)

    y<-rnorm(n_data, 0, sig_y)

    x <- matrix(0, nrow = n_var, ncol = n_data)

    for(i in 1:n_var)x[i,] <- rnorm(n_data, 0, sig_x)

    pval <- c()
    for(i in 1:n_var) pval[i] <- summary(lm(y~x[i, ]))$coeff[2, 4]
    var_sig <- which.min(pval)
    x <- x[var_sig,]

    df <- data.frame(x = x, y = y)
    p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
        ggplot2::geom_point(colour = "steelblue") +
        ggplot2::geom_smooth(method = 'lm', colour = "indianred4")+
        ggplot2::geom_hline(yintercept = mean(y), colour = "red") +
        ggplot2::theme(axis.title = ggplot2::element_text(face = "italic"))
    plot(p)

    summary(lm(y ~ x))$coeff %>% round(4)
}
