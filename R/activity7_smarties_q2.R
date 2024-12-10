#' Analysis of Smarties question 2
#'
#' Analysis of estimate in question 2 of Activity 7 in Playing With Statistics
#'
#' @param data_df dataframe containing data
#' @param nrep number of simulations for calculation of sampling distribution
#' @param bins number of bins for histogram
#'
#' @returns histogram of sampling distribution
#' @examples
#' activity7_smarties_q2(activity7_smarties_data)
#'
#' @export
#'
#'
activity7_smarties_q2 <- function(data_df, nrep = 10000, bins = 10){
    ssd <- c()
    n_smarties <- sum(data_df)
    for(i in 1:nrep)ssd[i]=sd(table(factor(sample(c('brown','orange','blue','purple','pink','yellow','red','green'),
                                                  n_smarties ,replace=T))))
    ssd_df <- data.frame(ssd)
    sd_data <- apply(data_df, 2, sum) %>% sd
    ggplot2::ggplot(ssd_df, ggplot2::aes(ssd)) +
        ggplot2::geom_histogram(fill="lightblue", colour="black", bins = bins,  ggplot2::aes(y =  ggplot2::after_stat(density))) +
        ggplot2::geom_vline(xintercept = sd_data, colour="red") +
        ggplot2::xlab("Standard deviation") +
        ggplot2::ylab("Probability density")

}
