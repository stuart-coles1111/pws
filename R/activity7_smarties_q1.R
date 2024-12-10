#' Analysis of Smarties question 1
#'
#' Analysis of estimate in question 1 of Activity 7 in Playing With Statistics
#'
#' @param data_df dataframe containing data
#' @param nrep number of simulations for calculation of sampling distribution
#' @param bins number of bins in histogram
#'
#' @returns histogram of sampling distribution
#' @examples
#' activity7_smarties_q1(activity7_smarties_data)
#'
#' @export
#'
#'
activity7_smarties_q1 <- function(data_df, nrep = 10000, bins = 10){
    data_sum <- apply(data_df, 2, sum)
    box_sizes <- apply(data_df, 1, sum)
    data <- rep(c('blue','brown','green','orange','pink','purple','red','yellow'),data_sum)
    sd_sd <- c()
    for(i in 1:nrep){
        newdata <- sample(data, sum(box_sizes))
        box <- rep(1:nrow(data_df), box_sizes)
        newdata_df <- data.frame(box, newdata)
        sd_by_box <- newdata_df  %>%
            dplyr::group_by(box) %>%
            dplyr::summarise(
                s <- sd(table(newdata))
            )
        sd_sd[i] <- sd(sd_by_box[[2]])
    }
    sd_sd_df <- data.frame(sd_sd)
    original_sd <- apply(data_df, 1, sd) %>% sd
    ggplot2::ggplot(sd_sd_df, ggplot2::aes(sd_sd)) +
        ggplot2::geom_histogram(fill="lightblue", colour="black", bins = bins, ggplot2::aes(y = ggplot2::after_stat(density))) +
        ggplot2::geom_vline(xintercept = original_sd, colour="red") +
        ggplot2::xlab("Standard deviation") +
        ggplot2::ylab("Probability density")
}

