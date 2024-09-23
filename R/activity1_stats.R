#' Statistics for Activity 1
#'
#' Calculates Statistics 1 and 2 for Activity 1 from Playing With Statistics
#'
#' @param x vector sequence of H and/or T
#'
#'
#'
#' @examples
#' x <- c('H','H','T','T','T','H')
#' activity1_stats(x)
#'
#' @export
#'
activity1_stats <- function(x) {
    statistic1 <- sum(x == "H")
    statistic2 <- rle(x)$lengths %>% max
    list(statistic1 = statistic1, statistic2 = statistic2)
}
