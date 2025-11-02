#' Simulated data for xG analysis
#'
#' distance and goal scored indicator for 5000 simulated goalscooring opportunities
#'
#' @format
#' A data frame with 5000 rows and 2 columns
#' Each row corresponds to one goalscoring opportunity
#' The columns give distance from gol and a binary indicator of whether a goal was scored or not
#'
#' @examples
#' head(xG_dist_data, 10) # Frst 10 rows of data
#' mean(xG_dist_data$goal) # Average xG value across all distances
"xG_dist_data"
