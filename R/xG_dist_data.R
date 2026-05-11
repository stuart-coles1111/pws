#' Simulated data for xG analysis
#'
#' Simulated data containing shot distance and a binary indicator
#' of whether a goal was scored for 5000 goal-scoring opportunities.
#'
#' @format A data frame with 5000 rows and 2 variables:
#' \describe{
#'   \item{distance}{Distance from goal.}
#'   \item{goal}{Binary indicator taking value 1 if a goal was scored
#'   and 0 otherwise.}
#' }
#'
#' @details
#' Each row corresponds to a single goal-scoring opportunity.
#'
#' @examples
#' head(xG_dist_data, 10)   # First 10 rows
#' mean(xG_dist_data$goal)  # Average xG value
"xG_dist_data"
