#' Premier League Points per Team per Game
#'
#' Points obtained by each team in Premier League fixtures from season 1995/96 to 2023/24
#'
#' @format
#' A data frame with 580 rows and 4 columns
#' Each row corresponds to one team in one season of Premier League
#' The columns give season, team, first-half-season points, second-half-season points
#'
#' @source {https://www.football-data.co.uk/data.php}
#' @examples
#' mean(PL_points$points_half1) # Mean number of points in first half of season
#' hist(PL_points$points_half2, col="lightblue", xlab= "Number of Points", main = "Histogram of second half of season points in Premier League matches") # histogram of all goals data
"PL_points"
