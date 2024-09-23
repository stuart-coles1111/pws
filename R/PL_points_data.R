#' Premier League Points per Team per Game
#'
#' Points obtained by each team in Premier League fixtures from season 1995/96 to 2023/24
#'
#' @format
#' A data frame with 11020 rows and 3 columns
#' Each row corresponds to a Premier League fixture
#' The columns give date, team1 (home) and team2 (away) names and respective points won
#'
#' @source {https://www.football-data.co.uk/data.php}
#' @examples
#' mean(PL_points$team1_points) # Find mean number of points obtained by home team
#' hist(PL_points$team1_points - PL_points$team2_points, col="lightblue", xlab= "Number of Goals", main = "Histogram of Points Difference per game in Premier League Matches") # histogram of all goals data
"PL_points"
