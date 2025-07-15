#' Premier League Total Goals per Game
#'
#' Total Goals scored in Premier League fixtures from season 1993/94 to 2023/24
#'
#' @format
#' A data frame with 12786 rows and 3 columns
#' Each row corresponds to a Premier League fixture
#' The columns give date, season and total goals
#'
#' @source {https://www.football-data.co.uk/data.php}
#' @examples
#'
#' mean(PL_goals$goals) # Find mean number of goals over all fixtures
#'
#' mean(subset(PL_goals$goals, PL_goals$season == "2023-24")) # Find mean number of goals over fixtures  2023/24 season
#'
#' hist(PL_goals$goals, col="lightblue", xlab= "Number of Goals", main = "Histogram of goals per game in Premier League Matches") # histogram of all goals data
#'
"PL_goals"
