#' Premier League match results (1992–2025)
#'
#' A dataset containing match results from the English Premier League,
#' including dates, teams, and goals scored.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{season}{Season in "YYYY-YYYY" format}
#'   \item{date}{Match date}
#'   \item{home_team}{Home team name}
#'   \item{away_team}{Away team name}
#'   \item{home_goals}{Goals scored by home team}
#'   \item{away_goals}{Goals scored by away team}
#' }
#'
#' @details
#' Data derived from the \code{EngSoccerData} package.
#' Original data compiled by Tony Ladson and contributors.
#'
#' @source
#' \code{EngSoccerData} R package.
#'
#' @examples
#'
#' mean(PL_goals$home_goals) # Find mean number of home goals over all fixtures
#'
#'subset(PL_goals$total_goals, PL_goals$season == "2023-2024") %>% mean # Find mean number of total goals over fixtures in 2023/24 season
#'
#' hist(PL_goals$goal_diff, col="lightblue", xlab= "Number of Goals", main = "Histogram of goal difference per game in Premier League Matches") # histogram of all goals data
#'
#' @usage PL_goals
"PL_goals"
