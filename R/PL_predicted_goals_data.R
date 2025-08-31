#' Predicted GOals in Premier League Fixtures
#'
#' Predicted and observed goals per fixture in series of Premier League fixtures
#'
#' @format
#' A data frame with 6302 rows and 3 columns
#' Each row corresponds to a Premier League fixture
#' The columns give fixture number, predicted total goals and observed total goals
#'
#' @source in-house
#' @examples
#'
#' with(PL_predicted_goals, plot(predicted_goals, jitter(observed_goals, .25), pch=16), ylab = "predicted_goals") # plot of observed against predicted goals (jittered to avoid overlap)
"PL_predicted_goals"
