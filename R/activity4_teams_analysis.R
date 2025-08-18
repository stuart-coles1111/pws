#' Analysis of all team quiz answers
#'
#' Analyses of all team replies to all questions in Activity 4 of Playing With Statistics
#'
#' @param response_matrix matrix of quiz repsonses
#' @param answers true answers
#' @param alpha probability of interval
#' @param dp decimal places for score
#'
#' @returns graphical analysis of score for quiz question
#' @examples
#' activity4_teams_analysis(activity5_responses_sm, activity4_quiz_answers) # analysis of answer to question 1 of quiz
#'
#' @export
#'
#'
activity4_teams_analysis <-
    function(response_matrix, answers, alpha = 0.95, dp = 2) {
        nteams <- length(unique(response_matrix$team))
        score <-
            sapply(unique(response_matrix$team), function(x)
                activity4_matrix_analysis(subset(response_matrix, team == x), answers, alpha = alpha, dp = dp)[[2]])
        data.frame(team = unique(response_matrix$team), score = score)
    }
