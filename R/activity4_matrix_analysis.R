#' Quiz Matrix Analysis
#'
#' Analyses response for a set of questions in Activity 4 of Playing With Statistics
#'
#' @param score_matrix matrix of scores and best guesses
#' @param answers true quiz answers
#' @param alpha probability of interval

#' @param dp decimal places for score
#'
#' @returns list containing
#' - scores summary of score per question
#' - total_score sum of scores across questions
#' @examples
#' team1_score  <- subset(activity4_responses_sm,team == 1)
#' quiz_score_matrix(team1_score, activity4_quiz_answers) #scores per question and total score for team 1
#' @export
#'
activity4_matrix_analysis <-
    function(score_matrix, answers, alpha = 0.95, dp = 2) {
        #calculate standard deviation based on probability of interval
        sigma <- score_matrix$S / qnorm((1 + alpha) / 2)

        #calculate score
        score <-
            dnorm(score_matrix$G, answers$Answer, sigma, log = TRUE) %>% round(dp)
        score_df <-
            data.frame(
                question = 1:10,
                G = score_matrix$G,
                S = score_matrix$S,
                Theta = answers$Answer,
                score = score
            )

        list(scores = score_df, total_score = sum(score))
    }
