#' Quiz Score Calculation
#'
#' Calculates Score for one or more answers in Activity 4 of Playing With Statistics
#'
#' @param G vector of best guesses (one per question)
#' @param S vector of measure of accuracy (one per question)
#' @param Theta true answer (one per question)
#' @param alpha probability of interval
#' @param dp decimal places for score
#'
#' @returns list containing:
#' - scores: score per question
#' - total_score: total score across all questions
#' - sigma: transformed measure of accuracy (standard deviation) for each question
#' @examples
#' activity4_response_score(c(150, 100), c(20, 60), activity5_quiz_answers$Answer[1:2]) # get scores for first 2 questions
#'
#' @export
#'
#'
activity4_response_score <- function(G, S, Theta, alpha = 0.95, dp = 2) {
    #calculate standard deviation based on probability of interval
    sigma <- S / qnorm((1 + alpha) / 2)

    #calculate score
    score <- dnorm(G, Theta, sigma, log = TRUE) %>% round(dp)

    list(scores = score,
         total_score = sum(score),
         sigma = sigma)
}
