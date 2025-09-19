#' Match Win Probabilities
#'
#' Match win probability calculation based on standard model with independent Poisson means for goal scores per team
#'
#' @param team1_pars vector of attack and defence parameters for team 1
#' @param team2_pars vector of attack and defence parameters for team 2
#' @param home_adv_par home advantage parameter
#' @param score_max upper limit for either team goal count
#' @param sig significant figures in output
#'
#' @returns  vector of 1/x/2 probabilities
#' @examples match_win_probs(c(.346,.192), c(.521,.249), .063) # man city vs liverpool example from Chapter 8
#'
#' @export
#'
#'

match_win_probs <- function(team1_pars, team2_pars, home_adv_par, score_max = 20, sig= 4){
    team1_mean <- exp( home_adv_par + team1_pars[1] - team2_pars[2]) # team1 poisson mean
    team2_mean <- exp(team2_pars[1] - team1_pars[2]) # team2 poisson mean
    team1_probs <- dpois(0:score_max, team1_mean) # score probs for team 1
    team2_probs <- dpois(0:score_max, team2_mean) # score probs for team 2
    score_probs <- outer(team1_probs, team2_probs, '*') # (team1, team2) score probs based on independent poisson assumption
    team1_win_prob <-  score_probs[lower.tri(score_probs)] %>% sum # sum of probabilities over scores where team1 wins
    team2_win_prob <-  score_probs[upper.tri(score_probs)] %>% sum # sum of probabilities over scores where team2 wins
    draw_prob <- diag(score_probs) %>% sum # sum of probabilities over drawn scores
    c(team1_win_prob = team1_win_prob, draw_prob = draw_prob, team2_win_prob = team2_win_prob) %>% round(sig)
}
