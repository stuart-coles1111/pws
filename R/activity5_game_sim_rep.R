#' Activity 5 repeat game simulation
#'
#' Repeats the game described in Activity 5 of Playing With Statistics multiple times
#'
#'
#' @param n_rep Number of repeats of game
#' @param n_rounds Number of rounds ine each game of simulated dice throws
#' @param Theta True value of Theta
#' @param rand_Theta randomise Theta (TRUE/FALSE)? if TRUE, Theta argument is ignored
#' @param show_estimates (TRUE/FALSE)? if TRUE, include estimates as part of output
#' @param seed Set seed to enable identical simulation across calls
#'
#'
#' @returns  list containing simulated scores and their mean value
#' @examples
#' activity5_game_sim_rep()
#'
#' @export
#'
activity5_game_sim_rep <-
    function(n_rep = 1000,
             n_rounds = 30,
             Theta = NULL,
             rand_Theta = FALSE,
             show_estimates = FALSE,
             seed = NULL) {

        if(!is.null(seed)) set.seed(seed)
        if(is.null(Theta)) Theta <- 7.5 # default hidden value of Theta
        if(rand_Theta) Theta <- sample(seq(1, 50, by = 0.25), 1) # randomised value of Theta if required
        ests <- lapply(1:n_rep, function(x, n_rounds, Theta, seed) activity5_game_sim(n_rounds = n_rounds, Theta = Theta, seed = seed, plot = FALSE)$mean_score,
                       n_rounds = n_rounds, Theta = Theta, seed = seed) %>% unlist
        if(show_estimates)
            list(true_Theta = Theta, estimates = ests, mean_estimates = mean(ests)) %>% return
        else
            list(true_Theta = Theta, mean_estimates = mean(ests) ) %>% return

    }
