#' Simulate goals
#'
#' Simulates goals as described in Chapter 1
#'
#' @param pois_mean Average number of events per game
#' @param beta_alpha_1 First parameter that affects probability events become goals
#' @param beta_alpha_2 Second parameter that affects probability events become goals
#' @param seed Set seed to enable identical simulation across calls
#'
#' @examples
#' goals_sim()
#'
#' @export
#'
goals_sim <-
    function(x,
             pois_mean = 100,
             beta_alpha_1 = 0.01,
             beta_alpha_2 = 0.5,
             seed = NULL) {
        if(!is.null(seed)) set.seed(seed)
        n_events <- rpois(1, pois_mean)
        p <- rbeta(n_events, beta_alpha_1, beta_alpha_2)
        outcome <- rbinom(n_events, 1, p)
        sum(outcome)
    }
