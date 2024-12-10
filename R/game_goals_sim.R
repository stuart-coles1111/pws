game_goals_sim <-
    function(x,
             pois_mean = 25,
             beta_alpha_1 = 0.01,
             beta_alpha_2 = 0.2,
             seed = NULL) {
        if(!is.null(seed)) set.seed(seed)
        n_events <- rpois(1, pois_mean)
        p <- rbeta(n_events, beta_alpha_1, beta_alpha_2)
        outcome <- rbinom(n_events, 1, p)
        sum(outcome)
    }
