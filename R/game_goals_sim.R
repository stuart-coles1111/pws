game_goals_sim <-
    function(x,
             pois_mean = 25,
             mu = 0.1,
             phi = 2,
             seed = NULL) {
        if(!is.null(seed)) set.seed(seed)
        n_events <- rpois(1, pois_mean)
        beta_1 <- mu * phi
        beta_2 <- (1 - mu) * phi
        p <- rbeta(n_events, beta_1, beta_2)
        outcome <- rbinom(n_events, 1, p)
        sum(outcome)
    }
