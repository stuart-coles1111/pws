activity7_round_sim_using_fits <-
    function(pars,
             home_player,
             away_player,
             n_games_per_match = 5) {
        colours <- c('blue', 'red', 'yellow', 'green')
        home_ind <- match(home_player, colours)
        away_ind <- match(away_player, colours)
        pars <- c(0, pars)
        bin_p <- pars[5] + pars[home_ind] - pars[away_ind]
        bin_p <- exp(bin_p) / (1 + exp(bin_p))
        total_home_wins <-
            rbinom(length(home_player), n_games_per_match, bin_p)
        total_home_wins
    }
