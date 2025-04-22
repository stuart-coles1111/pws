activity7_round_sim <-
    function(home_player,
             away_player,
             n_games_per_match = 5) {
        blue_probs <-  c(4, 5, 7, 9, 11, 0) / 36
        red_probs <- c(0, 11, 9, 7, 5, 4) / 36
        green_probs <- c(3, 7, 11, 9, 5, 1) / 36
        yellow_probs <- c(10, 8, 6, 4, 2, 6) / 36
        total_home_wins <- c()
        for (i in 1:length(home_player)) {
            home_probs <- paste0(home_player[i], "_probs") %>% get()
            away_probs <- paste0(away_player[i], "_probs") %>% get()
            home_score <-
                sample(1:6,
                       size = n_games_per_match,
                       replace = T,
                       prob = home_probs)
            away_score <-
                sample(1:6,
                       size = n_games_per_match,
                       replace = T,
                       prob = away_probs)
            home_supremacy <- home_score - away_score
            total_home_wins[i] <- sum(home_supremacy >= 0)
        }
        total_home_wins
    }
