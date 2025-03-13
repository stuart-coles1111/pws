#' Execute Activity 6
#'
#' Manage dice tournament in Activity 6 of Playing With Statistics
#'
#' @param nrounds number of rounds of tournament
#' @param n_games_per_match number of games per match
#' @param n_rounds_for_estimation number of rounds to use for data collection prior to estimation
#' @param nsim number of matches to simulate when calculating predictions
#' @param seed value of seed for random number generator
#'
#' @returns value of tournament winner and their win probability at point of estimation
#' @examples
#' activity6_run_tournament()
#'
#' @export
#'
#'
activity6_run_tournament <- function(nrounds = 5,
                            n_games_per_match = 5,
                            n_rounds_for_estimation = 2,
                            nsim = 1000,
                            seed = NULL) {

    if(!is.null(seed)) set.seed(seed)

    if (n_games_per_match < 1 |
        n_games_per_match %% 2 != 1)
        return("n_games_per_match must be positive odd integer")

    #1st round
    nplayers <- 2 ^ nrounds
    player_numbers <- 1:nplayers
    player_colours <-
        sample(rep(c("blue", "red", "green", "yellow"), nplayers / 4), nplayers, replace =
                   F)
    players_home <- seq(1, nplayers - 1, by = 2)
    players_away <- seq(2, nplayers, by = 2)
    home_colours <- player_colours[players_home]
    away_colours <- player_colours[players_away]
    results <- ""
    winners <- ""


    estimation_df <-
        data.frame(
            round = 1,
            players_home,
            players_away,
            home_colours,
            away_colours,
            results,
            winners
        )

    cat("Schedule for Round 1:\n\n")
    print(estimation_df[,1:5])


#    cat("\n\n Enter home wins for each game separated by space:")
#    results <- scan()
    cat("\n\n")

    ng <- nrow(estimation_df)
    cont <- T
    while(cont){
        cont <- F
        results <- readline("Enter number of home wins per game. Separate entries by space; finish with Return: ")
        results <-  strsplit(results, "\\s+") %>% unlist
        if(length(results) != ng){
            cont <- T
            cat('Error: wrong number of entries\n\n')
        }
        if(!all(results %in% (0:n_games_per_match))){
            cont <- T
            cat(paste0('Error: all entries must be integers between ',0,' and ',n_games_per_match, '\n\n'), fill=T)
        }
    }
    results <- as.integer(results)
    estimation_df$results <- results

    winners <-
        ifelse(estimation_df$results >= (n_games_per_match + 1) / 2,
               players_home,
               players_away)

    estimation_df$winners <- winners

    #subsequent rounds

    for (i in 2:(n_rounds_for_estimation)) {
        nplayers <- 2 ^ (nrounds - i + 1)
        players_home <- winners[seq(1, nplayers - 1, by = 2)]
        players_away <- winners[seq(2, nplayers, by = 2)]
        home_colours <- player_colours[players_home]
        away_colours <- player_colours[players_away]
        results <- ""
        winners <- ""

        round_df <-
            data.frame(
                round = i,
                players_home,
                players_away,
                home_colours,
                away_colours,
                results,
                winners
            )


        cat(paste0("Schedule for Round ",i,":\n\n"))
        print(round_df[,1:5])

        cat("\n\n")

        ng <- nrow(round_df)
        cont <- T
        while(cont){
            cont <- F
            results <- readline("Enter number of home wins per game. Separate entries by space; finish with Return: ")
            results <-  strsplit(results, "\\s+") %>% unlist
            if(length(results) != ng){
                cont <- T
                cat('Error: wrong number of entries\n\n')
            }
            if(!all(results %in% (0:n_games_per_match))){
                cont <- T
                cat(paste0('Error: all entries must be integers between ',0,' and ',n_games_per_match, '\n\n'), fill=T)
            }
        }
        results <- as.integer(results)
        estimation_df$results <- results

        winners <-
            ifelse(results >= (n_games_per_match + 1) / 2,
                   players_home,
                   players_away)


        current_winners <- winners
        estimation_df <-
            rbind(
                estimation_df,
                data.frame(
                    round = i,
                    players_home,
                    players_away,
                    home_colours,
                    away_colours,
                    results,
                    winners
                )
            )

    }

    cat("Data for Estimation:\n\n")
    print(estimation_df)
    invisible(readline(prompt="Press [enter] to continue"))

    pars <-
        optim(c(0, 0, 0, 0), pws:::activity6_neg_log_lik, dice_history = estimation_df)$par

    cat("\n\n Parameter Estimates:\n")
    full_pars <- c(0, pars)
    pars_df <- data.frame(parameter = c("blue", "red", "green", "yellow", "home_adv"), value = full_pars %>% round(3))

    print(pars_df)

    cat("\n\n")

    winner_vec <- c()

    for (j in 1:nsim) {
        winners_sim <- current_winners

        for (i in (n_rounds_for_estimation + 1):nrounds) {
            nplayers <- 2 ^ (nrounds - i + 1)
            players_home_sim <- winners_sim[seq(1, nplayers - 1, by = 2)]
            players_away_sim <- winners_sim[seq(2, nplayers, by = 2)]
            home_colours_sim <- player_colours[players_home_sim]
            away_colours_sim <- player_colours[players_away_sim]
            results_sim <-
                pws:::activity6_round_sim_using_fits(pars,
                                                     home_colours_sim,
                                                     away_colours_sim,
                                                     n_games_per_match = n_games_per_match)
            winners_sim <-
                ifelse(results_sim >= (n_games_per_match + 1) / 2,
                       players_home_sim,
                       players_away_sim)

        }

        winner_vec <- c(winner_vec, rev(winners_sim)[1])
    }
    winner_tab <- (winner_vec %>% table)/nsim

    winner_df <- data.frame(winner_tab)
    colnames(winner_df) <- c('Player','Probability')

    cat("Tournament win probability:\n")


    print(winner_df)

    cat("\n\n")

    cat("Now finish tournament")

    cat("\n\n")

    for (i in (n_rounds_for_estimation+1):nrounds) {
        nplayers <- 2 ^ (nrounds - i + 1)
        players_home <- winners[seq(1, nplayers - 1, by = 2)]
        players_away <- winners[seq(2, nplayers, by = 2)]
        home_colours <- player_colours[players_home]
        away_colours <- player_colours[players_away]
        results <- ""
        winners <- ""

        round_df <-
            data.frame(
                round = i,
                players_home,
                players_away,
                home_colours,
                away_colours,
                results,
                winners
            )


        cat(paste0("Schedule for Round ",i,":\n\n"))
        print(round_df[,1:5])

        cat("\n\n")

        ng <- nrow(round_df)
        cont <- T
        while(cont){
            cont <- F
            results <- readline("Enter number of home wins per game. Separate entries by space; finish with Return: ")
            results <-  strsplit(results, "\\s+") %>% unlist
            if(length(results) != ng){
                cont <- T
                cat('Error: wrong number of entries\n\n')
            }
            if(!all(results %in% (0:n_games_per_match))){
                cont <- T
                cat(paste0('Error: all entries must be integers between ',0,' and ',n_games_per_match, '\n\n'), fill=T)
            }
        }
        results <- as.integer(results)
        estimation_df$results <- results

        winners <-
            ifelse(results >= (n_games_per_match + 1) / 2,
                   players_home,
                   players_away)


        current_winners <- winners

    }

    cat("Tournament winner", winners, "...\n")
    invisible(readline(prompt="Press [enter] to continue"))
    cat("Had win probability", winner_df[match(winners, winner_df$Player), 2], "after round", n_rounds_for_estimation)
    invisible()

}
