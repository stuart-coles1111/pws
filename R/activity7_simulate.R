#' Simulate the Activity 7 dice tournament
#'
#' Simulates the dice tournament from Activity 7 of
#' *Playing With Statistics*. Results from the early rounds
#' are used to estimate player strengths and predict each
#' player's probability of winning the tournament.
#'
#' The tournament is then completed and the eventual winner
#' is compared with the estimated win probabilities.
#'
#' @param nrounds Number of tournament rounds. The tournament
#'   contains `2^nrounds` players.
#' @param n_games_per_match Number of games played in each match.
#'   Must be a positive odd integer.
#' @param n_rounds_for_estimation Number of rounds observed before
#'   estimating model parameters and tournament win probabilities.
#' @param nsim Number of simulated tournament completions used
#'   to estimate tournament win probabilities.
#' @param seed Optional random seed for reproducibility.
#'
#' @return
#' The function primarily produces interactive console output,
#' including tournament schedules, match results, parameter
#' estimates, and estimated tournament win probabilities.
#'
#' Invisibly returns `NULL`.
#'
#' @examples
#' \dontrun{
#' activity7_simulate()
#' }
#'
#' @export
activity7_simulate <- function(
        nrounds = 5,
        n_games_per_match = 5,
        n_rounds_for_estimation = 2,
        nsim = 1000,
        seed = NULL
) {

    if(!is.null(seed)) set.seed(seed)

    if (
        n_games_per_match < 1 ||
        n_games_per_match %% 2 != 1
    ) {
        stop("n_games_per_match must be a positive odd integer")
    }

    #1st round
    nplayers <- 2 ^ nrounds
    player_numbers <- 1:nplayers

    player_colours <-
        sample(
            rep(c("blue", "red", "green", "yellow"), nplayers / 4),
            nplayers,
            replace = FALSE
        )

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
    print(estimation_df[, 1:5])

    results <-
        pws:::activity7_round_sim(
            home_colours,
            away_colours,
            n_games_per_match = n_games_per_match
        )

    estimation_df$results <- results

    winners <-
        ifelse(
            estimation_df$results >= (n_games_per_match + 1) / 2,
            players_home,
            players_away
        )

    estimation_df$winners <- winners

    cat("\n\n Home wins for each match:\n\n")
    cat(results, "\n")

    cat("\n\n Match winners:\n\n")
    cat(winners, "\n")

    invisible(readline(prompt = "Press [enter] to continue"))

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

        cat(paste0("Schedule for Round ", i, ":\n\n"))
        print(round_df[, 1:5])

        results <-
            pws:::activity7_round_sim(
                home_colours,
                away_colours,
                n_games_per_match = n_games_per_match
            )

        cat(results, "\n")

        cat("\n\n Match winners:\n\n")

        winners <-
            ifelse(
                results >= (n_games_per_match + 1) / 2,
                players_home,
                players_away
            )

        cat(winners, "\n")

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

    cat("\n\n Data for Estimation:\n\n")
    print(estimation_df)

    invisible(readline(prompt = "Press [enter] to continue"))

    pars <-
        optim(
            c(0, 0, 0, 0),
            pws:::activity7_neg_log_lik,
            dice_history = estimation_df
        )$par

    cat("\n\n")

    cat("Parameter Estimates:\n")

    full_pars <- c(0, pars)

    pars_df <- data.frame(
        parameter = c(
            "blue",
            "red",
            "green",
            "yellow",
            "home advantage"
        ),
        value = round(full_pars, 3)
    )

    print(pars_df)

    cat("\n\n")

    invisible(readline(prompt = "Press [enter] to continue"))

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
                pws:::activity7_round_sim_using_fits(
                    pars,
                    home_colours_sim,
                    away_colours_sim,
                    n_games_per_match = n_games_per_match
                )

            winners_sim <-
                ifelse(
                    results_sim >= (n_games_per_match + 1) / 2,
                    players_home_sim,
                    players_away_sim
                )
        }

        winner_vec <- c(winner_vec, rev(winners_sim)[1])
    }

    winner_tab <- table(winner_vec) / nsim

    winner_df <- data.frame(winner_tab)
    colnames(winner_df) <- c("Player", "Probability")

    cat("Tournament win probability:\n")

    print(winner_df)

    cat("\n\n Now finish tournament\n\n")

    for (i in (n_rounds_for_estimation + 1):nrounds) {

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

        cat("\n\n")
        cat(paste0("Schedule for Round ", i, ":\n\n"))
        print(round_df[, 1:5])

        results <-
            pws:::activity7_round_sim(
                home_colours,
                away_colours,
                n_games_per_match = n_games_per_match
            )

        invisible(readline(prompt = "Press [enter] to continue"))

        cat(results, "\n")

        cat("\n\n Match winners:\n\n")

        winners <-
            ifelse(
                results >= (n_games_per_match + 1) / 2,
                players_home,
                players_away
            )

        cat(winners, "\n")

        current_winners <- winners
    }

    cat("\n\n")

    invisible(readline(prompt = "Press [enter] to continue"))

    cat("Tournament winner", winners, "...\n")

    invisible(readline(prompt = "Press [enter] to continue"))

    cat(
        "Had win probability",
        winner_df[match(winners, winner_df$Player), 2],
        "after round",
        n_rounds_for_estimation
    )

    invisible()
}
