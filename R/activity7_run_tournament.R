#' Execute Activity 7
#'
#' Manage dice tournament in Activity 7 of Playing With Statistics
#'
#' @param nrounds number of rounds of tournament
#' @param n_games_per_match number of games per match
#' @param n_rounds_for_estimation number of rounds to use for data collection prior to estimation
#' @param nsim number of matches to simulate when calculating predictions
#' @param seed value of seed for random number generator
#'
#' @returns value of tournament winner and their win probability at point of estimation
#' @examples
#' activity7_run_tournament()
#'
#' @export
#'
#'
activity7_run_tournament <- function(nrounds = 5,
                 n_games_per_match = 5,
                 n_rounds_for_estimation = 2,
                 nsim = 1000,
                 seed = NULL) {

    if (!is.null(seed)) {
        set.seed(seed)
    }

    # ----- VALIDATION -----

    if (!is.numeric(nrounds) ||
        nrounds < 1 ||
        nrounds %% 1 != 0) {
        stop("nrounds must be a positive integer")
    }

    if (!is.numeric(n_games_per_match) ||
        n_games_per_match < 1 ||
        n_games_per_match %% 2 != 1) {
        stop("n_games_per_match must be a positive odd integer")
    }

    if (!is.numeric(n_rounds_for_estimation) ||
        n_rounds_for_estimation < 1 ||
        n_rounds_for_estimation > nrounds) {
        stop("n_rounds_for_estimation must be between 1 and nrounds")
    }

    # ----- HELPER FUNCTIONS -----

    get_results <- function(ng, n_games_per_match) {

        repeat {

            x <- readline(
                paste0(
                    "Enter ",
                    ng,
                    " home-win counts separated by spaces: "
                )
            )

            x <- strsplit(trimws(x), "\\s+")[[1]]

            suppressWarnings(x_num <- as.integer(x))

            valid <- (
                length(x_num) == ng &&
                    all(!is.na(x_num)) &&
                    all(x_num >= 0) &&
                    all(x_num <= n_games_per_match)
            )

            if (valid) {
                return(x_num)
            }

            cat(
                paste0(
                    "\nInvalid input.\n",
                    "- Need exactly ", ng, " integers\n",
                    "- Each must be between 0 and ",
                    n_games_per_match, "\n\n"
                )
            )
        }
    }

    make_round_df <- function(round,
                              players_home,
                              players_away,
                              player_colours,
                              results = NA_integer_) {

        home_colours <- player_colours[players_home]
        away_colours <- player_colours[players_away]

        winners <- ifelse(
            results >= (n_games_per_match + 1) / 2,
            players_home,
            players_away
        )

        data.frame(
            round = round,
            players_home = players_home,
            players_away = players_away,
            home_colours = home_colours,
            away_colours = away_colours,
            results = results,
            winners = winners
        )
    }

    # ----- INITIAL SETUP -----

    nplayers <- 2^nrounds

    player_numbers <- 1:nplayers

    player_colours <- sample(
        rep(c("blue", "red", "green", "yellow"),
            length.out = nplayers)
    )

    winners <- player_numbers

    estimation_list <- list()

    # ==========================================================
    # ESTIMATION PHASE
    # ==========================================================

    for (i in 1:n_rounds_for_estimation) {

        nplayers_round <- length(winners)

        players_home <- winners[seq(1, nplayers_round - 1, by = 2)]
        players_away <- winners[seq(2, nplayers_round, by = 2)]

        schedule_df <- make_round_df(
            round = i,
            players_home = players_home,
            players_away = players_away,
            player_colours = player_colours
        )

        cat(paste0("\nSchedule for Round ", i, ":\n\n"))

        print(schedule_df[, 1:5])

        cat("\n")

        results <- get_results(
            ng = nrow(schedule_df),
            n_games_per_match = n_games_per_match
        )

        round_df <- make_round_df(
            round = i,
            players_home = players_home,
            players_away = players_away,
            player_colours = player_colours,
            results = results
        )

        estimation_list[[i]] <- round_df

        winners <- round_df$winners
    }

    estimation_df <- do.call(rbind, estimation_list)

    # ----- CHECK ESTIMATION DATA -----

    cat("\nData for Estimation:\n\n")
    print(estimation_df)

    invisible(readline(prompt = "Press [enter] to continue"))

    browser()

    # ==========================================================
    # PARAMETER ESTIMATION
    # ==========================================================

    pars <- optim(
        c(0, 0, 0, 0),
        pws:::activity7_neg_log_lik,
        dice_history = estimation_df
    )$par

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

    cat("\nParameter Estimates:\n\n")
    print(pars_df)

    # ==========================================================
    # SIMULATION PHASE
    # ==========================================================

    winner_vec <- integer(nsim)

    for (j in seq_len(nsim)) {

        winners_sim <- winners

        if (n_rounds_for_estimation < nrounds) {

            for (i in (n_rounds_for_estimation + 1):nrounds) {

                players_home_sim <-
                    winners_sim[seq(1, length(winners_sim) - 1, by = 2)]

                players_away_sim <-
                    winners_sim[seq(2, length(winners_sim), by = 2)]

                home_colours_sim <- player_colours[players_home_sim]
                away_colours_sim <- player_colours[players_away_sim]

                results_sim <-
                    pws:::activity7_round_sim_using_fits(
                        pars,
                        home_colours_sim,
                        away_colours_sim,
                        n_games_per_match = n_games_per_match
                    )

                winners_sim <- ifelse(
                    results_sim >= (n_games_per_match + 1) / 2,
                    players_home_sim,
                    players_away_sim
                )
            }
        }

        winner_vec[j] <- winners_sim[1]
    }

    winner_df <- as.data.frame(table(winner_vec) / nsim)

    colnames(winner_df) <- c("Player", "Probability")

    cat("\nTournament win probabilities:\n\n")
    print(winner_df)

    # ==========================================================
    # COMPLETE TOURNAMENT
    # ==========================================================

    if (n_rounds_for_estimation < nrounds) {

        cat("\nNow finish tournament\n\n")

        for (i in (n_rounds_for_estimation + 1):nrounds) {

            players_home <- winners[seq(1, length(winners) - 1, by = 2)]

            players_away <- winners[seq(2, length(winners), by = 2)]

            schedule_df <- make_round_df(
                round = i,
                players_home = players_home,
                players_away = players_away,
                player_colours = player_colours
            )

            cat(paste0("Schedule for Round ", i, ":\n\n"))

            print(schedule_df[, 1:5])

            cat("\n")

            results <- get_results(
                ng = nrow(schedule_df),
                n_games_per_match = n_games_per_match
            )

            round_df <- make_round_df(
                round = i,
                players_home = players_home,
                players_away = players_away,
                player_colours = player_colours,
                results = results
            )

            winners <- round_df$winners
        }
    }

    tournament_winner <- winners[1]

    cat("\nTournament winner:", tournament_winner, "\n")

    prob <- winner_df[
        match(tournament_winner, winner_df$Player),
        "Probability"
    ]

    cat(
        "Estimated win probability after round",
        n_rounds_for_estimation,
        ":",
        prob,
        "\n"
    )

    invisible(
        list(
            estimation_df = estimation_df,
            parameter_estimates = pars_df,
            winner_probabilities = winner_df,
            winner = tournament_winner
        )
    )
}
