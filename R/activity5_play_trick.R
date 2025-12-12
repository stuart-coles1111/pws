#' Card Trick
#'
#' Card trick for Activity 5
#'
#' @param picture_value score to give picture cards
#' @param time_pause time pause between turning of cards
#' @param card_path path for card figures
#' @param seed seed fo random number generator
#'
#' @returns  player's card and prediction
#' @examples activity5_play_trick()
#'
#' @export
#'
#'

activity5_play_trick <- function(picture_value = 10, time_pause = 1,
                     card_path = NULL, seed = NULL) {

    if(!is.null(seed)) set.seed(seed)

    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))
    Sys.sleep(0.5)

    c_value <- c("ace","two","three","four","five",
                 "six","seven","eight","nine","ten",
                 "jack","queen","king")
    c_suite <- c("hearts","clubs","diamonds","spades")

    cards <- expand.grid(number=c_value, suite=c_suite, stringsAsFactors = FALSE)
    cards$score <- 1:13
    cards$score <- ifelse(cards$score <= 10, cards$score, picture_value)

    cards_shuffled <- cards[sample(1:52, 52), ]

    # Pick the spectator’s chosen card
    current <- cards_shuffled[sample(1:52,1), ]

    cat("The Magician has shuffled the pack\n\n")
    cat("This is your key card\n\n")
    cat("*** This card is hidden from the Magician ***\n\n")

    show_card(current$number, current$suite, card_path)
    readline("Press [Enter] to continue...")
    cat("\n")

    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))
    Sys.sleep(0.5)

    cat("The Magician has shuffled the pack again\n\n")
    readline("Press [Enter] and the Magician will deal the cards, one by one...")
    cat("\n")

    cards_shuffled <- cards_shuffled[sample(1:52, 52), ]
    dealer <- cards_shuffled[1, ]

    count <- 0
    dealer_count <- -1

    for(i in 1:52){

        count <- count + 1
        dealer_count <- dealer_count + 1

        show_card(cards_shuffled[i,"number"],
                  cards_shuffled[i,"suite"],
                  card_path, card_num = i)

        if(dealer_count == dealer$score){
            dealer <- cards_shuffled[i, ]
            dealer_count <- 0
        }

        if(count == current$score){
            current <- cards_shuffled[i, ]
            count <- 0
        }

        Sys.sleep(time_pause)
    }

    readline("\nPress [Enter] to see the Magician's prediction of your final card...")

    cat("\n*** The Magician predicts your final card is the ",
        dealer$number, " of ", dealer$suite, " ***\n", sep="")

    show_card(dealer$number, dealer$suite, card_path)

    grid::grid.text(
        "Magician's Prediction",
        y = grid::unit(1, "npc") - grid::unit(0.03, "npc"),
        gp = grid::gpar(fontsize = 20, fontface = "bold")
    )

    invisible(NULL)
}
