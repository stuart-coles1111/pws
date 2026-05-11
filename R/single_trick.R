#' Simulate the card trick for Activity 5
#'
#' Simulates a single run of the card trick used in Activity 5.
#' The function either displays the final cards for the player
#' and dealer, or returns whether the trick succeeds.
#'
#' @param picture_value Numeric value assigned to picture cards.
#' @param show_result Logical; should the final cards be displayed?
#'   Defaults to `TRUE`.
#' @param seed Optional random seed for reproducibility.
#'
#' @return
#' If `show_result = TRUE`, the function displays the final cards.
#'
#' If `show_result = FALSE`, the function returns a logical value:
#' `TRUE` if the player and dealer end on the same card, and
#' `FALSE` otherwise.
#'
#' @examples
#' activity5_simulate_trick()
#'
#' @export
activity5_simulate_trick <- function(
        picture_value = 10,
        show_result = TRUE,
        seed = NULL
){

    if(!is.null(seed)) set.seed(seed)

    c_value <- c(
        "ace", "two", "three", "four", "five",
        "six", "seven", "eight", "nine", "ten",
        "jack", "queen", "king"
    )

    c_suite <- c(
        "hearts", "clubs", "diamonds", "spades"
    )

    cards <- expand.grid(
        number = c_value,
        suite = c_suite
    )

    cards$score <- rep(1:13, 4)

    cards$score <- ifelse(
        cards$score <= 10,
        cards$score,
        picture_value
    )

    cards_shuffled <- cards[sample(1:52, 52), ]

    current <- cards_shuffled[sample(1:52, 1), ]

    cards_shuffled <- cards_shuffled[sample(1:52, 52), ]

    count <- 0

    while(count < 52){

        count <- count + current$score

        if(count <= 52)
            current <- cards_shuffled[count, ]
    }

    dealer_current <- cards_shuffled[1, ]

    count <- 1

    while(count < 52){

        count <- count + dealer_current$score

        if(count <= 52)
            dealer_current <- cards_shuffled[count, ]
    }

    if(show_result){

        show_two_cards(
            current[1, "number"],
            current[1, "suite"],
            dealer_current[1, "number"],
            dealer_current[1, "suite"]
        )

    } else {

        return(all(current == dealer_current))
    }
}
