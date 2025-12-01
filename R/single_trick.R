#' Single Trick
#'
#' Output of single trick simulation for Activity 5
#'
#' @param picture_value score to give picture cards
#' @param show_result should result be displayed? (TRUE/FALSE)
#' @param seed value of seed for random number generator

#'
#' @returns  player and magician final card
#' @examples single_trick()
#'
#' @export
#'
#'
#'
single_trick <- function(picture_value = 10, show_result = TRUE, seed = NULL){
    if(!is.null(seed)) set.seed(seed)

    c_value <- c("ace","two","three","four","five",
                 "six","seven","eight","nine","ten", "jack", "queen", "king")
    c_suite <- c("hearts","clubs","diamonds","spades")
    cards <- expand.grid(number = c_value, suite = c_suite)

    cards$score <- rep(1:13, 4)
    cards$score <- ifelse(cards$score <= 10, cards$score, picture_value)
    cards_shuffled <- cards[sample(1:52, 52), ]
    current <- cards_shuffled[sample(1:52,1),]
    cards_shuffled <- cards_shuffled[sample(1:52, 52), ]


    count <- 0
    while(count < 52){
        count <- count + current$score
        if(count <= 52) current <- cards_shuffled[count, ]
    }

    dealer_current <- cards_shuffled[1,]
    count <- 1
    while(count < 52){
        count <- count + dealer_current$score
        if(count <= 52) dealer_current <- cards_shuffled[count, ]
    }

    if(show_result){
        #print(paste0("player card: ", current[1, "number"], " of ", current[1, "suite"]))
        #print(paste0("dealer card: ", dealer_current[1, "number"], " of ", dealer_current[1, "suite"]))
        show_two_cards(current[1, "number"], current[1, "suite"],dealer_current[1, "number"], dealer_current[1, "suite"])
    }
    else
        return(all(current==dealer_current))
}
