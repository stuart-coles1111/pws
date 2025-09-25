#' Player randomisation for Activity 7
#'
#' Facilitates randomisation of players in Activity 7 in Chapter 7 of Playing With Statistics
#'
#' @param players Character vector of player names
#' @param seed value of seed for random number generator
#
#'
#' @returns  dataframe with columns:
#' - Player: player name
#' - Number: randomised player number
#' @examples
#' activity7_randomise_players(player_names) # randomise names in player_names
#'
#' @export
#'
activity7_randomise_players <- function(players, seed = NULL){
    if(!is.null(seed))set.seed(seed)
    players$Number <- sample(1:nrow(players), nrow(players))
    players
}
