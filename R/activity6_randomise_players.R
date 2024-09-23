#' Player randomisation for Activity 6
#'
#' Facilitates randomisation of players in Activity 6 in Chapter 6 of Playing With Statistics
#'
#' @param players Character vector of player names
#
#'
#' @returns  dataframe with columns:
#' - Player: player name
#' - Number: randomised player number
#' @examples
#' activity6_randomise_players(activity6_player_names) # randomise names in activity6_player_names
#'
#' @export
#'
activity6_randomise_players <- function(players){
    players$Number <- sample(1:nrow(players), nrow(players))
    players
}
