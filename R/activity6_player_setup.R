#' Data entry for Activity 6
#'
#' Simplifies data entry for Activity 6 in Chapter 6 of Playing With Statistics
#'
#' @param nplayer Number of players in tournament
#
#'
#' @returns  character vector of names
#' @examples
#' activity6_player_setup(4) # enter data for tournament of 4 players
#'
#' @export
#'

activity6_player_setup <- function(nplayers = 32) {
    player_vector <- c()
    count <- 0
    while (count < nplayers) {
        count <- count + 1
        player <- readline(prompt = paste0("Name of Player ",count, ": "))
        player_vector <- c(player_vector, player)
    }
    return(player_vector)
}
