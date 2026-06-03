#' Premier League Model Parameters
#'
#' Premier League Model Parameters
#'
#' @format
#' List with 2 objects:
#' 1: value of tau, home advantage parameter
#' 2: dataframe of team parameters (alpha, beta)
#'
#' @source {in-house}
#' @examples
#' PL24_pars[[1]] # home advantage parameter
#' with(PL24_pars[[2]], plot(alpha, beta)) # plot of defence against attack parameters
#'
"PL24_pars"
