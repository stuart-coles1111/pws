activity7_neg_log_lik <- function(p, dice_history) {
    colours <- c('blue', 'red', 'yellow', 'green')
    home_ind <- match(dice_history$home_colours, colours)
    away_ind <- match(dice_history$away_colours, colours)
    pars <- c(0, p)
    bin_p <- pars[5] + pars[home_ind] - pars[away_ind]
    bin_p <- exp(bin_p) / (1 + exp(bin_p))
    - sum(dbinom(dice_history$results, 5, bin_p, log = T))
}
