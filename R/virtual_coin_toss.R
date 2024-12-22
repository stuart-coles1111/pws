virtual_coin_toss <- function(bank = 25, startTime, count, bankmax) {
    count <- count + 1

    cat(paste0('Virtual Coin Toss ', count, ". Minutes remaining: ", (startTime + 30 * 60 - Sys.time() )%>% as.double(units="mins") %>% round(1)), fill = T)
    cat("=======================================", fill = T)
    cat("", fill = T)

    if (Sys.time() > startTime + 30 * 60)
        return(paste0('Time up: you finished with $', bank))

    bet <- readline(prompt = "Bet choice (H or T)? ")
    while (!bet %in% c('H', 'T')) {
        bet <- readline(prompt = "Bet choice (H or T)? ")
    }

    stake <- as.numeric(readline(prompt = 'Stake (in dollars)? '))

    while (is.na(stake) | stake > bank | stake <= 0) {
        if (is.na(stake))
            cat("Stake has to be numeric", fill = T)
        else if (stake > bank)
            cat("Stake can't exceed bank", fill = T)
        else if (stake <= 0)
            cat('Stake has to be positive', fill = T)
        stake <-
            as.numeric(readline(prompt = 'Stake (in dollars)? '))
    }

    stake <- round(stake, 2)

    coin_toss <- sample(c('H', 'T'), 1, prob = c(0.6, 0.4))

    if (coin_toss == bet) {
        bank <- round(bank + stake, 2)
        cat("=======================================", fill = T)
        cat(crayon::green(paste0(
            coin_toss,
            ': you win $', stake, '. Current bank is $',
            min(bank, bankmax))
        ),
        fill = T)
        cat("=======================================", fill = T)
    }

    if (coin_toss != bet) {
        bank <- round(bank - stake, 2)
        cat("=======================================", fill = T)
        cat(crayon::red(paste0(coin_toss, ': you lose $', stake, '. Current bank is $', bank)),
                    fill = T)
        cat("=======================================", fill = T)
    }

    cat(fill = T)

    if (bank == 0)
        return(cat('You lost all your money and finished with $0'))

    if (bank >= bankmax)
        return(cat(paste0(
            'You reached the maximum bank size of $', bankmax
        )))

    virtual_coin_toss(bank, startTime, count, bankmax)
}
