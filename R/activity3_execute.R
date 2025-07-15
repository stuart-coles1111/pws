#' Execute Activity 3
#'
#' Individual execution of Activity 3 of Playing With Statistics
#'
#' @param bank Initial bank size
#' @param bankmax Maximum value of bank. If reached execution terminates.
#'
#' @returns  summary information from execution of Activity 3
#' @examples
#' activity3_execute()
#'
#' @export
#'
#'
activity3_execute <- function(bank = 25, bankmax = 250) {
    oldw <- getOption("warn")
    options(warn = -1)
    cat(fill = T)
    cat('====================================')
    cat(fill = T)
    cat(paste0('Initial bank is $', bank), fill = T)
    startTime <- Sys.time()
    return(pws:::virtual_coin_toss(bank, startTime, 0, bankmax))
    options(warn = oldw)
}
