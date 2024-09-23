#' Execute Activity 4
#'
#' Individual execution of Activity 4 of Playing With Statistics
#'
#' @param bank Initial bank size
#' @param bankmax MAximum value of bank. If reached execution terminates.
#'
#' @returns  summary information from execution of activity 4
#' @examples
#' activity4_execute()
#'
#' @export
#'
#'
activity4_execute <- function(bank = 25, bankmax = 250) {
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
