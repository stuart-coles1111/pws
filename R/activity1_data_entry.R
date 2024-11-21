#' Data entry for Activity 1
#'
#' Simplifies data entry for Activty 1 in Chapter 1 of Playing With Statistics
#'
#' @param nthrow Number of imaginary throws of coin per participant
#
#'
#' @returns  array of characters, one row per participant, one column per throw, derived from data entered at console
#' @examples
#' activity1_data_entry(5) # enter data when each participant has made just 5 imaginary throws of coin
#'
#' @export
#'

activity1_data_entry <- function(nthrow = 50) {
    data_array <- c()
    cont <- TRUE
    while (cont) {
        redo <- TRUE
        while (redo) {
            data_line <- readline(prompt = 'Individual data: ')
            data_line <- stringi::stri_replace_all_fixed(data_line, " ", "")
            data_line <- data_line %>% strsplit(split = "") %>% unlist
            if (length(data_line) != nthrow)
                cat('Incorrect number of entries', fill = TRUE)
            else if (!all(data_line %in% c("H", "T")))
                cat('Each entry must be "H" or "T"', fill = TRUE)
            else {
                redo <- F
                data_array <- rbind(data_array, data_line)
            }
        }
        more_data <- readline(prompt = 'More data? (Y/N): ')
        cat("", fill = TRUE)
        cont <- ifelse(more_data %in% c('y', 'Y'), TRUE, FALSE)
    }
    rownames(data_array) <- paste0("participant ", 1:nrow(data_array))
    colnames(data_array) <- paste0("choice ", 1:nthrow)

    return(data_array)
}
