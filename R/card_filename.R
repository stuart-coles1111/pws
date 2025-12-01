card_filename <- function(number, suite, card_path = NULL) {

    # Map numbers/suits to filenames
    number_map <- c(
        ace = "A", two = "2", three = "3", four = "4", five = "5",
        six = "6", seven = "7", eight = "8", nine = "9", ten = "10",
        jack = "J", queen = "Q", king = "K"
    )
    suite_map <- c(hearts="H", clubs="C", diamonds="D", spades="S")

    # If card_path not supplied, use internal package path
    if (is.null(card_path)) {
        card_path <- system.file("extdata/cards", package = "pws")
    }

    file.path(card_path, paste0(number_map[number], suite_map[suite], ".svg"))
}
