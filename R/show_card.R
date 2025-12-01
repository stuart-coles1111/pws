show_card <- function(number, suite, card_path = NULL, card_num = NULL, scale = 1.5) {

    file <- card_filename(number, suite, card_path)
    if(!file.exists(file)) stop(paste("Card file not found:", file))

    img <- magick::image_read(file, density = 300)
    ras <- as.raster(img)

    grid::grid.newpage()
    grid::grid.raster(ras, width = grid::unit(scale * 2.5, "cm"), height = grid::unit(scale * 3.75, "cm"))

    if (!is.null(card_num)) {
        grid::grid.text(
            paste0("Card number ", card_num),
            y = grid::unit(1, "npc") - grid::unit(0.05, "npc"),
            gp = grid::gpar(fontsize = 14, fontface = "bold")
        )
    }
}
