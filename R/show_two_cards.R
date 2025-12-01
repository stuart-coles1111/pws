show_two_cards <- function(number1, suite1,
                           number2, suite2,
                           card_path = NULL,
                           card_num1 = NULL,
                           card_num2 = NULL,
                           spacing = 0.2,
                           scale = 1.5) {

    # Helper to load and convert card images
    read_card <- function(number, suite) {
        file <- card_filename(number, suite, card_path)
        if(!file.exists(file)) stop(paste("Card file not found:", file))
        img <- magick::image_read(file, density = 300)
        as.raster(img)
    }

    ras1 <- read_card(number1, suite1)
    ras2 <- read_card(number2, suite2)

    grid::grid.newpage()

    card_width  <- grid::unit(scale * 2.5, "cm")
    card_height <- grid::unit(scale * 3.75, "cm")

    # Horizontal positions
    x1 <- 0.5 - spacing
    x2 <- 0.5 + spacing
    y  <- 0.5

    ### -------- LEFT CARD --------
    grid::pushViewport(grid::viewport(x = x1, y = y,
                          width = card_width, height = card_height,
                          just = "center"))

    grid::grid.raster(ras1, width = grid::unit(1, "npc"), height = grid::unit(1, "npc"))

    if(!is.null(card_num1)) {
        grid::grid.text(
            paste0("Card number ", card_num1),
            y = grid::unit(1, "npc") + grid::unit(0.05, "npc"),
            gp = gpar(fontsize = 12, fontface = "bold")
        )
    }

    grid::grid.text(
        "Player",
        y = grid::unit(1, "npc") + grid::unit(0.12, "npc"),
        gp = grid::gpar(fontsize = 16, fontface = "bold")
    )

    grid::popViewport()

    ### -------- RIGHT CARD --------
    grid::pushViewport(grid::viewport(x = x2, y = y,
                          width = card_width, height = card_height,
                          just = "center"))

    grid::grid.raster(ras2, width = grid::unit(1, "npc"), height = grid::unit(1, "npc"))

    if(!is.null(card_num2)) {
        ggrid::rid.text(
            paste0("Card number ", card_num2),
            y = grid::unit(1, "npc") + grid::unit(0.05, "npc"),
            gp = grid::gpar(fontsize = 12, fontface = "bold")
        )
    }

    grid::grid.text(
        "Magician",
        y = grid::unit(1, "npc") + grid::unit(0.12, "npc"),
        gp = grid::gpar(fontsize = 16, fontface = "bold")
    )

    grid::popViewport()
}
