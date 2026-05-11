library(shiny)
library(grid)
library(magick)
library(later)   # for scheduling delayed execution

# ----------------------------
# Helper functions
# ----------------------------
card_filename <- function(number, suite, card_path = NULL) {
    number_map <- c(
        ace = "A", two = "2", three = "3", four = "4", five = "5",
        six = "6", seven = "7", eight = "8", nine = "9", ten = "10",
        jack = "J", queen = "Q", king = "K"
    )
    suite_map <- c(hearts="H", clubs="C", diamonds="D", spades="S")

    if (is.null(card_path)) {
        card_path <- "~/pws/inst/extdata/cards"
    }
    card_path <- path.expand(card_path)

    file.path(card_path, paste0(number_map[number], suite_map[suite], ".svg"))
}

show_card_plot <- function(number, suite, card_num = NULL, scale = 1.5) {
    file <- card_filename(number, suite)
    img <- image_read(file, density = 300)
    ras <- as.raster(img)
    grid.newpage()
    grid.raster(ras, width = unit(scale*2.5,"cm"), height = unit(scale*3.75,"cm"))

    if(!is.null(card_num)) {
        grid.text(paste0("Card number ", card_num),
                  y = unit(1, "npc") - unit(0.05, "npc"),
                  gp = gpar(fontsize = 14, fontface = "bold"))
    }
}

# ----------------------------
# UI
# ----------------------------
ui <- fluidPage(
    titlePanel("Magician Card Trick"),
    sidebarLayout(
        sidebarPanel(
            numericInput("picture_value", "Picture card value:", value = 10, min = 1, max = 20),
            numericInput("seed", "Random seed:", value = 123, min = 1, step = 1),
            numericInput("pause_time", "Pause between cards (seconds):", value = 1, min = 0.1, step = 0.1),
            actionButton("start_trick", "Start Trick")
        ),
        mainPanel(
            plotOutput("card_plot"),
            verbatimTextOutput("message")
        )
    )
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {

    rv <- reactiveValues(
        cards_shuffled = NULL,
        player_card = NULL,
        dealer_card = NULL,
        count = 0,
        dealer_count = -1,
        i = 0,
        dealing = FALSE
    )

    # Step 1: Show the key card
    observeEvent(input$start_trick, {
        if(!is.null(input$seed)) set.seed(input$seed)

        # Build deck
        c_value <- c("ace","two","three","four","five",
                     "six","seven","eight","nine","ten",
                     "jack","queen","king")
        c_suite <- c("hearts","clubs","diamonds","spades")

        cards <- expand.grid(number=c_value, suite=c_suite, stringsAsFactors = FALSE)
        cards$score <- 1:13
        cards$score <- ifelse(cards$score <= 10, cards$score, input$picture_value)

        rv$cards_shuffled <- cards[sample(1:52, 52), ]
        rv$player_card <- rv$cards_shuffled[sample(1:52,1), ]

        # Show key card
        output$message <- renderText({
            paste("The Magician has shuffled the pack.\n\n",
                  "This is your key card (hidden from the Magician).")
        })

        output$card_plot <- renderPlot({
            show_card_plot(rv$player_card$number, rv$player_card$suite)
        })

        # Step 2: Start automatic dealing after 2 seconds
        later(function() {
            isolate({
                rv$dealing <- TRUE
                rv$dealer_card <- rv$cards_shuffled[1, ]
                rv$i <- 0
                rv$count <- 0
                rv$dealer_count <- -1
            })
        }, 2)
    })

    # Step 3: Automatic dealing
    observe({
        req(rv$dealing)
        invalidateLater(input$pause_time*1000)

        isolate({
            if(rv$i < 52){
                rv$i <- rv$i + 1
                card <- rv$cards_shuffled[rv$i, ]

                rv$count <- rv$count + 1
                rv$dealer_count <- rv$dealer_count + 1

                if(rv$dealer_count == rv$dealer_card$score){
                    rv$dealer_card <- card
                    rv$dealer_count <- 0
                }
                if(rv$count == rv$player_card$score){
                    rv$player_card <- card
                    rv$count <- 0
                }

                output$card_plot <- renderPlot({
                    show_card_plot(card$number, card$suite, card_num = rv$i)
                })

                output$message <- renderText("The Magician is dealing the cards...")

            } else {
                # All cards dealt, show prediction
                rv$dealing <- FALSE
                output$message <- renderText({
                    paste("*** The Magician predicts your final card is the ",
                          rv$dealer_card$number, " of ", rv$dealer_card$suite, " ***")
                })
                output$card_plot <- renderPlot({
                    show_card_plot(rv$dealer_card$number, rv$dealer_card$suite)
                    grid.text("Magician's Prediction",
                              y = unit(1, "npc") - unit(0.03, "npc"),
                              gp = gpar(fontsize = 20, fontface = "bold"))
                })
            }
        })
    })
}

# ----------------------------
# Run the app
# ----------------------------
shinyApp(ui, server)
