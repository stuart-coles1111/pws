suppressPackageStartupMessages({
    library(shiny)
    library(grid)
    library(magick)
    library(bslib)
    library(shinyjs)
    library(ggplot2)
})

# =========================================================
# Helper Functions
# =========================================================

card_filename <- function(number, suite, card_path = NULL){

    number_map <- c(
        ace = "A", two = "2", three = "3", four = "4",
        five = "5", six = "6", seven = "7", eight = "8",
        nine = "9", ten = "10", jack = "J",
        queen = "Q", king = "K"
    )

    suite_map <- c(
        hearts = "H",
        clubs = "C",
        diamonds = "D",
        spades = "S"
    )

    if(is.null(card_path)){
        card_path <- "~/pws/inst/extdata/cards"
    }

    file.path(
        path.expand(card_path),
        paste0(
            number_map[number],
            suite_map[suite],
            ".svg"
        )
    )
}

back_filename <- function(card_path = NULL){

    if(is.null(card_path)){
        card_path <- "~/pws/inst/extdata/cards"
    }

    file.path(
        path.expand(card_path),
        "back.svg"
    )
}

back_image <- function(){

    svg <- paste(
        readLines(
            back_filename(),
            warn = FALSE
        ),
        collapse = "\n"
    )

    paste0(
        "data:image/svg+xml;base64,",
        openssl::base64_encode(
            charToRaw(svg)
        )
    )
}

show_card_plot <- function(
        number,
        suite,
        card_num = NULL,
        scale = 1.6,
        newpage = TRUE
){

    if(newpage){
        grid.newpage()
    }

    img <- image_read(
        card_filename(number, suite),
        density = 300
    )

    ras <- as.raster(img)

    grid.raster(
        ras,
        width = unit(scale * 2.5, "cm"),
        height = unit(scale * 3.75, "cm")
    )

    if(!is.null(card_num)){

        grid.text(
            paste0("Card ", card_num),
            y = unit(0.95, "npc"),
            gp = gpar(
                fontsize = 25,
                fontface = "bold",
                col = "#2E3440"
            )
        )
    }
}

# =========================================================
# Trick Logic
# =========================================================

run_trick <- function(deck, start_card){

    current <- start_card
    counter <- 0

    for(i in seq_len(nrow(deck))){

        counter <- counter + 1

        if(counter == current$score){

            current <- deck[i, ]
            counter <- 0
        }
    }

    current
}

simulate_trick_once <- function(picture_value = 10){

    c_value <- c(
        "ace","two","three","four","five",
        "six","seven","eight","nine","ten",
        "jack","queen","king"
    )

    c_suite <- c(
        "hearts",
        "clubs",
        "diamonds",
        "spades"
    )

    cards <- expand.grid(
        number = c_value,
        suite = c_suite
    )

    cards$score <- 1:13

    cards$score <- ifelse(
        cards$score <= 10,
        cards$score,
        picture_value
    )

    shuffled <- cards[
        sample(1:52, 52),
    ]

    player_card <- shuffled[
        sample(1:52, 1),
    ]

    magician_card <- shuffled[1, ]

    identical(
        run_trick(shuffled, player_card),
        run_trick(shuffled, magician_card)
    )
}

activity5_trick_analysis <- function(
        nrep = 1000,
        picture_value = 10,
        seed = NULL
){

    if(!is.null(seed)){
        set.seed(seed)
    }

    results <- replicate(
        nrep,
        simulate_trick_once(picture_value)
    )

    n_correct <- sum(results)
    n_incorrect <- nrep - n_correct

    fail_prob <- n_incorrect / nrep

    fail_prob_se <- sqrt(
        fail_prob *
            (1 - fail_prob) / nrep
    )

    list(
        nrep = nrep,
        correct = n_correct,
        incorrect = n_incorrect,
        fail_prob = fail_prob,
        fail_prob_se = fail_prob_se
    )
}

# =========================================================
# UI
# =========================================================

ui <- page_navbar(

    title = "🪄 Activity 5: Statistics is Magic",

    theme = bs_theme(
        version = 5,
        bootswatch = "minty",
        primary = "#7B9ACC",
        bg = "#F7F7FB",
        fg = "#2E3440",
        base_font = font_google("Inter")
    ),

    # -------------------------
    # GLOBAL CSS (THIS IS KEY)
    # -------------------------

    header = tagList(

        useShinyjs(),

        tags$head(

            tags$script(
                src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.9.3/dist/confetti.browser.min.js"
            ),


            tags$script(HTML("

Shiny.addCustomMessageHandler('trigger_confetti', function(message) {
  confetti({ particleCount: 120, spread: 70, origin: { x: 0.2, y: 0.6 }});
  confetti({ particleCount: 120, spread: 70, origin: { x: 0.8, y: 0.6 }});
  confetti({ particleCount: 200, spread: 100, origin: { y: 0.5 }});
});


$(document).on('click','.select-card',function(){

    $('.select-card').removeClass('selected');

    $(this).addClass('selected');

    Shiny.setInputValue(
        'selected_card',
        $(this).data('card'),
        {priority:'event'}
    );

});


$(document).on('shiny:connected', function(){

    $('.accordion .accordion-collapse')
        .removeClass('show');

    $('.accordion .accordion-button')
        .addClass('collapsed')
        .attr('aria-expanded','false');

});


")),


            tags$style(HTML("

body{
    background:#F7F7FB;
}


.main-title{

    background:linear-gradient(90deg,#A8DADC,#CDB4DB);

    padding:20px;

    border-radius:16px;

    text-align:center;

    margin-bottom:20px;

}


.main-title h1{

    font-weight:800;

    color:#2E3440;

    margin-bottom:8px;

}


.main-title p{

    font-size:17px;

    color:#4C566A;

    margin:0;

}


.card-style{

    background:white;

    border-radius:16px;

    padding:22px;

    margin-bottom:18px;

    box-shadow:0 3px 12px rgba(0,0,0,0.08);

}


.message-text{

    background:linear-gradient(
        90deg,
        #F8F9FA,
        #EEF2FF
    );

    border-left:6px solid #7B9ACC;

    border-radius:12px;

    padding:16px 20px;

    margin-top:15px;

font-family:'Inter', sans-serif;

    font-size:18px;

    font-weight:500;

    color:#2E3440;

    line-height:1.5;

    box-shadow:0 3px 10px rgba(0,0,0,0.08);

}


.button-stack {

    display:flex;

    flex-direction:column;

    gap:12px;

}


.btn-race-info{

    background:#4C78A8 !important;

    color:white !important;

}


.btn-race-warning{

    background:#F58518 !important;

    color:white !important;

}


.btn-race-success{

    background:#54A24B !important;

    color:white !important;

}


.btn-race-danger{

    background:#E45756 !important;

    color:white !important;

}


.btn-race-info,
.btn-race-warning,
.btn-race-success,
.btn-race-danger {

    width:100%;

    padding:14px;

    font-size:16px;

    font-weight:700;

    border-radius:12px !important;

    box-shadow:0 3px 8px rgba(0,0,0,0.12);

}


button:disabled {

    opacity:0.45;

    cursor:not-allowed;

}


.card-selection-container {

    display:flex;

    flex-wrap:wrap;

    justify-content:center;

    gap:8px;

    padding:20px;

}


.select-card {

    width:55px;

    height:80px;

    border-radius:8px;

    overflow:hidden;

    border:2px solid white;

    box-shadow:0 3px 6px rgba(0,0,0,0.25);

    cursor:pointer;

    transition:all 0.25s ease;

}


.card-back {

    width:55px;

    height:80px;

    display:block;

}


.select-card:hover {

    transform:translateY(-10px) scale(1.08);

    box-shadow:0 8px 12px rgba(0,0,0,0.3);

}


.select-card.selected {

    transform:translateY(-12px) scale(1.12);

    border:3px solid #E76F51;

}


        "))

        ),


        div(
            class = "main-title",
            h1("🪄 Activity 5: Statistics is Magic")
        )

    ),




    # =====================================================
    # OVERVIEW TAB
    # =====================================================



    overview_page(
        explanation = tagList(

            p("This activity is based on a card trick that has surprising connections to probability and statistics."),

            p("You choose a card at random, hidden from the magician, and then follow a simple rule to generate a sequence of cards as the magician turns the deck over."),

            p("Once all the cards have been revealed, the magician attempts to predict the final card in your sequence.")
        ),

        individual = tagList(

            tags$ol(
                tags$li("Follow the card trick and observe the magician's prediction."),
                tags$li("Try to work out how the trick works, and why it sometimes fails."),
                tags$li("Repeat the experiment with different picture card values."),
                tags$li("Use the simulation mode to explore how the probability of failure depends on the picture card value.")
            )
        ),

        group = tagList(

            p("The activity can be adapted in several ways for group discussion:"),

            tags$ol(
                tags$li("Participants could follow the individual instructions, followed by a group discussion of their observations."),

                tags$li("Using physical cards instead of the app simulation, participants could be divided into magicians and players. Only the magicians are told how the trick works. The trick is then performed in pairs, followed by group discussion."),

                tags$li("The group could observe a single magician-player pair performing the trick, followed by discussion of how and why it works.")
            )

        ),
        question = tagList(

            tags$ul(
                tags$li("How does the trick work?"),
                tags$li("Why does the trick failure probability depend on the value assigned to picture cards?"),
                tags$li("What does simulation reveal that a single performance of the trick cannot?"),
                tags$li("How does the number of simulations affect the reliability of the estimated failure probability?")
            )
        )
    ),

    # =====================================================
    # ACTIVITY TAB (WRAPPER ONLY)
    # =====================================================

    nav_panel(

        "Activity",

        # IMPORTANT: NO OUTER layout_sidebar HERE

        navset_tab(

            # =================================================
            # TAB 1: CARD TRICK
            # =================================================

            nav_panel(

                "A Card Trick",

                div(class = "main-title", h1("A Card Trick")),

                layout_sidebar(

                    sidebar = div(

                        class = "card-style",

                        h4("Controls"),

                        numericInput(
                            "seed",
                            "Random seed:",
                            NULL,
                            1
                        ),

                        sliderInput(
                            "picture_value",
                            "Picture card value:",
                            min = 1,
                            max = 10,
                            value = 10
                        ),

                        sliderInput(
                            "pause_time",
                            "Pause between cards (seconds):",
                            0.1,
                            2,
                            0.75
                        ),

                        div(
                            class = "button-stack",

                            actionButton(
                                "start_trick",
                                "1: Choose your initial sequence card",
                                class = "btn-race-info"
                            ),

                            actionButton(
                                "shuffle_deal",
                                "2: Run the trick",
                                class = "btn-race-warning"
                            ),

                            actionButton(
                                "check_card",
                                "3: Check your final sequence card",
                                class = "btn-race-info"
                            ),

                            actionButton(
                                "reveal",
                                "4: Reveal magician's prediction",
                                class = "btn-race-success"
                            ),

                            actionButton(
                                "repeat_trick",
                                "Repeat trick",
                                class = "btn-race-warning"
                            )
                        )
                    ),


                    div(

                        class = "card-style",

                        bslib::accordion(

                            bslib::accordion_panel(

                                id = "trick_rules",
                                open = NULL,

                                "📖 Instructions for the trick",

                                p(
                                    "This activity involves two participants: a Magician and a Player.
                        The Player selects a hidden card, and the Magician attempts to
                        predict a final card determined by a simple counting rule."
                                ),

                                tags$ol(

                                    tags$li(
                                        "The Player selects a card at random and remembers it.
                            The card is then returned to the deck and the Magician shuffles."
                                    ),

                                    tags$li(
                                        "The Magician reveals the cards one at a time.
                            The Player uses their original card to determine a sequence
                            of cards within the deck."
                                    ),

                                    tags$li(
                                        "For example, if the Player's hidden card is the 4 of Diamonds,
                            they note the 4th card revealed. If this card is the 8 of Diamonds,
                            they then count forward 8 cards to find the next card."
                                    ),

                                    tags$li(
                                        "The process continues using the value of each selected card.
                            Picture cards (J, Q, K) have value 10."
                                    ),

                                    tags$li(
                                        "The final card reached before the deck runs out is the Player's
                            Magic Card."
                                    )
                                ),

                                p(
                                    "The Magician's challenge is to predict this Magic Card without
                        knowing the Player's original card."
                                )
                            )
                        )
                    ),


                    div(

                        class = "card-style",

                        uiOutput("card_selector"),

                        plotOutput(
                            "card_plot",
                            height = "420px"
                        ),

                        div(
                            class = "message-panel",
                            uiOutput("message")
                        )
                    )
                )
            ),

            # =================================================
            # TAB 2: SIMULATION
            # =================================================

            nav_panel(

                "Simulation Study",

                div(class = "main-title", h1("Simulation Study")),

                layout_sidebar(

                    sidebar = div(

                        class = "card-style",

                        h4("Simulation Controls"),

                        sliderInput(
                            "sim_nrep",
                            "Number of simulations:",
                            min = 100,
                            max = 5000,
                            value = 1000,
                            step = 100
                        ),

                        sliderInput(
                            "sim_picture",
                            "Picture card value:",
                            min = 1,
                            max = 10,
                            value = 10
                        ),

                        numericInput(
                            "sim_seed",
                            "Random seed:",
                            NULL,
                            1
                        ),

                        actionButton(
                            "run_simulation",
                            "Run Simulation",
                            class = "btn-race-danger"
                        )

                    ),

                    div(

                        class = "card-style",

                        bslib::accordion(

                            open = "📖 About the simulation",

                            bslib::accordion_panel(

                                "📖 About the simulation",

                                p(
                                    "In this simulation study, the card trick is repeated many times,
                        with the value assigned to picture cards specified in the sidebar."
                                ),

                                p(
                                    "The simulations are used to estimate the probability that the trick
                        fails, together with a 95% confidence interval for this probability."
                                ),

                                p(
                                    "By repeating the simulations for different picture card values and
                        plotting the estimated failure probabilities with their confidence
                        intervals, we can explore the relationship between the rules of the
                        trick and its reliability."
                                )
                            )
                        )

                    ),

                    div(

                        class = "card-style",

                        h3("Simulation Results"),

                        div(
                            class = "message-panel",
                            uiOutput("simulation_summary")
                        ),

                        hr(),

                        tableOutput("simulation_table"),

                        fluidRow(

                            column(
                                6,
                                plotOutput(
                                    "simulation_plot",
                                    height = "350px"
                                )
                            ),

                            column(
                                6,
                                plotOutput(
                                    "simulation_plot2",
                                    height = "350px"
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session){

    disable("shuffle_deal")
    disable("check_card")
    disable("reveal")
    disable("repeat_trick")

    observe({

        updateNumericInput(
            session,
            "seed",
            value = sample(1:999,1)
        )

        updateNumericInput(
            session,
            "sim_seed",
            value = sample(1:999,1)
        )
    })

    rv <- reactiveValues(

        cards_shuffled = NULL,
        player_card = NULL,
        dealer_card = NULL,

        selected_index = NULL,

        player_checked = FALSE,
        show_selector = TRUE,
        selecting_card = FALSE,

        final_player_card = NULL,
        final_dealer_card = NULL,

        count = 0,
        dealer_count = -1,

        i = 0,
        dealing = FALSE,

        sim_history = data.frame(
            picture_value = numeric(),
            fail_prob = numeric(),
            se = numeric()
        )
    )

    badge_text <- function(
        card,
        type = c("player","magician")
    ){

        cls <- if(type == "player"){
            "card-badge"
        } else {
            "magician-badge"
        }

        paste0(
            '<div class="',
            cls,
            '">',
            toupper(card$number),
            ' of ',
            toupper(card$suite),
            '</div>'
        )
    }

    output$card_selector <- renderUI({

        req(rv$selecting_card)

        if(!rv$show_selector){
            return(NULL)
        }

        div(
            class = "card-selection-container",

            lapply(1:52, function(i){

                tags$div(
                    class = "select-card",
                    `data-card` = i,

                    tags$img(
                        src = back_image(),
                        class = "card-back"
                    )
                )

            })
        )
    })
    # =====================================================
    # START TRICK
    # =====================================================

    observeEvent(input$start_trick, {

        rv$player_checked <- FALSE
        rv$show_selector <- TRUE
        rv$selecting_card <- TRUE

        disable("check_card")
        disable("reveal")

        set.seed(input$seed)

        c_value <- c(
            "ace","two","three","four","five",
            "six","seven","eight","nine","ten",
            "jack","queen","king"
        )

        c_suite <- c(
            "hearts",
            "clubs",
            "diamonds",
            "spades"
        )

        cards <- expand.grid(
            number = c_value,
            suite = c_suite
        )

        cards$score <- 1:13

        cards$score <- ifelse(
            cards$score <= 10,
            cards$score,
            input$picture_value
        )

        rv$cards_shuffled <- cards[
            sample(1:52),
        ]

        rv$player_card <- NULL

        output$message <- renderUI({

            HTML("
<div class='message-text'>
🃏 This is your key card, unseen by the magician.
</div>
")
        })

        output$card_plot <- renderPlot({

            grid.newpage()

            grid.text(
                "Choose a card from the deck above",
                gp = gpar(
                    fontsize = 22,
                    fontface = "bold",
                    col = "#2E3440"
                )
            )

        })
    })

    # =====================================================
    # DEAL LOOP
    # =====================================================

    observeEvent(input$shuffle_deal, {

        req(rv$cards_shuffled)

        disable("shuffle_deal")

        rv$dealing <- TRUE

        rv$i <- 0
        rv$count <- 0
        rv$dealer_count <- -1

        rv$dealer_card <- rv$cards_shuffled[1, ]
    })

    observe({

        req(rv$dealing)

        invalidateLater(
            input$pause_time * 1000
        )

        isolate({

            if(rv$i < 52){

                rv$i <- rv$i + 1

                card <- rv$cards_shuffled[
                    rv$i,
                ]

                rv$dealer_count <- rv$dealer_count + 1

                if(rv$dealer_count == rv$dealer_card$score){

                    rv$dealer_card <- card
                    rv$dealer_count <- 0
                }

                rv$count <- rv$count + 1

                if(rv$count == rv$player_card$score){

                    rv$player_card <- card
                    rv$count <- 0
                }

                rv$final_player_card <- rv$player_card
                rv$final_dealer_card <- rv$dealer_card

                output$message <- renderUI({

                    HTML("
<div class='message-text'>
✨ Dealing cards...
</div>
")
                })

                output$card_plot <- renderPlot({

                    show_card_plot(
                        card$number,
                        card$suite,
                        rv$i
                    )
                })

            } else {

                rv$dealing <- FALSE

                disable("shuffle_deal")
                enable("check_card")

                output$message <- renderUI({

                    HTML("
        <div class='message-text'>
        ✨ The dealing sequence is complete.<br>
        Check your final card in hidden sequence when ready.
        </div>
        ")

                })

            }
        })
    })

    # =====================================================
    # CHECK PLAYER CARD
    # =====================================================

    observeEvent(input$check_card, {

        req(rv$final_player_card)

        rv$player_checked <- TRUE

        enable("reveal")
        disable("check_card")

        output$message <- renderUI({

            HTML(
                paste0(

                    "<div class='message-text'>
                🃏 Your final card is:
                </div><br>",

                    badge_text(
                        rv$final_player_card,
                        "player"
                    )
                )
            )

        })

        output$card_plot <- renderPlot({

            show_card_plot(
                rv$final_player_card$number,
                rv$final_player_card$suite
            )

        })

    })

    # =====================================================
    # REVEAL
    # =====================================================

    observeEvent(input$reveal, {

        req(rv$player_checked)

        req(
            rv$final_player_card,
            rv$final_dealer_card
        )

        is_match <- identical(
            rv$final_player_card,
            rv$final_dealer_card
        )

        if(is_match){

            session$sendCustomMessage(
                "trigger_confetti",
                list()
            )
        }


        output$message <- renderUI({

            if(is_match){

                HTML("
        <div class='message-text'>
        🎉 The cards match!
        </div>
        ")

            } else {

                HTML("
        <div class='message-text'>
        No match this time.
        </div>
        ")

            }

        })

        output$card_plot <- renderPlot({

            grid.newpage()

            pushViewport(
                viewport(
                    layout = grid.layout(
                        2,
                        2,
                        heights = unit(
                            c(0.15,0.85),
                            "npc"
                        )
                    )
                )
            )

            pushViewport(
                viewport(layout.pos.row = 1, layout.pos.col = 1)
            )

            grid.text(
                "Your card",
                gp=gpar(
                    fontsize=18,
                    fontface="bold"
                )
            )

            upViewport()


            pushViewport(
                viewport(layout.pos.row = 2, layout.pos.col = 1)
            )

            show_card_plot(
                rv$final_player_card$number,
                rv$final_player_card$suite,
                newpage = FALSE
            )

            upViewport()

            pushViewport(
                viewport(layout.pos.row = 1, layout.pos.col = 2)
            )

            grid.text(
                "Magician's prediction",
                gp=gpar(
                    fontsize=18,
                    fontface="bold"
                )
            )

            upViewport()


            pushViewport(
                viewport(layout.pos.row = 2, layout.pos.col = 2)
            )

            show_card_plot(
                rv$final_dealer_card$number,
                rv$final_dealer_card$suite,
                newpage = FALSE
            )

            upViewport()
        })

        updateNumericInput(
            session,
            "seed",
            value = sample(1:999,1)
        )

        disable("reveal")
        enable("repeat_trick")
    })


    observeEvent(input$selected_card, {

        req(rv$cards_shuffled)

        rv$selected_index <- as.numeric(input$selected_card)

        rv$player_card <- rv$cards_shuffled[
            rv$selected_index,
        ]

        disable("start_trick")
        enable("shuffle_deal")

        rv$show_selector <- FALSE
        rv$selecting_card <- FALSE

        output$card_plot <- renderPlot({

            show_card_plot(
                rv$player_card$number,
                rv$player_card$suite
            )

        })

        output$message <- renderUI({

            HTML("
        <div class='message-text'>
        🃏 Your initial sequence card has been selected.<br>
        This card is hidden from the magician.
        </div>
        ")

        })

        enable("shuffle_deal")

    })

    observeEvent(input$repeat_trick, {

        rv$cards_shuffled <- NULL
        rv$player_card <- NULL
        rv$dealer_card <- NULL

        rv$final_player_card <- NULL
        rv$final_dealer_card <- NULL

        rv$player_checked <- FALSE
        rv$selecting_card <- FALSE

        rv$count <- 0
        rv$dealer_count <- -1
        rv$i <- 0
        rv$dealing <- FALSE

        disable("shuffle_deal")
        disable("check_card")
        disable("reveal")

        enable("start_trick")
        disable("repeat_trick")


        output$message <- renderUI({

            HTML("
        <div class='message-text'>
        Ready for a new trick.
        </div>
        ")

        })

        output$card_plot <- renderPlot({

            grid.newpage()

            grid.text(
                "Click 'Choose the hidden card' to begin",
                gp=gpar(
                    fontsize=22,
                    fontface="bold"
                )
            )

        })

    })

    # =====================================================
    # SIMULATION
    # =====================================================

    observeEvent(input$run_simulation, {

        if(input$sim_picture %in%
           rv$sim_history$picture_value){

            showNotification(
                paste0(
                    "Picture value ",
                    input$sim_picture,
                    " already simulated."
                ),
                type = "warning"
            )

            return(NULL)
        }

        set.seed(input$sim_seed)

        res <- activity5_trick_analysis(
            nrep = input$sim_nrep,
            picture_value = input$sim_picture
        )

        rv$sim_history <- rbind(

            rv$sim_history,

            data.frame(
                picture_value = input$sim_picture,
                fail_prob = res$fail_prob,
                se = res$fail_prob_se
            )
        )

        output$simulation_summary <- renderUI({

            HTML(
                paste0(

                    "<div class='message-text'>
                    Simulation Added
                    </div><br>",

                    "Picture value:
                    <b>",
                    input$sim_picture,
                    "</b><br>",

                    "Failure probability:
                    <b>",
                    round(res$fail_prob,3),
                    "</b><br>",

                    "SE:
                    <b>",
                    round(res$fail_prob_se,3),
                    "</b>"
                )
            )
        })

        output$simulation_table <- renderTable({

            data.frame(
                Outcome = c(
                    "Correct",
                    "Incorrect"
                ),

                Count = c(
                    res$correct,
                    res$incorrect
                )
            )
        })

        updateNumericInput(
            session,
            "sim_seed",
            value = sample(1:999,1)
        )
    })

    # =====================================================
    # SIMULATION PLOT 1
    # =====================================================

    output$simulation_plot <- renderPlot({

        req(nrow(rv$sim_history) > 0)

        latest <- tail(rv$sim_history,1)

        ggplot(

            data.frame(
                outcome = c("Correct","Incorrect"),
                count = c(
                    1 - latest$fail_prob,
                    latest$fail_prob
                )
            ),

            aes(outcome, count, fill = outcome)

        ) +

            geom_col(width = 0.6) +

            scale_fill_manual(
                values = c(
                    "#95D5B2",
                    "#E76F51"
                )
            ) +

            theme_minimal(base_size = 14) +

            theme(
                legend.position = "none"
            ) +

            ylab("Probability") +
            xlab(NULL)
    })

    # =====================================================
    # SIMULATION PLOT 2
    # =====================================================

    output$simulation_plot2 <- renderPlot({

        req(nrow(rv$sim_history) > 0)

        df <- rv$sim_history

            ggplot(df, aes(picture_value, fail_prob)) +

        {if (nrow(df) > 1)
            geom_line(
                color = "#7B9ACC",
                linewidth = 1.2
            )}+

            geom_point(
                color = "#CDB4DB",
                size = 4
            ) +

            geom_errorbar(
                aes(
                    ymin = fail_prob - 1.96 * se,
                    ymax = fail_prob + 1.96 * se
                ),
                width = 0.15,
                color = "#7B9ACC"
            ) +

            theme_minimal(base_size = 14) +

            labs(
                x = "Picture Card Value",
                y = "Failure Probability"
            )
    })
}

# =========================================================
# APP
# =========================================================

shinyApp(ui, server)
