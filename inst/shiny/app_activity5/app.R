library(shiny)
library(grid)
library(magick)
library(bslib)
library(shinyjs)
library(ggplot2)

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
                fontsize = 14,
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
")),

            tags$style(HTML("

body{
    background:#F7F7FB;
}

/* =========================================
TITLE
========================================= */

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

/* =========================================
CARDS
========================================= */

.card-style{
    background:white;
    border-radius:16px;
    padding:22px;
    margin-bottom:18px;
    box-shadow:0 3px 12px rgba(0,0,0,0.08);
}

/* =========================================
BUTTONS
========================================= */

.btn{
    border-radius:10px !important;
    font-weight:600 !important;
    border:none !important;
    box-shadow:0 2px 6px rgba(0,0,0,0.08);
    transition:all 0.2s ease;
}

.btn:hover{
    transform:translateY(-1px);
    box-shadow:0 4px 10px rgba(0,0,0,0.12);
}

.btn-primary{
    background:#7B9ACC !important;
    border:none !important;
    color:white !important;
}

.activity-btn{
    padding:12px !important;
    font-size:140% !important;
    margin-top:10px;
    width:100%;
}

.btn-race-info{
    background:#5BC0DE !important;
    color:white !important;
}

.btn-race-warning{
    background:#F0AD4E !important;
    color:white !important;
}

.btn-race-success{
    background:#5CB85C !important;
    color:white !important;
}

.btn-race-danger{
    background:#D9534F !important;
    color:white !important;
}

.btn-race-info,
.btn-race-warning,
.btn-race-success,
.btn-race-danger{

    width:100%;
    padding:12px;
    font-size:140%;
    font-weight:600;

    border:none !important;
    border-radius:10px !important;

    box-shadow:0 3px 8px rgba(0,0,0,0.10);

    margin-bottom:12px;
}

.btn{
    border-radius:10px !important;
    font-weight:600 !important;
    border:none !important;
}

/* =========================================
MESSAGE PANEL
========================================= */

.message-panel{
    background:#F8F9FB;
    border-radius:14px;
    padding:20px;
    min-height:110px;
    border-left:5px solid #7B9ACC;
}

.message-text{
    font-size:20px;
    font-weight:600;
    color:#2E3440;
}

/* =========================================
CARD BADGES
========================================= */

.card-badge{
    display:inline-block;
    padding:10px 16px;
    border-radius:12px;
    background:#E8F4FD;
    color:#355070;
    font-weight:700;
    margin:4px;
}

.magician-badge{
    display:inline-block;
    padding:10px 16px;
    border-radius:12px;
    background:#D8F3DC;
    color:#1B4332;
    font-weight:700;
    margin:4px;
}

/* =========================================
INFO BOX
========================================= */

.info-box{
    background:#F8F9FB;
    padding:18px;
    border-radius:14px;
    line-height:1.7;
}

"))
        )
    ),

    # =====================================================
    # TAB 1
    # =====================================================

    nav_panel(

        "🂱 Interactive Trick",

        div(
            class = "main-title",

            h1("🂱 Interactive Trick")
        ),

        layout_sidebar(

            sidebar = div(

                class = "card-style",

                h4("Controls"),

                numericInput(
                    "picture_value",
                    "Picture card value:",
                    10,
                    1,
                    10
                ),

                numericInput(
                    "seed",
                    "Random seed:",
                    NULL,
                    1
                ),

                numericInput(
                    "pause_time",
                    "Pause between cards (seconds):",
                    0.1,
                    0.1,
                    0.1
                ),

                actionButton(
                    "start_trick",
                    "1: Shuffle and Show Key Card",
                    class = "btn-race-info"
                ),

                actionButton(
                    "shuffle_deal",
                    "2: Shuffle and Deal Cards",
                    class = "btn-race-warning"
                ),

                actionButton(
                    "reveal",
                    "3: Magician's Prediction",
                    class = "btn-race-success"
                )
            ),

            div(

                class = "card-style",

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

    # =====================================================
    # TAB 2
    # =====================================================

    nav_panel(

        "📊 Simulation Study",

        div(
            class = "main-title",

            h1("📊 Simulation Study")
        ),

        layout_sidebar(

            sidebar = div(

                class = "card-style",

                h4("Simulation Controls"),

                numericInput(
                    "sim_nrep",
                    "Number of simulations:",
                    1000,
                    100,
                    100
                ),

                numericInput(
                    "sim_picture",
                    "Picture card value:",
                    10,
                    1,
                    10
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
    ),

    # =====================================================
    # TAB 3
    # =====================================================

    nav_panel(

        "📘 Summary",

        div(
            class = "main-title",

            h1("📘 Understanding the Card Trick"),

            p(
                "Key mathematical ideas behind the magician's prediction."
            )
        ),

        fluidRow(

            column(
                6,

                div(
                    class = "card-style",

                    h3("🃏 How the Trick Works"),

                    div(
                        class = "info-box",

                        tags$p(
                            "The trick is based on deterministic counting paths."
                        ),

                        tags$p(
                            "Both the player and the magician repeatedly move through the deck according to the numerical value of the current card."
                        ),

                        tags$p(
                            "Although the starting cards may differ, the counting paths often merge, causing both players to finish on the same final card."
                        ),

                        tags$p(
                            "This creates the illusion of a successful prediction."
                        )
                    )
                )
            ),

            column(
                6,

                div(
                    class = "card-style",

                    h3("🎲 Role of Simulation"),

                    div(
                        class = "info-box",

                        tags$p(
                            "Monte Carlo simulation is used to estimate how frequently the trick fails."
                        ),

                        tags$p(
                            "By repeatedly shuffling the deck and replaying the process thousands of times, we can approximate the probability of success."
                        ),

                        tags$p(
                            "The simulation helps investigate how changing the picture-card value affects the reliability of the trick."
                        )
                    )
                )
            )
        ),

        fluidRow(

            column(
                6,

                div(
                    class = "card-style",

                    h3("📐 Mathematical Ideas"),

                    div(
                        class = "info-box",

                        tags$ul(

                            tags$li(
                                "Deterministic processes"
                            ),

                            tags$li(
                                "Random shuffling"
                            ),

                            tags$li(
                                "Monte Carlo methods"
                            ),

                            tags$li(
                                "Probability estimation"
                            ),

                            tags$li(
                                "Simulation variability"
                            ),

                            tags$li(
                                "Confidence intervals"
                            )
                        )
                    )
                )
            ),

            column(
                6,

                div(
                    class = "card-style",

                    h3("🔍 Questions to Explore"),

                    div(
                        class = "info-box",

                        tags$ul(

                            tags$li(
                                "Why do the counting paths tend to merge?"
                            ),

                            tags$li(
                                "Which picture-card value gives the most reliable trick?"
                            ),

                            tags$li(
                                "How many simulations are needed for stable estimates?"
                            ),

                            tags$li(
                                "How does randomness interact with deterministic rules?"
                            ),

                            tags$li(
                                "Can the trick be represented as a graph or network?"
                            )
                        )
                    )
                )
            )
        ),

        fluidRow(

            column(
                12,

                div(
                    class = "card-style",

                    h3("🧠 Interpretation"),

                    div(
                        class = "info-box",

                        tags$p(
                            "The card trick demonstrates an important statistical idea:"
                        ),

                        tags$blockquote(
                            style = "
                                font-size:22px;
                                font-weight:700;
                                color:#7B9ACC;
                                border-left:5px solid #CDB4DB;
                                padding-left:18px;
                                margin-top:20px;
                            ",

                            "Randomness in the setup can still produce highly predictable outcomes."
                        ),

                        tags$p(
                            "This balance between randomness and structure appears throughout probability, statistics, computer science, and machine learning."
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

    # =====================================================
    # START TRICK
    # =====================================================

    observeEvent(input$start_trick, {

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

        rv$player_card <- rv$cards_shuffled[
            sample(1:52,1),
        ]

        output$message <- renderUI({

            HTML("
<div class='message-text'>
🃏 This is your key card, unseen by the magician.
</div>
")
        })

        output$card_plot <- renderPlot({

            show_card_plot(
                rv$player_card$number,
                rv$player_card$suite
            )
        })
    })

    # =====================================================
    # DEAL LOOP
    # =====================================================

    observeEvent(input$shuffle_deal, {

        req(rv$cards_shuffled)

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

                output$message <- renderUI({

                    HTML(
                        paste0(

                            "<div class='message-text'>
                            🔮 Final prediction:
                            </div><br>",

                            badge_text(
                                rv$final_dealer_card,
                                "magician"
                            )
                        )
                    )
                })

                output$card_plot <- renderPlot({

                    show_card_plot(
                        rv$final_dealer_card$number,
                        rv$final_dealer_card$suite
                    )
                })
            }
        })
    })

    # =====================================================
    # REVEAL
    # =====================================================

    observeEvent(input$reveal, {

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

                HTML(
                    paste0(

                        "<div class='message-text'>
                        🎉 The cards match!
                        </div><br>",

                        "Player:<br>",

                        badge_text(
                            rv$final_player_card,
                            "player"
                        ),

                        "<br><br>Magician:<br>",

                        badge_text(
                            rv$final_dealer_card,
                            "magician"
                        )
                    )
                )

            } else {

                HTML(
                    paste0(

                        "<div class='message-text'>
                        No match this time.
                        </div><br>",

                        "Player:<br>",

                        badge_text(
                            rv$final_player_card,
                            "player"
                        ),

                        "<br><br>Magician:<br>",

                        badge_text(
                            rv$final_dealer_card,
                            "magician"
                        )
                    )
                )
            }
        })

        output$card_plot <- renderPlot({

            grid.newpage()

            pushViewport(
                viewport(
                    layout = grid.layout(1,2)
                )
            )

            pushViewport(
                viewport(layout.pos.col = 1)
            )

            show_card_plot(
                rv$final_player_card$number,
                rv$final_player_card$suite,
                newpage = FALSE
            )

            upViewport()

            pushViewport(
                viewport(layout.pos.col = 2)
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
                    🎲 Simulation Added
                    </div><br>",

                    "Picture value:
                    <b>",
                    input$sim_picture,
                    "</b><br>",

                    "Failure probability:
                    <b>",
                    round(res$fail_prob,6),
                    "</b><br>",

                    "SE:
                    <b>",
                    round(res$fail_prob_se,6),
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

        ggplot(
            df,
            aes(
                picture_value,
                fail_prob
            )
        ) +

            geom_line(
                color = "#7B9ACC",
                linewidth = 1.2
            ) +

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
