library(shiny)
library(grid)
library(magick)
library(bslib)
library(shinyjs)

# =========================================================
# Helper Functions
# =========================================================

card_filename <- function(number, suite, card_path = NULL) {

    number_map <- c(
        ace = "A", two = "2", three = "3", four = "4", five = "5",
        six = "6", seven = "7", eight = "8", nine = "9", ten = "10",
        jack = "J", queen = "Q", king = "K"
    )

    suite_map <- c(
        hearts = "H",
        clubs = "C",
        diamonds = "D",
        spades = "S"
    )

    if (is.null(card_path)) {
        card_path <- "~/pws/inst/extdata/cards"
    }

    file.path(
        path.expand(card_path),
        paste0(number_map[number], suite_map[suite], ".svg")
    )
}

show_card_plot <- function(number, suite, card_num = NULL,
                           scale = 1.6, newpage = TRUE) {

    if (newpage) grid.newpage()

    img <- image_read(card_filename(number, suite), density = 300)
    ras <- as.raster(img)

    grid.raster(
        ras,
        width = unit(scale * 2.5, "cm"),
        height = unit(scale * 3.75, "cm")
    )

    if (!is.null(card_num)) {
        grid.text(
            paste0("Card ", card_num),
            y = unit(0.95, "npc"),
            gp = gpar(fontsize = 14,
                      fontface = "bold",
                      col = "#3D405B")
        )
    }
}

# =========================================================
# Trick Logic
# =========================================================

run_trick <- function(deck, start_card) {

    current <- start_card
    counter <- 0

    for (i in seq_len(nrow(deck))) {
        counter <- counter + 1

        if (counter == current$score) {
            current <- deck[i, ]
            counter <- 0
        }
    }

    current
}

simulate_trick_once <- function(picture_value = 10) {

    c_value <- c("ace","two","three","four","five",
                 "six","seven","eight","nine","ten",
                 "jack","queen","king")

    c_suite <- c("hearts","clubs","diamonds","spades")

    cards <- expand.grid(number = c_value, suite = c_suite)

    cards$score <- 1:13
    cards$score <- ifelse(cards$score <= 10, cards$score, picture_value)

    shuffled <- cards[sample(1:52, 52), ]

    player_card <- shuffled[sample(1:52, 1), ]
    magician_card <- shuffled[1, ]

    identical(
        run_trick(shuffled, player_card),
        run_trick(shuffled, magician_card)
    )
}

activity5_trick_analysis <- function(nrep = 1000,
                                     picture_value = 10,
                                     seed = NULL){

    if (!is.null(seed)) set.seed(seed)

    results <- replicate(nrep, simulate_trick_once(picture_value))

    n_correct <- sum(results)
    n_incorrect <- nrep - n_correct

    fail_prob <- n_incorrect / nrep
    fail_prob_se <- sqrt(fail_prob * (1 - fail_prob) / nrep)

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

    title = "🪄 Card Trick Explorer",

    useShinyjs(),

    theme = bs_theme(
        version = 5,
        bootswatch = "lux",
        primary = "#7B9ACC",
        bg = "#F6F8FC",
        fg = "#2E3440",
        base_font = font_google("Inter")
    ),

    header = tags$head(

        tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.9.3/dist/confetti.browser.min.js"),

        tags$script(HTML("
Shiny.addCustomMessageHandler('trigger_confetti', function(message) {
  confetti({ particleCount: 120, spread: 70, origin: { x: 0.2, y: 0.6 }});
  confetti({ particleCount: 120, spread: 70, origin: { x: 0.8, y: 0.6 }});
  confetti({ particleCount: 200, spread: 100, origin: { y: 0.5 }});
});
")),

        tags$style(HTML("
body{background:#F6F8FC;}
.main-title{background:linear-gradient(90deg,#A8DADC,#CDB4DB);padding:28px;border-radius:18px;margin-bottom:25px;text-align:center;box-shadow:0 4px 12px rgba(0,0,0,0.08);}
.main-title h1{font-weight:700;color:#1D3557;margin-bottom:10px;}
.main-title p{font-size:18px;color:#3D405B;margin:0;}
.card-style{background:white;border-radius:18px;padding:24px;margin-bottom:20px;box-shadow:0 4px 14px rgba(0,0,0,0.08);}
.btn-primary{background:#89C2D9!important;border-color:#89C2D9!important;font-weight:600!important;font-size:16px!important;border-radius:12px!important;padding:10px 18px!important;margin-top:10px;width:100%;}
.message-panel{background:linear-gradient(135deg,#FFFFFF,#F8FBFF);border-left:6px solid #89C2D9;padding:20px 24px;border-radius:16px;margin-top:15px;min-height:100px;}
.message-text{font-size:20px;font-weight:500;color:#2E4057;}
.card-badge{display:inline-block;padding:10px 16px;border-radius:14px;background:#F9E79F;color:#5C4B00;font-weight:bold;margin:4px;}
.magician-badge{display:inline-block;padding:10px 16px;border-radius:14px;background:#95D5B2;color:#1B4332;font-weight:bold;margin:4px;}
"))
    ),

    nav_panel(
        "🪄 Interactive Trick",
        div(class="main-title",
            h1("🪄 The Magician's Card Prediction"),
            p("A mathematical card mystery powered by deterministic paths.")
        ),
        layout_sidebar(
            sidebar = div(class="card-style",
                          h4("Controls"),
                          numericInput("picture_value","Picture card value:",10,1,10),
                          numericInput("seed","Random seed:",NULL,1),
                          numericInput("pause_time","Pause between cards (seconds):",0.1,0.1,0.1),
                          actionButton("start_trick","1: Show Key Card",class="btn-primary"),
                          actionButton("shuffle_deal","2: Shuffle and Deal Cards",class="btn-primary"),
                          actionButton("reveal","3: Reveal Cards",class="btn-primary")
            ),
            div(class="card-style",
                plotOutput("card_plot", height="420px"),
                div(class="message-panel", uiOutput("message"))
            )
        )
    ),

    nav_panel(
        "📊 Simulation Study",
        div(class="main-title",
            h1("📊 Monte Carlo Investigation"),
            p("Investigate how often the card trick succeeds.")
        ),
        layout_sidebar(
            sidebar = div(class="card-style",
                          h4("Simulation Controls"),
                          numericInput("sim_nrep","Number of simulations:",1000,100,100),
                          numericInput("sim_picture","Picture card value:",10,1,10),
                          numericInput("sim_seed","Random seed:",NULL,1),
                          actionButton("run_simulation","Run Simulation",class="btn-primary")
            ),
            div(class="card-style",
                h3("Simulation Results"),
                div(class="message-panel", uiOutput("simulation_summary")),
                tags$div(style="margin:18px 0;", tags$hr()),
                tableOutput("simulation_table"),
                fluidRow(
                    column(6, plotOutput("simulation_plot", height="350px")),
                    column(6, plotOutput("simulation_plot2", height="350px"))
                )
            )
        )
    )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session) {

    observe({
        updateNumericInput(session, "seed", value = sample(1:999, 1))
        updateNumericInput(session, "sim_seed", value = sample(1:999, 1))
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

    badge_text <- function(card, type = c("player","magician")) {
        cls <- if (type == "player") "card-badge" else "magician-badge"
        paste0('<div class="', cls, '">',
               toupper(card$number),' of ',toupper(card$suite),
               '</div>')
    }

    observeEvent(input$start_trick, {

        set.seed(input$seed)

        c_value <- c("ace","two","three","four","five",
                     "six","seven","eight","nine","ten",
                     "jack","queen","king")

        c_suite <- c("hearts","clubs","diamonds","spades")

        cards <- expand.grid(number = c_value, suite = c_suite)
        cards$score <- 1:13
        cards$score <- ifelse(cards$score <= 10,
                              cards$score,
                              input$picture_value)

        rv$cards_shuffled <- cards[sample(1:52), ]
        rv$player_card <- rv$cards_shuffled[sample(1:52,1), ]

        output$message <- renderUI({
            HTML("<div class='message-text'>🃏 This is your key card</div>")
        })

        output$card_plot <- renderPlot({
            show_card_plot(rv$player_card$number,
                           rv$player_card$suite)
        })
    })

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
        invalidateLater(input$pause_time * 1000)

        isolate({

            if (rv$i < 52) {

                rv$i <- rv$i + 1
                card <- rv$cards_shuffled[rv$i, ]

                rv$dealer_count <- rv$dealer_count + 1
                if (rv$dealer_count == rv$dealer_card$score) {
                    rv$dealer_card <- card
                    rv$dealer_count <- 0
                }

                rv$count <- rv$count + 1
                if (rv$count == rv$player_card$score) {
                    rv$player_card <- card
                    rv$count <- 0
                }

                rv$final_player_card <- rv$player_card
                rv$final_dealer_card <- rv$dealer_card

                output$message <- renderUI({
                    HTML("<div class='message-text'>✨ Dealing cards...</div>")
                })

                output$card_plot <- renderPlot({
                    show_card_plot(card$number, card$suite, rv$i)
                })

            } else {

                rv$dealing <- FALSE

                output$message <- renderUI({
                    HTML(paste0(
                        "<div class='message-text'>🔮 Final prediction:</div><br>",
                        badge_text(rv$final_dealer_card, "magician")
                    ))
                })

                output$card_plot <- renderPlot({
                    show_card_plot(rv$final_dealer_card$number,
                                   rv$final_dealer_card$suite)
                })
            }
        })
    })

    observeEvent(input$reveal, {

        req(rv$final_player_card, rv$final_dealer_card)

        is_match <- identical(rv$final_player_card,
                              rv$final_dealer_card)

        if (is_match) {
            session$sendCustomMessage("trigger_confetti", list())
        }

        output$message <- renderUI({
            if (is_match) {
                HTML(paste0(
                    "<div class='message-text'>🎉 The cards match!</div><br>",
                    "Player:<br>", badge_text(rv$final_player_card,"player"),
                    "<br><br>Magician:<br>", badge_text(rv$final_dealer_card,"magician")
                ))
            } else {
                HTML(paste0(
                    "<div class='message-text'>No match this time.</div><br>",
                    "Player:<br>", badge_text(rv$final_player_card,"player"),
                    "<br><br>Magician:<br>", badge_text(rv$final_dealer_card,"magician")
                ))
            }
        })

        output$card_plot <- renderPlot({

            grid.newpage()
            pushViewport(viewport(layout = grid.layout(1,2)))

            pushViewport(viewport(layout.pos.col = 1))
            show_card_plot(rv$final_player_card$number,
                           rv$final_player_card$suite,
                           newpage = FALSE)
            upViewport()

            pushViewport(viewport(layout.pos.col = 2))
            show_card_plot(rv$final_dealer_card$number,
                           rv$final_dealer_card$suite,
                           newpage = FALSE)
            upViewport()
        })

        updateNumericInput(session, "seed", value = sample(1:999, 1))
    })

    # =========================================================
    # UPDATED SIMULATION (BLOCK DUPLICATES)
    # =========================================================

    observeEvent(input$run_simulation, {

        if (input$sim_picture %in% rv$sim_history$picture_value) {

            showNotification(
                paste0("Picture value ", input$sim_picture,
                       " already simulated. Choose a new value."),
                type = "warning"
            )

            output$simulation_summary <- renderUI({
                HTML(paste0(
                    "<div class='message-text'>⚠️ Simulation blocked</div><br>",
                    "Already used value: <b>", input$sim_picture, "</b>"
                ))
            })

            return(NULL)
        }

        set.seed(input$sim_seed)

        res <- activity5_trick_analysis(
            nrep = input$sim_nrep,
            picture_value = input$sim_picture,
            seed = NULL
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
            HTML(paste0(
                "<div class='message-text'>🎲 Simulation Added</div><br>",
                "Picture value: <b>", input$sim_picture, "</b><br>",
                "Fail probability: <b>", round(res$fail_prob, 6), "</b><br>",
                "SE: <b>", round(res$fail_prob_se, 6), "</b>"
            ))
        })

        output$simulation_table <- renderTable({
            data.frame(
                Outcome = c("Correct","Incorrect"),
                Count = c(res$correct, res$incorrect)
            )
        })

        output$simulation_plot <- renderPlot({
            barplot(
                c(res$correct, res$incorrect),
                names.arg = c("Correct","Incorrect"),
                col = c("#95D5B2","#E76F51"),
                border = NA
            )
        })

        updateNumericInput(session, "sim_seed", value = sample(1:999, 1))
    })

    output$simulation_plot2 <- renderPlot({

        df <- rv$sim_history
        req(nrow(df) > 0)

        ci_upper <- df$fail_prob + 1.96 * df$se
        ci_lower <- df$fail_prob - 1.96 * df$se

        y_min <- min(ci_lower, na.rm = TRUE)
        y_max <- max(ci_upper, na.rm = TRUE)

        pad <- 0.05 * (y_max - y_min + 1e-9)

        plot(df$picture_value, df$fail_prob,
             ylim = c(y_min - pad, y_max + pad),
             xlim = c(1, 10),
             pch = 19,
             col = "#7B9ACC",
             xlab = "Picture card value",
             ylab = "Failure probability")

        for (i in seq_len(nrow(df))) {
            segments(df$picture_value[i], ci_lower[i],
                     df$picture_value[i], ci_upper[i],
                     col = "#7B9ACC")
        }

        points(df$picture_value, df$fail_prob,
               pch = 19,
               col = "#7B9ACC")

        n <- nrow(df)

        points(df$picture_value[n],
               df$fail_prob[n],
               pch = 19,
               col = "red",
               cex = 1.4)

        segments(df$picture_value[n], ci_lower[n],
                 df$picture_value[n], ci_upper[n],
                 col = "red",
                 lwd = 2)
    })
}

shinyApp(ui, server)
