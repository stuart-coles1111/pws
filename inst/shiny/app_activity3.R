library(shiny)
library(bslib)
library(ggplot2)

# =========================================================
# UI
# =========================================================

ui <- page_navbar(

    theme = bs_theme(
        version = 5,
        bootswatch = "minty",
        primary = "#7B9ACC",
        bg = "#F7F7FB",
        fg = "#2E3440",
        base_font = font_google("Inter")
    ),

    header = tagList(

        tags$style(HTML("

            .main-title{
                background:linear-gradient(90deg,#A8DADC,#CDB4DB);
                padding:18px;
                border-radius:16px;
                text-align:center;
                margin-bottom:18px;
            }

            .card-style{
                background:white;
                border-radius:16px;
                padding:22px;
                margin-bottom:18px;
                box-shadow:0 3px 12px rgba(0,0,0,0.08);
            }

            .big-bank{
                font-size:34px;
                font-weight:800;
                text-align:center;
            }

            .big-timer{
                font-size:22px;
                font-weight:700;
                text-align:center;
                color:#7B9ACC;
            }

            .p-highlight{
                font-size:34px;
                font-weight:800;
                text-align:center;
                padding:12px;
                background:#E8F4FD;
                border-radius:12px;
            }

            .log-box{
                max-height:260px;
                overflow-y:auto;
                font-family:monospace;
                background:#F8F9FB;
                padding:10px;
                border-radius:10px;
            }

            .info-box{
                background:#F8F9FB;
                padding:18px;
                border-radius:14px;
                line-height:1.7;
            }

        ")),

        div(
            class = "main-title",
            h2("🪙 Activity 3: Place Your Bets")
        )
    ),

    # =====================================================
    # MANUAL MODE
    # =====================================================

    nav_panel(

        "Manual Mode",

        layout_sidebar(

            sidebar = div(

                class = "card-style",

                selectInput(
                    "m_bet",
                    "Bet Choice",
                    c("H","T")
                ),

                numericInput(
                    "m_stake_fixed",
                    "Stake ($)",
                    value = 5,
                    min = 1
                ),

                sliderInput(
                    "m_stake_prop",
                    "Percentage of bank to stake",
                    min = 0,
                    max = 100,
                    value = 20,
                    step = 5
                ),

                checkboxInput(
                    "m_random_p",
                    "Simulate probability of tails",
                    FALSE
                ),

                checkboxInput(
                    "m_show_p",
                    "Show probability",
                    FALSE
                ),

                actionButton(
                    "m_go",
                    "Place Bet"
                ),

                actionButton(
                    "m_reset",
                    "Reset"
                )
            ),

            div(

                class = "card-style",

                uiOutput("m_p_display"),

                hr(),

                div(
                    class = "big-timer",
                    textOutput("m_timer")
                ),

                hr(),

                div(
                    class = "big-bank",
                    textOutput("m_bank_display")
                ),

                hr(),

                plotOutput(
                    "m_plot",
                    height = "220px"
                ),

                hr(),

                h4("Game Log"),

                div(
                    class = "log-box",
                    uiOutput("m_log_ui")
                )
            )
        )
    ),

    # =====================================================
    # SIMULATION MODE
    # =====================================================

    nav_panel(

        "Simulation Mode",

        layout_sidebar(

            sidebar = div(

                class = "card-style",

                selectInput(
                    "s_bet",
                    "Bet Choice",
                    c("H","T")
                ),

                checkboxInput(
                    "s_random_p",
                    "Simulate probability of tails",
                    FALSE
                ),

                checkboxInput(
                    "s_show_p",
                    "Show probability",
                    FALSE
                ),

                sliderInput(
                    "s_stake_prop",
                    "Percentage of bank to stake",
                    min = 0,
                    max = 100,
                    value = 20,
                    step = 5
                ),

                sliderInput(
                    "s_interval",
                    "Time between bets",
                    min = 0.2,
                    max = 5,
                    value = 2,
                    step = 0.2
                ),

                actionButton(
                    "s_start",
                    "Start"
                ),

                actionButton(
                    "s_stop",
                    "Stop"
                ),

                actionButton(
                    "s_reset",
                    "Reset"
                )
            ),

            div(

                class = "card-style",

                uiOutput("s_p_display"),

                hr(),

                div(
                    class = "big-timer",
                    textOutput("s_timer")
                ),

                hr(),

                div(
                    class = "big-bank",
                    textOutput("s_bank_display")
                ),

                hr(),

                plotOutput(
                    "s_plot",
                    height = "220px"
                ),

                hr(),

                h4("Game Log"),

                div(
                    class = "log-box",
                    uiOutput("s_log_ui")
                )
            )
        )
    ),

    # =====================================================
    # SUMMARY TAB
    # =====================================================

    nav_panel(

        "📘 Summary",

        div(
            class = "main-title",

            h1("📘 Understanding the Betting Game"),

            p(
                "Key statistical ideas behind betting strategies, probability, and risk."
            )
        ),

        fluidRow(

            column(
                6,

                div(
                    class = "card-style",

                    h3("🪙 How the Game Works"),

                    div(
                        class = "info-box",

                        tags$p(
                            "The game repeatedly simulates bets on a biased coin."
                        ),

                        tags$p(
                            "Players choose whether to bet on Heads or Tails and decide how much of their bank to risk."
                        ),

                        tags$p(
                            "Each outcome changes the player's remaining bank, producing a dynamic process over time."
                        ),

                        tags$p(
                            "Different staking strategies can dramatically affect long-term success."
                        )
                    )
                )
            ),

            column(
                6,

                div(
                    class = "card-style",

                    h3("📈 Role of Simulation"),

                    div(
                        class = "info-box",

                        tags$p(
                            "Simulation allows repeated betting scenarios to be explored quickly."
                        ),

                        tags$p(
                            "The app demonstrates how randomness can produce large variability in outcomes, even when probabilities remain fixed."
                        ),

                        tags$p(
                            "By changing stake sizes and probabilities, users can investigate the balance between growth and risk."
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
                                "Expected value"
                            ),

                            tags$li(
                                "Probability distributions"
                            ),

                            tags$li(
                                "Random processes"
                            ),

                            tags$li(
                                "Risk versus reward"
                            ),

                            tags$li(
                                "Bankroll management"
                            ),

                            tags$li(
                                "Simulation variability"
                            ),

                            tags$li(
                                "Long-run behaviour"
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
                                "How does stake size affect bankruptcy risk?"
                            ),

                            tags$li(
                                "Can a favourable probability still lead to losses?"
                            ),

                            tags$li(
                                "What happens when betting aggressively?"
                            ),

                            tags$li(
                                "How important is randomness in short runs?"
                            ),

                            tags$li(
                                "Which strategies produce more stable growth?"
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
                            "The betting simulation highlights an important statistical principle:"
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

                            "Even favourable odds do not guarantee success in the short term."
                        ),

                        tags$p(
                            "Random variation, stake sizing, and repeated exposure to risk all influence long-run outcomes."
                        ),

                        tags$p(
                            "These ideas are central to finance, gambling theory, insurance, and statistical modelling."
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

    duration <- 30 * 60

    make_state <- function(){

        reactiveValues(
            bank = 25,
            log = character(),
            history = data.frame(),
            step = 0,
            active = FALSE,
            running = FALSE,
            start_time = NULL,
            p_t = 0.4
        )
    }

    m <- make_state()
    s <- make_state()

    # =====================================================
    # RESET
    # =====================================================

    reset_state <- function(rv, random_p){

        rv$bank <- 25
        rv$log <- character()
        rv$history <- data.frame()
        rv$step <- 0
        rv$active <- FALSE
        rv$running <- FALSE
        rv$start_time <- NULL

        rv$p_t <- if(random_p){
            runif(1, 0.2, 0.8)
        } else {
            0.4
        }
    }

    reset_state(m, FALSE)
    reset_state(s, FALSE)

    # =====================================================
    # UPDATE P(T)
    # =====================================================

    observeEvent(input$m_random_p, {

        m$p_t <- if(input$m_random_p){
            runif(1, 0.2, 0.8)
        } else {
            0.4
        }
    })

    observeEvent(input$s_random_p, {

        s$p_t <- if(input$s_random_p){
            runif(1, 0.2, 0.8)
        } else {
            0.4
        }
    })

    # =====================================================
    # STAKE SYNC
    # =====================================================

    update_manual_stake <- function(){

        req(m$bank, input$m_stake_prop)

        new_val <- round(
            m$bank * input$m_stake_prop /100
        )

        updateNumericInput(
            session,
            "m_stake_fixed",
            value = max(1, new_val)
        )
    }

    observeEvent(input$m_stake_prop, {
        update_manual_stake()
    })

    # =====================================================
    # RESET OBSERVERS
    # =====================================================

    observeEvent(input$m_reset, {

        reset_state(
            m,
            input$m_random_p
        )

        update_manual_stake()
    })

    observeEvent(input$s_reset, {

        reset_state(
            s,
            input$s_random_p
        )
    })

    # =====================================================
    # COIN
    # =====================================================

    coin <- function(p){

        sample(
            c("H","T"),
            1,
            prob = c(1-p, p)
        )
    }

    # =====================================================
    # TIMER
    # =====================================================

    format_time <- function(start_time){

        if(is.null(start_time)){
            return("30:00")
        }

        elapsed <- as.numeric(
            difftime(
                Sys.time(),
                start_time,
                units = "secs"
            )
        )

        remaining <- max(
            duration - elapsed,
            0
        )

        sprintf(
            "%02d:%02d",
            floor(remaining / 60),
            floor(remaining %% 60)
        )
    }

    observe({

        invalidateLater(1000, session)

        output$m_timer <- renderText({

            if(m$running){

                paste(
                    "Time Remaining:",
                    format_time(m$start_time)
                )

            } else {

                "Timer not started"
            }
        })

        output$s_timer <- renderText({

            if(s$running){

                paste(
                    "Time Remaining:",
                    format_time(s$start_time)
                )

            } else {

                "Timer not started"
            }
        })
    })

    # =====================================================
    # MANUAL BET
    # =====================================================

    manual_bet <- function(){

        if(!m$running){

            m$running <- TRUE
            m$start_time <- Sys.time()
        }

        if(m$bank <= 0){

            m$log <- c(
                m$log,
                paste0("#", m$step + 1, " 💀 BANKRUPT")
            )

            return()
        }

        stake <- round(input$m_stake_fixed)

        if(stake > m$bank){

            showNotification(
                paste0(
                    "Invalid bet: you only have $",
                    round(m$bank, 2),
                    " remaining."
                ),
                type = "error",
                duration = 4
            )

            return()
        }

        result <- coin(m$p_t)

        m$step <- m$step + 1

        if(result == input$m_bet){

            m$bank <- m$bank + stake

            m$log <- c(
                m$log,
                paste0(
                    "#", m$step,
                    " 🟢 WIN +$",
                    stake,
                    " | Result: ",
                    result,
                    " | Bank: $",
                    round(m$bank,2)
                )
            )

        } else {

            m$bank <- m$bank - stake

            m$log <- c(
                m$log,
                paste0(
                    "#", m$step,
                    " 🔴 LOSS -$",
                    stake,
                    " | Result: ",
                    result,
                    " | Bank: $",
                    round(m$bank,2)
                )
            )
        }

        if(m$bank <= 0){

            m$bank <- 0

            m$log <- c(
                m$log,
                paste0("#", m$step, " 💀 BANKRUPT")
            )
        }

        m$history <- rbind(
            m$history,
            data.frame(
                step = m$step,
                bank = m$bank
            )
        )

        update_manual_stake()
    }

    observeEvent(input$m_go, {
        manual_bet()
    })

    # =====================================================
    # SIMULATION
    # =====================================================

    observeEvent(input$s_start, {

        s$active <- TRUE

        if(!s$running){

            s$running <- TRUE
            s$start_time <- Sys.time()
        }
    })

    observeEvent(input$s_stop, {

        s$active <- FALSE
    })

    observe({

        req(s$active)

        invalidateLater(
            input$s_interval * 1000,
            session
        )

        isolate({

            if(s$bank <= 0){

                s$log <- c(
                    s$log,
                    paste0("#", s$step + 1, " 💀 BANKRUPT")
                )

                s$active <- FALSE

                return()
            }

            stake <- round(
                s$bank * input$s_stake_prop /100
            )

            result <- coin(s$p_t)

            s$step <- s$step + 1

            if(result == input$s_bet){

                s$bank <- s$bank + stake

                s$log <- c(
                    s$log,
                    paste0(
                        "#", s$step,
                        " 🟢 WIN +$",
                        stake,
                        " | Result: ",
                        result,
                        " | Bank: $",
                        round(s$bank,2)
                    )
                )

            } else {

                s$bank <- s$bank - stake

                s$log <- c(
                    s$log,
                    paste0(
                        "#", s$step,
                        " 🔴 LOSS -$",
                        stake,
                        " | Result: ",
                        result,
                        " | Bank: $",
                        round(s$bank,2)
                    )
                )
            }

            if(s$bank <= 0){

                s$bank <- 0

                s$log <- c(
                    s$log,
                    paste0("#", s$step, " 💀 BANKRUPT")
                )

                s$active <- FALSE
            }

            s$history <- rbind(
                s$history,
                data.frame(
                    step = s$step,
                    bank = s$bank
                )
            )
        })
    })

    # =====================================================
    # OUTPUTS
    # =====================================================

    output$m_bank_display <- renderText({

        paste0(
            "Bank: $",
            round(m$bank,2)
        )
    })

    output$s_bank_display <- renderText({

        paste0(
            "Bank: $",
            round(s$bank,2)
        )
    })

    output$m_log_ui <- renderUI({

        HTML(
            paste(
                rev(m$log),
                collapse = "<br>"
            )
        )
    })

    output$s_log_ui <- renderUI({

        HTML(
            paste(
                rev(s$log),
                collapse = "<br>"
            )
        )
    })

    output$m_p_display <- renderUI({

        if(!input$m_show_p){
            return(NULL)
        }

        div(
            class = "p-highlight",
            paste0(
                "P(Tails) = ",
                round(m$p_t,3)
            )
        )
    })

    output$s_p_display <- renderUI({

        if(!input$s_show_p){
            return(NULL)
        }

        div(
            class = "p-highlight",
            paste0(
                "P(Tails) = ",
                round(s$p_t,3)
            )
        )
    })

    output$m_plot <- renderPlot({

        req(nrow(m$history) > 1)

        ggplot(
            m$history,
            aes(step, bank, group = 1)
        ) +
            geom_line(
                color = "#7B9ACC",
                linewidth = 1.2
            ) +
            geom_point(
                color = "#CDB4DB",
                size = 3
            ) +
            theme_minimal()
    })

    output$s_plot <- renderPlot({

        req(nrow(s$history) > 1)

        ggplot(
            s$history,
            aes(step, bank, group = 1)
        ) +
            geom_line(
                color = "#7B9ACC",
                linewidth = 1.2
            ) +
            geom_point(
                color = "#CDB4DB",
                size = 3
            ) +
            theme_minimal()
    })
}

# =========================================================
# APP
# =====================================================

shinyApp(ui, server)
