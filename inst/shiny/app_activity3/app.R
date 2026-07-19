suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(ggplot2)
})

# =========================================================
# HELPER
# =========================================================

draw_ceiling <- function(mode) {
    switch(
        mode,

        fixed = 250,

        random = sample(200:1000, 1),

        none = Inf
    )
}

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
        tags$style(
            HTML(
                "

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

            .button-spacing .btn{
    margin-top:8px;
    margin-bottom:8px;
    width:100%;
}

.btn-start{
    background:#89C2D9 !important;
    border-color:#89C2D9 !important;
    color:#1D3557 !important;
}

.btn-start:hover{
    background:#7B9ACC !important;
    color:white !important;
}

.btn-stop{
    background:#CDB4DB !important;
    border-color:#CDB4DB !important;
    color:#2E3440 !important;
}

.btn-stop:hover{
    background:#B89ACB !important;
    color:white !important;
}

.btn-reset{
    background:#A8DADC !important;
    border-color:#A8DADC !important;
    color:#1D3557 !important;
}

.btn-reset:hover{
    background:#89C2D9 !important;
    color:white !important;
}

.button-spacing .btn{
    width:100%;
    margin-top:8px;
    margin-bottom:8px;
    border-radius:12px !important;
    font-weight:700 !important;
}

        ")
        ),

        div(class = "main-title", h2("🪙 Activity 3: Place Your Bets"))
    ),

    # =====================================================
    # OVERVIEW
    # =====================================================

    overview_page(

        explanation = tagList(
            p("This activity replicates and extends a published experiment on betting strategy. The original study examined how closely people's betting decisions approach the mathematically optimal strategy.
              Here, the experiment is extended to explore the relationships between probability, expectation, and decision-making discussed in Playing With Statistics."),

            p("The app simulates a sequence of coin tosses and allows participants to bet on each event."),

            p("The rules of play are:"),

            tags$ol(
                tags$li("Choose Heads or Tails on a sequence of coin tosses."),
                tags$li("Your starting bank is $25."),
                tags$li("You may bet any whole-dollar amount up to your current bank."),
                tags$li("Bets and winnings are rounded to the nearest dollar."),
                tags$li("The game ends when:", tags$ul(
                    tags$li("You reach a hidden winning threshold."),
                    tags$li("Your bank reaches zero."),
                    tags$li("30 minutes expires.")
                ))
            ),

            p("Additionally:"),

            tags$ul(
                tags$li("The probability of Tails on each coin toss is 0.4 by default, but this value can be randomised to either a shown or hidden value."),
                tags$li("The ceiling at which the game stops can also be randomised to a different value or removed completely."),
                tags$li("Bet sizes can either be entered manually for each bet, or chosen to be a specified proportion of current bank value."),
                tags$li("The game can also be run automatically using a fixed proportion of the current bank as the stake on every bet. In this auto mode, bets can be placed faster by
                        reducing the time between automatic bet placements.")
            )
        ),

        individual = tagList(
              p("Play the game in both Manual and Auto modes. Explore different staking strategies to determine what appears to work well and what does not.
                In Auto Mode, also experiment with simulated values of the probability of tails, and investigate how changing or removing the bank ceiling affects the outcome."),

              p("Subsequently, read Section ?.? of Playing With Statistics in which the notion of optimal staking is discussed."),

              p("Optionally, play the game again in light of this additional information."),

        ),

        group = tagList(
             p("The activity is intended to be completed individually before a group meeting. Participants should explore the game in both Manual and Auto modes,
             as described in the Individual Implementation panel. During the meeting, discussion should focus on comparing strategies,
               explaining different outcomes, and connecting these observations to the theoretical results presented in Section ?.? of Playing With Statistics."),

        ),

        question = tagList(
            tags$ul(
                tags$li("Should you always bet on Heads, always bet on Tails, or use some combination of the two?"),
                tags$li("What is the effect of stake sizes that are too small or too large?"),
                tags$li("s there a staking proportion that appears to be optimal? Does it depend on the probability of tails?"),
                tags$li("What additional challenges arise when the probability of tails is unknown? How might these be addressed?"),
                tags$li("What effect does the bank ceiling have on the optimal strategy?")
            )
        )
    ),

    # =====================================================
    # ACTIVITY
    # =====================================================

    nav_panel(

        "Activity",

        navset_tab(

            nav_panel(

                "Manual Mode",

                layout_sidebar(

                    accordion(
                        accordion_panel(
                            "📖 Rules of Play",

                            tags$ol(
                                tags$li("Choose Heads or Tails on a sequence of coin tosses."),
                                tags$li("Your starting bank is $25."),
                                tags$li("You may bet any whole-dollar amount up to your current bank."),
                                tags$li("Bets and winnings are rounded to the nearest dollar."),
                                tags$li("The game ends when:", tags$ul(
                                    tags$li("You reach a hidden winning threshold."),
                                    tags$li("Your bank reaches zero."),
                                    tags$li("30 minutes expires.")
                                ))
                            )
                        ),
                        open = FALSE
                    ),

                    sidebar = div(
                        class = "card-style",

                        checkboxInput("m_random_p", "Simulate probability of tails", FALSE),
                        checkboxInput("m_show_p", "Show probability", FALSE),

                        hr(),

                        radioButtons(
                            "m_ceiling_mode",
                            "Winning Threshold",
                            choices = c(
                                "Fixed" = "fixed",
                                "Random" = "random",
                                "No Ceiling" = "none"
                            ),
                            selected = "fixed"
                        ),

                        hr(),

                        selectInput("m_bet", "Bet Choice", c("H", "T")),

                        numericInput("m_stake_fixed", "Stake ($)", value = 5, min = 1),

                        sliderInput(
                            "m_stake_prop",
                            "Proportion of bank to stake (%)",
                            min = 0,
                            max = 100,
                            value = 20,
                            step = 5
                        ),

                        div(
                            class = "button-spacing",

                            actionButton("m_go", "Place Bet", class = "btn-start"),
                            actionButton("m_reset", "Reset", class = "btn-reset")
                        )
                    ),

                    div(
                        class = "card-style",

                        uiOutput("m_p_display"),

                        hr(),
                        div(class = "big-timer", textOutput("m_timer")),
                        hr(),
                        div(class = "big-bank", textOutput("m_bank_display")),
                        hr(),

                        fluidRow(
                            column(
                                4,
                                div(
                                    h4("Game Log"),
                                    div(class = "log-box", uiOutput("m_log_ui"))
                                )
                            ),
                            column(
                                8,
                                plotOutput("m_plot", height = "300px")
                            )
                        )
                    )
                )
            ),

            # =====================================================
            # SIMULATION MODE
            # =====================================================

            nav_panel(
                "Auto Mode",

                layout_sidebar(

                    accordion(
                        accordion_panel(
                            "📖 Rules of Play",

                            tags$ol(
                                tags$li("Choose Heads or Tails on a sequence of coin tosses."),
                                tags$li("Your initial bank is $25."),
                                tags$li("Bets and winnings are rounded to the nearest dollar."),
                                tags$li(
                                    "The game stops if:",
                                    tags$ul(
                                        tags$li("Your bank reaches a hidden threshold."),
                                        tags$li("Your bank reaches zero."),
                                        tags$li("30 minutes expires.")
                                    )
                                )
                            )
                        ),
                        open = FALSE
                    ),

                    sidebar = div(

                        class = "card-style",

                        numericInput(
                            "s_seed",
                            "Random Seed",
                            value = sample(1:999, 1),
                            min = 1,
                            max = 999
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

                        hr(),

                        radioButtons(
                            "s_ceiling_mode",
                            "Winning Threshold",
                            choices = c(
                                "Hidden" = "fixed",
                                "Random" = "random",
                                "No Ceiling" = "none"
                            ),
                            selected = "fixed"
                        ),

                        hr(),

                        selectInput(
                            "s_bet",
                            "Bet Choice",
                            c("H", "T")
                        ),

                        sliderInput(
                            "s_stake_prop",
                            "Proportion of bank to stake (%)",
                            min = 0,
                            max = 100,
                            value = 20,
                            step = 5
                        ),

                        sliderInput(
                            "s_interval",
                            "Time between bets (s)",
                            min = 0.05,
                            max = 1,
                            value = 0.5,
                            step = 0.05
                        ),

                        div(
                            class = "button-spacing",

                            actionButton(
                                "s_start",
                                "Start",
                                class = "btn-start"
                            ),

                            actionButton(
                                "s_stop",
                                "Stop",
                                class = "btn-stop"
                            ),

                            actionButton(
                                "s_reset",
                                "Reset",
                                class = "btn-reset"
                            )
                        )
                    ),

                    div(
                        class = "card-style",

                        uiOutput("s_p_display"),

                        hr(),

                        div(class = "big-timer", textOutput("s_timer")),

                        hr(),

                        div(class = "big-bank", textOutput("s_bank_display")),

                        hr(),

                        fluidRow(
                            column(
                                width = 4,
                                div(
                                    h4("Game Log"),
                                    div(class = "log-box", uiOutput("s_log_ui"))
                                )
                            ),

                            column(
                                width = 8,
                                plotOutput("s_plot", height = "300px")
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

server <- function(input, output, session) {
    duration <- 30 * 60

    make_state <- function() {
        reactiveValues(
            bank = 25,
            ceiling = 250,
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

    reset_state <- function(rv, random_p, ceiling_mode = "fixed") {
        rv$bank <- 25
        rv$log <- character()
        rv$history <- data.frame()
        rv$step <- 0
        rv$active <- FALSE
        rv$running <- FALSE
        rv$start_time <- NULL

        rv$p_t <- if (random_p) {
            runif(1, 0.2, 0.8)
        } else {
            0.4
        }

        rv$ceiling <- draw_ceiling(ceiling_mode)
    }

    reset_state(m, FALSE)
    reset_state(s, FALSE)

    # =====================================================
    # UPDATE P(T)
    # =====================================================

    observeEvent(input$m_random_p, {
        m$p_t <- if (input$m_random_p) {
            runif(1, 0.2, 0.8)
        } else {
            0.4
        }
    })

    observeEvent(input$s_random_p, {
        s$p_t <- if (input$s_random_p) {
            runif(1, 0.2, 0.8)
        } else {
            0.4
        }
    })

    # =====================================================
    # STAKE SYNC
    # =====================================================

    update_manual_stake <- function() {
        req(m$bank, input$m_stake_prop)

        new_val <- round(m$bank * input$m_stake_prop / 100)

        updateNumericInput(session, "m_stake_fixed", value = max(1, new_val))
    }

    observeEvent(input$m_stake_prop, {
        update_manual_stake()
    })

    # =====================================================
    # RESET OBSERVERS
    # =====================================================

    observeEvent(input$m_reset, {
        reset_state(m, input$m_random_p, input$m_ceiling_mode)

        update_manual_stake()
    })

    observeEvent(input$s_reset, {
        updateNumericInput(session, "s_seed", value = sample(1:999, 1))

        reset_state(s, input$s_random_p, input$s_ceiling_mode)
    })

    # =====================================================
    # COIN
    # =====================================================

    coin <- function(p) {
        sample(c("H", "T"), 1, prob = c(1 - p, p))
    }

    # =====================================================
    # TIMER
    # =====================================================

    format_time <- function(start_time) {
        if (is.null(start_time)) {
            return("30:00")
        }

        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

        remaining <- max(duration - elapsed, 0)

        sprintf("%02d:%02d",
                floor(remaining / 60),
                floor(remaining %% 60))
    }

    time_expired <- function(start_time) {
        if (is.null(start_time)) {
            return(FALSE)
        }

        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

        elapsed >= duration
    }

    observe({
        invalidateLater(1000, session)

        output$m_timer <- renderText({
            if (m$bank <= 0) {
                return("💀 Game Over")
            }

            if (m$bank >= m$ceiling) {
                return("🏆 You Win")
            }

            if (!m$running) {
                return("Ready")
            }

            paste("Time Remaining:", format_time(m$start_time))
        })

        output$s_timer <- renderText({
            if (s$bank <= 0) {
                return("💀 Game Over")
            }

            if (s$bank >= s$ceiling) {
                return("🏆 You Win")
            }

            if (!s$running) {
                return("Ready")
            }

            paste("Time Remaining:", format_time(s$start_time))
        })
    })

    # =====================================================
    # MANUAL BET
    # =====================================================

    manual_bet <- function() {
        if (!m$running) {
            m$running <- TRUE
            m$start_time <- Sys.time()
        }

        if (time_expired(m$start_time)) {
            m$running <- FALSE

            m$log <- c(m$log, "⏰ Time limit reached")

            showNotification("⏰ Time limit reached.", duration = 5)

            return()
        }

        if (m$bank <= 0) {
            m$log <- c(m$log, paste0("#", m$step + 1, " 💀 BANKRUPT"))

            return()
        }

        if (m$bank >= m$ceiling) {
            m$log <- c(m$log, paste0("#", m$step, " 🏆 WIN!"))

            m$running <- FALSE

            showNotification("🏆 You reached the target bank.", duration = NULL)

            m$history <- rbind(m$history, data.frame(step = m$step, bank = m$bank))

            return()
        }

        stake <- round(input$m_stake_fixed)

        if (stake > m$bank) {
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

        if (result == input$m_bet) {
            m$bank <- m$bank + stake

            m$log <- c(
                m$log,
                paste0(
                    "#",
                    m$step,
                    " 🟢 WIN +$",
                    stake,
                    " | Result: ",
                    result,
                    " | Bank: $",
                    round(m$bank, 2)
                )
            )

        } else {
            m$bank <- m$bank - stake

            m$log <- c(
                m$log,
                paste0(
                    "#",
                    m$step,
                    " 🔴 LOSS -$",
                    stake,
                    " | Result: ",
                    result,
                    " | Bank: $",
                    round(m$bank, 2)
                )
            )
        }

        if (m$bank <= 0) {
            m$bank <- 0

            m$log <- c(m$log, paste0("#", m$step, " 💀 BANKRUPT"))
        }

        if (m$bank >= m$ceiling) {
            m$log <- c(m$log, paste0("#", m$step, " 🏆 WIN!"))

            m$active <- FALSE
            m$running <- FALSE

            showNotification("🏆  reached the target bank.", duration = NULL)

            s$history <- rbind(s$history, data.frame(step = s$step, bank = s$bank))

            return()
        }

        m$history <- rbind(m$history, data.frame(step = m$step, bank = m$bank))

        update_manual_stake()
    }

    observeEvent(input$m_go, {
        manual_bet()
    })

    # =====================================================
    # SIMULATION
    # =====================================================

    observeEvent(input$s_start, {
        set.seed(input$s_seed)

        s$active <- TRUE

        if (!s$running) {
            s$running <- TRUE
            s$start_time <- Sys.time()
        }
    })

    observeEvent(input$s_stop, {
        s$active <- FALSE
    })

    observe({
        req(s$active)

        invalidateLater(input$s_interval * 1000, session)

        isolate({
            if (time_expired(s$start_time)) {
                s$active <- FALSE
                s$running <- FALSE

                s$log <- c(s$log, "⏰ Time limit reached")

                showNotification("⏰ Time limit reached.", duration = 5)

                return()
            }

            if (s$bank <= 0) {
                s$log <- c(s$log, paste0("#", s$step + 1, " 💀 BANKRUPT"))

                s$active <- FALSE

                return()
            }

            stake <- round(s$bank * input$s_stake_prop / 100)

            result <- coin(s$p_t)

            s$step <- s$step + 1

            if (result == input$s_bet) {
                s$bank <- s$bank + stake

                s$log <- c(
                    s$log,
                    paste0(
                        "#",
                        s$step,
                        " 🟢 WIN +$",
                        stake,
                        " | Result: ",
                        result,
                        " | Bank: $",
                        round(s$bank, 2)
                    )
                )

            } else {
                s$bank <- s$bank - stake

                s$log <- c(
                    s$log,
                    paste0(
                        "#",
                        s$step,
                        " 🔴 LOSS -$",
                        stake,
                        " | Result: ",
                        result,
                        " | Bank: $",
                        round(s$bank, 2)
                    )
                )
            }

            if (s$bank <= 0) {
                s$bank <- 0

                s$log <- c(s$log, paste0("#", s$step, " 💀 BANKRUPT"))

                s$active <- FALSE
            }

            if (s$bank >= s$ceiling) {
                s$log <- c(s$log, paste0("#", s$step, " 🏆 WIN!"))

                s$active <- FALSE
                s$running <- FALSE

                showNotification("🏆 Simulation reached the target bank.",
                                 duration = NULL)

                s$history <- rbind(s$history,
                                   data.frame(step = s$step, bank = s$bank))

                return()
            }

            s$history <- rbind(s$history, data.frame(step = s$step, bank = s$bank))
        })
    })

    # =====================================================
    # OUTPUTS
    # =====================================================

    output$m_bank_display <- renderText({
        if (m$bank >= m$ceiling) {
            paste0("🏆 Bank: $", round(m$bank, 2))

        } else {
            paste0("Bank: $", round(m$bank, 2))
        }
    })

    output$s_bank_display <- renderText({
        if (s$bank >= s$ceiling) {
            paste0("🏆 Bank: $", round(s$bank, 2))

        } else {
            paste0("Bank: $", round(s$bank, 2))
        }
    })

    output$m_log_ui <- renderUI({
        HTML(paste(rev(m$log), collapse = "<br>"))
    })

    output$s_log_ui <- renderUI({
        HTML(paste(rev(s$log), collapse = "<br>"))
    })

    output$m_p_display <- renderUI({

        if (isFALSE(input$m_show_p)) {
            return(NULL)
        }

        div(
            class = "p-highlight",
            paste0("P(Tails) = ", round(m$p_t, 3))
        )
    })

    output$s_p_display <- renderUI({
        if (!input$s_show_p) {
            return(NULL)
        }

        div(class = "p-highlight", paste0("P(Tails) = ", round(s$p_t, 3)))
    })

    output$m_plot <- renderPlot({
        req(nrow(m$history) > 1)

        ggplot(m$history, aes(step, bank, group = 1)) +
            geom_line(color = "#7B9ACC", linewidth = 1.2) +
            geom_point(color = "#CDB4DB", size = 3) +
            xlab("Number of Bets") + ylab("Bank (dollars)") +
            theme_minimal()
    })

    output$s_plot <- renderPlot({
        req(nrow(s$history) > 1)

        ggplot(s$history, aes(step, bank, group = 1)) +
            geom_line(color = "#7B9ACC", linewidth = 1.2) +
            geom_point(color = "#CDB4DB", size = 3) +
            xlab("Number of Bets") + ylab("Bank (dollars)") +
            theme_minimal()
    })
}

# =========================================================
# APP
# =====================================================

shinyApp(ui, server)
