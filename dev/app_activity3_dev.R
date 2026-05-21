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

    tags$head(
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

        "))
    ),

    div(class="main-title",
        h2("🪙 Activity 3: Place Your Bets")
    ),

    nav_panel(

        "Manual Mode",

        layout_sidebar(

            sidebar = div(class="card-style",

                          selectInput("m_bet","Bet Choice", c("H","T")),

                          numericInput("m_stake_fixed","Stake ($)", value=5, min=1),

                          sliderInput(
                              "m_stake_prop",
                              "Proportion of bank",
                              min = 0,
                              max = 1,
                              value = 0.2,
                              step = 0.05,
                              ticks = TRUE
                          ),

                          checkboxInput("m_random_p","Random probability", FALSE),
                          checkboxInput("m_show_p","Show probability", TRUE),

                          actionButton("m_go","Place Bet"),
                          actionButton("m_reset","Reset")
            ),

            div(class="card-style",

                uiOutput("m_p_display"),

                hr(),

                div(class="big-timer", textOutput("m_timer")),
                hr(),

                div(class="big-bank", textOutput("m_bank_display")),
                hr(),

                plotOutput("m_plot", height="220px"),

                hr(),

                h4("Game Log"),
                div(class="log-box", uiOutput("m_log_ui"))
            )
        )
    ),

    nav_panel(

        "Simulation Mode",

        layout_sidebar(

            sidebar = div(class="card-style",

                          selectInput("s_bet","Bet Choice", c("H","T")),

                          checkboxInput("s_random_p","Random probability", FALSE),
                          checkboxInput("s_show_p","Show probability", TRUE),

                          sliderInput(
                              "s_stake_prop",
                              "Stake (% of bank)",
                              min = 0,
                              max = 1,
                              value = 0.1,
                              step = 0.05,
                              ticks = TRUE
                          ),

                          sliderInput(
                              "s_interval",
                              "Time between bets",
                              min=0.2,
                              max=5,
                              value=2,
                              step=0.2
                          ),

                          actionButton("s_start","Start"),
                          actionButton("s_stop","Stop"),
                          actionButton("s_reset","Reset")
            ),

            div(class="card-style",

                uiOutput("s_p_display"),

                hr(),

                div(class="big-timer", textOutput("s_timer")),
                hr(),

                div(class="big-bank", textOutput("s_bank_display")),
                hr(),

                plotOutput("s_plot", height="220px"),

                hr(),

                h4("Game Log"),
                div(class="log-box", uiOutput("s_log_ui"))
            )
        )
    )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session){

    bankmax <- 250
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

    # =========================================================
    # RESET
    # =========================================================

    reset_state <- function(rv, random_p){

        rv$bank <- 25
        rv$log <- character()
        rv$history <- data.frame()
        rv$step <- 0
        rv$active <- FALSE
        rv$running <- FALSE
        rv$start_time <- NULL

        rv$p_t <- if(random_p) runif(1,0.2,0.8) else 0.4
    }

    observeEvent(input$m_reset, {
        reset_state(m, input$m_random_p)
        update_manual_stake()
    })

    observeEvent(input$s_reset, {
        reset_state(s, input$s_random_p)
    })

    # =========================================================
    # COIN
    # =========================================================

    coin <- function(p){
        sample(c("H","T"),1,prob=c(1-p,p))
    }

    # =========================================================
    # STAKE SYNC (FIXED CORE LOGIC)
    # =========================================================

    update_manual_stake <- function(){

        req(m$bank, input$m_stake_prop)

        new_val <- round(m$bank * input$m_stake_prop)

        updateNumericInput(
            session,
            "m_stake_fixed",
            value = max(1, new_val)
        )
    }

    observeEvent(input$m_stake_prop, {
        update_manual_stake()
    })

    # =========================================================
    # TIMER
    # =========================================================

    format_time <- function(start_time){

        if(is.null(start_time)) return("30:00")

        elapsed <- as.numeric(difftime(Sys.time(), start_time, units="secs"))
        remaining <- max(duration - elapsed, 0)

        sprintf("%02d:%02d", floor(remaining/60), floor(remaining%%60))
    }

    observe({

        invalidateLater(1000, session)

        output$m_timer <- renderText({
            if(m$running)
                paste("Time Remaining:", format_time(m$start_time))
            else "Timer not started"
        })

        output$s_timer <- renderText({
            if(s$running)
                paste("Time Remaining:", format_time(s$start_time))
            else "Timer not started"
        })
    })

    # =========================================================
    # MANUAL BET
    # =========================================================

    manual_bet <- function(){

        if(!m$running){
            m$running <- TRUE
            m$start_time <- Sys.time()
        }

        if(m$bank <= 0){
            m$log <- c(m$log, "💀 BANKRUPT")
            return()
        }

        stake <- round(input$m_stake_fixed)
        result <- coin(m$p_t)

        if(result == input$m_bet){

            m$bank <- m$bank + stake
            m$log <- c(m$log,
                       paste0("🟢 WIN +$", stake,
                              " | Bank: $", round(m$bank,2)))

        } else {

            m$bank <- m$bank - stake
            m$log <- c(m$log,
                       paste0("🔴 LOSS -$", stake,
                              " | Bank: $", round(m$bank,2)))
        }

        if(m$bank <= 0){
            m$bank <- 0
            m$log <- c(m$log, "💀 BANKRUPT")
        }

        m$step <- m$step + 1
        m$history <- rbind(m$history,
                           data.frame(step=m$step, bank=m$bank))

        # 🔥 KEY FIX: always recompute stake after bank change
        update_manual_stake()
    }

    observeEvent(input$m_go, manual_bet())

    # =========================================================
    # SIMULATION
    # =========================================================

    observeEvent(input$s_start, {
        s$active <- TRUE
        s$running <- TRUE
        s$start_time <- Sys.time()
    })

    observeEvent(input$s_stop, s$active <- FALSE)

    observe({

        req(s$active)
        invalidateLater(input$s_interval * 1000, session)

        isolate({

            if(s$bank <= 0){
                s$log <- c(s$log, "💀 BANKRUPT")
                s$active <- FALSE
                return()
            }

            stake <- round(s$bank * input$s_stake_prop)
            result <- coin(s$p_t)

            if(result == input$s_bet){

                s$bank <- s$bank + stake
                s$log <- c(s$log,
                           paste0("🟢 WIN +$", stake,
                                  " | Bank: $", round(s$bank,2)))

            } else {

                s$bank <- s$bank - stake
                s$log <- c(s$log,
                           paste0("🔴 LOSS -$", stake,
                                  " | Bank: $", round(s$bank,2)))
            }

            if(s$bank <= 0){
                s$bank <- 0
                s$log <- c(s$log, "💀 BANKRUPT")
                s$active <- FALSE
            }

            s$step <- s$step + 1
            s$history <- rbind(s$history,
                               data.frame(step=s$step, bank=s$bank))
        })
    })

    # =========================================================
    # OUTPUTS
    # =========================================================

    output$m_bank_display <- renderText(paste0("Bank: $", round(m$bank,2)))
    output$s_bank_display <- renderText(paste0("Bank: $", round(s$bank,2)))

    output$m_log_ui <- renderUI(HTML(paste(rev(m$log), collapse="<br>")))
    output$s_log_ui <- renderUI(HTML(paste(rev(s$log), collapse="<br>")))

    output$m_p_display <- renderUI({
        if(!input$m_show_p) return(NULL)
        div(class="p-highlight",
            paste0("P(T) = ", round(m$p_t,3)))
    })

    output$s_p_display <- renderUI({
        if(!input$s_show_p) return(NULL)
        div(class="p-highlight",
            paste0("P(T) = ", round(s$p_t,3)))
    })

    output$m_plot <- renderPlot({
        req(nrow(m$history)>0)
        ggplot(m$history, aes(step, bank)) +
            geom_line(color="#7B9ACC", linewidth=1.2) +
            geom_point(color="#CDB4DB", size=3) +
            theme_minimal()
    })

    output$s_plot <- renderPlot({
        req(nrow(s$history)>0)
        ggplot(s$history, aes(step, bank)) +
            geom_line(color="#7B9ACC", linewidth=1.2) +
            geom_point(color="#CDB4DB", size=3) +
            theme_minimal()
    })
}

shinyApp(ui, server)
