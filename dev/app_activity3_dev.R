library(shiny)
library(bslib)
library(ggplot2)

# =========================================================
# UI
# =========================================================

ui <- page_fluid(

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
                padding:20px;border-radius:14px;margin-bottom:20px;text-align:center;
            }
            .card-style{
                background:white;border-radius:14px;padding:20px;margin-bottom:20px;
                box-shadow:0 3px 10px rgba(0,0,0,0.08);
            }
            .bank-big{
                font-size:28px;font-weight:700;text-align:center;
            }
            .log-box{
                max-height:260px;overflow-y:auto;font-family:monospace;
                background:#F8F9FB;padding:10px;border-radius:10px;
            }
        "))
    ),

    div(class = "main-title",
        h1("🪙 Activity 3: Virtual Coin Toss Bank Game")
    ),

    layout_sidebar(

        sidebar = div(class = "card-style",

                      selectInput("bet", "Bet Choice", c("H", "T")),

                      radioButtons(
                          "stake_mode", "Stake mode",
                          choices = c("Fixed $" = "fixed",
                                      "% of bank" = "prop")
                      ),

                      numericInput("stake_fixed", "Fixed stake ($)", value = 25, min = 1),

                      sliderInput("stake_prop", "Proportion of bank",
                                  min = 0.01, max = 1, value = 0.1, step = 0.01),

                      checkboxInput("random_p", "Random probability of T (set at start)", FALSE),
                      checkboxInput("show_p", "Show probability", TRUE),

                      checkboxInput("auto_bet", "Automatic betting", FALSE),

                      sliderInput(
                          "interval",
                          "Interval (seconds)",
                          min = 0.1,
                          max = 5,
                          value = 1,
                          step = 0.1
                      ),

                      actionButton("go", "Place Bet", class = "btn-primary"),
                      actionButton("reset", "Reset Game")
        ),

        div(class = "card-style",

            textOutput("clock"),
            textOutput("p_display"),

            hr(),

            div(class = "bank-big",
                textOutput("bank_display")
            ),

            hr(),

            plotOutput("bank_plot", height = "220px"),

            hr(),

            h4("Game Log"),
            div(class = "log-box",
                uiOutput("log_ui")
            )
        )
    )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session){

    rv <- reactiveValues(
        bank = 25,
        log = character(),
        active = TRUE,
        start_time = Sys.time(),
        frozen_time = NULL,
        history = data.frame(),
        valid_bets = 0,
        p_t = 0.4   # fixed per game probability
    )

    bankmax <- 250
    duration <- 30 * 60

    # ---------------------------------------------------------
    # Reset
    # ---------------------------------------------------------

    observeEvent(input$reset, {

        rv$bank <- 25
        rv$log <- character()
        rv$active <- TRUE
        rv$start_time <- Sys.time()
        rv$frozen_time <- NULL
        rv$history <- data.frame()
        rv$valid_bets <- 0

        rv$p_t <- if (input$random_p) runif(1, 0.2, 0.8) else 0.4

    }, ignoreInit = TRUE)

    # ---------------------------------------------------------
    # Clock
    # ---------------------------------------------------------

    time_remaining <- reactive({

        invalidateLater(1000, session)

        if(!is.null(rv$frozen_time))
            return(rv$frozen_time)

        elapsed <- as.numeric(Sys.time() - rv$start_time, units = "secs")
        max(0, duration - elapsed)
    })

    output$clock <- renderText({

        rem <- time_remaining()
        mins <- floor(rem / 60)
        secs <- round(rem %% 60)

        paste0(
            "⏱ Time: ", mins, "m ", secs, "s",
            "   |   🎯 Bets: ", rv$valid_bets
        )
    })

    # ---------------------------------------------------------
    # Coin toss (fixed per game probability)
    # ---------------------------------------------------------

    coin_toss <- function(){

        sample(c("H", "T"), 1, prob = c(1 - rv$p_t, rv$p_t))
    }

    output$p_display <- renderText({

        if(!input$show_p) return("")

        paste0("P(T) fixed for this game = ", round(rv$p_t, 3))
    })

    # ---------------------------------------------------------
    # Core betting function
    # ---------------------------------------------------------

    place_bet <- function(){

        if(!rv$active) return()

        if(rv$bank <= 0 || rv$bank >= bankmax){
            rv$active <- FALSE
            rv$frozen_time <- time_remaining()
            return()
        }

        stake <- if (input$stake_mode == "fixed") {
            input$stake_fixed
        } else {
            round(rv$bank * input$stake_prop)
        }

        if(is.na(stake) || stake <= 0) return()

        result <- coin_toss()

        if(result == input$bet){
            rv$bank <- rv$bank + stake
            rv$log <- c(rv$log, paste0("🟢 ", result, " WIN +$", stake))
        } else {
            rv$bank <- rv$bank - stake
            rv$log <- c(rv$log, paste0("🔴 ", result, " LOSS -$", stake))
        }

        rv$valid_bets <- rv$valid_bets + 1

        rv$history <- rbind(
            rv$history,
            data.frame(step = rv$valid_bets, bank = rv$bank)
        )

        if(rv$bank <= 0){
            rv$bank <- 0
            rv$log <- c(rv$log, "💀 Bankrupt")
            rv$active <- FALSE
        }

        if(rv$bank >= bankmax){
            rv$log <- c(rv$log, "🏆 Target reached!")
            rv$active <- FALSE
        }
    }

    # ---------------------------------------------------------
    # Manual betting
    # ---------------------------------------------------------

    observeEvent(input$go, {
        place_bet()
    })

    # ---------------------------------------------------------
    # Auto betting (faster than 0.5s supported)
    # ---------------------------------------------------------

    observe({

        req(input$auto_bet)
        invalidateLater(max(100, input$interval * 1000), session)

        isolate({
            place_bet()
        })
    })

    # ---------------------------------------------------------
    # Outputs
    # ---------------------------------------------------------

    output$bank_display <- renderText({
        paste0("Bank: $", rv$bank)
    })

    output$log_ui <- renderUI({
        HTML(paste(rv$log, collapse = "<br>"))
    })

    output$bank_plot <- renderPlot({

        req(nrow(rv$history) > 0)

        ggplot(rv$history, aes(step, bank)) +
            geom_line(linewidth = 1.2, color = "#7B9ACC") +
            geom_point(color = "#CDB4DB", size = 2) +
            labs(x = "Bet Number", y = "Bank") +
            theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank())
    })
}

shinyApp(ui, server)
