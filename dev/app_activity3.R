library(shiny)
library(bslib)
library(ggplot2)

# =========================================================
# UI
# =========================================================

ui <- page_fluid(

    theme = bs_theme(
        version=5,
        bootswatch="minty",
        primary="#7B9ACC",
        bg="#F7F7FB",
        fg="#2E3440",
        base_font=font_google("Inter")
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

            #go { margin-bottom: 10px; }
        "))
    ),

    div(class="main-title",
        h1("🪙 Activity 3: Virtual Coin Toss Bank Game")
    ),

    layout_sidebar(

        sidebar = div(class="card-style",

                      selectInput("bet","Bet Choice",c("H","T")),
                      numericInput("stake","Stake ($)", value = 25, min = 1),

                      actionButton("go","Place Bet", class="btn-primary"),
                      actionButton("reset","Reset Game")
        ),

        div(class="card-style",

            textOutput("clock"),

            hr(),

            div(class="bank-big",
                textOutput("bank_display")
            ),

            hr(),

            plotOutput("bank_plot", height = "220px"),

            hr(),

            h4("Game Log"),
            div(class="log-box",
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
        history = data.frame(step = integer(), bank = numeric()),
        valid_bets = 0
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
        rv$history <- data.frame(step = 0, bank = 25)
        rv$valid_bets <- 0

        updateNumericInput(session, "stake", value = 25)

    }, ignoreInit = TRUE)

    # ---------------------------------------------------------
    # Dynamic stake = current bank
    # ---------------------------------------------------------

    observe({
        req(rv$bank)
        updateNumericInput(session, "stake", value = rv$bank)
    })

    # ---------------------------------------------------------
    # Coin
    # ---------------------------------------------------------

    coin_toss <- function(){
        sample(c("H","T"), 1, prob = c(0.6,0.4))
    }

    # ---------------------------------------------------------
    # Clock (FREEZES properly)
    # ---------------------------------------------------------

    time_remaining <- reactive({

        invalidateLater(1000, session)

        if(!is.null(rv$frozen_time))
            return(rv$frozen_time)

        elapsed <- as.numeric(Sys.time() - rv$start_time, units="secs")
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
    # Betting logic
    # ---------------------------------------------------------

    observeEvent(input$go, {

        req(rv$active)

        stake <- as.numeric(input$stake)
        bet <- input$bet

        # time check
        if(!rv$active) return()

        if(rv$bank <= 0 || rv$bank >= bankmax){
            rv$frozen_time <- time_remaining()
            rv$active <- FALSE
            return()
        }

        if(is.na(stake) || stake <= 0 || stake > rv$bank){
            rv$log <- c(rv$log, "❌ Invalid stake")
            return()
        }

        result <- coin_toss()

        if(result == bet){
            rv$bank <- round(rv$bank + stake, 2)
            rv$log <- c(rv$log,
                        paste0("🟢 ", result, " WIN +$", stake))
        } else {
            rv$bank <- round(rv$bank - stake, 2)
            rv$log <- c(rv$log,
                        paste0("🔴 ", result, " LOSS -$", stake))
        }

        rv$valid_bets <- rv$valid_bets + 1

        rv$history <- rbind(
            rv$history,
            data.frame(step = nrow(rv$history), bank = rv$bank)
        )

        # stopping rules
        if(rv$bank <= 0){
            rv$bank <- 0
            rv$log <- c(rv$log, "💀 Bankrupt")
            rv$frozen_time <- time_remaining()
            rv$active <- FALSE
        }

        if(rv$bank >= bankmax){
            rv$log <- c(rv$log, "🏆 Target reached!")
            rv$frozen_time <- time_remaining()
            rv$active <- FALSE
        }

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

    # ---------------------------------------------------------
    # Plot
    # ---------------------------------------------------------

    output$bank_plot <- renderPlot({

        req(nrow(rv$history) > 1)

        ggplot(rv$history, aes(step, bank)) +
            geom_line(linewidth=1.2, color="#7B9ACC") +
            geom_point(color="#CDB4DB", size=2) +
            labs(x="Bet Number", y="Bank") +
            theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank())
    })

}

shinyApp(ui, server)
