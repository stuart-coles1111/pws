library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(shinyjs)
library(dipsaus)

# =========================================================
# DATA / PARAMETERS
# =========================================================

horse_pics <- paste0("horse", 1:6, ".jpg")

commission_max <- 0.2
allowed_time <- 180
pool_init <- 100
nominal_stake <- 100
nteams <- 10

total_stake <- matrix(0, nr = 6, nc = nteams)
rownames(total_stake) <- c("Red Rum","Secretariat","Seabiscuit",
                           "Shergar","Galileo","Best Mate")
colnames(total_stake) <- paste0("team", 1:nteams)

bank <- matrix(10000, nr = 1, nc = nteams)
colnames(bank) <- paste0("t", 1:nteams)

pool <- matrix(pool_init, nr = 1, nc = 6)
colnames(pool) <- rownames(total_stake)

price <- (sum(pool) + nominal_stake) / (pool + nominal_stake - pool_init)

winnings_init <- rep(0, nteams)

# =========================================================
# RACE SIMULATION
# =========================================================

simulate_race <- function(ratings = c(10, 40, 20, 60, 25, 30)) {
    df <- data.frame(horse = 1:6, ratings = ratings)
    df_left <- df
    place <- c()

    for (i in 1:5) {
        place[i] <- sample(df_left$horse, 1, prob = df_left$ratings)
        df_left <- subset(df_left, horse != place[i])
    }

    c(place, df_left$horse)
}

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
        background: linear-gradient(90deg,#A8DADC,#CDB4DB);
        padding: 20px;
        border-radius: 14px;
        margin-bottom: 20px;
        text-align: center;
      }

      .card-style{
        background: white;
        border-radius: 14px;
        padding: 18px;
        margin-bottom: 18px;
        box-shadow: 0 3px 10px rgba(0,0,0,0.08);
      }

      .clock{
        font-size: 42px;
        font-weight: 700;
        color: #7B9ACC;
        text-align: center;
      }
    "))
    ),

    useShinyjs(),

    div(class = "main-title",
        h1("🏇 Activity: A Day at the Races")
    ),

    layout_sidebar(

        sidebar = div(class = "card-style",

                      h4("Betting Panel"),

                      selectInput("team", "Team",
                                  choices = setNames(1:10, paste("Team", 1:10))),

                      selectInput("horse", "Horse",
                                  choices = setNames(1:6, rownames(total_stake))),

                      numericInput("stake", "Stake ($100s)", 1, 0, 100, 1),

                      br(),

                      fluidRow(
                          column(6,
                                 dipsaus::actionButtonStyled(
                                     "stake_enter", "Enter Stake",
                                     type = "info",
                                     style = "padding:10px;font-size:14px"
                                 )
                          ),
                          column(6,
                                 dipsaus::actionButtonStyled(
                                     "stake_undo", "Undo",
                                     type = "warning",
                                     style = "padding:10px;font-size:14px"
                                 )
                          )
                      ),

                      hr(),

                      div(class = "clock", textOutput("timer"))
        ),

        div(

            div(class = "card-style",
                h3(textOutput("header_text"))
            ),

            div(class = "card-style",
                h4("Pool"),
                DTOutput("pool"),
                br(),
                DTOutput("price")
            ),

            div(class = "card-style",
                h4("Bank"),
                DTOutput("bank")
            ),

            div(class = "card-style",
                h4("Controls"),

                fluidRow(
                    column(4,
                           dipsaus::actionButtonStyled("start_timer", "Open Bets", type = "primary")
                    ),
                    column(4,
                           dipsaus::actionButtonStyled("run_race", "Run Race", type = "success")
                    ),
                    column(4,
                           dipsaus::actionButtonStyled("next_race", "Next Race", type = "danger")
                    )
                )
            ),

            div(class = "card-style",
                h4("Race Result"),
                textOutput("text_win"),
                imageOutput("image")
            ),

            div(class = "card-style",
                h4("Winnings"),
                plotOutput("plot_winnings", height = "200px")
            )

        )
    )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session) {

    counter_race <- reactiveVal(1)

    remaining_time <- reactiveVal(allowed_time)
    timer_active <- reactiveVal(FALSE)

    values <- reactiveValues(
        current_pool = pool,
        current_pool_adj = pool,
        current_bank = bank,
        price = price,
        total_stake = total_stake,
        total_stake_net = total_stake,
        bank_updated = bank,
        winnings = winnings_init,
        undo_status = 1L,
        team_entry = 1L,
        horse_entry = 1L,
        stake_entry = 0L,
        commission_entry = 0L
    )

    # =========================================================
    # TIMER (FIXED)
    # =========================================================

    output$timer <- renderText({
        paste0(remaining_time(), " s")
    })

    observe({
        invalidateLater(1000, session)
        isolate({
            if (timer_active()) {
                remaining_time(remaining_time() - 1)

                if (remaining_time() <= 0) {
                    remaining_time(0)
                    timer_active(FALSE)
                }
            }
        })
    })

    observeEvent(input$start_timer, {
        timer_active(TRUE)
        remaining_time(allowed_time)
    })

    # =========================================================
    # HEADER
    # =========================================================

    output$header_text <- renderText({
        paste("Race number", counter_race())
    })

    # =========================================================
    # BETTING
    # =========================================================

    observeEvent(input$stake_enter, {

        team <- as.numeric(input$team)
        horse <- as.numeric(input$horse)
        stake <- input$stake * 100

        values$team_entry <- team
        values$horse_entry <- horse
        values$stake_entry <- stake
        values$undo_status <- 0L

        if (values$bank_updated[team] < stake) {
            showModal(modalDialog("Bet rejected: insufficient funds"))
            values$undo_status <- 1L
            return()
        }

        values$total_stake[horse, team] <- values$total_stake[horse, team] + stake
        values$current_pool[horse] <- values$current_pool[horse] + stake
        values$bank_updated[team] <- values$bank_updated[team] - stake

        values$price <- (sum(values$current_pool) + nominal_stake) /
            (values$current_pool + nominal_stake - pool_init)
    })

    observeEvent(input$stake_undo, {

        if (values$undo_status == 1L) {
            showModal(modalDialog("Nothing to undo"))
            return()
        }

        team <- values$team_entry
        horse <- values$horse_entry
        stake <- values$stake_entry

        values$total_stake[horse, team] <- values$total_stake[horse, team] - stake
        values$current_pool[horse] <- values$current_pool[horse] - stake
        values$bank_updated[team] <- values$bank_updated[team] + stake
    })

    # =========================================================
    # TABLES
    # =========================================================

    output$pool <- DT::renderDT({
        datatable(values$current_pool, options = list(dom = "t"))
    })

    output$price <- DT::renderDT({
        datatable(round(values$price, 1), options = list(dom = "t"))
    })

    output$bank <- DT::renderDT({
        datatable(values$bank_updated, options = list(dom = "t"))
    })

    # =========================================================
    # RACE
    # =========================================================

    observeEvent(input$run_race, {

        winner <- simulate_race()[1]

        output$text_win <- renderText({
            paste("Winning horse:", rownames(total_stake)[winner])
        })

        output$image <- renderImage({
            list(
                src = file.path("www", paste0("horse", winner, ".jpg")),
                contentType = "image/jpg"
            )
        }, deleteFile = FALSE)

        values$winnings <- rep(0, nteams)
    })

    # =========================================================
    # PLOT
    # =========================================================

    output$plot_winnings <- renderPlot({
        ggplot(
            data.frame(team = 1:nteams, winnings = values$winnings),
            aes(team, winnings)
        ) +
            geom_col(fill = "#CDB4DB") +
            geom_hline(yintercept = 0, color = "red") +
            theme_minimal(base_size = 12)
    })

}

shinyApp(ui, server)
