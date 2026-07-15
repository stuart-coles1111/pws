suppressPackageStartupMessages({
    library(shiny)
    library(shinydashboard)
    library(shinyjs)
    library(DT)
    library(ggplot2)
    library(dplyr)
    library(bslib)
    library(shinyWidgets)
})

# =========================================================
# GLOBALS
# =========================================================

horse_pics <- paste0("horse", 1:6, ".jpg")

#commission_max <- 0.2
commission_max <- 0
allowed_time <- 30

bet_rate <- 5

bet_proportion <- 0.1

pool_init <- 100
nominal_stake <- 100

winnings_init <- rep(0, 10)
seed = 1

# =========================================================
# RACE SIMULATION
# =========================================================

simulate_race <- function(ratings = c(10, 40, 20, 60, 25, 30)) {

    df <- data.frame(
        horse = 1:6,
        ratings = ratings
    )

    df_left <- df
    place <- c()

    for (i in 1:5) {

        place[i] <- sample(
            df_left$horse,
            1,
            prob = df_left$ratings
        )

        df_left <- subset(
            df_left,
            horse != place[i]
        )
    }

    place <- c(place, df_left$horse)

    Sys.sleep(5)

    return(place)
}

# =========================================================
# INITIALISE STAKES / BANK / POOL
# =========================================================

nteams <- 10

total_stake <- matrix(
    as.integer(0),
    nr = 6,
    nc = nteams
)

rownames(total_stake) <- c(
    "1: Red Rum",
    "2: Secretariat",
    "3: Seabiscuit",
    "4: Shergar",
    "5: Galileo",
    "6: Best Mate"
)

colnames(total_stake) <- paste0("team", 1:nteams)

bank <- matrix(
    as.integer(10000),
    nr = 1,
    nc = nteams
) %>%
    as.data.frame()

colnames(bank) <- paste0("t", 1:nteams)

pool <- matrix(
    as.integer(pool_init),
    nr = 1,
    nc = 6
) %>%
    as.data.frame()

colnames(pool) <- c(
    "Red Rum",
    "Secretariat",
    "Seabiscuit",
    "Shergar",
    "Galileo",
    "Best Mate"
)

price <- (
    sum(pool) + nominal_stake
) / (
    pool + nominal_stake - pool_init
)

winnings <- data.frame()

# =========================================================
# UI
# =========================================================

ui <- page_navbar(

    title = "🏇 A Day at the Races",
    header = tagList(

        useShinyjs(),

        tags$head(

            tags$style(HTML("

            .main-title{
                background: linear-gradient(90deg,#A8DADC,#CDB4DB);
                padding: 28px;
                border-radius: 18px;
                margin-bottom: 25px;
                text-align: center;
                box-shadow: 0 4px 12px rgba(0,0,0,0.08);
            }

            .main-title h1{
                font-weight: 700;
                color: #1D3557;
                font-size: 32px;
                margin: 0;
            }

            .main-title p{
                font-size: 18px;
                color: #3D405B;
                margin-top: 6px;
            }

            body{
                background:#F7F7FB;
            }

        "))
        ),

        div(
            class = "main-title",
            h1("🏇 A Day at the Races")
        )
    ),

    overview_page(
        explanation = tagList(
            p("This activity explores betting markets and probabilistic outcomes in horse racing.")
        ),
        individual = tagList(
            tags$ol(
                tags$li("Place stakes on horses."),
                tags$li("Observe race outcomes."),
                tags$li("Track performance over time.")
            )
        ),
        group = tagList(
            tags$ol(
                tags$li("Compare strategies across participants."),
                tags$li("Analyse risk and reward."),
                tags$li("Discuss randomness in outcomes.")
            )
        ),
        question = tagList(
            tags$ul(
                tags$li("Can strategies beat randomness?"),
                tags$li("What drives long-term winnings?"),
                tags$li("Is success skill or luck?")
            )
        )
    ),

    nav_panel(
        "Activity",

        # 👇 THIS is where your OLD app goes (next step)
        ui <- shinydashboard::dashboardPage(

            skin = "green",

            header = shinydashboard::dashboardHeader(
                title = "Settings",
                titleWidth = 200
            ),

            sidebar = shinydashboard::dashboardSidebar(

                width = 200,

                shiny::tags$head(

                    bslib::bs_theme_dependencies(

                        bslib::bs_theme(
                            version = 5,
                            bootswatch = "minty",
                            primary = "#7B9ACC",
                            bg = "#F7F7FB",
                            fg = "#2E3440"
                        )
                    ),

                    shiny::tags$style(shiny::HTML("

body {
  background-color:#F7F7FB;
}

.content-wrapper, .right-side {
  background-color:#F7F7FB;
}

.main-header .logo,
.main-header .navbar {
    height: 50px !important;
    line-height: 50px !important;
}

.main-header .logo {
    background-color:#7B9ACC !important;
    color:white !important;
    font-weight:700;
}

.main-header .navbar {
    background-color:#7B9ACC !important;
}

.main-sidebar {
  background: linear-gradient(180deg,#A8DADC,#CDB4DB);
}

.sidebar-menu > li > a {
  color:#2E3440 !important;
  font-weight:600;
}

.box {
  border-radius:16px !important;
  border:none !important;
  box-shadow:0 4px 12px rgba(0,0,0,0.08);
  background:white;
}

.btn {
  border-radius:10px !important;
  font-weight:600;
  border:none !important;
}

.dataTables_wrapper {
  font-size:15px;
}

table.dataTable {
  border-radius:12px;
  overflow:hidden;
}

.race-banner {
  background: linear-gradient(90deg,#E76F51,#F4A261);
  color:white;
  font-size:28px;
  font-weight:700;
  text-align:center;
  padding:18px;
  border-radius:14px;
  margin-bottom:20px;
  box-shadow:0 4px 12px rgba(0,0,0,0.12);
}

.sidebar .btn {
    width: 75% !important;
    margin-left: auto;
    margin-right: auto;
    display: block;
}

.sidebar .form-control {
    width: 75% !important;
    margin-left: auto;
    margin-right: auto;
}



.box {
    overflow: visible !important;
}

.box-body {
    padding-bottom: 15px !important;
}

.box-header .box-title {
    font-weight: 700 !important;
    font-size: 16px;
}

.shiny-plot-output {
    height: auto !important;
}

/* Coloured betting checkboxes */

input[type=checkbox] {
    accent-color: #7B9ACC;
    width: 18px;
    height: 18px;
}

.checkbox label {
    font-size: 16px;
    font-weight: 600;
    color: #2E3440;
}

.checkbox input {
    margin-right: 8px;
}

/* Betting radio button groups */

.btn-group .btn {
    background-color: white !important;
    color: #2E3440 !important;
    border: 2px solid #7B9ACC !important;
    font-weight: 600 !important;
}

.btn-group .btn:hover {
    background-color: #A8DADC !important;
    color: #2E3440 !important;
}

.btn-group .btn.active,
.btn-group .btn.active:hover {
    background-color: #7B9ACC !important;
    color: white !important;
    border-color: #7B9ACC !important;
}

/* Betting panel headings */

h4 {
    color: #2E3440;
    font-weight: 700;
}

.btn-group .btn {
    margin-right: 4px;
    border-radius: 8px !important;
}

"))
                ),

                br(),

                selectInput(
                    "game_mode",
                    "Mode",
                    choices = c(
                        "Multiplayer" = "multi",
                        "Single Player" = "single"
                    ),
                    selected = "multi",
                    width = "90%"
                ),

                br(),

                sliderInput(
                    "commission_max",
                    "Maximum Commission",
                    min = 0,
                    max = 0.5,
                    value = 0.2,
                    step = 0.05,
                    width = "90%"
                ),

                sliderInput(
                    "allowed_time",
                    "Allowed Time (seconds)",
                    min = 30,
                    max = 300,
                    value = 180,
                    step = 30,
                    width = "90%"
                ),

                div(
                    style = "text-align:center;",

                    h4(
                        textOutput("header_text"),
                        style="
                color:#2E3440;
                font-weight:600;
                margin-bottom:10px;
            "
                    ),

                    h3(
                        textOutput("timer"),
                        style="
                color:#7B9ACC;
                font-weight:700;
                margin-bottom:20px;
            "
                    )
                ),

                hr(),

                uiOutput("sidebar_buttons")

            ),

            body = shinydashboard::dashboardBody(

                shinyjs::useShinyjs(),

                shinydashboard::tabBox(
                    id = "inTabset",
                    width = 12,

                    # =====================================================
                    # TAB 1
                    # =====================================================

                    shiny::tabPanel(
                        "Pool",
                        value = "panel1",

                        uiOutput("pool_page")
                    ),

                    # =====================================================
                    # TAB 2
                    # =====================================================

                    shiny::tabPanel(

                        "Race",
                        value = "panel2",

                        shinydashboard::box(
                            width = 12,
                            title = "Race Result",
                            status = "warning",

                            shiny::fluidRow(

                                shiny::column(
                                    5,

                                    shiny::h2(
                                        shiny::textOutput("text_win"),
                                        style = "
                color:#7B9ACC;
                font-weight:700;
                margin-top:20px;"
                                    )
                                ),

                                shiny::column(
                                    5,
                                    offset = 1,
                                    shiny::imageOutput("image")
                                )
                            ),

                            shiny::br(),

                            shiny::fluidRow(
                                shiny::column(
                                    10,
                                    offset = 1,

                                    shiny::plotOutput(
                                        "plot_winnings",
                                        height = "35vh"
                                    )
                                )
                            ),
                            shiny::br(),

                            shiny::fluidRow(

                                shiny::column(
                                    4,
                                    offset = 4,

                                    dipsaus::actionButtonStyled(
                                        inputId = "return_pool",
                                        label = "Return to Pool",
                                        type = "primary"
                                    )
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

server <- function(input, output, session) {

    counter_race <- shiny::reactiveVal(value = 1)

    race_run <- shiny::reactiveVal(value = 0)

    # =======================================================
    # TIMER
    # =======================================================

    remaining_time <- shiny::reactiveVal(300)

    timer_active <- shiny::reactiveVal(FALSE)

    output$timer <- shiny::renderText({
        paste0(remaining_time(), " s")
    })

    shiny::observe({

        shiny::invalidateLater(1000, session)

        shiny::isolate({

            if (timer_active()) {

                remaining_time(remaining_time() - 1)

                if (remaining_time() < 1) {

                    timer_active(FALSE)
                    betting_closed(TRUE)

                    shinyjs::enable("run_race")

                    shiny::insertUI(
                        selector = "#run_race",
                        where = "afterEnd",

                        ui = shiny::tags$audio(
                            src = "bell.mp3",
                            type = "audio/mp3",
                            autoplay = TRUE,
                            controls = NA,
                            style = "display:none;"
                        ),

                        immediate = TRUE
                    )

                    shiny::showModal(
                        shiny::modalDialog(
                            title = "Pool message",
                            "No more bets!"
                        )
                    )
                }
            }
        })
    })

    shiny::observeEvent(input$start_timer, {

        # Open betting
        betting_open(TRUE)
        betting_closed(FALSE)
        timer_active(TRUE)

        remaining_time(input$allowed_time)

        shinyjs::disable("run_race")
        shinyjs::disable("game_mode")


        # Generate automated bets in single player mode
        if (input$game_mode == "single") {

            nbets <- rpois(1, bet_rate) + nteams - 1

            bet_times <- sample(
                1:(input$allowed_time - 1),
                nbets,
                replace = TRUE
            )

            bet_team_ind <- sample(
                1:nbets,
                nteams - 1,
                replace = FALSE
            )

            bet_team <- rep(NA, nbets)

            # Automated teams are 2:n
            bet_team[bet_team_ind] <- 2:nteams

            rem_ind <- setdiff(
                1:nbets,
                bet_team_ind
            )

            rem_team <- sample(
                2:nteams,
                length(rem_ind),
                replace = TRUE
            )

            bet_team[rem_ind] <- rem_team

            bet_horse <- sample(
                1:6,
                nbets,
                replace = TRUE
            )

            bets_df <- data.frame(
                bet_times = bet_times,
                bet_team = bet_team,
                bet_horse = bet_horse
            )

            bets_df <- dplyr::arrange(
                bets_df,
                bet_times
            )

            bets_df$proportions <- runif(
                nbets,
                0,
                bet_proportion
            )

            values$bets_df <- bets_df

        }

    })


    observe({

        req(input$game_mode == "single")

        bets <- values$bets_df

        if (nrow(bets) == 0)
            return()

        bet_times <- unique(
            bets$bet_times
        )

        bet_times_remaining <-
            input$allowed_time - bet_times

        if (any(
            bet_times_remaining ==
            remaining_time()
        )) {

            idx <- which(
                bet_times_remaining ==
                    remaining_time()
            )

            team <- as.numeric(
                bets[idx, "bet_team"]
            )

            horse <- as.numeric(
                bets[idx, "bet_horse"]
            )

            real_stake <- round(
                as.numeric(
                    values$bank_updated[team] *
                        bets[idx, "proportions"]
                ),
                0
            )

            values$total_stake[
                horse,
                team
            ] <-

                values$total_stake[
                    horse,
                    team
                ] +

                real_stake

            values$current_pool[horse] <-

                values$current_pool[horse] +

                real_stake

            values$bank_updated[team] <-

                values$bank_updated[team] -

                real_stake

            values$price <-

                (
                    sum(values$current_pool) +
                        nominal_stake
                ) /

                (
                    values$current_pool +
                        nominal_stake -
                        pool_init
                )

            commission <-

                (
                    input$allowed_time -
                        remaining_time()
                ) /

                input$allowed_time *

                input$commission_max

            values$current_pool_adj[horse] <-

                values$current_pool_adj[horse] +

                round(
                    real_stake *
                        (1 - commission),
                    -2
                )

            values$total_stake_net[
                horse,
                team
            ] <-

                values$total_stake_net[
                    horse,
                    team
                ] +

                round(
                    real_stake *
                        (1 - commission),
                    -2
                )

            values$bets_df <-
                values$bets_df[-idx, ]
        }

    })

    observeEvent(input$return_pool, {

        updateTabsetPanel(
            session,
            "inTabset",
            selected = "panel1"
        )

    })

    # =======================================================
    # REACTIVE VALUES
    # =======================================================

    betting_open <- reactiveVal(FALSE)

    betting_closed <- reactiveVal(FALSE)

    values <- shiny::reactiveValues(

        current_pool = pool,
        current_pool_adj = pool,

        current_bank = bank,

        price = price,

        total_stake = total_stake,
        total_stake_net = total_stake,

        bank_updated = bank,

        racing_now = 0L,

        winnings = winnings_init,

        stake_text = "",
        error_text = "",

        team_entry = 1L,
        horse_entry = 1L,
        stake_entry = 0L,

        selected_team = NULL,
        selected_horse = NULL,
        selected_stake = 1L,

        commission_entry = 0L,

        undo_status = 1L,

        bets_df = data.frame()
    )


    # =======================================================
    # REACTIVE TABLE DATA
    # =======================================================

    currentBank <- shiny::reactive({

        data.frame(
            team = 1:nteams,
            bank = values$bank_updated %>% as.numeric()
        )
    })

    currentPrice <- shiny::reactive({

        data.frame(
            horse = 1:6,
            price = values$price %>% as.numeric()
        )
    })

    currentWinnings <- shiny::reactive({

        shiny::validate(
            shiny::need(values$racing_now == 1, "")
        )

        data.frame(
            team = 1:nteams,
            winnings = values$winnings %>% as.numeric()
        )
    })

    observe({

        if (betting_closed()) {

            shinyjs::disable("stake_enter")
            shinyjs::disable("stake_undo")

        } else {

            shinyjs::enable("stake_enter")
            shinyjs::enable("stake_undo")

        }

    })
    # =======================================================
    # ENTER STAKE
    # =======================================================


    shiny::observeEvent(input$stake_enter, {

        team <- as.numeric(input$selected_team)

        horse <- as.numeric(input$selected_horse)

        if (input$selected_stake == "Custom") {

            if (is.null(input$custom_stake) ||
                input$custom_stake <= 0) {

                shiny::showModal(
                    shiny::modalDialog(
                        title = "Pool message",
                        "Please enter a valid custom stake."
                    )
                )

                return()

            }

            stake <- input$custom_stake

        } else {

            stake <- as.numeric(input$selected_stake)

        }

        real_stake <- stake * 100

        values$team_entry <- team

        values$horse_entry <- horse

        values$stake_entry <- real_stake

        values$undo_status <- 0L

        new_bank <- values$bank_updated[team] - real_stake

        if (new_bank < 0) {

            values$undo_status <- 1L

            shiny::showModal(
                shiny::modalDialog(
                    title = "Pool message",
                    "Negative Bank: Bet declined"
                )
            )

        } else {

            values$total_stake[horse, team] <-
                values$total_stake[horse, team] + real_stake

            values$current_pool[horse] <-
                values$current_pool[horse] + real_stake

            values$bank_updated[team] <-
                values$bank_updated[team] - real_stake

            values$price <-
                (
                    sum(values$current_pool) + nominal_stake
                ) / (
                    values$current_pool + nominal_stake - pool_init
                )

            commission <-

                (
                    input$allowed_time -
                        remaining_time()
                ) /
                input$allowed_time *
                input$commission_max

            values$commission_entry <- commission

            values$current_pool_adj[horse] <-
                values$current_pool_adj[horse] +
                round(real_stake * (1 - commission), -2)

            values$total_stake_net[horse, team] <-
                values$total_stake_net[horse, team] +
                round(real_stake * (1 - commission), -2)
        }
    })

    # =======================================================
    # UNDO STAKE
    # =======================================================

    shiny::observeEvent(input$stake_undo, {

        if (values$undo_status == 1L) {

            shiny::showModal(
                shiny::modalDialog(
                    title = "Pool message",
                    "No stake entered"
                )
            )

        } else {

            values$undo_status <- 1L

            team <- values$team_entry
            horse <- values$horse_entry
            stake <- values$stake_entry
            commission <- values$commission_entry

            values$total_stake[horse, team] <-
                values$total_stake[horse, team] - stake

            values$current_pool[horse] <-
                values$current_pool[horse] - stake

            values$bank_updated[team] <-
                values$bank_updated[team] + stake

            values$price <-
                (
                    sum(values$current_pool) + nominal_stake
                ) / (
                    values$current_pool + nominal_stake - pool_init
                )

            values$current_pool_adj[horse] <-
                values$current_pool_adj[horse] -
                round(stake * (1 - commission), -2)

            values$total_stake_net[horse, team] <-
                values$total_stake_net[horse, team] -
                round(stake * (1 - commission), -2)
        }
    })

    output$pool_page <- renderUI({

        tagList(

            h2(
                paste("Race number", counter_race()),
                style = "
                font-weight:700;
                color:#7B9ACC;
                margin-bottom:20px;"
            ),

            div(id = "race_banner_placeholder"),

            if (!betting_open()) {

                tagList(

                    # ==================================================
                    # TOP ROW: PLOTS (always visible)
                    # ==================================================

                    fluidRow(

                        column(
                            4,

                            box(
                                width = 12,
                                plotOutput("plot_pool", height = "320px")
                            )
                        ),

                        column(
                            4,

                            box(
                                width = 12,
                                plotOutput("plot_price", height = "320px")
                            )
                        ),

                        column(
                            4,

                            box(
                                width = 12,
                                plotOutput("plot_bank", height = "320px")
                            )
                        )
                    ),

                    # ==================================================
                    # BOTTOM ROW: TABLES
                    # ==================================================

                    fluidRow(

                        column(
                            4,

                            box(
                                width = 12,
                                title = "Pool",
                                DTOutput("pool")
                            )
                        ),

                        column(
                            4,

                            box(
                                width = 12,
                                title = "Prices",
                                DTOutput("price")
                            )
                        ),

                        column(
                            4,

                            box(
                                width = 12,
                                title = "Bank",
                                DTOutput("bank")
                            )
                        )
                    )
                )


            } else {

                tagList(

                    # ==================================================
                    # TOP ROW: PLOTS (same as above)
                    # ==================================================

                    fluidRow(

                        column(
                            4,

                            box(
                                width = 12,
                                plotOutput("plot_pool", height = "320px")
                            )
                        ),

                        column(
                            4,

                            box(
                                width = 12,
                                plotOutput("plot_price", height = "320px")
                            )
                        ),

                        column(
                            4,

                            box(
                                width = 12,
                                plotOutput("plot_bank", height = "320px")
                            )
                        )
                    ),


                    # ==================================================
                    # BOTTOM ROW: BETTING PANEL
                    # ==================================================

                    fluidRow(

                        column(
                            8,
                            offset = 2,

                            box(
                                width = 12,
                                title = "Betslip Entry",
                                status = "primary",

                                div(
                                    style = "
                    text-align:center;
                    padding:10px 20px;
                ",

                                    uiOutput("betting_panel"),

                                    br(),

                                    div(
                                        style = "
                        font-size:18px;
                        font-weight:600;
                        color:#2E3440;
                    ",
                                        textOutput("current_bet")
                                    ),

                                    br(),

                                    fluidRow(

                                        column(
                                            6,

                                            dipsaus::actionButtonStyled(
                                                "stake_enter",
                                                "Enter Stake",
                                                type = "primary"
                                            )
                                        ),

                                        column(
                                            6,

                                            dipsaus::actionButtonStyled(
                                                "stake_undo",
                                                "Undo Stake",
                                                type = "warning"
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            }
        )
    })
    # =======================================================
    # TABLES
    # =======================================================

    output$betting_panel <- renderUI({

        tagList(

            if (input$game_mode == "single") {

                tagList(

                    h4("Team"),

                    div(
                        style = "
                    background:#A8DADC;
                    padding:12px;
                    border-radius:10px;
                    font-weight:700;
                    color:#2E3440;
                    text-align:center;
                    ",

                        "Single Player Mode: You are Team 1"
                    ),

                    # hidden input to keep server logic unchanged
                    div(
                        style = "display:none;",

                        shinyWidgets::radioGroupButtons(
                            inputId = "selected_team",
                            choices = 1,
                            selected = 1,
                            justified = FALSE,
                            size = "sm"
                        )
                    )

                )

            } else {

                tagList(

                    h4("Team"),

                    shinyWidgets::radioGroupButtons(
                        inputId = "selected_team",
                        choices = 1:10,
                        justified = FALSE,
                        size = "sm"
                    )

                )
            },


            br(),

            h4("Horse"),

            shinyWidgets::radioGroupButtons(
                inputId = "selected_horse",
                choices = c(
                    "Red Rum" = 1,
                    "Secretariat" = 2,
                    "Seabiscuit" = 3,
                    "Shergar" = 4,
                    "Galileo" = 5,
                    "Best Mate" = 6
                ),
                justified = FALSE,
                size = "sm"
            ),


            br(),

            h4("Stake ($100s)"),

            shinyWidgets::radioGroupButtons(
                inputId = "selected_stake",
                choices = c(
                    1,
                    5,
                    10,
                    20,
                    50,
                    "Custom"
                ),
                selected = 1,
                justified = TRUE,
                size = "sm"
            ),


            conditionalPanel(
                condition = "input.selected_stake == 'Custom'",

                numericInput(
                    "custom_stake",
                    NULL,
                    value = 1,
                    min = 1
                )
            ),


            br(),

            textOutput("current_bet")
        )

    })

    output$pool <- DT::renderDT({

        pool_df <- data.frame(
            Horse = c(
                "Red Rum",
                "Secretariat",
                "Seabiscuit",
                "Shergar",
                "Galileo",
                "Best Mate"
            ),
            Pool = as.numeric(values$current_pool)
        )

        DT::datatable(
            pool_df,
            rownames = FALSE,
            options = list(
                dom = "t",
                paging = FALSE
            )
        )
    })

    output$price <- DT::renderDT({

        price_df <- data.frame(
            Horse = c(
                "Red Rum",
                "Secretariat",
                "Seabiscuit",
                "Shergar",
                "Galileo",
                "Best Mate"
            ),
            Price = round(as.numeric(values$price), 1)
        )

        DT::datatable(
            price_df,
            rownames = FALSE,
            options = list(
                dom = "t",
                paging = FALSE
            )
        )
    })

    output$bank <- DT::renderDT({

        bank_df <- data.frame(
            Team = paste0("Team ", 1:10),
            Bank = as.numeric(values$bank_updated)
        )

        DT::datatable(
            bank_df,
            rownames = FALSE,
            options = list(
                dom = "t",
                paging = FALSE
            )
        )
    })

    output$sidebar_buttons <- renderUI({

        if (!betting_open() && race_run() == 0) {

            dipsaus::actionButtonStyled(
                inputId = "start_timer",
                label = "Open Bets",
                type = "primary",
                width = "100%"
            )

        } else if (betting_open()) {

            dipsaus::actionButtonStyled(
                inputId = "run_race",
                label = "Run Race",
                type = "success",
                width = "100%"
            )

        } else {

            dipsaus::actionButtonStyled(
                inputId = "next_race",
                label = "Next Race",
                type = "danger",
                width = "100%"
            )

        }

    })

    # =======================================================
    # RUN RACE
    # =======================================================

    shiny::observeEvent(input$run_race, {

        race_run(1)

        betting_open(FALSE)

        values$racing_now <- 1

        # SHOW BANNER
        shiny::insertUI(
            selector = "#race_banner_placeholder",
            where = "afterBegin",

            ui = shiny::div(
                id = "race_banner_active",
                class = "race-banner",
                "🏇 Race in Progress..."
            ),

            immediate = TRUE
        )

        # PLAY AUDIO
        shiny::insertUI(
            selector = "#run_race",
            where = "afterEnd",

            ui = shiny::tags$audio(
                src = "race.mp3",
                type = "audio/mp3",
                autoplay = TRUE,
                controls = NA,
                style = "display:none;"
            ),

            immediate = TRUE
        )

        # force browser repaint
        Sys.sleep(0.25)

        # RUN RACE
        winner <- simulate_race()[1]

        # REMOVE BANNER
        shiny::removeUI(
            selector = "#race_banner_active",
            immediate = TRUE
        )

        # SWITCH TAB
        shiny::updateTabsetPanel(
            session,
            "inTabset",
            selected = "panel2"
        )

        output$text_win <- shiny::renderText({

            paste(
                "Winning horse is",
                rownames(total_stake)[winner]
            )
        })

        output$image <- shiny::renderImage({

            list(
                src = file.path("www", horse_pics[winner]),
                contentType = "image/jpeg",
                width = "80%",
                alt = "Winning horse"
            )

        }, deleteFile = FALSE)

        total_pool <- sum(values$current_pool_adj)

        total_pool_winner <- values$current_pool_adj[winner]

        if (total_pool_winner == pool_init) {

            values$winnings <- rep(0, nteams)

            values$bank_updated <-
                values$bank_updated +
                apply(values$total_stake, 2, sum)

        } else {

            winnings <-

                total_pool *
                values$total_stake_net[winner, ] /
                sum(values$total_stake_net[winner, ]) -

                apply(values$total_stake, 2, sum)

            values$winnings <- round(winnings, -2)

            values$bank_updated <-
                values$bank_updated +
                values$winnings +
                apply(values$total_stake, 2, sum)
        }

        values$undo_status <- 1L

        shinyjs::runjs("
          $('a[data-value=\"panel2\"]').tab('show');
        ")
    })

    # =======================================================
    # NEXT RACE
    # =======================================================

    shiny::observeEvent(input$next_race, {

        race_run(0)
        shinyjs::enable("game_mode")

        betting_open(FALSE)
        betting_closed(FALSE)

        tmp0 <- counter_race()

        counter_race(tmp0 + 1)

        values$current_pool <- pool

        values$current_pool_adj <- pool

        values$total_stake <- total_stake

        values$total_stake_net <- total_stake

        values$price <- (
            sum(pool) + nominal_stake
        ) / (
            pool + nominal_stake - pool_init
        )

        output$text_win <- NULL

        output$image <- NULL

        values$winnings <- winnings_init

        values$bets_df <- data.frame()
    })

    observeEvent(input$run_race, {

        shinyjs::disable("game_mode")

    })

    # =======================================================
    # PLOTS
    # =======================================================

    output$plot_price <- shiny::renderPlot({

        ggplot2::ggplot(

            data = currentPrice(),

            ggplot2::aes(
                x = factor(horse),
                y = price,
                fill = price
            )
        ) +

            ggplot2::geom_col(
                width = 0.65,
                alpha = 0.9
            ) +

            ggplot2::geom_text(

                ggplot2::aes(
                    label = round(price, 1)
                ),

                vjust = -0.4,

                size = 6,

                fontface = "bold",

                color = "#2E3440"
            ) +

            scale_fill_gradient(
                low = "#CDB4DB",
                high = "#6D597A"
            ) +

            ggplot2::labs(
                title = "Notional Prices",
                x = "Horse",
                y = "Price"
            ) +

            ggplot2::scale_y_continuous(
                expand = ggplot2::expansion(mult = c(0, 0.12))
            ) +

            ggplot2::theme_minimal(base_size = 15) +

            ggplot2::theme(
                legend.position = "none",

                plot.title = ggplot2::element_text(
                    face = "bold",
                    size = 20
                ),

                axis.title = ggplot2::element_text(
                    face = "bold",
                    size = 20
                ),

                axis.text = ggplot2::element_text(
                    size = 18
                ),

                panel.grid.minor =
                    ggplot2::element_blank()
            )

    }, height = 320)

    output$plot_bank <- shiny::renderPlot({

        ggplot2::ggplot(

            data = currentBank(),

            ggplot2::aes(
                x = factor(team),
                y = bank,
                fill = bank
            )
        ) +

            ggplot2::geom_col(
                width = 0.7,
                alpha = 0.9
            ) +

            ggplot2::geom_text(

                ggplot2::aes(
                    label = round(bank / 1000, 1)
                ),

                vjust = -0.4,

                size = 5,

                fontface = "bold"
            ) +

            scale_fill_gradient(
                low = "#B7E4C7",
                high = "#40916C"
            ) +

            ggplot2::labs(
                title = "Team Bank",
                x = "Team",
                y = "Bank"
            ) +

            ggplot2::scale_y_continuous(
                expand = ggplot2::expansion(mult = c(0, 0.12))
            ) +

            ggplot2::theme_minimal(base_size = 15) +

            ggplot2::theme(
                legend.position = "none",

                plot.title = ggplot2::element_text(
                    face = "bold",
                    size = 20
                ),

                axis.title = ggplot2::element_text(
                    face = "bold",
                    size = 20
                ),

                axis.text = ggplot2::element_text(
                    size = 18
                ),

                panel.grid.minor =
                    ggplot2::element_blank()
            )

    }, height = 320)

    output$plot_pool <- shiny::renderPlot({

        pool_df <- data.frame(
            horse = factor(1:6),
            pool = as.numeric(values$current_pool)
        )

        ggplot2::ggplot(
            pool_df,
            ggplot2::aes(
                x = horse,
                y = pool,
                fill = pool
            )
        ) +

            ggplot2::geom_col(
                width = 0.7,
                alpha = 0.9
            ) +

            ggplot2::geom_text(
                ggplot2::aes(
                    label = round(pool / 1000, 1)
                ),
                vjust = -0.4,
                size = 5,
                fontface = "bold",
                color = "#2E3440"
            ) +

            scale_fill_gradient(
                low = "#A8DADC",
                high = "#457B9D"
            ) +

            ggplot2::labs(
                title = "Current Pool",
                x = "Horse",
                y = "Pool"
            ) +

            ggplot2::scale_y_continuous(
                expand = ggplot2::expansion(mult = c(0, 0.15))
            ) +

            ggplot2::theme_minimal(base_size = 15) +

            ggplot2::theme(
                legend.position = "none",

                plot.title = ggplot2::element_text(
                    face = "bold",
                    size = 20
                ),

                axis.title = ggplot2::element_text(
                    face = "bold",
                    size = 20
                ),

                axis.text = ggplot2::element_text(
                    size = 18
                ),

                panel.grid.minor =
                    ggplot2::element_blank()
            )

    }, height = 320)

    output$plot_winnings <- shiny::renderPlot({

        ggplot2::ggplot(

            data = currentWinnings(),

            ggplot2::aes(
                x = factor(team),
                y = winnings,
                fill = winnings > 0
            )
        ) +

            ggplot2::geom_col(
                width = 0.65,
                alpha = 0.9
            ) +

            ggplot2::geom_hline(
                yintercept = 0,
                colour = "#2E3440",
                linewidth = 1
            ) +

            ggplot2::scale_fill_manual(
                values = c(
                    "TRUE" = "#7B9ACC",
                    "FALSE" = "#E76F51"
                )
            ) +

            ggplot2::labs(
                title = "Race Winnings",
                x = "Team",
                y = "Winnings"
            ) +

            ggplot2::theme_minimal(base_size = 15) +

            ggplot2::theme(
                legend.position = "none",
                plot.title = ggplot2::element_text(
                    face = "bold",
                    size = 20
                ),
                panel.grid.minor =
                    ggplot2::element_blank()
            )

    }, height = 300)
}


# =========================================================
# RUN APP
# =========================================================

shiny::shinyApp(ui, server)
