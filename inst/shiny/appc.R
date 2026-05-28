library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(ggplot2)
library(dplyr)
library(bslib)

# =========================================================
# GLOBALS
# =========================================================

horse_pics <- paste0("horse", 1:6, ".jpg")

commission_max <- 0.2
allowed_time <- 5
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

ui <- shinydashboard::dashboardPage(

    skin = "green",

    header = shinydashboard::dashboardHeader(
        title = "🏇 A day at the races"
    ),

    sidebar = shinydashboard::dashboardSidebar(

        width = 300,

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

        #timer {
          font-size:500%;
          font-weight:700;
          color:#7B9ACC;
        }

        .dataTables_wrapper {
          font-size:15px;
        }

        table.dataTable {
          border-radius:12px;
          overflow:hidden;
        }

      "))
        ),

        shiny::br(),

        shiny::selectInput(
            "team",
            "Team",
            choices = list(
                "Team 1" = 1,
                "Team 2" = 2,
                "Team 3" = 3,
                "Team 4" = 4,
                "Team 5" = 5,
                "Team 6" = 6,
                "Team 7" = 7,
                "Team 8" = 8,
                "Team 9" = 9,
                "Team 10" = 10
            ),
            width = "80%"
        ),

        shiny::selectInput(
            "horse",
            "Horse",
            choices = list(
                "1: Red Rum" = 1,
                "2: Secretariat" = 2,
                "3: Seabiscuit" = 3,
                "4: Shergar" = 4,
                "5: Galileo" = 5,
                "6: Best Mate" = 6
            ),
            width = "80%"
        ),

        shiny::numericInput(
            "stake",
            "Stake ($100's)",
            1,
            0,
            100,
            1,
            width = "80%"
        ),

        shiny::br(),

        shiny::fluidRow(

            shiny::column(
                5,
                align = "center",

                dipsaus::actionButtonStyled(
                    inputId = "stake_enter",
                    label = "Enter Stake",
                    type = "info",
                    class = "btn-sm",
                    style = "padding:10px; font-size:140%"
                )
            ),

            shiny::column(
                5,
                align = "center",
                offset = 1,

                dipsaus::actionButtonStyled(
                    inputId = "stake_undo",
                    label = "Undo Stake",
                    type = "warning",
                    class = "btn-sm",
                    style = "padding:10px; font-size:140%"
                )
            )
        ),

        shiny::br(),

        shiny::fluidRow(
            shiny::column(
                12,
                align = "center",
                shiny::textOutput("timer")
            )
        )
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

                shiny::h2(
                    shiny::textOutput("header_text"),
                    style = "
          font-weight:700;
          color:#7B9ACC;
          margin-bottom:20px;"
                ),

                shinydashboard::box(
                    width = 12,
                    title = "Pool Overview",
                    status = "primary",

                    shiny::fluidRow(

                        shiny::column(
                            4,

                            DT::DTOutput("pool"),

                            shiny::br(),

                            DT::DTOutput("price")
                        ),

                        shiny::column(
                            7,
                            offset = 1,

                            shiny::plotOutput(
                                "plot_price",
                                height = "320px"
                            )
                        )
                    )
                ),

                shinydashboard::box(
                    width = 12,
                    title = "Team Bank",
                    status = "success",

                    shiny::fluidRow(

                        shiny::column(
                            4,
                            DT::DTOutput("bank")
                        ),

                        shiny::column(
                            7,
                            offset = 1,

                            shiny::plotOutput(
                                "plot_bank",
                                height = "320px"
                            )
                        )
                    )
                ),

                shiny::fluidRow(

                    shiny::column(
                        3,
                        offset = 1,

                        dipsaus::actionButtonStyled(
                            inputId = "start_timer",
                            label = "Open bets",
                            type = "primary",
                            class = "btn-sm",
                            style = "padding:10px; font-size:140%"
                        )
                    ),

                    shiny::column(
                        3,
                        offset = 1,

                        dipsaus::actionButtonStyled(
                            inputId = "run_race",
                            label = "Run Race",
                            type = "success",
                            class = "btn-sm",
                            style = "padding:10px; font-size:140%"
                        )
                    ),

                    shiny::column(
                        3,
                        offset = 1,

                        dipsaus::actionButtonStyled(
                            inputId = "next_race",
                            label = "Next Race",
                            type = "danger",
                            class = "btn-sm",
                            style = "padding:10px; font-size:140%"
                        )
                    )
                )
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
                                height = "300px"
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
    # HEADER
    # =======================================================

    output$header_text <- shiny::renderText({
        paste("Race number", counter_race())
    })

    # =======================================================
    # TIMER
    # =======================================================

    remaining_time <- shiny::reactiveVal(allowed_time)

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

        timer_active(TRUE)

        remaining_time(allowed_time)
    })

    # =======================================================
    # REACTIVE VALUES
    # =======================================================

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

        commission_entry = 0L,

        undo_status = 1L
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

    # =======================================================
    # ENTER STAKE
    # =======================================================

    shiny::observeEvent(input$stake_enter, {

        team <- as.numeric(input$team)

        horse <- as.numeric(input$horse)

        real_stake <- input$stake * 100

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
                    allowed_time - remaining_time()
                ) / allowed_time * commission_max

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

    # =======================================================
    # TABLES
    # =======================================================

    output$pool <- DT::renderDT({

        DT::datatable(

            values$current_pool,

            rownames = FALSE,

            caption = "Pool",

            colnames = c(
                "Red Rum",
                "Secretariat",
                "Seabiscuit",
                "Shergar",
                "Galileo",
                "Best Mate"
            ),

            options = list(
                dom = "t",
                pageLength = 1
            )
        )
    })

    output$price <- DT::renderDT({

        DT::datatable(

            round(values$price, 1),

            rownames = FALSE,

            caption = "Notional Prices",

            colnames = c(
                "Red Rum",
                "Secretariat",
                "Seabiscuit",
                "Shergar",
                "Galileo",
                "Best Mate"
            ),

            options = list(
                dom = "t",
                pageLength = 1
            )
        )
    })

    output$bank <- DT::renderDT({

        DT::datatable(

            values$bank_updated,

            rownames = FALSE,

            caption = "Bank",

            colnames = paste0("Team ", 1:10),

            options = list(
                dom = "t",
                pageLength = 1
            )
        )
    })

    # =======================================================
    # RUN RACE
    # =======================================================

    shiny::observeEvent(input$run_race, {

        shiny::updateTabsetPanel(
            session,
            "inTabset",
            selected = "panel2"
        )

        race_run(1)

        values$racing_now <- 1

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

        winner <- simulate_race()[1]

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

            ggplot2::scale_fill_gradient(
                low = "#A8DADC",
                high = "#7B9ACC"
            ) +

            ggplot2::labs(
                title = "Notional Prices",
                x = "Horse",
                y = "Price"
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

            ggplot2::scale_fill_gradient(
                low = "#CDB4DB",
                high = "#7B9ACC"
            ) +

            ggplot2::labs(
                title = "Team Bank",
                x = "Team",
                y = "Bank"
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
