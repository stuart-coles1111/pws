library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(ggplot2)
library(dplyr)
library(dipsaus)

# =========================================================
# GLOBAL STYLE HELPERS
# =========================================================

theme_racing <- function(){
    ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            axis.title = element_text(face = "bold"),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank()
        )
}

# =========================================================
# DATA / CONSTANTS
# =========================================================

horse_pics <- paste0("horse", 1:6,".jpg")

commission_max <- 0.2
allowed_time <- 180
pool_init <- 100
nominal_stake <- 100
nteams <- 10
winnings_init <- rep(0, 10)

simulate_race <- function(ratings = c(10, 40, 20, 60, 25, 30)){
    df <- data.frame(horse = 1:6, ratings = ratings)
    df_left <- df
    place <- c()
    for (i in 1:5) {
        place[i] <- sample(df_left$horse, 1, prob = df_left$ratings)
        df_left <- subset(df_left, horse != place[i])
    }
    place <- c(place, df_left$horse)
    Sys.sleep(5)
    return(place)
}

total_stake <- matrix(0, nr = 6, nc = nteams)
rownames(total_stake) <- c("1: Red Rum", "2: Secretariat", "3: Seabiscuit",
                           "4: Shergar","5: Galileo","6: Best Mate")
colnames(total_stake) <- paste0("team", 1:nteams)

bank <- matrix(10000, nr = 1, nc = nteams) %>% as.data.frame()
colnames(bank) <- paste0("t", 1:nteams)

pool <- matrix(pool_init, nr = 1, nc = 6) %>% as.data.frame()
colnames(pool) <- c("Red Rum", "Secretariat", "Seabiscuit","Shergar","Galileo","Best Mate")

price <- (sum(pool) + nominal_stake) /(pool + nominal_stake - pool_init)

# =========================================================
# UI
# =========================================================

ui <- dashboardPage(
    skin = "green",

    dashboardHeader(title = "A day at the races"),

    dashboardSidebar(
        width = 300,

        selectInput("team", "Team",
                    choices = paste("Team", 1:10),
                    width='60%'),

        selectInput("horse", "Horse",
                    choices = c("Red Rum"=1,"Secretariat"=2,"Seabiscuit"=3,
                                "Shergar"=4,"Galileo"=5,"Best Mate"=6),
                    width='60%'),

        numericInput("stake", "Stake ($100's)", 1, 0, 100, 1, width = '60%'),

        br(),

        fluidRow(
            column(5,
                   dipsaus::actionButtonStyled(
                       "stake_enter","Enter Stake",
                       type="info",
                       style="padding:8px;font-size:140%;border-radius:10px;"
                   )
            ),
            column(5, offset=1,
                   dipsaus::actionButtonStyled(
                       "stake_undo","Undo Stake",
                       type="warning",
                       style="padding:8px;font-size:140%;border-radius:10px;"
                   )
            )
        ),

        tags$style("#timer {font-size:48px; font-weight:700; color:#1f2937;}"),

        fluidRow(column(12, align="center", textOutput("timer")))
    ),

    dashboardBody(

        shinyjs::useShinyjs(),

        tags$head(
            tags$style(HTML("

        body { background-color:#F7F7FB; }

        .content-wrapper, .right-side {
          background-color:#F7F7FB !important;
        }

        .box {
          border-radius:14px !important;
          box-shadow:0 2px 12px rgba(0,0,0,0.06);
          border-top:none !important;
        }

        .btn { border-radius:10px !important; }

        .small-box {
          border-radius:14px !important;
        }

        table.dataTable {
          border-radius:10px;
          overflow:hidden;
        }

      "))
        ),

        tabsetPanel(id="inTabset",

                    # =====================================================
                    # POOL TAB
                    # =====================================================

                    tabPanel("Pool",

                             h3(textOutput("header_text")),

                             h3("Pool"),

                             fluidRow(
                                 column(4,
                                        DTOutput("pool"),
                                        br(),
                                        DTOutput("price")
                                 ),
                                 column(6, offset=1,
                                        plotOutput("plot_price", height="320px")
                                 )
                             ),

                             h3("Bank"),

                             fluidRow(
                                 column(4, DTOutput("bank")),
                                 column(6, offset=1, plotOutput("plot_bank", height="320px"))
                             ),

                             hr(),

                             fluidRow(
                                 column(3,
                                        dipsaus::actionButtonStyled(
                                            "start_timer","Open bets","primary",
                                            style="padding:8px;font-size:140%;border-radius:10px;"
                                        )
                                 ),
                                 column(3,
                                        dipsaus::actionButtonStyled(
                                            "run_race","Run Race","success",
                                            style="padding:8px;font-size:140%;border-radius:10px;"
                                        )
                                 ),
                                 column(3,
                                        dipsaus::actionButtonStyled(
                                            "next_race","Next Race","danger",
                                            style="padding:8px;font-size:140%;border-radius:10px;"
                                        )
                                 )
                             )
                    ),

                    # =====================================================
                    # RACE TAB
                    # =====================================================

                    tabPanel("Race",

                             h3("Race Result"),

                             fluidRow(
                                 column(5, h3(textOutput("text_win"), style="color:#d97706;font-weight:700;")),
                                 column(5, offset=1, imageOutput("image"))
                             ),

                             br(), br(),

                             fluidRow(
                                 column(10, offset=1,
                                        plotOutput("plot_winnings", height="320px")
                                 )
                             )
                    )
        )
    )
)

# =========================================================
# SERVER (UNCHANGED LOGIC + STYLING ONLY)
# =========================================================

server <- function(input, output, session){

    counter_race <- reactiveVal(1)
    race_run <- reactiveVal(0)

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
        team_entry = 1L,
        horse_entry = 1L,
        stake_entry = 0L,
        commission_entry = 0L,
        undo_status = 1L
    )

    # =====================================================
    # TIMER
    # =====================================================

    output$timer <- renderText({
        paste0(remaining_time(), " s")
    })

    observe({
        invalidateLater(1000, session)
        isolate({
            if(timer_active()){
                remaining_time(remaining_time() - 1)

                if(remaining_time() < 1){
                    timer_active(FALSE)

                    showModal(modalDialog("No more bets!"))

                    insertUI(
                        selector="#run_race",
                        where="afterEnd",
                        ui=tags$audio(src="bell.mp3", autoplay=TRUE, style="display:none;")
                    )
                }
            }
        })
    })

    observeEvent(input$start_timer,{
        timer_active(TRUE)
        remaining_time(allowed_time)
    })

    # =====================================================
    # RENDER TABLES
    # =====================================================

    output$pool <- renderDT({
        datatable(values$current_pool, options=list(dom="t"))
    })

    output$price <- renderDT({
        datatable(round(values$price,1), options=list(dom="t"))
    })

    output$bank <- renderDT({
        datatable(values$bank_updated, options=list(dom="t"))
    })

    # =====================================================
    # PLOTS (MODERNISED ONLY)
    # =====================================================

    output$plot_price <- renderPlot({
        ggplot(currentPrice(), aes(x=factor(horse), y=price)) +
            geom_col(fill="#6366f1", alpha=0.9) +
            geom_text(aes(label=round(price,1)), vjust=-0.5, fontface="bold") +
            labs(title="Notional Prices", x="Horse", y="Price") +
            theme_racing()
    })

    output$plot_bank <- renderPlot({
        ggplot(currentBank(), aes(x=factor(team), y=bank)) +
            geom_col(fill="#0ea5e9", alpha=0.9) +
            geom_text(aes(label=round(bank/1000,1)), vjust=-0.5) +
            labs(title="Bank", x="Team", y="Balance") +
            theme_racing()
    })

    output$plot_winnings <- renderPlot({
        ggplot(currentWinnings(), aes(x=factor(team), y=winnings)) +
            geom_col(fill=ifelse(values$winnings>=0,"#22c55e","#ef4444")) +
            geom_hline(yintercept=0, colour="grey40") +
            geom_text(aes(label=round(winnings,0)), vjust=-0.5) +
            labs(title="Winnings", x="Team", y="") +
            theme_racing()
    })
}

shinyApp(ui, server)
