#vector of filenames for horse photos
horse_pics <- paste0("horse", 1:6,".jpg")

# set commission maximum
commission_max <- 0.2

allowed_time <- 30

pool_init <- 100

nominal_stake <- 100

winnings_init <- rep(0, 10)

# function for race simulation
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

# initialize stakes, pool and bank

nteams <- 10
total_stake <- matrix(as.integer(0), nr = 6, nc = nteams)
rownames(total_stake) <- c("1: Red Rum", "2: Secretariat", "3: Seabiscuit","4: Shergar","5: Galileo","6: Best Mate")
colnames(total_stake) <- paste0("team", 1:nteams)

bank <- matrix(as.integer(10000), nr = 1, nc = nteams) %>% as.data.frame
colnames(bank) <- paste0("t", 1:nteams)

pool <- matrix(as.integer(pool_init), nr = 1, nc = 6) %>% as.data.frame
colnames(pool) <- c("Red Rum", "Secretariat", "Seabiscuit","Shergar","Galileo","Best Mate")

#price <- sum(pool) /(pool + nominal_stake - pool_init) + 1
price <- (sum(pool) + nominal_stake) /(pool + nominal_stake - pool_init)

winnings <- data.frame()

ui <- shinydashboard::dashboardPage(skin = "green",
                                    shinydashboard::dashboardHeader(title = "A day at the races"),
                                    shinydashboard::dashboardSidebar(
                                        width = 300,

                                        shiny::selectInput("team", "Team", choices = list("Team 1" = 1, "Team 2" = 2, "Team 3" = 3, "Team 4" = 4
                                                                                          , "Team 5" = 5, "Team 6" = 6, "Team 7" = 7, "Team 8" = 8
                                                                                          , "Team 9" = 9, "Team 10" = 10), width='60%'),

                                        shiny::selectInput("horse", "Horse", choices = list("1: Red Rum" = 1, "2: Secretariat" = 2, "3: Seabiscuit" = 3, "4: Shergar" = 4
                                                                                            , "5: Galileo" = 5, "6: Best Mate" = 6), width='60%'),

                                        shiny::numericInput("stake", "Stake ($100's)", 1, 0, 100, 1, width = '60%'),
                                        shiny::br(),
                                        shiny::fluidRow(shiny::column(5, align = "center",
                                                                      dipsaus::actionButtonStyled(inputId = "stake_enter",
                                                                                                  label = "Enter Stake",
                                                                                                  type = "info",
                                                                                                  class = "btn-xs",
                                                                                                  style = "padding:8px; font-size:150%")),
                                                        shiny::column(5, align = "center", offset = 1,
                                                                      dipsaus::actionButtonStyled(inputId = "stake_undo",
                                                                                                  label = "Undo Stake",
                                                                                                  type = "warning",
                                                                                                  class = "btn-xs",
                                                                                                  style = "padding:8px; font-size:150%"))),
                                        shiny::tags$style("#timer {font-size:500%}"),
                                        shiny::fluidRow(column(12, align = "center", shiny::textOutput("timer"))
                                        )
                                    ),
                                    shinydashboard::dashboardBody(
                                        shinyjs::useShinyjs(),
                                        shiny::tabsetPanel(id = "inTabset",
                                                           shiny::tabPanel("Pool", value="panel1",
                                                                           shiny::h3(shiny::textOutput("header_text")),
                                                                           shiny::h3("Pool"),

                                                                           shiny::fluidRow(
                                                                               column(4,
                                                                                      DT::DTOutput("pool"),
                                                                                      shiny::br(),
                                                                                      shiny::br(),
                                                                                      shiny::br(),
                                                                                      DT::DTOutput("price")),
                                                                               shiny::column(6, offset = 1,
                                                                                             shiny::plotOutput("plot_price", height = "50%")),
                                                                           ),
                                                                           shiny::h3("Bank"),
                                                                           shiny::fluidRow(
                                                                               shiny::column(4,
                                                                                             DT::DTOutput("bank")),
                                                                               shiny::column(6, offset = 1, shiny::plotOutput("plot_bank"))),
                                                                           shiny::hr(),
                                                                           shiny::fluidRow(
                                                                               shiny::column(3, offset = 1,
                                                                                             dipsaus::actionButtonStyled(inputId = "start_timer",
                                                                                                                         label = "Open bets",
                                                                                                                         type = "primary",
                                                                                                                         class = "btn-xs",
                                                                                                                         style = "padding:8px; font-size:150%")),
                                                                               shiny::column(3, offset = 1,
                                                                                             dipsaus::actionButtonStyled(inputId = "run_race",
                                                                                                                         label = "Run Race",
                                                                                                                         type = "success",
                                                                                                                         class = "btn-xs",
                                                                                                                         style = "padding:8px; font-size:150%")),
                                                                               shiny::column(3, offset = 1,
                                                                                             dipsaus::actionButtonStyled(inputId = "next_race",
                                                                                                                         label = "Next Race",
                                                                                                                         type = "danger",
                                                                                                                         class = "btn-xs",
                                                                                                                         style = "padding:8px; font-size:150%"))
                                                                           )
                                                           ),

                                                           shiny::tabPanel("Race",  value="panel2",
                                                                           shiny::h3("Race Result"),
                                                                           shiny::fluidRow(shiny::column(5,
                                                                                                         shiny::h3(textOutput("text_win"), style = "color:#FF7F50;")),
                                                                                           shiny::column(5, offset = 1,  shiny::imageOutput("image"))),
                                                                           shiny::br(),
                                                                           shiny::br(),
                                                                           shiny::fluidRow(column(10, offset = 1, shiny::plotOutput("plot_winnings", height = "50%")))

                                                           )

                                        ))

)

server <- function(input, output, session) {

    #vector of filenames for horse photos
    horse_pics <- paste0("horse", 1:6, ".jpg")

    # set counters
    counter_race <- shiny::reactiveVal(value = 1)
    race_run <- shiny::reactiveVal(value = 0)

    # text for header
    output$header_text <- shiny::renderText(
        paste("Race number ", counter_race())
    )

    # observe timer
    remaining_time <- shiny::reactiveVal(allowed_time)
    timer_active <- shiny::reactiveVal(FALSE)

    # Output the time left.
    output$timer <- shiny::renderText({
        paste0(remaining_time(), " s")
    })


    # observer that invalidates every second. If timer is active, decrease by one.
    observe({
        shiny::invalidateLater(1000, session)
        shiny::isolate({
            if (timer_active()) {
                remaining_time(remaining_time() - 1)
                if (remaining_time() < 1) {
                    timer_active(FALSE)
                    shiny::insertUI(selector = "#run_race",
                                    where = "afterEnd",
                                    ui = tags$audio(src = "bell.mp3", type = "audio/mp3", autoplay = TRUE, controls = NA, style="display:none;"), immediate = TRUE
                    )
                    shiny::showModal(shiny::modalDialog(title = "Pool message", "No more bets!"))
                }
            }
        })
    })

    shiny::observeEvent(input$start_timer, {
        timer_active(TRUE)
        remaining_time(allowed_time)
    })


# this is an attempt to force stake enter button and enter bet values automatically
# doesn't work but i think the shinyjs::click("stake_enter") is the correct way
# but currently the bet is entered immediately without waiting and also team, horse, stake values are incorrect
# also tried input$team etc but wasn't allowed. also tried to make values reactive but didn't work.
    shiny::observe({

        # this is just for example purposes
        # what i'd really like to do is activate the stake enter button automatically at specified time points
        # with specified values of team, horse and stake
        # of course, the app fails to pick up the correct values of team/horse/stake
        # but there's another problem....
        # the button gets clicked immediately and then every 5000 ms after
        # whereas what i want is the button to be clicked just once after (say) 5000 ms

        # you can now access the bets using values[["bets_df"]]
        bets <- values[["bets_df"]]

        bet_times <- unique(bets$bet_times)
        bet_times_remaining <- allowed_time - bet_times

        if (any(bet_times_remaining == remaining_time())) {

            idx <- which(bet_times_remaining == remaining_time())

            # which team bet
            team <- as.numeric(bets[idx, "bet_team"])

            # which horse bet
            horse <- as.numeric(bets[idx, "bet_horse"])

            # using proportion get amount they can staked
            # Round the stake

            real_stake <- round(as.numeric(values$bank_updated[team] * bets[idx, "proportions"]), 0)

            new_bank <- values$bank_updated[team] - real_stake

            values$total_stake[horse, team] <- values$total_stake[horse, team] + real_stake
            values$current_pool[horse] <- values$current_pool[horse] + real_stake
            values$bank_updated[team] <- values$bank_updated[team] - real_stake
            values$price <- (sum(values$current_pool) + nominal_stake) / (values$current_pool + nominal_stake - pool_init)
            commission <- (allowed_time-remaining_time()) / allowed_time * commission_max
            values$commission_entry <- commission
            values$current_pool_adj[horse] <- values$current_pool_adj[horse] + ((real_stake * (1 - commission)) %>% round(-2))
            values$total_stake_net[horse, team] <- values$total_stake_net[horse, team] + ((real_stake * (1 - commission)) %>% round(-2))

            # remove bet from values$bets - i.e get whatever bets are still remaining
            values[["bets_df"]] <- values[["bets_df"]][-idx, ]
        }
    })



# i've added this bit so that every time the start_timer button is pressed, to signify the start of the betting period
    # for a new race, this stuff simulates the bet times, team number, horce number and staking proportion for
    # all teams from 2-6 for this race. i've stored this here in bets_df. i'd then like to automatically click the
    # "stake enter" button at the appropriate times and with these values of team and horse number, and with the
    # stake calculated from the simulated proportion. (e.g. if bank is 1000 and proportion is 0.1, stake is 100 etc)

    # this won't be accessible as bets df is just local here, you would need to create a reactivate values
    shiny::observeEvent(input$start_timer, {
        bet_rate <- 5
        bet_horse <- c()
        nbets <- rpois(1, bet_rate) + nteams
        bet_times <- sample(1:(allowed_time-1), nbets)
        bet_team_ind <- sample(1:nbets, nteams - 1, replace = FALSE)
        bet_team <- rep(NA, nbets)
        bet_team[bet_team_ind] <- 2:nteams
        rem_ind <- setdiff(1:nbets,bet_team_ind)
        rem_team <- sample(2:nteams, length(rem_ind), replace = TRUE)
        bet_team[rem_ind] <- rem_team
        bet_horse <- sample(1:6, nbets, replace = TRUE)
        bets_df <- data.frame(bet_times = bet_times, bet_team = bet_team, bet_horse = bet_horse)
        bets_df <- dplyr::arrange(bets_df, bet_times)
        bets_df$proportions <- runif(nbets, 0, 0.1)
        values[["bets_df"]] <- bets_df
    })

    #define reactive values
    values <- shiny::reactiveValues(current_pool = pool,
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
                                    undo_status = 1L,
                                    bets_df = data.frame())

    ## plot data.frames

    currentPool <- shiny::reactive({
        data.frame(horse = 1:6, pool = values$current_pool %>% as.numeric)
    })

    currentPool_adj <- shiny::reactive({
        data.frame(horse = 1:6, pool = values$current_pool_adj %>% as.numeric)
    })

    currentBank <- shiny::reactive({
        data.frame(team = 1:nteams, bank = values$bank_updated %>% as.numeric)
    })

    currentPrice <- shiny::reactive({
        data.frame(horse = 1:6, price = values$price %>% as.numeric)
    })

    currentWinnings <- shiny::reactive({
        #        validate(need(!is.null(winnings) && nrow(winnings) > 0, "Pending race result"))
        shiny::validate(need(values$racing_now == 1, ""))
        data.frame(team = 1:nteams,
                   winnings = values$winnings %>% as.numeric)
    })

    shiny::observeEvent(input$stake_enter, {

        team <- input$team %>% as.numeric
        horse <- input$horse %>% as.numeric
        real_stake <- input$stake * 100

        values$team_entry <- team
        values$horse_entry <- horse
        values$stake_entry <- real_stake
        values$undo_status <- 0L

        new_bank <- values$bank_updated[team] - real_stake
        if(new_bank < 0){
            values$undo_status <- 1L
            shiny::showModal(shiny::modalDialog(title = "Pool message", "Negative Bank: Bet declined"))
        }
        else{
            values$total_stake[horse, team] <- values$total_stake[horse, team] + real_stake
            values$current_pool[horse] <- values$current_pool[horse] + real_stake
            values$bank_updated[team] <- values$bank_updated[team] - real_stake
            values$price <- (sum(values$current_pool) + nominal_stake) / (values$current_pool + nominal_stake - pool_init)
            commission <- (allowed_time-remaining_time())/allowed_time*commission_max
            values$commission_entry <- commission
            values$current_pool_adj[horse] <- values$current_pool_adj[horse] + ((real_stake * (1 - commission)) %>% round(-2))
            values$total_stake_net[horse, team] <- values$total_stake_net[horse, team] + ((real_stake * (1 - commission)) %>% round(-2))
        }

    })

    shiny::observeEvent(input$stake_undo, {
        if(values$undo_status == 1L){
            shiny::showModal(shiny::modalDialog(title = "Pool message", "No stake entered"))
        }
        else{
            values$undo_status <- 1L
            team <- values$team_entry
            horse <- values$horse_entry
            stake <- values$stake_entry
            commission <- values$commission_entry
            values$total_stake[horse, team] <- values$total_stake[horse, team] - stake
            values$current_pool[horse] <- values$current_pool[horse] - stake
            values$bank_updated[team] <- values$bank_updated[team] + stake
            values$price <- (sum(values$current_pool) + nominal_stake) / (values$current_pool + nominal_stake - pool_init)
            values$current_pool_adj[horse] <- values$current_pool_adj[horse] - ((stake * (1 - commission)) %>% round(-2))
            values$total_stake_net[horse, team] <- values$total_stake_net[horse, team] - ((stake * (1 - commission)) %>% round(-2))
        }

    })

    output$error_text <- shiny::renderText(values$error_text)

    output$invalidate_text <- shiny::renderText(values$stake_text)

    output$pool <- DT::renderDT({
        DT::datatable(
            values$current_pool,
            rownames = FALSE,
            caption = "Pool",
            colnames = c("Red Rum", "Secretariat", "Seabiscuit","Shergar","Galileo","Best Mate"),
            options = list(dom = "t")
        )
    })

    output$price <- DT::renderDT({
        DT::datatable(
            values$price %>% round(1),
            rownames = FALSE,
            caption = "Notional Prices",
            colnames = c("Red Rum", "Secretariat", "Seabiscuit","Shergar","Galileo","Best Mate"),
            options = list(dom = "t")
        )
    })

    output$bank <- DT::renderDT({
        DT::datatable(
            values$bank_updated,
            rownames = FALSE,
            caption = "Bank",
            colnames = paste0("Team ",1:10 ),
            options = list(dom = "t")
        )
    })

    observeEvent(input$run_race, {

        shiny::updateTabsetPanel(session, "inTabset", selected = "panel2")
        race_run(1)
        values$racing_now <- 1
        values$stake_text <- ""

        shiny::insertUI(selector = "#run_race",
                        where = "afterEnd",
                        ui = tags$audio(src = "race.mp3", type = "audio/mp3", autoplay = TRUE, controls = NA, style="display:none;"), immediate = TRUE
        )

        winner <- simulate_race()[1]

        output$text_win <- shiny::renderText(paste("Winning horse is  "
                                                   , rownames(total_stake)[winner]))

        output$image <- shiny::renderImage({
            width<- "80%"
            height<- "20%"
            list(src = paste0("www/",horse_pics[winner]),
                 contentType = "image/jpg",
                 width = width,
                 height = "auto"
            )
        }, deleteFile = FALSE)

        total_pool <- sum(values$current_pool_adj)
        total_pool_winner <- values$current_pool_adj[winner]

        if (total_pool_winner == pool_init) {
            values$winnings <- rep(0, nteams)
            values$bank_updated <- values$bank_updated +  apply(values$total_stake, 2, sum)
        }
        else{
            winnings <-
                total_pool * values$total_stake_net[winner,] / sum(values$total_stake_net[winner,]) - apply(values$total_stake, 2, sum)
            values$winnings <- winnings %>% round(-2)
            values$bank_updated <- values$bank_updated + values$winnings +  apply(values$total_stake, 2, sum)
        }
        values$undo_status <- 1L
    })


    observeEvent(input$next_race, {


        race_run(0)
        values$stake_text <- ""

        tmp0 <- counter_race() # save current value of counter
        counter_race(tmp0 + 1) # update counter


        values$current_pool <- pool
        values$current_pool_adj <- pool
        values$total_stake <- total_stake
        values$total_stake_net <- total_stake

        values$price <- (sum(pool) + nominal_stake)  /(pool + nominal_stake - pool_init)
        output$header_text <- renderText(
            paste(
                "Race number ",
                counter_race()
            )

        )

        output$text_win <- NULL
        #   output$plot_winnings <- NULL
        output$image <- NULL
        values$winnings <- winnings_init

    })


    ## Plots
    output$plot_price <- shiny::renderPlot({
        ggplot2::ggplot(data = currentPrice(), ggplot2::aes(x = horse, y = price)) +
            ggplot2::geom_bar(stat = "identity", fill = "gray73", width=0.4) +
            ggplot2::geom_text(ggplot2::aes(label = round(price, 1)) , vjust = 1.5, size=10, color = "red") +
            ggplot2::scale_x_continuous(breaks = 1:6) +
            ggplot2::theme(axis.text = ggplot2::element_text(size = 16),
                           axis.title = ggplot2::element_text(size = 16, face = "bold"),
                           plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5)) +
            ggplot2::ggtitle("Notional Prices")
    }, height = 300)

    output$plot_bank <- shiny::renderPlot({
        ggplot2::ggplot(data = currentBank(), ggplot2::aes(x = team, y = bank)) +
            ggplot2::geom_bar(stat = "identity", fill = "lightblue", width=0.6) +
            ggplot2::geom_text(ggplot2::aes(label = (bank / 1000) %>% round(2)), vjust = 1.5, size= 10, color = "red") +
            ggplot2::scale_x_continuous(breaks = 1:nteams) +
            ggplot2::theme(axis.text = ggplot2::element_text(size = 16),
                           axis.title = ggplot2::element_text(size = 16, face = "bold"),
                           plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5)) +
            ggplot2::ggtitle("Bank")
    }, height = 300)

    output$plot_winnings <- shiny::renderPlot({
        ggplot2::ggplot(data = currentWinnings(), ggplot2::aes(x = team, y = winnings)) +
            ggplot2::geom_bar(stat = "identity", fill = "olivedrab", width=0.4) +
            ggplot2::scale_x_continuous(breaks = 1:nteams) +
            ggplot2::geom_hline(yintercept = 0, color = "red") +
            ggplot2::theme(axis.text = ggplot2::element_text(size = 16),
                           axis.title = ggplot2::element_text(size = 16, face = "bold"),
                           plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5)) +
            ggplot2::ggtitle("Winnings")
    }, height = 200)


}

shiny::shinyApp(ui, server)
