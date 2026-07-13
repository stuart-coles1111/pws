suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
})

# =========================================================
# HELPER FUNCTIONS
# =========================================================

activity7_round_sim <- function(
        home_player,
        away_player,
        n_games_per_match = 5
) {

    prob_list <- list(
        blue   = c(4, 5, 7, 9, 11, 0) / 36,
        red    = c(0, 11, 9, 7, 5, 4) / 36,
        green  = c(3, 7, 11, 9, 5, 1) / 36,
        yellow = c(10, 8, 6, 4, 2, 6) / 36
    )

    total_home_wins <- numeric(length(home_player))

    for(i in seq_along(home_player)) {

        home_probs <- prob_list[[home_player[i]]]
        away_probs <- prob_list[[away_player[i]]]

        home_score <- sample(1:6, n_games_per_match, TRUE, home_probs)
        away_score <- sample(1:6, n_games_per_match, TRUE, away_probs)

        total_home_wins[i] <- sum(home_score >= away_score)
    }

    total_home_wins
}

activity7_round_sim_using_fits <- function(
        pars,
        home_player,
        away_player,
        n_games_per_match = 5
) {

    colours <- c("blue","red","yellow","green")

    home_ind <- match(home_player, colours)
    away_ind <- match(away_player, colours)

    pars <- c(0, pars)

    bin_p <- pars[5] + pars[home_ind] - pars[away_ind]
    bin_p <- exp(bin_p) / (1 + exp(bin_p))

    rbinom(length(home_player), n_games_per_match, bin_p)
}

activity7_neg_log_lik <- function(
        p,
        dice_history,
        n_games_per_match = 5
) {

    colours <- c("blue","red","yellow","green")

    home_ind <- match(dice_history$home_colours, colours)
    away_ind <- match(dice_history$away_colours, colours)

    pars <- c(0,p)

    bin_p <- pars[5] + pars[home_ind] - pars[away_ind]
    bin_p <- exp(bin_p)/(1+exp(bin_p))

    -sum(dbinom(dice_history$results, n_games_per_match, bin_p, log=TRUE))
}

# =========================================================
# UI
# =========================================================

ui <- page_navbar(

    title = "🎲 Activity 7: A Dice Tournament",

    theme = bs_theme(
        version = 5,
        bootswatch = "minty",
        primary = "#7B9ACC",
        bg = "#F7F7FB",
        fg = "#2E3440",
        base_font = font_google("Inter")
    ),

    # =====================================================
    # GLOBAL CSS + JS
    # =====================================================

    header = tagList(

        useShinyjs(),

        tags$head(

            tags$style(HTML("
            .app-header{
                background:linear-gradient(90deg,#A8DADC,#CDB4DB);
                padding:18px;
                border-radius:14px;
                margin-bottom:20px;
                text-align:center;
            }

            .card-style{
                background:white;
                border-radius:14px;
                padding:20px;
                margin-bottom:20px;
                box-shadow:0 3px 10px rgba(0,0,0,0.08);
            }

            .btn-primary{
                background:#89C2D9!important;
                border-color:#89C2D9!important;
            }

            .player-badge{
                display:inline-block;
                padding:10px 16px;
                border-radius:20px;
                color:white;
                font-weight:700;
                margin:4px;
                min-width:140px;
                text-align:center;
            }

            .badge-blue{background:#6FA8DC;}
            .badge-red{background:#E5989B;}
            .badge-green{background:#95D5B2;color:#1B4332;}
            .badge-yellow{background:#F9E79F;color:#5C4B00;}
        ")),

            tags$script(src =
                            "https://cdn.jsdelivr.net/npm/canvas-confetti@1.9.3/dist/confetti.browser.min.js"
            ),

            tags$script(HTML("
            Shiny.addCustomMessageHandler('confetti', function(message) {

                const duration = 2500;
                const end = Date.now() + duration;

                (function frame(){

                    confetti({
                        particleCount: 6,
                        spread: 60,
                        origin: { x: 0 }
                    });

                    confetti({
                        particleCount: 6,
                        spread: 60,
                        origin: { x: 1 }
                    });

                    if(Date.now() < end)
                        requestAnimationFrame(frame);

                })();

            });
        ")),

            tags$script(HTML("

$(document).on('change', 'input[id^=\"match_\"]', function(){

    let id = this.id;

    if(id.endsWith('_away'))
        return;

    let parts = id.split('_');

    let round = parts[1];
    let match = parts[2];

    let home = Number($(this).val());

    let games = Number($('#games').val());

    if(isNaN(home) || isNaN(games))
        return;

    if(home < 0 || home > games)
        return;

    let away = games - home;

    let away_id = '#match_' + round + '_' + match + '_away';

$(away_id).val(away).change();
});


$(document).on('change', 'input[id$=\"_away\"]', function(){

    let id = this.id;

    let parts = id.split('_');

    let round = parts[1];
    let match = parts[2];

    let away = Number($(this).val());

    let games = Number($('#games').val());

    if(isNaN(away) || isNaN(games))
        return;

    if(away < 0 || away > games)
        return;

    let home = games - away;

    let home_id = '#match_' + round + '_' + match;

$(home_id).val(home).change();

});

        "))

        ),

        div(
            class = "app-header",
            h1("🎲 Activity 7: A Dice Tournament")
        )
    ),

    # =====================================================
    # OVERVIEW PAGE
    # =====================================================

    overview_page(

        explanation = tagList(
            p("This activity explores how simple probabilistic rules generate complex tournament behaviour."),
            p("Players compete in a knockout structure where each match outcome is probabilistic rather than deterministic.")
        ),

        individual = tagList(
            tags$ol(
                tags$li("Run or simulate tournament rounds."),
                tags$li("Observe how randomness affects progression."),
                tags$li("Estimate hidden probabilities from results."),
                tags$li("Compare strategies across modes.")
            )
        ),

        group = tagList(
            tags$ol(
                tags$li("Compare tournament outcomes across groups."),
                tags$li("Investigate variability across simulations."),
                tags$li("Discuss fairness and randomness in competition structures."),
                tags$li("Relate results to real sports analytics.")
            )
        ),

        question = tagList(
            tags$ul(
                tags$li("How predictable are knockout tournaments?"),
                tags$li("Can we infer skill from limited data?"),
                tags$li("How does randomness propagate through rounds?"),
                tags$li("What makes a tournament 'fair'?")
            )
        )
    ),

    # =====================================================
    # ACTIVITY
    # =====================================================

    nav_panel(

        "Activity",

        layout_sidebar(

            sidebar = div(

                class = "card-style",

                accordion(

                    accordion_panel(

                        title = "⚙️ Tournament Setup",

                        radioButtons(
                            "mode",
                            "Mode",
                            c(
                                "Live (enter results)" = "human",
                                "Simulation (auto-play)" = "sim",
                                "Demo (guided example)" = "demo"
                            )
                        ),

                        conditionalPanel(

                            condition = "input.mode == 'human'",

                            hr(),

                            h5("Player List"),

                            helpText(
                                "Optional. Upload a CSV containing one player name per row. The names will be assigned randomly to tournament positions."
                            ),

                            fileInput(
                                "player_file",
                                "Upload player names",
                                accept = ".csv"
                            )
                        ),

                        numericInput(
                            "nrounds",
                            "Number of rounds",
                            5,
                            1,
                            8
                        ),

                        numericInput(
                            "games",
                            "Games per match",
                            5,
                            min = 1,
                            max = 9,
                            step = 2
                        ),

                        numericInput(
                            "estimate_round",
                            "Estimation round",
                            2,
                            min = 1
                        ),

                        numericInput(
                            "seed",
                            "Seed",
                            value = sample(1:999,1)
                        )
                    )
                ),

                br(),

                accordion(

                    open = FALSE,

                    accordion_panel(

                        title = "🎮 Tournament Controls",

                        uiOutput("action_ui")
                    )
                ),

                br(),

                actionButton(
                    "start",
                    "Start Tournament",
                    class = "btn-primary"
                ),
            ),

            accordion(

                id = "rules_accordion2",
                open = FALSE,

                accordion_panel(



                    title = "📖 Rules of Play",

                    tags$ol(

                        tags$li(
                            "Players are randomised at the start of the tournament draw."
                        ),

                        tags$li(
                            "Each player is randomly assigned a colour — Blue, Red, Green, or Yellow — and given a pair of dice of that colour. This assignment remains fixed for the duration of the tournament."
                        ),

                        tags$li(
                            "In the first round, players are paired according to the tournament draw. Winners progress to the next round."
                        ),

                        tags$li(
                            "By default, each match consists of five games. In each game, both players roll their dice and obtain scores according to the rules specified below. The player with the higher score wins the game. The match winner is the player who wins the most games. All games are played, even if the match result has already been decided."
                        ),

                        tags$li(
                            "At the end of each round, winners are identified and the draw for the next round is generated."
                        ),

                        tags$li(
                            "After the selected estimation round has been completed, the tournament is paused to allow model development. This produces estimates of win probabilities for matches between players with dice of any colour, and subsequently probabilities for the overall tournament winner."
                        ),

                        tags$li(
                            "The tournament is then completed through to the final, and the overall winner is identified."
                        ),

                        tags$li(
                            "Default settings can be changed at the start of the tournament, including the number of rounds, games per match, and the estimation round."
                        )

                    )

                ),

                accordion_panel(

                    title = "🎲 Dice Scoring Rules",

                    p(
                        "Each player keeps the same coloured pair of dice throughout the tournament. In every game, both dice are rolled and the player's score is calculated according to the colour of their dice."
                    ),

                    tags$table(

                        class = "table table-sm table-striped",

                        tags$thead(

                            tags$tr(

                                tags$th("Colour"),
                                tags$th("Scoring rule"),
                                tags$th("Example")

                            )

                        ),

                        tags$tbody(

                            tags$tr(

                                tags$td("🔵 Blue"),

                                tags$td(
                                    "Take the larger of the two dice, subtract 2, and record a score of 1 if the result is zero or negative."
                                ),

                                tags$td(
                                    "Roll 3 and 6 → score = 4"
                                )

                            ),

                            tags$tr(

                                tags$td("🔴 Red"),

                                tags$td(
                                    "Take the smaller of the two dice, add 1, and record a score of 6 if the result is 7."
                                ),

                                tags$td(
                                    "Roll 2 and 5 → score = 3"
                                )

                            ),

                            tags$tr(

                                tags$td("🟢 Green"),

                                tags$td(
                                    "Add the two dice, divide by 2, and round down to the nearest whole number."
                                ),

                                tags$td(
                                    "Roll 3 and 6 → score = 4"
                                )

                            ),

                            tags$tr(

                                tags$td("🟡 Yellow"),

                                tags$td(
                                    "Take the absolute difference between the two dice. If the difference is 0, record a score of 6."
                                ),

                                tags$td(
                                    "Roll 4 and 4 → score = 6"
                                )

                            )

                        )

                    ),

                    hr(),

                    p(
                        HTML("<b>Home advantage:</b> In every game, the home player receives an additional <b>0.5 points</b>.")
                    )

                )

            ),

            div(
                class = "card-style",

                h3("Tournament Bracket"),

                uiOutput("round_section"),

                hr(),

                uiOutput("prob_section")
            )
        )
    )
)

# =========================================================
# SERVER (UNCHANGED)
# =========================================================

server <- function(input, output, session){

    colours <- c("blue","red","green","yellow")

    nsim <- 10000

    demo_scores <- list(
        c(3,4,2,2,3,5,1,2,0,3,4,1,2,4,4,5),
        c(4,4,2,3,1,0,5,3),
        c(5,2,4,3),
        c(1,3),
        c(4)
    )

    rv <- reactiveValues(
        round=1,
        player_colours=NULL,
        current_players=NULL,
        display_names=NULL,
        estimation_df=NULL,
        winner_probs=NULL,
        pars_df=NULL,
        estimated=FALSE,
        sim_preview=NULL,
        sim_ready = FALSE,
        confetti=FALSE,
        started = FALSE
    )

    player_badge <- function(id, col){

        div(
            class = paste("player-badge", paste0("badge-", col)),
            rv$display_names[id]
        )
    }


    player_names <- reactive({

        if (is.null(input$player_file))
            return(NULL)

        dat <- read.csv(
            input$player_file$datapath,
            stringsAsFactors = FALSE
        )

        trimws(dat[[1]])
    })

    start_tournament <- function(){

        rv$sim_ready <- FALSE
        rv$sim_preview <- NULL
        rv$winner_probs <- NULL
        rv$pars_df <- NULL
        rv$estimation_df <- data.frame()
        rv$estimated <- FALSE

        set.seed(input$seed)

        nplayers <- 2^input$nrounds

        nm <- player_names()

        if (input$mode == "human" && !is.null(nm)) {

            nm <- trimws(nm)
            nm <- nm[nm != ""]

            if (length(nm) < nplayers) {

                nm <- c(
                    nm,
                    paste("Player", (length(nm) + 1):nplayers)
                )

            }

            nm <- sample(nm)[1:nplayers]

        } else {

            nm <- paste("Player", 1:nplayers)

        }

        rv$display_names <- nm

        rv$player_colours <- setNames(
            sample(rep(colours, length.out = nplayers)),
            1:nplayers
        )

        rv$current_players <- sample(1:nplayers)
        rv$round <- 1
        rv$confetti <- FALSE
        rv$started <- TRUE

    }


    fixtures_df <- reactive({
        req(rv$current_players)

        if(length(rv$current_players) < 2) return(NULL)

        data.frame(
            Home = rv$current_players[seq(1, length(rv$current_players), 2)],
            Away = rv$current_players[seq(2, length(rv$current_players), 2)],
            HomeColour = rv$player_colours[
                rv$current_players[seq(1, length(rv$current_players), 2)]
            ],

            AwayColour = rv$player_colours[
                rv$current_players[seq(2, length(rv$current_players), 2)]
            ]
        )
    })

    simulate_round_only <- function(players){

        home <- players[seq(1,length(players),2)]
        away <- players[seq(2,length(players),2)]

        stopifnot(length(players) %% 2 == 0)

        results <- activity7_round_sim(
            rv$player_colours[as.character(home)],
            rv$player_colours[as.character(away)],
            input$games
        )

        winners <- ifelse(results >= (input$games+1)/2, home, away)

        list(home=home,away=away,results=results,winners=winners)
    }

    observeEvent(input$simulate_results, {

        req(rv$current_players)

        rv$sim_preview <- simulate_round_only(rv$current_players)

        rv$sim_ready <- TRUE

    })

    observeEvent(input$nrounds, {

        updateNumericInput(
            session,
            "estimate_round",
            max = input$nrounds - 1,
            value = min(input$estimate_round, input$nrounds - 1)
        )

    })

    observeEvent(input$start, {

        if(rv$started){

            showModal(
                modalDialog(

                    title = "Restart tournament?",

                    p(
                        "Starting a new tournament will erase the current draw, results, and estimates."
                    ),

                    footer = tagList(

                        modalButton("Cancel"),

                        actionButton(
                            "confirm_restart",
                            "Restart Tournament",
                            class="btn-danger"
                        )
                    )
                )
            )

            return()

        }

        start_tournament()

    })

    observeEvent(input$confirm_restart, {

        removeModal()

        start_tournament()

    })

    observeEvent(
        {
            req(input$mode)

            switch(
                input$mode,
                sim = input$next_round,
                demo = input$demo_next,
                human = input$submit_results
            )

        },
        {

            req(rv$current_players)

            df <- fixtures_df()
            req(!is.null(df))

            sim <- switch(
                input$mode,

                "sim" = if(!is.null(rv$sim_preview)) rv$sim_preview else simulate_round_only(rv$current_players),

                "human" = {

                    res <- sapply(1:nrow(df), function(i) {


                        home_score <- input[[paste0("match_", rv$round, "_", i)]]

                        away_score <- input[[paste0(
                            "match_",
                            rv$round,
                            "_",
                            i,
                            "_away"
                        )]]


                        if(is.na(home_score) || is.na(away_score)) {
                            return(NA)
                        }

                        # check individual score ranges first
                        if(home_score < 0 ||
                           away_score < 0 ||
                           home_score > input$games ||
                           away_score > input$games) {
                            return(NA)
                        }

                        # check total games
                        if(home_score + away_score != input$games){
                            return(NA)
                        }

                        home_score
                    })

                    res <- as.numeric(res)

                    if(any(is.na(res))) {

                        showNotification(
                            paste0(
                                "Each match must have scores between 0 and ",
                                input$games,
                                " and the two scores must sum to ",
                                input$games,
                                "."
                            ),
                            type = "warning",
                            duration = 5
                        )

                        return(NULL)
                    }

                    list(
                        home = df$Home,
                        away = df$Away,
                        results = res,
                        winners = ifelse(
                            res >= (input$games + 1)/2,
                            df$Home,
                            df$Away
                        )
                    )
                },

                "demo" = {
                    res <- demo_scores[[rv$round]]
                    list(
                        home=df$Home,
                        away=df$Away,
                        results=res,
                        winners=ifelse(res>=(input$games+1)/2,df$Home,df$Away)
                    )
                }
            )

            if(is.null(sim)){
                return()
            }

            rv$estimation_df <- dplyr::bind_rows(
                rv$estimation_df,
                data.frame(
                    round=rv$round,
                    home_colours=rv$player_colours[as.character(sim$home)],
                    away_colours=rv$player_colours[as.character(sim$away)],
                    results=sim$results
                )
            )

            rv$current_players <- sim$winners
            rv$round <- rv$round + 1
            rv$sim_preview <- NULL

            if(length(sim$winners) == 1){
                rv$confetti <- TRUE
                session$sendCustomMessage("confetti", list())
            }

            if(!rv$estimated && rv$round > input$estimate_round && nrow(rv$estimation_df)>0){

                fit <- optim(
                    c(0,0,0,0),
                    activity7_neg_log_lik,
                    dice_history=rv$estimation_df,
                    n_games_per_match=input$games
                )

                rv$pars_df <- data.frame(
                    parameter = c("blue","red","green","yellow","home advantage"),
                    value = sprintf("%.3f", c(0, fit$par))
                )

                winner_vec <- replicate(nsim, {

                    players <- rv$current_players

                    repeat {
                        n <- length(players)
                        if (n == 1) break

                        h <- players[seq(1, n, 2)]
                        a <- players[seq(2, n, 2)]

                        res <- activity7_round_sim_using_fits(
                            fit$par,
                            rv$player_colours[h],
                            rv$player_colours[a],
                            input$games
                        )

                        players <- ifelse(res >= (input$games + 1) / 2, h, a)
                    }

                    players
                })

                all_players <- as.character(rv$current_players)
                tab <- table(factor(winner_vec, levels = all_players)) / nsim

                rv$winner_probs <- tab
                rv$estimated <- TRUE
            }
        }
    )

    observeEvent(input$mode, {

        if(input$mode == "demo"){

            updateNumericInput(
                session,
                "nrounds",
                value = 5
            )

            updateNumericInput(
                session,
                "games",
                value = 5
            )

            updateNumericInput(
                session,
                "estimate_round",
                value = 2
            )

            shinyjs::disable("nrounds")
            shinyjs::disable("games")
            shinyjs::disable("estimate_round")

        } else {

            shinyjs::enable("nrounds")
            shinyjs::enable("games")
            shinyjs::enable("estimate_round")

        }

    }, ignoreInit = FALSE)

    observeEvent(input$next_round, {

        rv$sim_preview <- NULL

        rv$sim_ready <- FALSE

    })

    output$round_section <- renderUI({

        if(is.null(rv$current_players)){

            return(
                div(

                    style = "
                    text-align:center;
                    padding:40px;
                    color:#6C757D;
                ",

                    h4("Tournament not started"),

                    p(
                        "Choose your settings and press ",
                        tags$strong("Start Tournament"),
                        " to generate the draw."
                    ),

                    p(
                        "Players will then be randomly assigned positions and dice colours before the first round begins."
                    )
                )
            )
        }

        if(length(rv$current_players) < 2){

            return(
                div(
                    h2("🏆 Tournament Winner"),

                    player_badge(
                        rv$current_players[1],
                        rv$player_colours[rv$current_players[1]]
                    )
                )
            )
        }

        df <- fixtures_df()

        tagList(

            h3(paste("Round", rv$round)),

            lapply(1:nrow(df), function(i){

                div(
                    class = "card-style",

                    fluidRow(

                        # =========================
                        # FIXTURE
                        # =========================

                        column(
                            width = 7,

                            player_badge(
                                df$Home[i],
                                df$HomeColour[i]
                            ),

                            " vs ",

                            player_badge(
                                df$Away[i],
                                df$AwayColour[i]
                            )
                        ),


                        # =========================
                        # SCORES
                        # =========================

                        if(input$mode %in% c("human","demo")){

                            column(
                                width = 5,

                                fluidRow(

                                    column(
                                        width = 6,

                                        numericInput(
                                            paste0("match_", rv$round, "_", i),

                                            rv$display_names[df$Home[i]],

                                            value =
                                                if(input$mode=="demo")
                                                    demo_scores[[rv$round]][i]
                                            else
                                                NULL,

                                            min = 0,
                                            max = input$games,
                                            width = "100%"
                                        )
                                    ),


                                    column(
                                        width = 6,

                                        numericInput(
                                            paste0(
                                                "match_",
                                                rv$round,
                                                "_",
                                                i,
                                                "_away"
                                            ),

                                            rv$display_names[df$Away[i]],

                                            value =
                                                if(input$mode=="demo")
                                                    input$games - demo_scores[[rv$round]][i]
                                            else
                                                NULL,

                                            min = 0,
                                            max = input$games,
                                            width = "100%"
                                        )
                                    )
                                )
                            )

                        } else if(input$mode=="sim" && !is.null(rv$sim_preview)){

                            column(
                                width = 5,

                                tags$span(
                                    style="
                                display:inline-block;
                                margin-top:15px;
                                padding:6px 12px;
                                background:#EEF2FF;
                                border-radius:8px;
                                ",

                                    paste0(
                                        "Result: ",
                                        rv$sim_preview$results[i],
                                        " - ",
                                        input$games -
                                            rv$sim_preview$results[i]
                                    )
                                )
                            )
                        }
                    )
                )
            })
        )
    })



    output$action_ui <- renderUI({

        req(rv$current_players, input$mode)

        if(length(rv$current_players)==1) return(NULL)

        if(input$mode=="human"){

            tagList(

                p(
                    style="
                font-size:14px;
                color:#555;
                margin-bottom:15px;
                ",
                    "Enter results before moving to the next round."
                ),

                uiOutput("result_inputs"),

                actionButton(
                    "submit_results",
                    "Submit Results",
                    class="btn-primary"
                )
            )

        } else if(input$mode=="demo"){

            tagList(

                uiOutput("result_inputs"),

                actionButton(
                    "demo_next",
                    "Next Round",
                    class="btn-primary"
                )
            )

        } else {

            tagList(

                if(rv$sim_ready){

                    shinyjs::disabled(
                        actionButton(
                            "simulate_results",
                            "Simulate Results",
                            class="btn-primary"
                        )
                    )

                } else {

                    actionButton(
                        "simulate_results",
                        "Simulate Results",
                        class="btn-primary"
                    )

                },

                tags$div(
                    style="height:10px;"
                ),

                if(!rv$sim_ready){

                    shinyjs::disabled(
                        actionButton(
                            "next_round",
                            "Next Round",
                            class="btn-primary"
                        )
                    )

                } else {

                    actionButton(
                        "next_round",
                        "Next Round",
                        class="btn-primary"
                    )

                }

            )
        }
    })


    output$prob_section <- renderUI({

        if(is.null(rv$pars_df) || is.null(rv$winner_probs)){
            return(NULL)
        }

        fluidRow(

            column(
                width = 6,

                h4("Estimated Parameters"),

                tableOutput("pars_table")
            ),

            column(
                width = 6,

                h4("Tournament Winner Probabilities"),

                tableOutput("win_probs")
            )
        )

    })

    output$pars_table <- renderTable({
        req(rv$pars_df)
        rv$pars_df
    })

    output$win_probs <- renderTable({

        req(rv$winner_probs)

        ids <- as.numeric(names(rv$winner_probs))

        data.frame(
            Player = rv$display_names[ids],
            Probability = sprintf("%.3f", as.numeric(rv$winner_probs))
        )
    })

    observe({
        if(rv$confetti){
            invalidateLater(2000)
            rv$confetti <- FALSE
        }
    })
}

shinyApp(ui, server)
