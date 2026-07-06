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

        total_home_wins[i] <- sum(home_score - away_score >= 0)
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

                    (function frame() {
                        confetti({ particleCount: 6, spread: 60, origin: { x: 0 } });
                        confetti({ particleCount: 6, spread: 60, origin: { x: 1 } });

                        if (Date.now() < end) requestAnimationFrame(frame);
                    })();
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

                radioButtons(
                    "mode",
                    "Mode",
                    c(
                        "Human (enter results)" = "human",
                        "Simulation (auto-play)" = "sim",
                        "Demo (guided example)" = "demo"
                    )
                ),

                numericInput("nrounds", "Number of rounds", 5, 1, 8),
                numericInput("games", "Games per match", 5, 1, step = 2),
                numericInput("estimate_round", "Estimation round", 2, 1),
                numericInput("nsim", "Simulations", 1000, 100),
                numericInput("seed", "Seed", 999),

                actionButton("start", "Start Tournament", class = "btn-primary")
            ),

            div(
                class = "card-style",
                uiOutput("round_section"),
                hr(),
                uiOutput("prob_section")
            ),

            div(
                class = "card-style",
                uiOutput("action_ui")
            )
        )
    )
)

# =========================================================
# SERVER (UNCHANGED)
# =========================================================

server <- function(input, output, session){

    colours <- c("blue","red","green","yellow")

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
        estimation_df=NULL,
        winner_probs=NULL,
        pars_df=NULL,
        estimated=FALSE,
        sim_preview=NULL,
        confetti=FALSE
    )

    player_badge <- function(id,col)
        div(class=paste("player-badge",paste0("badge-",col)),paste("Player",id))

    observeEvent(input$start,{
        set.seed(input$seed)

        nplayers <- 2^input$nrounds

        rv$player_colours <- sample(rep(colours,length.out=nplayers))
        rv$current_players <- 1:nplayers
        rv$round <- 1
        rv$estimation_df <- data.frame()
        rv$sim_preview <- NULL
        rv$estimated <- FALSE
        rv$confetti <- FALSE
    })

    fixtures_df <- reactive({
        req(rv$current_players)

        if(length(rv$current_players) < 2) return(NULL)

        data.frame(
            Home = rv$current_players[seq(1, length(rv$current_players), 2)],
            Away = rv$current_players[seq(2, length(rv$current_players), 2)],
            HomeColour = rv$player_colours[seq(1, length(rv$current_players), 2)],
            AwayColour = rv$player_colours[seq(2, length(rv$current_players), 2)]
        )
    })

    simulate_round_only <- function(players){

        home <- players[seq(1,length(players),2)]
        away <- players[seq(2,length(players),2)]

        results <- activity7_round_sim(
            rv$player_colours[home],
            rv$player_colours[away],
            input$games
        )

        winners <- ifelse(results >= (input$games+1)/2, home, away)

        list(home=home,away=away,results=results,winners=winners)
    }

    observeEvent(input$simulate_results,{
        req(rv$current_players)
        rv$sim_preview <- simulate_round_only(rv$current_players)
    })

    observeEvent(
        {
            if(input$mode=="sim") input$next_round
            else if(input$mode=="demo") input$demo_next
            else input$submit_results
        },
        {

            req(rv$current_players)

            df <- fixtures_df()
            req(!is.null(df))

            sim <- switch(
                input$mode,

                "sim" = if(!is.null(rv$sim_preview)) rv$sim_preview else simulate_round_only(rv$current_players),

                "human" = {
                    res <- sapply(1:nrow(df), function(i) input[[paste0("match_",i)]])
                    res <- as.numeric(res)

                    if(any(is.na(res) | res < 0 | res > input$games)) {
                        showNotification(
                            paste0("All values must be between 0 and ", input$games),
                            type = "warning",
                            duration = 5
                        )
                        return(NULL)
                    }

                    list(
                        home = df$Home,
                        away = df$Away,
                        results = res,
                        winners = ifelse(res >= (input$games + 1)/2, df$Home, df$Away)
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

            rv$estimation_df <- dplyr::bind_rows(
                rv$estimation_df,
                data.frame(
                    round=rv$round,
                    home_colours=rv$player_colours[sim$home],
                    away_colours=rv$player_colours[sim$away],
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

                winner_vec <- replicate(input$nsim, {

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
                tab <- table(factor(winner_vec, levels = all_players)) / input$nsim

                rv$winner_probs <- tab
                rv$estimated <- TRUE
            }
        }
    )

    output$round_section <- renderUI({

        req(rv$current_players)

        if(length(rv$current_players) < 2){
            return(div(
                h2("🏆 Champion"),
                player_badge(rv$current_players, rv$player_colours[rv$current_players])
            ))
        }

        df <- fixtures_df()

        tagList(
            h3(paste("Round",rv$round)),

            if(input$mode=="sim" && !is.null(rv$sim_preview)){
                lapply(seq_along(rv$sim_preview$results), function(i){
                    div(
                        player_badge(df$Home[i],df$HomeColour[i]),
                        " vs ",
                        player_badge(df$Away[i],df$AwayColour[i]),
                        tags$span(
                            style="margin-left:10px;padding:4px 8px;background:#EEF2FF;border-radius:8px;",
                            paste0("Home wins: ",rv$sim_preview$results[i])
                        )
                    )
                })
            } else {
                lapply(1:nrow(df), function(i){
                    div(
                        player_badge(df$Home[i],df$HomeColour[i]),
                        " vs ",
                        player_badge(df$Away[i],df$AwayColour[i])
                    )
                })
            }
        )
    })

    output$result_inputs <- renderUI({

        req(input$mode %in% c("human","demo"))
        df <- fixtures_df()
        req(!is.null(df))

        tagList(
            h3("Home Scores"),

            div(
                style="display:grid;grid-template-columns:repeat(8,1fr);gap:10px;",

                lapply(1:nrow(df), function(i){

                    default_value <- if(input$mode=="demo") demo_scores[[rv$round]][i] else 0

                    numericInput(
                        paste0("match_",i),
                        paste("Match",i),
                        value=default_value,
                        min=0,
                        max=input$games,
                        width="100%"
                    )
                })
            )
        )
    })

    output$action_ui <- renderUI({

        req(rv$current_players)

        if(length(rv$current_players)==1) return(NULL)

        if(input$mode=="human"){
            tagList(uiOutput("result_inputs"),
                    actionButton("submit_results","Submit",class="btn-primary"))

        } else if(input$mode=="demo"){
            tagList(uiOutput("result_inputs"),
                    actionButton("demo_next","Next",class="btn-primary"))

        } else {
            tagList(
                actionButton("simulate_results","Simulate",class="btn-primary"),
                actionButton("next_round","Next",class="btn-primary")
            )
        }
    })

    output$prob_section <- renderUI({

        req(rv$pars_df)

        tagList(
            h3("Parameter Estimates"),
            tableOutput("pars_table"),
            h3("Win Probabilities"),
            tableOutput("win_probs")
        )
    })

    output$pars_table <- renderTable({
        req(rv$pars_df)
        rv$pars_df
    })

    output$win_probs <- renderTable({
        req(rv$winner_probs)

        data.frame(
            Player = names(rv$winner_probs),
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
