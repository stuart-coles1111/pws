
# =========================================================
# PLOTTING
# =========================================================

league_position_plot <- function(df, league_position, rows = 4, scales = "fixed") {

    df_out <- data.frame(position = NULL)
    team_id <- 1:20

    for (i in team_id) {
        df_team <- data.frame(
            team = rep(df[i, 1], 10000),
            position = as.numeric(league_position[i, ])
        )
        df_out <- rbind(df_out, df_team)
    }

    ggplot(df_out, aes(x = position)) +
        geom_bar(
            fill = "lightblue",
            aes(y = 20 * after_stat(count) / sum(after_stat(count)))
        ) +
        scale_x_continuous(
            breaks = c(1, 5, 10, 15, 20),
            limits = c(0.5, 20.5)
        ) +
        xlab("Final league position") +
        ylab("Probability") +
        facet_wrap(~ team, nrow = rows, scales = scales)
}

# =========================================================
# SIMULATION CORE
# =========================================================

season_sim <- function(df, team_h, team_a, tau) {

    mu_h <- exp(
        tau +
            df[match(team_h, df$teams), "alpha"] -
            df[match(team_a, df$teams), "beta"]
    )

    mu_a <- exp(
        df[match(team_a, df$teams), "alpha"] -
            df[match(team_h, df$teams), "beta"]
    )

    g_h <- rpois(length(team_h), mu_h)
    g_a <- rpois(length(team_a), mu_a)

    p_h <- ifelse(g_h > g_a, 3, ifelse(g_h == g_a, 1, 0))
    p_a <- ifelse(g_h > g_a, 0, ifelse(g_h == g_a, 1, 3))

    list(p_h, p_a, g_h - g_a, g_h, g_a)
}

league_sim <- function(df, schedule, tau) {

    results <- season_sim(
        df,
        schedule[,2],
        schedule[,3],
        tau
    )

    points <- schedule
    points$hp <- results[[1]]
    points$ap <- results[[2]]
    points$gd <- results[[3]]
    points$gh <- results[[4]]
    points$ga <- results[[5]]

    df_h <- points %>%
        group_by(Home.Team) %>%
        summarise(tot = sum(hp), gd = sum(gd), gf = sum(gh), .groups = "drop")

    df_a <- points %>%
        group_by(Away.Team) %>%
        summarise(tot = sum(ap), gd = -sum(gd), gf = sum(ga), .groups = "drop")

    df_out <- df_h
    df_out[,-1] <- df_h[,-1] + df_a[,-1]

    match(
        df$teams,
        arrange(df_out, desc(tot), desc(gd), desc(gf))[[1]]
    )
}

# =========================================================
# UI
# =========================================================

chapter8_ui <- function(id) {

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("A Football Calculator"),

        hr(),

        h5("Data entry (optional)"),

        fileInput(
            ns("pars_file"),
            "Upload team parameters CSV",
            accept = ".csv"
        ),

        fileInput(
            ns("tau_file"),
            "Upload home advantage CSV",
            accept = ".csv"
        ),

        fileInput(
            ns("schedule_file"),
            "Upload fixture CSV",
            accept = ".csv"
        ),

        selectInput(
            ns("team1"),
            "Home team",
            choices = NULL,
            selected = "Manchester City"
        ),

        selectInput(
            ns("team2"),
            "Away team",
            choices = NULL,
            selected = "Liverpool"
        ),

        hr(),

        h5("Model parameters"),

        sliderInput(
            ns("alpha_home"),
            "Home attack (α)",
            min = -1.5,
            max = 1.5,
            value = 0,
            step = 0.01
        ),

        sliderInput(
            ns("beta_home"),
            "Home defence (β)",
            min = -1.5,
            max = 1.5,
            value = 0,
            step = 0.01
        ),

        sliderInput(
            ns("alpha_away"),
            "Away attack (α)",
            min = -1.5,
            max = 1.5,
            value = 0,
            step = 0.01
        ),

        sliderInput(
            ns("beta_away"),
            "Away defence (β)",
            min = -1.5,
            max = 1.5,
            value = 0,
            step = 0.01
        ),

        sliderInput(
            ns("tau"),
            "Home advantage (τ)",
            min = 0,
            max = 1,
            value = 0.2,
            step = 0.01
        ),

        hr(),

        uiOutput(ns("data_source")),

        numericInput(ns("seed"), "Random seed", value = 44),

        numericInput(ns("n_sim"), "Number of  seasons to simulate",
                     value = 1000, min = 100, step = 100),


        hr(),

        actionButton(ns("run_static"),
                     "Calculate static league position probabilities"),

        hr(),

        sliderInput(ns("sigma"), "Dynamic variation (sigma)",
                    min = 0, max = 0.2, value = 0.05, step = 0.01),

        actionButton(ns("run_dynamic"),
                     "Calculate dynamic league position probabilities"),

        hr(),

        selectInput(
            ns("comparison_team"),
            "Team for comparison",
            choices = NULL,
            selected = "Arsenal"
        ),

        actionButton(ns("run_compare"),
                     "Compare static vs dynamic")
    )

    overview_panel <- div(

        card(

            style = "
        border-radius: 16px;
        border: none;
        box-shadow: 0 4px 12px rgba(0,0,0,0.08);
        padding: 10px;
    ",

            card_header(
                div(
                    "🕸️ Understanding Football as a Complex System",
                    style = "
                font-size: 1.4rem;
                font-weight: 700;
                color: #2c3e50;
            "
                )
            ),

            p(
                strong("Main idea: "),
                "A football season is not just a collection of matches.
            It is a complex system where uncertainty at the match level
            aggregates into unpredictable season-level outcomes."
            ),

            hr(),

            h5("What is happening in this chapter?"),

            tags$div(
                style = "margin-left: 10px;",

                p("① We model match outcomes using a Poisson goal-scoring process."),

                p("② Team strength is represented through attack, defence, and home advantage."),

                p("③ We simulate full league seasons many times to capture uncertainty in final standings."),

                p("④ We compare two worlds: one where team strength is fixed, and one where it evolves over time.")
            ),

            hr(),

            h5("Your job"),

            p(
                "Use simulation to explore how randomness and structural assumptions shape an entire league season."
            ),

            tags$ul(
                tags$li("Simulate full seasons under different assumptions"),
                tags$li("Compare static and dynamic representations of team strength"),
                tags$li("Investigate how uncertainty propagates from matches to league tables"),
                tags$li("Explore how parameter choices affect long-run outcomes")
            ),

            hr(),

            h5("What will you see?"),

            tags$ul(
                tags$li("Simulated distributions of final league positions"),
                tags$li("Match-level win probabilities between teams"),
                tags$li("Side-by-side comparisons of static vs dynamic models"),
                tags$li("Visual evidence of how uncertainty changes across model assumptions")
            ),

            hr(),

            div(
                style = "
            background-color: #f8f9fa;
            border-left: 5px solid #7B9ACC;
            padding: 12px;
            border-radius: 8px;
        ",

                h5("Questions to investigate"),

                tags$ul(
                    tags$li("How much of a season’s outcome is determined by randomness?"),
                    tags$li("What changes when team strength is allowed to evolve?"),
                    tags$li("Why do strong teams still fail to finish at the top sometimes?"),
                    tags$li("When does a static model become too simple to be useful?")
                )
            )
        )
    )

    code_panel <- div(

        card(
            card_header("Generated R Code"),

            tags$pre(textOutput(ns("generated_code")))
        )
    )

    results_panel <- div(

        uiOutput(ns("sim_banner")),

        layout_columns(

            card(
                card_header("Match Summary"),
                uiOutput(ns("match_summary"))
            ),

            card(
                card_header("Scoreline Probability Matrix"),
                plotOutput(
                    ns("score_matrix"),
                    height = 350
                )
            ),

            col_widths = c(3, 9)

        ),

        br(),

        accordion(

            open = FALSE,

            accordion_panel(

                title = "📖 Simulation Pipeline",

                tags$ol(

                    tags$li(
                        strong("Estimate team strengths"),
                        " using attack (α), defence (β) and home advantage (τ)."
                    ),

                    tags$li(
                        strong("Convert strengths into expected goals"),
                        " using the Poisson model."
                    ),

                    tags$li(
                        strong("Simulate every match"),
                        " in the season."
                    ),

                    tags$li(
                        strong("Award league points"),
                        " and apply tie-break rules."
                    ),

                    tags$li(
                        strong("Repeat thousands of times"),
                        " to estimate uncertainty in final league positions."
                    ),

                    tags$li(
                        strong("Dynamic model only:"),
                        " allow team strengths to evolve throughout the season."
                    )

                )

            )

        ),

        br(),

        card(
            card_header("Final League Positions (Static Model)"),
            plotOutput(ns("static_plot"), height = 650)
        ),

        br(),

        card(
            card_header("Final League Positions (Dynamic Model)"),
            plotOutput(ns("dynamic_plot"), height = 650)
        ),

        br(),

        card(
            card_header("Static vs Dynamic Comparison"),
            plotOutput(ns("comparison_plot"), height = 400)
        )
    )



    chapter_page_ui(
        id = id,
        title = "🕸️ Chapter 8: Complexity",
        sidebar = sidebar_controls,
        overview = overview_panel,
        code = code_panel,
        results = results_panel,
        learn = learn_panel
    )
}

# =========================================================
# SERVER
# =========================================================

chapter8_server <- function(id) {

    moduleServer(id, function(input, output, session) {

        validate_teams <- function(df){

            required <- c(
                "teams",
                "alpha",
                "beta"
            )

            if(!all(required %in% names(df))){
                stop(
                    "Parameters file must contain: teams, alpha, beta"
                )
            }

            df
        }


        teams_data <- reactive({

            if(is.null(input$pars_file)){

                PL24_pars$teams

            } else {

                validate_teams(
                    read.csv(input$pars_file$datapath)
                )

            }

        })

        tau_data <- reactive({

            if (is.null(input$tau_file)) {

                PL24_pars$tau

            } else {

                read.csv(input$tau_file$datapath)$tau[1]

            }

        })


        schedule_data <- reactive({

            if (is.null(input$schedule_file)) {

                PL25_schedule

            } else {

                read.csv(
                    input$schedule_file$datapath,
                    stringsAsFactors = FALSE
                )

            }

        })
        observeEvent(teams_data(), {

            updateSelectInput(
                session,
                "team1",
                choices = teams_data()$teams
            )

            updateSelectInput(
                session,
                "team2",
                choices = teams_data()$teams
            )

            updateSelectInput(
                session,
                "comparison_team",
                choices = teams_data()$teams
            )

        })

        observeEvent(input$team1, {

            home <- teams_data() |>
                dplyr::filter(
                    teams == input$team1
                )

            updateSliderInput(
                session,
                "alpha_home",
                value = home$alpha
            )

            updateSliderInput(
                session,
                "beta_home",
                value = home$beta
            )

        }, ignoreInit = FALSE)

        observeEvent(input$team2, {

            away <- teams_data() |>
                dplyr::filter(
                    teams == input$team2
                )

            updateSliderInput(
                session,
                "alpha_away",
                value = away$alpha
            )

            updateSliderInput(
                session,
                "beta_away",
                value = away$beta
            )

        }, ignoreInit = FALSE)

        match_means <- reactive({

            list(

                home =
                    exp(
                        input$tau +
                            input$alpha_home -
                            input$beta_away
                    ),

                away =
                    exp(
                        input$alpha_away -
                            input$beta_home
                    )

            )

        })

        score_matrix_data <- reactive({

            mu <- match_means()

            hp <- c(
                dpois(0:5, mu$home),
                1 - ppois(5, mu$home)
            )

            ap <- c(
                dpois(0:5, mu$away),
                1 - ppois(5, mu$away)
            )

            df <- expand.grid(

                Home = factor(
                    c(0:5, "6+"),
                    levels = c(0:5, "6+")
                ),

                Away = factor(
                    c(0:5, "6+"),
                    levels = c(0:5, "6+")
                )

            )

            df$Prob <- as.vector(
                outer(
                    hp,
                    ap
                )
            )

            df

        })

        output$data_source <- renderUI({

            if(is.null(input$pars_file)){

                "Using built-in 2024/25 model parameters"

            } else {

                paste(
                    "Using uploaded parameters:",
                    input$pars_file$name
                )

            }

        })

        output$match_summary <- renderUI({

            mu <- match_means()

            df <- score_matrix_data()

            best <- df[which.max(df$Prob), ]

            probs <- match_win_probs(

                c(
                    input$alpha_home,
                    input$beta_home
                ),

                c(
                    input$alpha_away,
                    input$beta_away
                ),

                input$tau
            )

            tags$div(

                style = "padding:10px;",

                h5("Expected goals"),

                p(
                    strong(input$team1),
                    paste0(": ", round(mu$home, 2)),
                    br(),
                    strong(input$team2),
                    paste0(": ", round(mu$away, 2))
                ),

                h5("Most likely score"),

                p(
                    paste0(best$Home, " – ", best$Away),
                    br(),
                    scales::percent(
                        best$Prob,
                        accuracy = 0.1
                    )
                ),

                h5("Total expected goals"),

                p(
                    round(
                        mu$home + mu$away,
                        2
                    )
                )
            )
        })

        output$score_matrix <- renderPlot({

            df <- score_matrix_data()

            ggplot(

                df,

                aes(
                    x = Away,
                    y = Home,
                    fill = Prob
                )

            ) +

                geom_tile() +

                geom_text(

                    aes(
                        label = ifelse(
                            Prob < 0.005,
                            "",
                            scales::percent(
                                Prob,
                                accuracy = 0.1
                            )
                        )
                    ),

                    size = 3

                ) +

                scale_fill_gradient(
                    low = "#f7f7f7",
                    high = "#4C78A8",
                    labels = scales::percent
                ) +

                labs(
                    x = "Away goals",
                    y = "Home goals",
                    fill = "Probability"
                ) +

                theme_minimal()

        })

        # -------------------------
        # BANNER STATE
        # -------------------------

        rv <- reactiveValues(sim_running = FALSE)


        output$sim_banner <- renderUI({
            if (rv$sim_running) {
                div(
                    style = "
                        padding: 12px;
                        margin-bottom: 10px;
                        background-color: #fff3cd;
                        border: 1px solid #ffeeba;
                        border-radius: 6px;
                        font-weight: 600;",
                    "Simulations in Progress"
                )
            }
        })

        # -------------------------
        # MATCH PROBS
        # -------------------------

        output$match_probs <- DT::renderDT({

            probs <- match_win_probs(

                c(
                    input$alpha_home,
                    input$beta_home
                ),

                c(
                    input$alpha_away,
                    input$beta_away
                ),

                input$tau
            )

            DT::datatable(
                df,
                options = list(
                    dom = "t",
                    paging = FALSE,
                    searching = FALSE,
                    info = FALSE
                ),
                rownames = FALSE
            )


        })



        output$generated_code <- renderText({

            paste0(
                "# Static model\n",
                "league_sim(\n",
                "  teams = teams_data(),\n",
                "  tau = ", round(tau_data(), 3), "\n",
                ")\n\n",

                "# Dynamic model\n",
                "dynamic_league_sim(\n",
                "  teams = teams_data(),\n",
                "  schedule = schedule_data(),\n",
                "  tau = ", round(tau_data(), 3), ",\n",
                "  sigma = ", input$sigma, "\n",
                ")"
            )

        })

        # =====================================================
        # STATIC SIM (FIXED)
        # =====================================================

        static_sim <- reactiveVal(NULL)

        observeEvent(input$run_static, {

            rv$sim_running <- TRUE

            seed <- input$seed
            n_sim <- input$n_sim
            teams <- teams_data()
            tau <- tau_data()
            sched <- schedule_data()

            later::later(function() {

                set.seed(seed)

                sims <- sapply(
                    1:n_sim,
                    function(x) league_sim(
                        teams,
                        sched,
                        tau
                    )
                )

                rownames(sims) <- teams$teams
                static_sim(sims)

                rv$sim_running <- FALSE

            }, 0.05)
        })
        # =====================================================
        # DYNAMIC SIM (FIXED)
        # =====================================================

        make_dynamic <- function(teams, ro = 0.9, sigma = 0.1) {

            teams_dynamic <- teams
            teams_dynamic <- cbind(teams_dynamic, matrix(0, nr = 20, nc = 76))

            colnames(teams_dynamic)[4:41] <- paste0("a_round_", 1:38)
            colnames(teams_dynamic)[42:79] <- paste0("b_round_", 1:38)

            for (i in 1:20) {
                td <- mvrnorm(
                    37,
                    c(0, 0),
                    matrix(c(1, ro, ro, 1), nr = 2) * sigma^2
                )

                td <- rbind(c(0, 0), td)

                ad <- teams[i, "alpha"] + cumsum(td[, 1])
                bd <- teams[i, "beta"] + cumsum(td[, 2])

                teams_dynamic[i, 8:45] <- ad
                teams_dynamic[i, 46:83] <- bd
            }

            teams_dynamic
        }

        dynamic_season_sim <- function(df, round, team_h, team_a, tau,
                                       ro = 0.9, sigma = 0.1) {

            teams_dynamic <- make_dynamic(df, ro = ro, sigma = sigma)

            mu_h <- mu_a <- c()

            for (i in 1:length(round)) {

                mu_h <- c(mu_h,
                          exp(tau +
                                  teams_dynamic[match(team_h[i], df$teams), round[i] + 7] -
                                  teams_dynamic[match(team_a[i], df$teams), round[i] + 45])
                )

                mu_a <- c(mu_a,
                          exp(
                              teams_dynamic[match(team_a[i], df$teams), round[i] + 7] -
                                  teams_dynamic[match(team_h[i], df$teams), round[i] + 45]
                          )
                )
            }

            g_h <- rpois(length(team_h), mu_h)
            g_a <- rpois(length(team_a), mu_a)

            p_h <- ifelse(g_h > g_a, 3, ifelse(g_h == g_a, 1, 0))
            p_a <- ifelse(g_h > g_a, 0, ifelse(g_h == g_a, 1, 3))

            list(p_h, p_a, g_h - g_a, g_h, g_a)
        }

        dynamic_league_sim <- function(df, schedule, tau, sigma = 0.1) {

            results <- dynamic_season_sim(
                df,
                schedule[,1],
                schedule[,2],
                schedule[,3],
                tau,
                sigma = sigma
            )

            points <- schedule
            points$hp <- results[[1]]
            points$ap <- results[[2]]
            points$gd <- results[[3]]
            points$gh <- results[[4]]
            points$ga <- results[[5]]

            df_h <- points %>%
                group_by(Home.Team) %>%
                summarise(tot = sum(hp), gd = sum(gd), gf = sum(gh), .groups = "drop")

            df_a <- points %>%
                group_by(Away.Team) %>%
                summarise(tot = sum(ap), gd = -sum(gd), gf = sum(ga), .groups = "drop")

            df_out <- df_h
            df_out[,-1] <- df_h[,-1] + df_a[,-1]

            match(df$teams,
                  arrange(df_out, desc(tot), desc(gd), desc(gf))[[1]])
        }

        dynamic_sim <- reactiveVal(NULL)

        observeEvent(input$run_dynamic, {

            rv$sim_running <- TRUE

            seed <- input$seed
            n_sim <- input$n_sim
            sigma <- input$sigma
            teams <- teams_data()
            tau <- tau_data()
            sched <- schedule_data()

            later::later(function() {

                set.seed(seed)

                sims <- sapply(
                    1:n_sim,
                    function(x)
                        dynamic_league_sim(
                            teams,
                            sched,
                            tau,
                            sigma
                        )
                )

                rownames(sims) <- teams$teams
                dynamic_sim(sims)

                rv$sim_running <- FALSE

            }, 0.05)
        })

        # -------------------------
        # PLOTS
        # -------------------------

        output$static_plot <- renderPlot({
            req(static_sim())
            league_position_plot(teams_data(), static_sim(), 4)
        })

        output$dynamic_plot <- renderPlot({
            req(dynamic_sim())
            league_position_plot(teams_data(), dynamic_sim(), 4)
        })

        output$comparison_plot <- renderPlot({

            req(input$run_compare)
            req(static_sim(), dynamic_sim())

            team <- input$comparison_team

            s <- static_sim()[team, ]
            d <- dynamic_sim()[team, ]

            df <- data.frame(
                Position = c(s, d),
                Model = rep(c("Static", "Dynamic"), each = length(s))
            )

            ggplot(df, aes(Position, fill = Model)) +
                geom_bar(aes(y = after_stat(count / sum(count))),
                         position = "dodge") +
                labs(x = "League Position", y = "Probability") +
                theme_minimal()
        })
    })
}
