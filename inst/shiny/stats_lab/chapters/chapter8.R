
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
        facet_wrap(~ team, nrow = rows, scales = scales)+
        theme_minimal(base_size = 16) +
        theme(
            axis.title = element_text(size = 18, face = "bold"),
            axis.text = element_text(size = 14),
            strip.text = element_text(size = 14, face = "bold")
        )
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

        h5("Data source"),

        radioButtons(
            ns("data_source"),
            NULL,

            choices = c(
                "PL fits: 25/26; Predict: 26/27" = "current",
                "PL fits: 24/25; Predict: 25/26" = "previous",
                "Upload my own data" = "upload"
            ),

            selected = "current"
        ),

        conditionalPanel(
            condition = "input.data_source == 'upload'",
            ns = ns,

            fileInput(
                ns("pars_file"),
                "Upload team parameters CSV",
                accept = ".csv"
            ),

            helpText(
                "Required columns: teams, alpha, beta"
            ),

            downloadButton(
                ns("download_pars_template"),
                "Download parameters template"
            ),

            fileInput(
                ns("schedule_file"),
                "Upload fixture CSV",
                accept = ".csv"
            ),

            helpText(
                "Required columns: Round, Home.Team, Away.Team"
            ),

            downloadButton(
                ns("download_schedule_template"),
                "Download fixture template"
            )
        ),

        sliderInput(
            ns("tau"),
            "Home advantage (τ)",
            min = -0.5,
            max = 1,
            value = 0.2,
            step = 0.01
        ),

        h5("Analysis mode"),

        radioButtons(
            ns("analysis_mode"),
            NULL,
            choices = c(
                "Team analysis" = "team",
                "League simulation" = "league"
            ),
            selected = "team"
        ),

        hr(),
        conditionalPanel(
            condition = "input.analysis_mode == 'team'",
            ns = ns,

            selectInput(
                ns("team1"),
                "Home team",
                choices = NULL
            ),

            selectInput(
                ns("team2"),
                "Away team",
                choices = NULL
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
            )
        ),

        hr(),

        uiOutput(ns("data_source")),

        conditionalPanel(
            condition = "input.analysis_mode == 'league'",
            ns = ns,

            numericInput(
                ns("seed"),
                "Random seed",
                value = 44
            ),

            numericInput(
                ns("n_sim"),
                "Number of seasons to simulate",
                value = 1000,
                min = 100,
                step = 100
            ),

            hr(),

            actionButton(
                ns("run_static"),
                "Calculate static league position probabilities"
            ),

            hr(),

            sliderInput(
                ns("sigma"),
                "Dynamic variation (sigma)",
                min = 0,
                max = 0.2,
                value = 0.05,
                step = 0.01
            ),

            actionButton(
                ns("run_dynamic"),
                "Calculate dynamic league position probabilities"
            ),

            hr(),

            selectInput(
                ns("comparison_team"),
                "Team for comparison",
                choices = NULL,
                selected = "Arsenal"
            ),

            actionButton(
                ns("run_compare"),
                "Compare static vs dynamic",
                disabled = TRUE
            )
        )
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
                    "🕸️ Studying Output from a Football Model",
                    style = "
                    font-size: 1.4rem;
                    font-weight: 700;
                    color: #2c3e50;
                "
                )
            ),

            p(
                strong("Main idea: "),
                "This module allows you to explore how a statistical model of football matches can be used to calculate match probabilities and simulate possible final league positions. ",
                "Rather than treating a single simulated season as a prediction of exactly what will happen, the simulations show the range of outcomes that could plausibly occur given the assumptions of the model."
            ),

            hr(),

            h5("Background"),

            p(
                "The model used in this module is a simplified version of the Dixon–Coles approach to modelling football scores. ",
                "It assumes that the goals scored by the home and away teams are independent Poisson random variables, with their expected values determined by the attacking and defensive strengths of the teams and an additional home advantage."
            ),

            tags$ul(

                tags$li(
                    strong("Match model: "),
                    "expected goals depend on the attacking strength of one team, the defensive strength of the other team, and the home advantage."
                ),

                tags$li(
                    strong("Team strengths: "),
                    "each team has parameters describing its attacking and defensive ability."
                ),

                tags$li(
                    strong("Team analysis: "),
                    "the model can be used to explore the probabilities of different scorelines and match outcomes for two teams."
                ),

                tags$li(
                    strong("League simulation: "),
                    "the model can be used repeatedly to simulate an entire season and examine the distribution of possible final league positions."
                ),

                tags$li(
                    strong("Dynamic model: "),
                    "team strengths can be allowed to change over the course of a season, introducing an additional source of uncertainty."
                )

            ),

            p(
                "The module therefore provides a way to move from a model for an individual match to a simulation of an entire football season."
            ),

            hr(),

            h5("Your options"),

            p(
                "You can choose between pre-specified Premier League data or provide your own team parameters and fixture list."
            ),

            tags$ul(

                tags$li(
                    strong("Use the current Premier League data: "),
                    "use fitted parameters from the 2025/26 season to explore possible outcomes for the 2026/27 season."
                ),

                tags$li(
                    strong("Use the previous Premier League data: "),
                    "use fitted parameters from the 2024/25 season to explore possible outcomes for the 2025/26 season."
                ),

                tags$li(
                    strong("Upload your own data: "),
                    "provide team attack and defence parameters together with a fixture list, allowing the model to be applied to another league or season."
                )

            ),

            p(
                "You can then choose between two types of analysis:"
            ),

            tags$ul(

                tags$li(
                    strong("Team analysis: "),
                    "select two teams and examine the probabilities associated with different possible scorelines."
                ),

                tags$li(
                    strong("League simulation: "),
                    "simulate many complete seasons and examine the probability distribution of each team's final league position."
                )

            ),

            p(
                "For league simulations, you can also compare a static model, in which team strengths remain fixed, with a dynamic model in which team strengths are allowed to vary during the season, with the amount of variation controlled by you."
            ),

            hr(),

            h5("A note on model complexity"),

            p(
                "The dynamic model is more complicated than the static model because it allows team strengths to change during the season. ",
                "However, greater complexity does not automatically make a model better."
            ),

            p(
                "A more complicated model introduces additional assumptions and sources of variation. ",
                "The useful question is therefore whether that additional complexity provides a more informative representation of the process we are trying to understand."
            ),

            p(
                "As you explore the results, consider not only which model produces the most interesting or realistic-looking distributions, but also what assumptions are responsible for those differences."
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

                    tags$li(
                        "How much of a football season's outcome is determined by team strength and how much by random variation?"
                    ),

                    tags$li(
                        "How different are the conclusions from analysing a single match compared with simulating an entire season?"
                    ),

                    tags$li(
                        "Why can a team with a high expected finishing position still have a substantial probability of finishing much lower?"
                    ),

                    tags$li(
                        "How does allowing team strength to change over time affect the distribution of possible league positions?"
                    ),

                    tags$li(
                        "Why might two teams with different strengths still have overlapping distributions of possible finishing positions?"
                    ),

                    tags$li(
                        "When does a more complicated model provide useful additional information?"
                    )

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

        conditionalPanel(
            condition = "input.analysis_mode == 'team'",
            ns = ns,

            layout_columns(

                card(
                    card_header("Scoreline Probability Matrix"),
                    plotOutput(
                        ns("score_matrix"),
                        height = 450
                    )
                ),

                col_widths = c(12)

            ),

            br(),

            card(
                card_header("Match Summary"),
                uiOutput(ns("match_summary"))
            )
        ),


        conditionalPanel(
            condition = "input.analysis_mode == 'league'",
            ns = ns,

            card(
                card_header("Final League Positions (Static Model)"),
                plotOutput(
                    ns("static_plot"),
                    height = 650
                )
            ),

            br(),

            card(
                card_header("Final League Positions (Dynamic Model)"),
                plotOutput(
                    ns("dynamic_plot"),
                    height = 650
                )
            ),

            br(),

            card(
                card_header("Static vs Dynamic Comparison"),
                plotOutput(
                    ns("comparison_plot"),
                    height = 400
                )
            )
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

        static_sim <- reactiveVal(NULL)
        dynamic_sim <- reactiveVal(NULL)

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


        # =====================================================
        # DATA SOURCE
        # =====================================================

        teams_data <- reactive({

            source <- input$data_source

            if (source == "current") {

                validate_teams(
                    PL25_pars$teams
                )

            } else if (source == "previous") {

                validate_teams(
                    PL24_pars$teams
                )

            } else if (source == "upload") {

                req(input$pars_file)

                validate_teams(
                    read.csv(
                        input$pars_file$datapath,
                        stringsAsFactors = FALSE
                    )
                )

            }

        })

        observeEvent(input$data_source, {

            tau <- switch(
                input$data_source,

                current = PL25_pars$tau,

                previous = PL24_pars$tau,

                upload = NULL
            )

            if (
                !is.null(tau) &&
                length(tau) == 1 &&
                !is.na(tau)
            ) {

                updateSliderInput(
                    session,
                    "tau",
                    value = tau
                )

            }

        })


        schedule_data <- reactive({

            source <- input$data_source

            if (source == "current") {

                PL26_schedule

            } else if (source == "previous") {

                PL25_schedule

            } else if (source == "upload") {

                req(input$schedule_file)

                read.csv(
                    input$schedule_file$datapath,
                    stringsAsFactors = FALSE
                )

            }

        })


        tau_data <- reactive({
            input$tau
        })

        observe({

            if(
                !is.null(static_sim()) &&
                !is.null(dynamic_sim())
            ){

                updateActionButton(
                    session,
                    "run_compare",
                    disabled = FALSE
                )

            } else {

                updateActionButton(
                    session,
                    "run_compare",
                    disabled = TRUE
                )

            }

        })

        observeEvent(teams_data(), {

            teams <- teams_data()$teams

            updateSelectInput(
                session,
                "team1",
                choices = teams,
                selected = teams[1]
            )

            updateSelectInput(
                session,
                "team2",
                choices = teams,
                selected = teams[2]
            )

            updateSelectInput(
                session,
                "comparison_team",
                choices = teams,
                selected = teams[1]
            )

        })

        observeEvent(input$team1, {

            home <- teams_data() |>
                dplyr::filter(teams == input$team1)

            if (nrow(home) != 1) return()

            updateSliderInput(
                session,
                "alpha_home",
                value = home$alpha[[1]]
            )

            updateSliderInput(
                session,
                "beta_home",
                value = home$beta[[1]]
            )

        }, ignoreInit = FALSE)

        observeEvent(input$team2, {

            away <- teams_data() |>
                dplyr::filter(teams == input$team2)

            if (nrow(away) != 1) return()

            updateSliderInput(
                session,
                "alpha_away",
                value = away$alpha[[1]]
            )

            updateSliderInput(
                session,
                "beta_away",
                value = away$beta[[1]]
            )

        }, ignoreInit = FALSE)


        match_means <- reactive({

            req(input$analysis_mode == "team")

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

            source <- input$data_source

            if (source == "current") {

                div(
                    style = "
                padding: 10px;
                background-color: #e8f4ea;
                border-radius: 6px;
                font-weight: 600;
            ",
                    "Using fitted 2025/26 model parameters and 2025/26 fixtures"
                )

            } else if (source == "previous") {

                div(
                    style = "
                padding: 10px;
                background-color: #e8eef8;
                border-radius: 6px;
                font-weight: 600;
            ",
                    "Using fitted 2024/25 model parameters and 2026/27 fixtures"
                )

            } else {

                req(input$pars_file, input$schedule_file)

                div(
                    style = "
                padding: 10px;
                background-color: #fff3cd;
                border-radius: 6px;
                font-weight: 600;
            ",

                    "Using uploaded data",

                    br(),

                    paste(
                        "Parameters:",
                        input$pars_file$name
                    ),

                    br(),

                    paste(
                        "Fixtures:",
                        input$schedule_file$name
                    )
                )

            }

        })

        observeEvent(input$data_source, {

            static_sim(NULL)
            dynamic_sim(NULL)

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

                    size = 5

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

                theme_minimal(base_size = 16) +
                theme(
                    axis.title = element_text(size = 18, face = "bold"),
                    axis.text = element_text(size = 15)
                )

        })

        output$download_pars_template <- downloadHandler(

            filename = function() {
                "team_parameters_template.csv"
            },

            content = function(file) {

                write.csv(
                    PL25_pars$teams,
                    file,
                    row.names = FALSE
                )
            }
        )

        output$download_schedule_template <- downloadHandler(

            filename = function() {
                "fixture_template.csv"
            },

            content = function(file) {

                write.csv(
                    PL26_schedule,
                    file,
                    row.names = FALSE
                )

            }
        )


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

            source_text <- switch(
                input$data_source,

                current =
                    paste0(
                        "teams <- PL25_pars$teams\n",
                        "schedule <- PL26_schedule\n"
                    ),

                previous =
                    paste0(
                        "teams <- PL24_pars$teams\n",
                        "schedule <- PL25_schedule\n"
                    ),

                upload =
                    paste0(
                        "teams <- read.csv(\"your_parameters.csv\")\n",
                        "schedule <- read.csv(\"your_schedule.csv\")\n"
                    )
            )

            paste0(

                "# Data source\n",
                source_text,
                "\n",

                "# Static model\n",
                "league_sim(\n",
                "  df = teams,\n",
                "  schedule = schedule,\n",
                "  tau = ", round(tau_data(), 3), "\n",
                ")\n\n",

                "# Dynamic model\n",
                "dynamic_league_sim(\n",
                "  df = teams,\n",
                "  schedule = schedule,\n",
                "  tau = ", round(tau_data(), 3), ",\n",
                "  sigma = ", input$sigma, "\n",
                ")"

            )

        })
        # =====================================================
        # STATIC SIM (FIXED)
        # =====================================================

        observeEvent(input$run_static, {

            req(input$analysis_mode == "league")

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

        make_dynamic <- function(
        teams,
        ro = 0.9,
        sigma = 0.1
        ) {

            n_teams <- nrow(teams)
            n_rounds <- 38

            teams_dynamic <- teams

            for (r in seq_len(n_rounds)) {

                teams_dynamic[[paste0("a_round_", r)]] <- NA_real_
                teams_dynamic[[paste0("b_round_", r)]] <- NA_real_

            }

            for (i in seq_len(n_teams)) {

                td <- MASS::mvrnorm(
                    n_rounds - 1,
                    mu = c(0, 0),
                    Sigma = matrix(
                        c(1, ro, ro, 1),
                        nrow = 2
                    ) * sigma^2
                )

                td <- rbind(
                    c(0, 0),
                    td
                )

                ad <- teams$alpha[i] + cumsum(td[, 1])
                bd <- teams$beta[i] + cumsum(td[, 2])

                teams_dynamic[i, paste0("a_round_", 1:n_rounds)] <- ad
                teams_dynamic[i, paste0("b_round_", 1:n_rounds)] <- bd

            }

            teams_dynamic

        }

        dynamic_season_sim <- function(
        df,
        round,
        team_h,
        team_a,
        tau,
        ro = 0.9,
        sigma = 0.1
        ) {

            teams_dynamic <- make_dynamic(
                df,
                ro = ro,
                sigma = sigma
            )

            mu_h <- numeric(length(round))
            mu_a <- numeric(length(round))

            for (i in seq_along(round)) {

                home_idx <- match(
                    team_h[i],
                    df$teams
                )

                away_idx <- match(
                    team_a[i],
                    df$teams
                )

                r <- round[i]

                mu_h[i] <- exp(
                    tau +
                        teams_dynamic[
                            home_idx,
                            paste0("a_round_", r)
                        ] -
                        teams_dynamic[
                            away_idx,
                            paste0("b_round_", r)
                        ]
                )

                mu_a[i] <- exp(
                    teams_dynamic[
                        away_idx,
                        paste0("a_round_", r)
                    ] -
                        teams_dynamic[
                            home_idx,
                            paste0("b_round_", r)
                        ]
                )

            }

            g_h <- rpois(
                length(team_h),
                mu_h
            )

            g_a <- rpois(
                length(team_a),
                mu_a
            )

            p_h <- ifelse(
                g_h > g_a,
                3,
                ifelse(g_h == g_a, 1, 0)
            )

            p_a <- ifelse(
                g_h > g_a,
                0,
                ifelse(g_h == g_a, 1, 3)
            )

            list(
                p_h,
                p_a,
                g_h - g_a,
                g_h,
                g_a
            )

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


        observeEvent(input$run_dynamic, {

            req(input$analysis_mode == "league")

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
                labs(x = "League Position", y = "Probability")+
                theme_minimal(base_size = 16) +
                theme(
                    axis.title = element_text(size = 18, face = "bold"),
                    axis.text = element_text(size = 14),
                    strip.text = element_text(size = 14, face = "bold")
                )
        })
    })
}
