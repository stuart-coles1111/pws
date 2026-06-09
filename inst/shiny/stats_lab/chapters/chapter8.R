
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

league_sim <- function(df, tau) {

    results <- season_sim(df, PL25_schedule[, 2], PL25_schedule[, 3], tau)

    points <- PL25_schedule
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

        selectInput(ns("team1"), "Home team",
                    choices = PL24_pars$teams$teams,
                    selected = "Manchester City"),

        selectInput(ns("team2"), "Away team",
                    choices = PL24_pars$teams$teams,
                    selected = "Liverpool"),

        numericInput(ns("n_sim"), "League simulations",
                     value = 1000, min = 100, step = 100),

        sliderInput(ns("sigma"), "Dynamic variation (sigma)",
                    min = 0, max = 0.2, value = 0.05, step = 0.01),

        numericInput(ns("seed"), "Random seed", value = 44),

        hr(),

        actionButton(ns("run_static"),
                     "Calculate static league position probabilities"),

        actionButton(ns("run_dynamic"),
                     "Calculate dynamic league position probabilities"),

        hr(),

        selectInput(ns("comparison_team"),
                    "Comparison team",
                    choices = PL24_pars$teams$teams,
                    selected = "Manchester City"),

        actionButton(ns("run_compare"),
                     "Compare static vs dynamic")
    )

    overview_panel <- div(
        card(
            card_header("What this chapter explores"),
            p("Football seasons are complex systems."),
            p("Simulation reveals uncertainty in outcomes."),
            tags$ul(
                tags$li("Match outcome probabilities"),
                tags$li("Poisson goal model"),
                tags$li("League simulation"),
                tags$li("Static vs dynamic models")
            )
        )
    )

    code_panel <- div(

        card(
            card_header("Generated Code"),

            tags$pre(textOutput(ns("generated_code")))
        ),

        br(),

        card(

            card_header("Simulation Pipeline"),

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
    )

    results_panel <- div(

        uiOutput(ns("sim_banner")),

        div(
            style = "display:flex; gap:15px;",

            div(
                style = "flex:1;",

                card(
                    card_header("Team Parameters"),
                    DTOutput(ns("parameter_table"))
                ),

                div(
                    style = "margin-top:10px;
                             padding:10px;
                             background:#f8f9fa;
                             border-radius:8px;",
                    strong("Home advantage (τ)"),
                    br(),
                    textOutput(ns("tau_value"))
                )
            ),

            div(
                style = "flex:1;",
                card(
                    card_header("Match Outcome Probabilities"),
                    DTOutput(ns("match_probs"))
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

    # =======================================================
    # Learn Panel
    # =======================================================

    learn_panel <- div(

        card(

            card_header("Key Ideas"),

            tags$ul(

                tags$li(
                    strong("Complex systems"),
                    " contain many interacting components whose behaviour cannot be understood by studying parts in isolation."
                ),

                tags$li(
                    strong("Emergence"),
                    " occurs when simple match-level rules generate unexpected season-level outcomes."
                ),

                tags$li(
                    strong("Sensitivity to randomness"),
                    " means small changes in individual matches can alter final league positions."
                ),

                tags$li(
                    strong("Monte Carlo simulation"),
                    " approximates the distribution of possible futures by repeatedly simulating complete seasons."
                ),

                tags$li(
                    strong("Dynamic models"),
                    " recognise that team quality may change over time rather than remaining fixed."
                )
            )
        ),

        br(),

        card(

            card_header("Static vs Dynamic Thinking"),

            p(
                "The static model assumes team strengths remain constant for the entire season."
            ),

            p(
                "The dynamic model allows attack and defence parameters to evolve from week to week."
            ),

            p(
                "This introduces an additional source of uncertainty and often produces wider distributions of possible league positions."
            )
        ),

        br(),

        card(

            card_header("Questions to Explore"),

            tags$ul(

                tags$li(
                    "Which teams are most affected by introducing dynamic variation?"
                ),

                tags$li(
                    "Does increasing σ make the league more predictable or less predictable?"
                ),

                tags$li(
                    "Why do strong teams still occasionally finish lower than expected?"
                ),

                tags$li(
                    "Can a team become more likely to win the league even if its average strength stays unchanged?"
                )
            )
        )
    )

    # =======================================================
    # Activity Panel
    # =======================================================

    activity_panel <- div(

        card(

            card_header("Interactive Activity"),

            p(
                "Launch the companion activity for this chapter."
            ),

            actionButton(
                ns("launch_activity"),
                "Launch Activity",
                class = "btn-success"
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
        learn = learn_panel,
        activity = activity_panel
    )
}

# =========================================================
# SERVER
# =========================================================

chapter8_server <- function(id) {

    moduleServer(id, function(input, output, session) {

        observeEvent(input$launch_activity, {

            url <- pws:::launch_activity_from_lab(8)

            shinyjs::runjs(
                sprintf(
                    "window.open('%s', '_blank');",
                    url
                )
            )
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

        output$match_probs <- renderDT({

            home <- PL24_pars$teams |> dplyr::filter(teams == input$team1)
            away <- PL24_pars$teams |> dplyr::filter(teams == input$team2)

            probs <- match_win_probs(
                c(home$alpha, home$beta),
                c(away$alpha, away$beta),
                PL24_pars$tau
            )

            datatable(
                data.frame(
                    Outcome = c("Home win", "Draw", "Away win"),
                    Probability = round(as.numeric(probs), 3)
                ),
                options = list(dom = "t"),
                rownames = FALSE
            )
        })

        output$tau_value <- renderText(round(PL24_pars$tau, 3))

        output$parameter_table <- renderDT({

            home <- PL24_pars$teams |>
                dplyr::filter(teams == input$team1) |>
                dplyr::select(Team = teams, Attack = alpha, Defence = beta)

            away <- PL24_pars$teams |>
                dplyr::filter(teams == input$team2) |>
                dplyr::select(Team = teams, Attack = alpha, Defence = beta)

            datatable(
                bind_rows(home, away),
                options = list(dom = "t"),
                rownames = FALSE
            ) |>
                formatRound(c("Attack", "Defence"), digits = 3)
        })

        output$generated_code <- renderText({

            paste0(
                "# Static model\n",
                "league_sim(\n",
                "  teams = PL24_pars$teams,\n",
                "  tau = ", round(PL24_pars$tau,3), "\n",
                ")\n\n",
                "# Dynamic model\n",
                "dynamic_league_sim(\n",
                "  teams = PL24_pars$teams,\n",
                "  PL25_schedule = PL25_schedule,\n",
                "  tau = ", round(PL24_pars$tau,3), ",\n",
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

            # snapshot reactive values BEFORE leaving reactive context
            seed <- input$seed
            n_sim <- input$n_sim
            teams <- PL24_pars$teams
            tau <- PL24_pars$tau

            later::later(function() {

                set.seed(seed)

                sims <- sapply(
                    1:n_sim,
                    function(x) league_sim(teams, tau)
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

            teams_dynamic <- make_dynamic(PL24_pars$teams, ro = ro, sigma = sigma)

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

        dynamic_league_sim <- function(df, PL25_schedule, tau, sigma = 0.1) {

            results <- dynamic_season_sim(
                df,
                PL25_schedule[,1],
                PL25_schedule[,2],
                PL25_schedule[,3],
                tau,
                sigma = sigma
            )

            points <- PL25_schedule
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
            teams <- PL24_pars$teams
            tau <- PL24_pars$tau
            sched <- PL25_schedule

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
            league_position_plot(PL24_pars$teams, static_sim(), 4)
        })

        output$dynamic_plot <- renderPlot({
            req(dynamic_sim())
            league_position_plot(PL24_pars$teams, dynamic_sim(), 4)
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
