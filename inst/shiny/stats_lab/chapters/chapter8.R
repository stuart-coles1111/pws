library(shiny)
library(ggplot2)
library(DT)
library(dplyr)

# =========================================================
# Chapter 8 UI
# =========================================================

chapter8_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("Complexity controls"),

        p("Explore how uncertainty affects football matches
          and league outcomes."),

        selectInput(
            ns("team1"),
            "Home team",
            choices = PL24_pars$teams$teams,
            selected = "Manchester City"
        ),

        selectInput(
            ns("team2"),
            "Away team",
            choices = PL24_pars$teams$teams,
            selected = "Liverpool"
        ),

        numericInput(
            ns("n_sim"),
            "League simulations",
            value = 5000,
            min = 100,
            step = 100
        ),

        sliderInput(
            ns("sigma"),
            "Dynamic variation (sigma)",
            min = 0,
            max = 0.20,
            value = 0.05,
            step = 0.01
        ),

        numericInput(
            ns("seed"),
            "Random seed",
            value = 44
        ),

        hr(),

        actionButton(
            ns("run"),
            "Run simulations",
            class = "btn-primary"
        )
    )

    overview_panel <- div(

        card(
            card_header("What this chapter explores"),

            p("Football seasons are complex systems."),
            p("Simulation reveals uncertainty in outcomes."),

            tags$ul(
                tags$li("Match outcome probabilities"),
                tags$li("Poisson goal models"),
                tags$li("League simulation"),
                tags$li("Uncertainty in rankings"),
                tags$li("Static versus dynamic models")
            )
        )
    )

    code_panel <- div(

        card(
            card_header("Generated R code"),

            tags$pre(
                style = "
                    background:#F8F9FA;
                    padding:15px;
                    border-radius:10px;
                    font-size:15px;
                ",
                textOutput(ns("generated_code"))
            )
        )
    )

    # =========================================================
    # RESULTS PANEL (UPDATED LAYOUT)
    # =========================================================

    results_panel <- div(

        # =====================================================
        # ROW 1: LEFT (TEAM + TAU) | RIGHT (MATCH PROBS)
        # =====================================================

        div(
            style = "
                display:flex;
                gap:15px;
                align-items:flex-start;
            ",

            # -----------------------------
            # LEFT COLUMN
            # -----------------------------

            div(
                style = "flex:1;",

                card(
                    card_header("Team Parameters"),

                    p("Attack and defence parameters for selected teams."),

                    DTOutput(ns("parameter_table"))
                ),

                div(style = "margin-top:10px;"),

                # Subtle home advantage block
                div(
                    style = "
                        padding:10px 12px;
                        background:#f8f9fa;
                        border-radius:8px;
                        border:1px solid #e9ecef;
                    ",

                    div(
                        style = "
                            font-size:12px;
                            color:#6c757d;
                            margin-bottom:4px;
                        ",
                        "Home advantage (τ)"
                    ),

                    div(
                        style = "
                            font-size:22px;
                            font-weight:600;
                            color:#495057;
                        ",
                        textOutput(ns("tau_value"))
                    )
                )
            ),

            # -----------------------------
            # RIGHT COLUMN
            # -----------------------------

            div(
                style = "flex:1;",

                card(
                    card_header("Match Outcome Probabilities"),

                    p("Estimated probabilities for the selected match."),

                    DTOutput(ns("match_probs"))
                )
            )
        ),

        br(),

        # =====================================================
        # STATIC / DYNAMIC PLOTS
        # =====================================================

        card(
            card_header("Final League Positions (Static Model)"),
            plotOutput(ns("static_plot"), height = 700)
        ),

        br(),

        card(
            card_header("Final League Positions (Dynamic Model)"),
            plotOutput(ns("dynamic_plot"), height = 700)
        ),

        br(),

        card(
            card_header("Static vs Dynamic Comparison"),
            plotOutput(ns("comparison_plot"), height = 450)
        ),

        br(),

        card(
            card_header("Top 4 and Relegation Probabilities"),
            DTOutput(ns("prob_table"))
        )
    )

    learn_panel <- div(

        card(
            card_header("Key ideas"),

            tags$ul(
                tags$li("A league table is one possible outcome."),
                tags$li("Simulation explores many futures."),
                tags$li("Randomness accumulates over a season."),
                tags$li("Small differences matter.")
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

chapter8_server <- function(id){

    moduleServer(id, function(input, output, session){

        # ---------------------------
        # STATIC SIM
        # ---------------------------

        static_sim <- eventReactive(input$run, {

            set.seed(input$seed)

            pos <- sapply(
                1:input$n_sim,
                function(x)
                    league_sim(df24, tau = PL24_pars$tau)
            )

            rownames(pos) <- df24$teams
            pos
        }, ignoreNULL = FALSE)

        # ---------------------------
        # DYNAMIC SIM
        # ---------------------------

        dynamic_sim <- eventReactive(input$run, {

            set.seed(input$seed)

            pos <- sapply(
                1:input$n_sim,
                function(x)
                    dynamic_league_sim(
                        df24,
                        schedule,
                        tau = PL24_pars$tau,
                        sigma = input$sigma
                    )
            )

            rownames(pos) <- df24$teams
            pos
        }, ignoreNULL = FALSE)

        # ---------------------------
        # CODE OUTPUT
        # ---------------------------

        output$generated_code <- renderText({

            paste0(
                "league_position <- sapply(1:", input$n_sim,
                ", function(x) league_sim(df24, tau = PL24_pars$tau))\n\n",
                "dynamic_position <- sapply(1:", input$n_sim,
                ", function(x) dynamic_league_sim(df24, schedule, tau = PL24_pars$tau, sigma = ",
                input$sigma, "))"
            )
        })

        # ---------------------------
        # TEAM PARAMETERS
        # ---------------------------

        output$parameter_table <- renderDT({

            home <- PL24_pars$teams |>
                filter(teams == input$team1) |>
                select(Team = teams, Attack = alpha, Defence = beta)

            away <- PL24_pars$teams |>
                filter(teams == input$team2) |>
                select(Team = teams, Attack = alpha, Defence = beta)

            df <- bind_rows(home, away) |>
                mutate(across(where(is.numeric), \(x) round(x, 3)))

            datatable(
                df,
                options = list(dom = "t", paging = FALSE),
                rownames = FALSE
            )
        })

        # ---------------------------
        # HOME ADVANTAGE
        # ---------------------------

        output$tau_value <- renderText({
            round(PL24_pars$tau, 3)
        })

        # ---------------------------
        # MATCH PROBABILITIES
        # ---------------------------

        output$match_probs <- renderDT({

            home <- PL24_pars$teams |> filter(teams == input$team1)
            away <- PL24_pars$teams |> filter(teams == input$team2)

            probs <- match_win_probs(
                c(home$alpha, home$beta),
                c(away$alpha, away$beta),
                PL24_pars$tau
            )

            df <- data.frame(
                Outcome = c(
                    "Home win probability",
                    "Draw probability",
                    "Away win probability"
                ),
                Probability = round(as.numeric(probs), 3)
            )

            datatable(df, options = list(dom = "t"), rownames = FALSE)
        })

        # ---------------------------
        # PLOTS
        # ---------------------------

        output$static_plot <- renderPlot({
            league_position_plot(df24, static_sim(), 4)
        })

        output$dynamic_plot <- renderPlot({
            league_position_plot(df24, dynamic_sim(), 4)
        })

        output$comparison_plot <- renderPlot({

            team <- input$team1

            comparison <- c(
                static_sim()[team, ],
                dynamic_sim()[team, ]
            )

            df_plot <- data.frame(
                Model = rep(c("Static", "Dynamic"), each = input$n_sim),
                Position = comparison
            )

            ggplot(df_plot, aes(x = Position, fill = Model)) +
                geom_bar(
                    aes(y = after_stat(count) / sum(after_stat(count))),
                    position = "dodge"
                ) +
                labs(x = "League Position", y = "Probability") +
                theme_minimal(base_size = 14)
        })

        output$prob_table <- renderDT({

            pos <- dynamic_sim()

            probs <- data.frame(
                Team = rownames(pos),
                Top4 = rowMeans(pos <= 4),
                Relegation = rowMeans(pos >= 18)
            ) |>
                arrange(desc(Top4))

            datatable(
                mutate(probs, across(where(is.numeric), \(x) round(x, 3))),
                options = list(pageLength = 20),
                rownames = FALSE
            )
        })
    })
}
