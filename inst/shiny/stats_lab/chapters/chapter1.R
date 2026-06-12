# =========================================================
# UI
# =========================================================

chapter1_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("Simulation controls"),

        numericInput(ns("n_sim"), "Number of simulated games",
                     value = 10000, min = 100),

        sliderInput(ns("pois_mean"), "Average number of goal scoring opportunities",
                    min = 1, max = 50, value = 25),

        sliderInput(ns("mu"), "Average probability of goal conversion",
                    min = 0, max = 1, value = 0.1, step = 0.01),

        sliderInput(ns("phi"), "Concentration",
                    min = 0, max = 50, value = 2, step = 0.01),

        numericInput(ns("seed"), "Random seed",
                     value = sample(1:999, 1)),

        actionButton(ns("run"), "Run simulation", class = "btn-primary")
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
                    "🎲 Understanding Random Goal Scoring",
                    style = "
                    font-size: 1.4rem;
                    font-weight: 700;
                    color: #2c3e50;
                "
                )
            ),

            p(
                strong("Main idea: "),
                "Although individual football matches are unpredictable,
             the pattern of goals across many matches follows
             predictable statistical rules."
            ),

            hr(),

            h5("What happens in this model?"),

            tags$div(
                style = "margin-left: 10px;",

                p("① A game contains a random number of scoring opportunities."),

                p("② Each opportunity has some chance of becoming a goal."),

                p("③ That conversion probability varies from game to game."),

                p("④ The total number of goals scored is recorded.")
            ),

            hr(),

            h5("Your job"),

            p(
                "Use the controls to create your own goal-scoring model by choosing:"
            ),

            tags$ul(
                tags$li("The average number of opportunities per game"),
                tags$li("The average probability of converting an opportunity"),
                tags$li("How much the conversion probability varies"),
                tags$li("How many games to simulate")
            ),

            hr(),

            h5("What will you see?"),

            tags$ul(
                tags$li(
                    "The distributions used to generate opportunities and scoring probabilities"
                ),
                tags$li(
                    "The theoretical distribution of goals implied by your choices"
                ),
                tags$li(
                    "The observed results from simulated matches"
                ),
                tags$li(
                    "A comparison between theory and simulation"
                )
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
                        "How do the parameter values affect the shape of the goal distribution?"
                    ),
                    tags$li(
                        "How closely do simulated results match theoretical expectations?"
                    ),
                    tags$li(
                        "What changes when the number of simulated games increases?"
                    ),
                    tags$li(
                        "Which inputs have the strongest influence on scoring outcomes?"
                    )
                )
            )
        )
    )

    code_panel <- div(
        card(
            card_header("Generated Code"),
            tags$pre(textOutput(ns("generated_code")))
        )
    )

    results_panel <- div(
        card(
            card_header("Goal scoring specification"),
            plotOutput(ns("components"), height = 300)
        ),
        card(
            card_header("Distribution of goals"),
            plotOutput(ns("hist"), height = 400)
        )
    )

    learn_panel <- div(

        card(

            style = "
            border-radius: 16px;
            border: none;
            box-shadow: 0 4px 12px rgba(0,0,0,0.08);
            padding: 10px;
        ",

            card_header(
                div(
                    "What should you have learned?",
                    style = "
                    font-size: 1.3rem;
                    font-weight: 700;
                    color: #2c3e50;
                "
                )
            ),

            tags$div(

                h5("1. Random outcomes can still be predictable"),

                p(
                    "The number of goals in any single match is uncertain,
                 but the pattern across many matches follows a
                 predictable probability distribution."
                ),

                hr(),

                h5("2. Randomness occurs at more than one level"),

                p(
                    "Some games generate many opportunities and some generate few.
                 Even when an opportunity occurs, whether it becomes a goal
                 is still uncertain."
                ),

                hr(),

                h5("3. Assumptions shape outcomes"),

                p(
                    "Changing the average number of opportunities or the average
                 conversion probability changes the distribution of goals.
                 Different assumptions lead to different predictions."
                ),

                hr(),

                h5("4. Simulation reveals theory"),

                p(
                    "As the number of simulated games increases, the observed
                 frequencies move closer to the theoretical frequencies.
                 Random variation becomes less noticeable."
                ),

                hr(),

                h5("Key takeaway"),

                div(
                    style = "
                    background-color: #f8f9fa;
                    border-left: 5px solid #28a745;
                    padding: 12px;
                    border-radius: 8px;
                ",

                    p(
                        strong("Statistical models do not predict individual games."),
                        br(),
                        "They describe the long-run patterns produced by many
                     games and help us understand how randomness generates
                     observable outcomes."
                    )
                )
            )
        )
    )

    activity_panel <- div(

        card(

            style = "
            border-radius: 16px;
            box-shadow: 0 4px 12px rgba(0,0,0,0.08);
            border: none;
            padding: 10px;
            font-family: 'Inter', sans-serif;
        ",

            card_header(
                div(
                    "Activity 1",
                    style = "
                    font-size: 1.4rem;
                    font-weight: 700;
                    color: #2c3e50;
                "
                )
            ),

            uiOutput(ns("launch_activity_ui"))
        )
    )

    chapter_page_ui(
        id = id,
        title = "🎲 Chapter 1: Randomness",
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

chapter1_server <- function(id){

    moduleServer(id, function(input, output, session){

        # -------------------------------------------------
        # Store snapshot of last simulation inputs
        # -------------------------------------------------

        last_run_inputs <- reactiveVal(NULL)

        # NEW: store simulation output explicitly
        sim_data <- reactiveVal(NULL)

        # -------------------------------------------------
        # Run simulation (triggered only on button press)
        # -------------------------------------------------

        observeEvent(input$run, {

            updateNumericInput(session, "seed",
                               value = sample(1:999, 1))

            last_run_inputs(list(
                n_sim = input$n_sim,
                pois_mean = input$pois_mean,
                mu = input$mu,
                phi = input$phi
            ))

            sim_data(
                pws::goals_sim(
                    n_sim = input$n_sim,
                    pois_mean = input$pois_mean,
                    mu = input$mu,
                    phi = input$phi,
                    seed = input$seed
                )
            )
        })



        output$launch_activity_ui <- renderUI({

            tags$a(
                href = pws:::run_activity(1),
                target = "_blank",
                class = "btn btn-success",
                "Launch Activity 1: Picturing Randomness"
            )

        })


        # -------------------------------------------------
        # Code display
        # -------------------------------------------------

        output$generated_code <- renderText({
            paste0(
                "goals_sim(\n",
                "  n_sim = ", input$n_sim, ",\n",
                "  pois_mean = ", input$pois_mean, ",\n",
                "  mu = ", input$mu, ",\n",
                "  phi = ", input$phi, ",\n",
                "  seed = ", input$seed, "\n",
                ")"
            )
        })

        # -------------------------------------------------
        # Components plot
        # -------------------------------------------------

        output$components <- renderPlot({

            phi <- pmax(input$phi, 0.01)

            beta_1 <- input$mu * input$phi
            beta_2 <- (1 - input$mu) * input$phi

            max_n <- qpois(0.999, input$pois_mean)

            pois_df <- data.frame(
                Opportunities = 0:max_n,
                Probability = dpois(0:max_n, lambda = input$pois_mean)
            )

            p1 <- ggplot(pois_df,
                         aes(x = factor(Opportunities),
                             y = Probability)) +
                geom_col(fill = "#7B9ACC") +
                theme_minimal(base_size = 12) +
                labs(title = "Number of chances",
                     x = "Opportunities",
                     y = "Probability")

            p_grid <- seq(0, 1, length.out = 500)

            beta_df <- data.frame(
                p = p_grid,
                Density = dbeta(p_grid, beta_1, beta_2)
            )

            p2 <- ggplot(beta_df,
                         aes(x = p,
                             y = Density)) +
                geom_line(colour = "#CDB4DB", linewidth = 1.2) +
                geom_vline(xintercept = input$mu,
                           linetype = 2,
                           colour = "grey40") +
                theme_minimal(base_size = 12) +
                labs(title = "Conversion probability",
                     x = "p",
                     y = "Density")

            p1 + p2
        })

        # -------------------------------------------------
        # MAIN PLOT
        # -------------------------------------------------

        output$hist <- renderPlot({

            # -----------------------------
            # THEORETICAL (always shown)
            # -----------------------------

            lambda <- input$pois_mean * input$mu
            phi <- pmax(input$phi, 0.01)

            beta_1 <- input$mu * phi
            beta_2 <- (1 - input$mu) * phi

            goals <- 0:qpois(0.999, lambda)

            probs <- dpois(
                goals,
                lambda = input$pois_mean * beta_1 / (beta_1 + beta_2)
            )

            probs <- pmax(probs, 0)
            probs <- probs / sum(probs)

            theoretical_df <- data.frame(
                Goals = goals,
                Frequency = input$n_sim * probs,
                Type = "Theoretical"
            )

            plot_df <- theoretical_df

            # -----------------------------
            # OBSERVED (only after run)
            # -----------------------------

            sim_out <- sim_data()

            show_observed <- FALSE

            if (!is.null(sim_out) && !is.null(last_run_inputs())) {

                prev <- last_run_inputs()

                show_observed <-
                    identical(prev$n_sim, input$n_sim) &&
                    identical(prev$pois_mean, input$pois_mean) &&
                    identical(prev$mu, input$mu) &&
                    identical(prev$phi, input$phi)
            }

            if (show_observed) {

                observed <- sim_out$table

                observed_df <- data.frame(
                    Goals = observed$Goals,
                    Frequency = observed$Frequency,
                    Type = "Observed"
                )

                plot_df <- rbind(observed_df, theoretical_df)
            }

            plot_df$Goals <- factor(
                plot_df$Goals,
                levels = sort(unique(plot_df$Goals))
            )

            ggplot(plot_df,
                   aes(x = Goals,
                       y = Frequency,
                       fill = Type)) +
                geom_col(position = position_dodge(width = 0.9)) +
                scale_fill_manual(values = c(
                    Observed = "#7B9ACC",
                    Theoretical = "#CDB4DB"
                )) +
                theme_minimal(base_size = 14) +
                labs(x = "Goals scored",
                     y = "Frequency",
                     fill = NULL)
        })
    })
}
