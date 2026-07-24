# =========================================================
# UI
# =========================================================

chapter1_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("Model Settings"),

        sliderInput(ns("pois_mean"), "Average number of goal scoring opportunities",
                    min = 1, max = 50, value = 25),

        sliderInput(ns("mu"), "Average probability of goal conversion",
                    min = 0, max = 1, value = 0.1, step = 0.01),

        sliderInput(ns("phi"), "Shape",
                    min = 0, max = 200, value = 10, step = 1),

        numericInput(ns("seed"), "Random seed",
                     value = sample(1:999, 1)),

        numericInput(ns("n_sim"), "Number of simulated games",
                     value = 1000, min = 100),

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
                    "🎲 A Probability Model for Total Goals",
                    style = "
                    font-size: 1.4rem;
                    font-weight: 700;
                    color: #2c3e50;
                "
                )
            ),

            p(
                strong("Main idea: "),
                "Complex probability models can often be built by combining simpler ones. In this module, a model for the total number of goals scored in a football match is constructed from a model for the number of chances created and a model for the probability that each chance becomes a goal."
            ),

            hr(),

            h5("How the model is built"),

            p("The model is defined by specifying:"),

            tags$ul(

                tags$li(
                    "The probabilities for the number of chances in a game."
                ),

                tags$li(
                    "The probability that any chance is converted into a goal."
                )

            ),

            p(
                "To make things more realistic, the model assumes that different chances have different probabilities of being converted into goals."
            ),

            hr(),

            h5("Your options"),

            p(
                "The controls in the sidebar allow you to choose:"
            ),

            tags$ul(

                tags$li(
                    "The average number of chances per game."
                ),

                tags$li(
                    "The average probability that a chance is converted into a goal."
                ),

                tags$li(
                    "The variation in the conversion probability from one chance to another. If the shape parameter is large, conversion probabilities remain close to the chosen average value. If the shape parameter is small, conversion probabilities can vary substantially, while still having the same average."
                )

            ),

            p(
                "You can also choose the number of games to simulate."
            ),

            hr(),

            h5("Things to observe"),

            tags$ul(

                tags$li(
                    "The top row shows the probability distribution for the number of chances per game and the probability density function (introduced fully in Chapter 2 of Playing With Statistics) for the conversion probability. These graphs will always be consistent with your chosen input values."
                ),

                tags$li(
                    "The bottom row shows the expected frequencies of the number of goals per game implied by the model and your chosen parameter values."
                ),

                tags$li(
                    "After pressing 'Run Simulation', the lower graph will also display a histogram of the observed frequencies from the simulated matches."
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
                        "How do the input values affect the shape of the goal distribution?"
                    ),

                    tags$li(
                        "Which inputs have the strongest influence? Which have the least?"
                    ),

                    tags$li(
                        "How closely do simulated results match theoretical expectations?"
                    ),

                    tags$li(
                        "What effect does the number of simulated games have?"
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


    chapter_page_ui(
        id = id,
        title = "🎲 Chapter 1: Randomness",
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
