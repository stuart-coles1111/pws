library(shiny)
library(ggplot2)
library(patchwork)

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
            card_header("Overview"),
            p("Poisson opportunities with Beta scoring probability."),
            p("We compare simulated outcomes to the exact theoretical distribution.")
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
            card_header("Model components"),
            plotOutput(ns("components"), height = 300)
        ),

        card(
            card_header("Distribution of goals"),
            plotOutput(ns("hist"), height = 400)
        )
    )

    learn_panel <- div(
        card(
            card_header("Key ideas"),
            tags$ul(
                tags$li("Two-stage randomness: N then Binomial thinning"),
                tags$li("Beta introduces heterogeneity in scoring probability"),
                tags$li("Mixture creates overdispersion"),
                tags$li("Theory now matches simulation exactly")
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

        observeEvent(input$run, {
            updateNumericInput(session, "seed", value = sample(1:999, 1))
        })

        sim <- eventReactive(input$run, {

            phi <- pmax(input$phi, 0.01)

            pws::goals_sim(
                n_sim = input$n_sim,
                pois_mean = input$pois_mean,
                beta_1 = input$mu * input$phi,
                beta_2 = (1 - input$mu) * input$phi,
                seed = input$seed
            )
        })

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

        output$components <- renderPlot({

            phi <- pmax(input$phi, 0.01)

            beta_1 <- input$mu * input$phi
            beta_2 <- (1 - input$mu) * input$phi

            # ------------------------------------
            # Poisson opportunities
            # ------------------------------------

            max_n <- qpois(0.999, input$pois_mean)

            pois_df <- data.frame(
                Opportunities = 0:max_n,
                Probability = dpois(
                    0:max_n,
                    lambda = input$pois_mean
                )
            )

            p1 <- ggplot(
                pois_df,
                aes(
                    x = factor(Opportunities),
                    y = Probability
                )
            ) +
                geom_col(fill = "#7B9ACC") +
                theme_minimal(base_size = 12) +
                labs(
                    title = "Number of chances",
                    x = "Opportunities",
                    y = "Probability"
                )

            # ------------------------------------
            # Beta conversion probability
            # ------------------------------------

            p_grid <- seq(
                0,
                1,
                length.out = 500
            )

            beta_df <- data.frame(
                p = p_grid,
                Density = dbeta(
                    p_grid,
                    shape1 = beta_1,
                    shape2 = beta_2
                )
            )

            p2 <- ggplot(
                beta_df,
                aes(
                    x = p,
                    y = Density
                )
            ) +
                geom_line(
                    colour = "#CDB4DB",
                    linewidth = 1.2
                ) +
                geom_vline(
                    xintercept = input$mu,
                    linetype = 2,
                    colour = "grey40"
                ) +
                theme_minimal(base_size = 12) +
                labs(
                    title = "Conversion probability",
                    x = "p",
                    y = "Density"
                )

            p1 + p2
        })

        output$hist <- renderPlot({

            observed <- sim()$table

            lambda <- input$pois_mean * input$mu

            max_goal <- max(
                observed$Goals,
                qpois(0.999, lambda)
            )


            goals <- 0:max_goal

            # =====================================================
            # TRUE theoretical distribution
            # =====================================================

            phi <- pmax(input$phi, 0.01)

            beta_1 <- input$mu * input$phi
            beta_2 <- (1 - input$mu) * input$phi

            probs <- dpois(
                goals,
                lambda = input$pois_mean * beta_1 /(beta_1 + beta_2)
            )

            # numerical safety
            probs <- pmax(probs, 0)
            probs <- probs / sum(probs)

            theoretical <- data.frame(
                Goals = goals,
                Frequency = input$n_sim * probs,
                Type = "Theoretical"
            )

            observed_df <- data.frame(
                Goals = observed$Goals,
                Frequency = observed$Frequency,
                Type = "Observed"
            )

            plot_df <- rbind(observed_df, theoretical)
            plot_df$Goals <- factor(plot_df$Goals, levels = sort(unique(plot_df$Goals)))

            ggplot(plot_df, aes(x = Goals, y = Frequency, fill = Type)) +
                geom_col(position = position_dodge(width = 0.9)) +
                scale_fill_manual(
                    values = c(
                        Observed = "#7B9ACC",
                        Theoretical = "#CDB4DB"
                    )
                ) +
                theme_minimal(base_size = 14) +
                labs(
                    x = "Goals scored",
                    y = "Frequency",
                    fill = NULL
                )
        })
    })
}
