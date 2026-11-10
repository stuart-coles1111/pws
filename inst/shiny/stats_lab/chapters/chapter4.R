# =========================================================
# CHAPTER 4
# Binomial Comparison / Bayesian Updating Explorer
# =========================================================


# =========================================================
# UI
# =========================================================

chapter4_ui <- function(id){

    ns <- NS(id)

    shinyjs::useShinyjs()


    # =====================================================
    # SIDEBAR
    # =====================================================

    sidebar_controls <- sidebar(

        h4("Example"),

        radioButtons(
            ns("example"),
            "Choose an example",
            choices = c(
                "Binomial simulation" = "binom",
                "Binomial PMF and likelihood" = "likelihood",
                "Bayesian updating" = "bayes"
            ),
            selected = "binom"
        ),


        # =================================================
        # BINOMIAL CONTROLS
        # =================================================

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'binom'",
                ns("example")
            ),

            hr(),

            h5("Model settings"),


            sliderInput(
                ns("n"),
                "Number of trials",
                min = 5,
                max = 100,
                value = 20,
                step = 1
            ),

            sliderInput(
                ns("p1"),
                "Probability of success",
                min = 0.05,
                max = 0.95,
                value = 0.4,
                step = 0.05
            ),

            hr(),

            h5("Simulation settings"),

            sliderInput(
                ns("nsim"),
                "Number of simulations",
                min = 10,
                max = 1000,
                value = 100,
                step = 10
            ),

            numericInput(
                ns("binom_seed"),
                "Random seed",
                value = sample(1:999, 1),
                min = 1,
                max = 999,
                step = 1
            ),

            actionButton(
                ns("generate_binom"),
                "Simulate sample",
                class = "btn-primary"
            ),

            hr(),

            checkboxInput(
                ns("overlay_binom"),
                "Overlay theoretical and simulated distributions",
                value = FALSE
            )
        ),


        # =================================================
        # BINOMIAL PMF AND LIKELIHOOD CONTROLS
        # =================================================

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'likelihood'",
                ns("example")
            ),

            hr(),

            h5("Binomial model"),

            numericInput(
                ns("m_lik"),
                "Number of trials (m)",
                value = 100,
                min = 1,
                max = 1000,
                step = 1
            ),

            actionButton(
                ns("set_m_lik"),
                "Set m",
                class = "btn-primary"
            ),

            br(),
            br(),

            sliderInput(
                ns("x_lik"),
                "Observed number of sixes (x)",
                min = 0,
                max = 100,
                value = 15,
                step = 1
            ),

            sliderInput(
                ns("theta_lik"),
                "Probability of six (θ)",
                min = 0,
                max = 1,
                value = 1/6,
                step = 0.001,
                ticks = TRUE
            )
        ),


        # =================================================
        # BAYESIAN CONTROLS
        # =================================================

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'bayes'",
                ns("example")
            ),

            hr(),

            h5("Population"),

            sliderInput(
                ns("true_mu"),
                "True mean",
                min = -10,
                max = 10,
                value = 3,
                step = 0.5
            ),

            sliderInput(
                ns("sigma"),
                "True SD",
                min = 0.5,
                max = 5,
                value = 2,
                step = 0.5
            ),

            hr(),

            h5("Prior for Population Mean"),

            sliderInput(
                ns("prior_mean"),
                "Prior mean",
                min = -10,
                max = 10,
                value = 0,
                step = 0.5
            ),

            sliderInput(
                ns("prior_sd"),
                "Prior SD",
                min = 0.5,
                max = 10,
                value = 2,
                step = 0.5
            ),

            hr(),

            h5("Data"),

            sliderInput(
                ns("n_bayes"),
                "Number of observations",
                min = 1,
                max = 100,
                value = 10,
                step = 1
            ),

            numericInput(
                ns("bayes_seed"),
                "Random seed",
                value = sample(1:999, 1),
                min = 1,
                max = 999,
                step = 1
            ),

            actionButton(
                ns("generate_bayes"),
                "Simulate data",
                class = "btn-primary"
            ),

            br(),

            hr(),

            h5("Posterior for Population Mean"),

            actionButton(
                ns("obtain_posterior"),
                "Obtain posterior distribution",
                class = "btn-success"
            ),

            # Hidden state used to reveal the overlay checkbox
            div(
                style = "display:none;",

                checkboxInput(
                    ns("bayes_posterior_ready"),
                    "",
                    value = FALSE
                )
            ),

            conditionalPanel(

                condition = sprintf(
                    "input['%s'] == true",
                    ns("bayes_posterior_ready")
                ),

                hr(),

                checkboxInput(
                    ns("overlay_bayes"),
                    "Overlay prior and posterior distributions",
                    value = FALSE
                )
            ),

            hr(),

            checkboxInput(
                ns("show_true"),
                "Show true population mean",
                value = FALSE
            ),

            checkboxInput(
                ns("show_data_mean"),
                "Show mean of data",
                value = FALSE
            )
        ),


        # =================================================
        # START OVER
        # =================================================

        hr(),

        actionButton(
            ns("start_over"),
            "Start over",
            class = "btn-secondary",
            width = "100%"
        )
    )


    # =====================================================
    # OVERVIEW
    # =====================================================

    overview_panel <- div(

        card(

            style = "
                border-radius: 16px;
                border: none;
                box-shadow: 0 4px 12px rgba(0,0,0,0.08);
                padding: 10px;
            ",

            # =================================================
            # BINOMIAL
            # =================================================

            conditionalPanel(

                condition = sprintf(
                    "input['%s'] == 'binom'",
                    ns("example")
                ),

                card_header(
                    div(
                        "Module 4: Binomial distributions",
                        style = "
                            font-size: 1.4rem;
                            font-weight: 700;
                            color: #2c3e50;
                        "
                    )
                ),

                p(
                    strong(
                        "This example provides an interactive comparison between a theoretical binomial distribution and data simulated from that distribution."
                    )
                ),

                p(
                    "The binomial distribution describes the number of successes
                    obtained in a fixed number of independent trials, when each
                    trial has the same probability of success."
                ),

                p(
                    "The theoretical distribution is determined by the number of
                    trials and the probability of success. The simulation provides
                    a particular sample generated from this model."
                ),

                hr(),

                h5("Theoretical distribution"),

                p(
                    "The theoretical distribution is calculated directly from
                    the binomial probability model. It updates automatically
                    when the model settings are changed."
                ),

                h5("Simulation"),

                p(
                    "Press ",
                    strong("Generate new simulation"),
                    " to generate the specified number of independent binomial
                    observations."
                ),

                p(
                    "The simulated results are displayed as frequency density.
                    For each possible number of successes, the observed frequency is
                    divided by the number of simulations. This puts the simulated
                    distribution on the same scale as the theoretical probabilities."
                ),

                p(
                    "Once a simulation has been generated, the model settings are
                    locked until ",
                    strong("Start over"),
                    " is pressed."
                ),

                h5("Random seed"),

                p(
                    "The random seed controls the simulated sample. Using the
                    same seed with the same model settings will reproduce the
                    same simulation."
                ),

                h5("Overlay comparison"),

                p(
                    "After simulation, select ",
                    strong(
                        "Overlay theoretical and simulated distributions"
                    ),
                    " to display both distributions on the same graph."
                ),

                hr(),

                h5("How to use the Explorer"),

                tags$ol(

                    tags$li(
                        "Choose the number of trials."
                    ),

                    tags$li(
                        "Choose the probability of success."
                    ),

                    tags$li(
                        "Choose the number of simulated observations."
                    ),

                    tags$li(
                        "Choose a random seed, or use the randomly generated seed."
                    ),

                    tags$li(
                        "Press ",
                        strong("Generate new simulation"),
                        "."
                    ),

                    tags$li(
                        "Compare the simulated frequency density with the
                        theoretical probabilities."
                    ),

                    tags$li(
                        "Try the overlay option after the simulation has
                        been generated."
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
                            "How does the shape of the theoretical distribution
                            change when the probability of success changes?"
                        ),

                        tags$li(
                            "What happens to the theoretical distribution when
                            the number of trials increases?"
                        ),

                        tags$li(
                            "How closely does a simulated distribution resemble
                            the theoretical distribution?"
                        ),

                        tags$li(
                            "What happens when the number of simulated
                            observations is increased?"
                        ),

                        tags$li(
                            "How do the theoretical and simulated means compare?"
                        ),

                        tags$li(
                            "How do the theoretical and simulated standard
                            deviations compare?"
                        ),

                        tags$li(
                            "What can you see more clearly when the distributions
                            are overlaid?"
                        ),

                        tags$li(
                            "What happens when the same random seed is used with
                            the same model settings?"
                        )
                    )
                )
            ),


            # =================================================
            # BINOMIAL PMF AND LIKELIHOOD
            # =================================================

            conditionalPanel(

                condition = sprintf(
                    "input['%s'] == 'likelihood'",
                    ns("example")
                ),

                card_header(
                    div(
                        "Module 4: Binomial probability and likelihood",
                        style = "
                            font-size: 1.4rem;
                            font-weight: 700;
                            color: #2c3e50;
                        "
                    )
                ),

                p(
                    strong(
                        "This example explores the relationship between the binomial probability mass function and the likelihood."
                    )
                ),

                p(
                    "Choose the number of trials, m, to display the graphs. Then use the sliders to choose the observed number of successes, x, and the success probability, θ."
                ),

                p(
                    "The left-hand graph shows the binomial probability mass function for the selected value of θ. The selected value of x is highlighted in a contrasting colour."
                ),

                p(
                    "The right-hand graph shows the likelihood of the observed value x as a function of θ. The selected value of θ is marked on the likelihood curve."
                ),

                p(
                    "Notice that the probability mass function describes the distribution of possible observations for a fixed θ, whereas the likelihood considers how the observed data support different values of θ."
                ),

                hr(),

                h5("How to use the Explorer"),

                tags$ol(
                    tags$li("Choose the number of trials, m, to reveal the plots."),
                    tags$li("Choose the observed number of successes, x."),
                    tags$li("Choose a value of θ."),
                    tags$li("Compare the probability mass function with the likelihood curve."),
                    tags$li("Change x and observe how the likelihood curve changes."),
                    tags$li("Change θ and observe how the probability mass function changes.")
                )
            ),


            # =================================================
            # BAYESIAN
            # =================================================

            conditionalPanel(

                condition = sprintf(
                    "input['%s'] == 'bayes'",
                    ns("example")
                ),

                card_header(
                    div(
                        "Module 4: Bayesian updating",
                        style = "
                            font-size: 1.4rem;
                            font-weight: 700;
                            color: #2c3e50;
                        "
                    )
                ),

                p(
                    strong(
                        "This example provides an interactive exploration of Bayesian updating."
                    )
                ),

                p(
                    "Bayesian statistics provides a way of combining information
                    we already have with new information from observed data."
                ),

                p(
                    "In this example, the quantity of interest is a single unknown
                    population mean, represented by ",
                    tags$em("μ"),
                    ". Before seeing any data, we describe our beliefs about this
                    value using a normal prior distribution."
                ),

                p(
                    tags$span(
                        style = "font-family: serif; font-style: italic;",
                        "μ ~ N(μ₀, σ₀²)"
                    )
                ),

                p(
                    "The prior distribution is displayed immediately and changes
                    dynamically when the prior mean or prior SD is changed."
                ),

                p(
                    "Press ",
                    strong("Simulate new data"),
                    " to generate observations from the selected true population
                    mean. The observations are then added to the first graph."
                ),

                p(
                    "Once the data have been generated, press ",
                    strong("Obtain posterior distribution"),
                    " to calculate and display the posterior distribution."
                ),

                p(
                    "The posterior combines the information in the prior with
                    the information provided by the observed data."
                ),

                h5("Random seed"),

                p(
                    "The random seed controls the simulated observations. Using
                    the same seed with the same model settings will reproduce the
                    same simulated data."
                ),

                hr(),

                h5("How to use the Explorer"),

                tags$ol(

                    tags$li(
                        "Choose the true population mean."
                    ),

                    tags$li(
                        "Choose the mean and SD of the prior distribution."
                    ),

                    tags$li(
                        "Choose the number of observations and their SD."
                    ),

                    tags$li(
                        "Choose a random seed, or use the randomly generated seed."
                    ),

                    tags$li(
                        "Press ",
                        strong("Simulate new data"),
                        "."
                    ),

                    tags$li(
                        "Press ",
                        strong("Obtain posterior distribution"),
                        "."
                    ),

                    tags$li(
                        "Compare the prior and posterior distributions."
                    ),

                    tags$li(
                        "After the posterior has been calculated, you can
                        overlay the prior and posterior distributions."
                    )
                ),

                p(
                    "The options to show the true population mean and the mean
                    of the observed data can be used to add reference lines
                    to the graphs."
                ),

                hr(),

                h5("Questions to investigate"),

                div(
                    style = "
                        background-color: #f8f9fa;
                        border-left: 5px solid #7B9ACC;
                        padding: 12px;
                        border-radius: 8px;
                    ",

                    tags$ul(

                        tags$li(
                            "What happens when the prior is very uncertain?"
                        ),

                        tags$li(
                            "What happens when more observations are collected?"
                        ),

                        tags$li(
                            "What happens when the prior and the data disagree?"
                        ),

                        tags$li(
                            "How does the posterior distribution compare
                            with the prior?"
                        ),

                        tags$li(
                            "How does the posterior become more concentrated
                            as information accumulates?"
                        ),

                        tags$li(
                            "What can you see more clearly when the prior and
                            posterior distributions are overlaid?"
                        ),

                        tags$li(
                            "What happens when the same random seed is used with
                            the same model settings?"
                        )
                    )
                )
            )
        )
    )


    # =====================================================
    # CODE PANEL
    # =====================================================

    code_panel <- card(

        card_header("Generated R code"),

        tags$pre(

            style = "
                background:#f8f9fa;
                padding:16px;
                border-radius:8px;
                font-size:0.85rem;
                white-space:pre-wrap;
            ",

            textOutput(
                ns("code")
            )
        )
    )


    # =====================================================
    # RESULTS
    # =====================================================

    results_panel <- div(

        # =================================================
        # BINOMIAL RESULTS
        # =================================================

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'binom'",
                ns("example")
            ),

            card(

                style = "
                    border-radius: 16px;
                    border: none;
                    box-shadow: 0 4px 12px rgba(0,0,0,0.08);
                    padding: 10px;
                ",

                card_header("Binomial comparison"),

                p(
                    "The theoretical distribution is shown on the left.
                    After a simulation has been generated, the simulated
                    distribution is shown on the right."
                ),

                fluidRow(

                    column(
                        6,

                        plotOutput(
                            ns("binom_theoretical_plot"),
                            height = "450px"
                        )
                    ),

                    column(
                        6,

                        plotOutput(
                            ns("binom_simulation_plot"),
                            height = "450px"
                        )
                    )
                ),

                fluidRow(

                    column(
                        6,

                        card(

                            style = "
                                background-color: #f8f9fa;
                                border: none;
                                border-radius: 12px;
                            ",

                            h5("Theoretical distribution"),

                            tableOutput(
                                ns("binom_theoretical_stats")
                            )
                        )
                    ),

                    column(
                        6,

                        card(

                            style = "
                                background-color: #f8f9fa;
                                border: none;
                                border-radius: 12px;
                            ",

                            h5("Simulated distribution"),

                            tableOutput(
                                ns("binom_simulation_stats")
                            )
                        )
                    )
                )
            )
        ),


        # =================================================
        # BINOMIAL PMF AND LIKELIHOOD RESULTS
        # =================================================

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'likelihood'",
                ns("example")
            ),

            accordion(
                open = FALSE,
                accordion_panel(
                    title = "📖 The One-Dice game and binomial model",
                    p(
                        "The binomial model describes X, the number of sixes obtained ",
                        "in m independent plays of the One-Dice game."
                    ),
                    p(
                        "On each play, θ is the probability of rolling a six. ",
                        "The probability θ is assumed to be the same on every play. ",
                        "If the dice is fair, θ = 1/6; a loaded dice may have ",
                        "a different value of θ."
                    ),
                    p(
                        "Choose m, the number of plays, and x, the observed number ",
                        "of sixes. The probability mass function shows the ",
                        "probabilities of different values of X for a fixed θ. ",
                        "The likelihood function shows how the probability of the ",
                        "observed x varies with θ."
                    )
                )
            ),

            card(

                style = "
                    border-radius: 16px;
                    border: none;
                    box-shadow: 0 4px 12px rgba(0,0,0,0.08);
                    padding: 10px;
                ",

                card_header("Binomial PMF and likelihood"),

                p(
                    "The left panel shows the probability mass function for the selected value of θ. The right panel shows the likelihood of the selected observation x."
                ),

                fluidRow(

                    column(
                        6,

                        plotOutput(
                            ns("binom_pmf_likelihood_plot"),
                            height = "450px"
                        )
                    ),

                    column(
                        6,

                        plotOutput(
                            ns("binom_likelihood_plot"),
                            height = "450px"
                        )
                    )
                )
            )
        ),


        # =================================================
        # BAYESIAN RESULTS
        # =================================================

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'bayes'",
                ns("example")
            ),

            card(

                card_header("Bayesian updating"),

                p(
                    "The upper panel shows the prior distribution and,
                    once generated, the observed data. The lower panel
                    shows the posterior distribution after it has been obtained."
                ),

                plotOutput(
                    ns("bayes_prior_data_plot"),
                    height = "400px"
                ),

                plotOutput(
                    ns("bayes_posterior_plot"),
                    height = "400px"
                )
            )
        )
    )


    # =====================================================
    # PAGE
    # =====================================================

    chapter_page_ui(

        id = id,

        title = "📉 Module 4: Uncertainty",

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

chapter4_server <- function(id){

    moduleServer(id, function(input, output, session){


        # =================================================
        # STATE
        # =================================================

        likelihood_m <- reactiveVal(NULL)

        binom_values <- reactiveVal(NULL)

        data_values <- reactiveVal(NULL)

        posterior_ready <- reactiveVal(FALSE)

        binom_locked <- reactiveVal(FALSE)

        bayes_data_locked <- reactiveVal(FALSE)

        bayes_posterior_locked <- reactiveVal(FALSE)


        # =================================================
        # HELPER:
        # ENABLE / DISABLE SHINY SLIDERS
        # =================================================

        set_slider_state <- function(id, enabled = TRUE){

            # This ID is needed for the JavaScript selector
            input_id <- session$ns(id)


            if (enabled) {

                shinyjs::enable(id)

                shinyjs::runjs(
                    sprintf(
                        "
                        $('#%s')
                            .prop('disabled', false)
                            .closest('.shiny-input-container')
                            .find('.irs')
                            .css({
                                'pointer-events': 'auto',
                                'opacity': '1'
                            });
                        ",
                        input_id
                    )
                )

            } else {

                shinyjs::disable(id)

                shinyjs::runjs(
                    sprintf(
                        "
                        $('#%s')
                            .prop('disabled', true)
                            .closest('.shiny-input-container')
                            .find('.irs')
                            .css({
                                'pointer-events': 'none',
                                'opacity': '0.55'
                            });
                        ",
                        input_id
                    )
                )
            }
        }


        # =================================================
        # INITIAL CONTROL STATES
        # =================================================

        # We wait until the UI has been sent to the browser.
        # This is especially important because the Bayesian
        # controls are inside a conditionalPanel.

        session$onFlushed(

            function(){

                # -----------------------------------------
                # Binomial initial state
                # -----------------------------------------

                set_slider_state("n", TRUE)

                set_slider_state("p1", TRUE)

                set_slider_state("nsim", TRUE)

                shinyjs::enable("binom_seed")

                shinyjs::enable("generate_binom")


                # -----------------------------------------
                # Bayesian initial state
                # -----------------------------------------

                set_slider_state("true_mu", TRUE)

                set_slider_state("prior_mean", TRUE)

                set_slider_state("prior_sd", TRUE)

                set_slider_state("n_bayes", TRUE)

                set_slider_state("sigma", TRUE)

                shinyjs::enable("bayes_seed")

                # Simulate new data:
                # ENABLED

                shinyjs::enable("generate_bayes")

                # Obtain posterior:
                # DISABLED

                shinyjs::disable("obtain_posterior")

            },

            once = TRUE

        )


        # Lock the number of trials only after Set m is pressed.
        observeEvent(input$set_m_lik, {
            m <- input$m_lik
            if (is.null(m) || length(m) != 1 || is.na(m) ||
                !is.finite(m) || m < 1 || m > 1000 || m != floor(m)) {
                showNotification("Choose a whole number of trials between 1 and 1000.",
                                 type = "error")
                return()
            }

            likelihood_m(as.integer(m))
            shinyjs::disable("m_lik")
            shinyjs::disable("set_m_lik")
            updateSliderInput(session, "x_lik", min = 0, max = m,
                              value = min(15, m))
        })

        # Display a clean 0-to-1 scale, but keep theta strictly inside (0, 1).
        observeEvent(input$theta_lik, {
            req(input$theta_lik)
            if (input$theta_lik <= 0) {
                updateSliderInput(session, "theta_lik", value = 0.001)
            } else if (input$theta_lik >= 1) {
                updateSliderInput(session, "theta_lik", value = 0.999)
            }
        })

        # =================================================
        # BINOMIAL PMF AND LIKELIHOOD PLOTS
        # =================================================

        output$binom_pmf_likelihood_plot <- renderPlot({

            m <- likelihood_m()
            req(m)
            x_obs <- min(input$x_lik, m)
            theta <- input$theta_lik

            pmf_data <- data.frame(
                x = 0:m,
                probability = dbinom(0:m, size = m, prob = theta)
            )

            # Focus the PMF display around its main mass, while
            # ensuring that the selected observation remains visible.
            pmf_sd <- sqrt(m * theta * (1 - theta))
            x_min <- max(0, floor(min(m * theta - 4 * pmf_sd, x_obs) - 1))
            x_max <- min(m, ceiling(max(m * theta + 4 * pmf_sd, x_obs) + 1))

            ggplot(pmf_data, aes(x = x, y = probability)) +
                geom_segment(
                    aes(xend = x, y = 0, yend = probability),
                    colour = "grey50"
                ) +
                geom_point(
                    colour = "indianred4",
                    size = 1.5
                ) +
                geom_segment(
                    data = pmf_data[pmf_data$x == x_obs, ],
                    aes(xend = x, y = 0, yend = probability),
                    colour = "#2474B5",
                    linewidth = 1.4
                ) +
                geom_point(
                    data = pmf_data[pmf_data$x == x_obs, ],
                    colour = "#2474B5",
                    size = 3
                ) +
                labs(
                    title = bquote(bold("Binomial PMF: " * theta == .(round(theta, 3)))),
                    subtitle = paste0("m = ", m, ", selected x = ", x_obs),
                    x = "Number of sixes (x)",
                    y = "Probability"
                ) +
                coord_cartesian(
                    xlim = c(x_min, x_max),
                    ylim = c(0, max(pmf_data$probability) * 1.1)
                ) +
                theme_minimal(base_size = 13) +
                theme(
                    plot.title = element_text(face = "bold"),
                    panel.grid.minor = element_blank()
                )
        })


        output$binom_likelihood_plot <- renderPlot({

            m <- likelihood_m()
            req(m)
            x_obs <- min(input$x_lik, m)
            theta_selected <- input$theta_lik

            theta_grid <- seq(0, 1, length.out = 1001)

            lik_data <- data.frame(
                theta = theta_grid,
                likelihood = dbinom(
                    x_obs,
                    size = m,
                    prob = theta_grid
                )
            )

            selected_likelihood <- dbinom(
                x_obs,
                size = m,
                prob = theta_selected
            )

            ggplot(lik_data, aes(x = theta, y = likelihood)) +
                geom_line(
                    linewidth = 0.9,
                    colour = "indianred4"
                ) +
                geom_vline(
                    xintercept = theta_selected,
                    linetype = "dashed",
                    colour = "grey40"
                ) +
                geom_point(
                    data = data.frame(
                        theta = theta_selected,
                        likelihood = selected_likelihood
                    ),
                    aes(x = theta, y = likelihood),
                    inherit.aes = FALSE,
                    colour = "black",
                    size = 2.5
                ) +
                labs(
                    title = paste0("Likelihood: x = ", x_obs),
                    subtitle = paste0("Selected θ = ", round(theta_selected, 3)),
                    x = expression(theta),
                    y = "Likelihood"
                ) +
                coord_cartesian(
                    xlim = c(0, 1),
                    ylim = c(0, max(lik_data$likelihood) * 1.1)
                ) +
                theme_minimal(base_size = 13) +
                theme(
                    plot.title = element_text(face = "bold"),
                    panel.grid.minor = element_blank()
                )
        })


        # =================================================
        # BINOMIAL SIMULATION
        # =================================================

        observeEvent(

            input$generate_binom,

            {

                n <- input$n

                p <- input$p1

                nsim <- input$nsim

                seed <- input$binom_seed


                # -----------------------------------------
                # Set random seed
                # -----------------------------------------

                set.seed(seed)


                # -----------------------------------------
                # Generate simulation
                # -----------------------------------------

                s <- rbinom(

                    n = nsim,

                    size = n,

                    prob = p

                )


                # -----------------------------------------
                # Store simulation
                # -----------------------------------------

                binom_values(

                    list(

                        s = s,

                        n = n,

                        p = p,

                        nsim = nsim,

                        seed = seed

                    )

                )


                binom_locked(TRUE)


                # -----------------------------------------
                # Generate next seed
                # -----------------------------------------

                next_seed <- sample(

                    setdiff(

                        1:999,

                        seed

                    ),

                    1

                )


                updateNumericInput(

                    session,

                    "binom_seed",

                    value = next_seed

                )


                # -----------------------------------------
                # Lock binomial settings
                # -----------------------------------------

                set_slider_state(
                    "n",
                    FALSE
                )

                set_slider_state(
                    "p1",
                    FALSE
                )

                set_slider_state(
                    "nsim",
                    TRUE
                )

            }

        )


        # =================================================
        # BINOMIAL THEORETICAL PLOT
        # =================================================

        output$binom_theoretical_plot <- renderPlot({

            n <- input$n

            p <- input$p1

            x <- 0:n

            probability <- dbinom(

                x,

                n,

                p

            )


            y_max <- max(probability) * 1.1


            ggplot(

                data.frame(

                    x = x,

                    probability = probability

                ),

                aes(

                    x = x,

                    y = probability

                )

            ) +

                geom_col(

                    fill = "#4C78A8",

                    width = 0.8

                ) +

                labs(

                    title = "Theoretical distribution",

                    subtitle = paste0(

                        "n = ",

                        n,

                        ", p = ",

                        p

                    ),

                    x = "Number of Successes",

                    y = "Probability"

                ) +

                scale_x_continuous(

                    breaks = pretty(

                        x,

                        n = 8

                    )

                ) +

                coord_cartesian(

                    ylim = c(

                        0,

                        y_max

                    )

                ) +

                theme_minimal(

                    base_size = 14

                ) +

                theme(

                    plot.title = element_text(

                        size = 14,

                        face = "bold"

                    ),

                    plot.subtitle = element_text(

                        size = 12

                    ),

                    panel.grid.minor = element_blank()

                )

        })


        # =================================================
        # BINOMIAL SIMULATION PLOT
        # =================================================

        output$binom_simulation_plot <- renderPlot({

            values <- binom_values()

            req(values)


            s <- values$s

            n <- values$n

            p <- values$p

            nsim <- values$nsim


            simulation_table <- table(

                factor(

                    s,

                    levels = 0:n

                )

            ) |>

                as.data.frame()


            names(simulation_table) <- c(

                "x",

                "frequency"

            )


            simulation_table$x <- as.numeric(

                as.character(

                    simulation_table$x

                )

            )


            simulation_table$frequency_density <-

                simulation_table$frequency /

                nsim


            theoretical_table <- data.frame(

                x = 0:n,

                probability = dbinom(

                    0:n,

                    n,

                    p

                )

            )


            y_max <- max(

                simulation_table$frequency_density

            ) * 1.1


            # ---------------------------------------------
            # Overlay
            # ---------------------------------------------

            if (isTRUE(input$overlay_binom)) {

                overlay_y_max <- max(

                    c(

                        theoretical_table$probability,

                        simulation_table$frequency_density

                    )

                ) * 1.1


                ggplot() +

                    geom_col(

                        data = theoretical_table,

                        aes(

                            x = x,

                            y = probability,

                            fill = "Theoretical"

                        ),

                        width = 0.8,

                        alpha = 0.55

                    ) +

                    geom_col(

                        data = simulation_table,

                        aes(

                            x = x,

                            y = frequency_density,

                            fill = "Simulated"

                        ),

                        width = 0.55,

                        alpha = 0.75

                    ) +

                    scale_fill_manual(

                        name = NULL,

                        values = c(

                            "Theoretical" = "#4C78A8",

                            "Simulated" = "#E76F51"

                        )

                    ) +

                    labs(

                        title = "Theoretical and simulated distributions",

                        subtitle = paste0(

                            "n = ",

                            n,

                            ", p = ",

                            p,

                            ", simulations = ",

                            nsim

                        ),

                        x = "Number of Successes",

                        y = "Frequency density"

                    ) +

                    scale_x_continuous(

                        breaks = pretty(

                            0:n,

                            n = 8

                        )

                    ) +

                    coord_cartesian(

                        ylim = c(

                            0,

                            overlay_y_max

                        )

                    ) +

                    theme_minimal(

                        base_size = 14

                    ) +

                    theme(

                        plot.title = element_text(

                            size = 14,

                            face = "bold"

                        ),

                        plot.subtitle = element_text(

                            size = 12

                        ),

                        panel.grid.minor = element_blank(),

                        legend.position = "top"

                    )

            } else {

                ggplot(

                    simulation_table,

                    aes(

                        x = x,

                        y = frequency_density

                    )

                ) +

                    geom_col(

                        fill = "#E76F51",

                        width = 0.8

                    ) +

                    labs(

                        title = "Simulated distribution",

                        subtitle = paste0(

                            "n = ",

                            n,

                            ", p = ",

                            p,

                            ", simulations = ",

                            nsim

                        ),

                        x = "Number of Wins",

                        y = "Frequency density"

                    ) +

                    scale_x_continuous(

                        breaks = pretty(

                            0:n,

                            n = 8

                        )

                    ) +

                    coord_cartesian(

                        ylim = c(

                            0,

                            y_max

                        )

                    ) +

                    theme_minimal(

                        base_size = 14

                    ) +

                    theme(

                        plot.title = element_text(

                            size = 14,

                            face = "bold"

                        ),

                        plot.subtitle = element_text(

                            size = 12

                        ),

                        panel.grid.minor = element_blank()

                    )

            }

        })


        # =================================================
        # BINOMIAL SUMMARY
        # =================================================

        output$binom_theoretical_stats <- renderTable({

            n <- input$n

            p <- input$p1


            data.frame(

                Statistic = c(

                    "Mean",

                    "Standard deviation"

                ),

                Value = c(

                    n * p,

                    sqrt(

                        n * p * (1 - p)

                    )

                ),

                check.names = FALSE

            )

        },

        digits = 3,

        striped = FALSE,

        bordered = FALSE,

        spacing = "s"

        )


        output$binom_simulation_stats <- renderTable({

            values <- binom_values()

            req(values)


            data.frame(

                Statistic = c(

                    "Mean",

                    "Standard deviation"

                ),

                Value = c(

                    mean(values$s),

                    sd(values$s)

                ),

                check.names = FALSE

            )

        },

        digits = 3,

        striped = FALSE,

        bordered = FALSE,

        spacing = "s"

        )


        # =================================================
        # BAYESIAN DATA SIMULATION
        # =================================================

        observeEvent(

            input$generate_bayes,

            {

                true_mu <- input$true_mu

                prior_mean <- input$prior_mean

                prior_sd <- input$prior_sd

                n_bayes <- input$n_bayes

                sigma <- input$sigma

                seed <- input$bayes_seed


                # -----------------------------------------
                # Generate observations
                # -----------------------------------------

                set.seed(seed)


                y <- rnorm(

                    n = n_bayes,

                    mean = true_mu,

                    sd = sigma

                )


                # -----------------------------------------
                # Store generated data
                # -----------------------------------------

                data_values(

                    list(

                        y = y,

                        true_mu = true_mu,

                        prior_mean = prior_mean,

                        prior_sd = prior_sd,

                        n_bayes = n_bayes,

                        sigma = sigma,

                        seed = seed

                    )

                )


                # -----------------------------------------
                # Update state
                # -----------------------------------------

                bayes_data_locked(TRUE)

                posterior_ready(FALSE)

                bayes_posterior_locked(FALSE)


                # -----------------------------------------
                # Generate next seed
                # -----------------------------------------

                next_seed <- sample(

                    setdiff(

                        1:999,

                        seed

                    ),

                    1

                )


                updateNumericInput(

                    session,

                    "bayes_seed",

                    value = next_seed

                )


                # -----------------------------------------
                # LOCK BAYESIAN SLIDERS
                # -----------------------------------------

                set_slider_state(
                    "true_mu",
                    FALSE
                )

                set_slider_state(
                    "prior_mean",
                    FALSE
                )

                set_slider_state(
                    "prior_sd",
                    FALSE
                )

                set_slider_state(
                    "n_bayes",
                    FALSE
                )

                set_slider_state(
                    "sigma",
                    FALSE
                )


                # -----------------------------------------
                # Lock seed
                # -----------------------------------------

                shinyjs::disable("bayes_seed")


                # -----------------------------------------
                # BUTTON STATES
                #
                # Simulate new data = DISABLED
                # Obtain posterior = ENABLED
                # -----------------------------------------

                shinyjs::disable("generate_bayes")

                shinyjs::enable("obtain_posterior")

            }

        )


        # =================================================
        # BAYESIAN POSTERIOR
        # =================================================

        posterior <- reactive({

            values <- data_values()

            req(values)


            y <- values$y

            prior_mean <- values$prior_mean

            prior_var <- values$prior_sd^2

            data_mean <- mean(y)

            data_var <- values$sigma^2

            n <- length(y)


            posterior_var <- 1 / (

                1 / prior_var +

                    n / data_var

            )


            posterior_mean <- posterior_var * (

                prior_mean / prior_var +

                    n * data_mean / data_var

            )


            list(

                mean = posterior_mean,

                sd = sqrt(posterior_var),

                data_mean = data_mean

            )

        })


        # =================================================
        # COMMON BAYESIAN X-AXIS
        # =================================================

        bayes_x_range <- reactive({

            values <- data_values()


            prior_mean <- if (is.null(values))
                input$prior_mean
            else
                values$prior_mean


            prior_sd <- if (is.null(values))
                input$prior_sd
            else
                values$prior_sd


            true_mu <- if (is.null(values))
                input$true_mu
            else
                values$true_mu


            sigma <- if (is.null(values))
                input$sigma
            else
                values$sigma


            candidates_min <- c(

                prior_mean - 4 * prior_sd,

                true_mu - 4 * sigma

            )


            candidates_max <- c(

                prior_mean + 4 * prior_sd,

                true_mu + 4 * sigma

            )


            if (!is.null(values)) {

                y <- values$y

                candidates_min <- c(

                    candidates_min,

                    min(y)

                )

                candidates_max <- c(

                    candidates_max,

                    max(y)

                )

            }


            # ---------------------------------------------
            # Include posterior once it exists
            # ---------------------------------------------

            if (

                posterior_ready() &&

                !is.null(values)

            ) {

                post <- posterior()

                candidates_min <- c(

                    candidates_min,

                    post$mean - 4 * post$sd

                )

                candidates_max <- c(

                    candidates_max,

                    post$mean + 4 * post$sd

                )

            }


            xmin <- min(candidates_min)

            xmax <- max(candidates_max)


            c(

                xmin = xmin - 1,

                xmax = xmax + 1

            )

        })


        # =================================================
        # OBTAIN POSTERIOR
        # =================================================

        observeEvent(

            input$obtain_posterior,

            {

                req(data_values())


                # -----------------------------------------
                # Mark posterior as ready
                # -----------------------------------------

                posterior_ready(TRUE)

                bayes_posterior_locked(TRUE)


                # -----------------------------------------
                # Button is now disabled
                # -----------------------------------------

                shinyjs::disable("obtain_posterior")


                # Simulate remains disabled

                shinyjs::disable("generate_bayes")


                # -----------------------------------------
                # Reveal overlay checkbox
                # -----------------------------------------

                updateCheckboxInput(

                    session,

                    "bayes_posterior_ready",

                    value = TRUE

                )

            }

        )


        # =================================================
        # BAYESIAN TOP PLOT
        # =================================================

        output$bayes_prior_data_plot <- renderPlot({

            values <- data_values()


            # -----------------------------------------
            # Current prior
            # -----------------------------------------

            prior_mean <- if (is.null(values))
                input$prior_mean
            else
                values$prior_mean


            prior_sd <- if (is.null(values))
                input$prior_sd
            else
                values$prior_sd


            # -----------------------------------------
            # Data
            # -----------------------------------------

            if (!is.null(values)) {

                y <- values$y

                true_mu <- values$true_mu

                data_mean <- mean(y)

            } else {

                y <- numeric(0)

                true_mu <- input$true_mu

                data_mean <- NA

            }


            # -----------------------------------------
            # Common x-axis
            # -----------------------------------------

            axis_range <- bayes_x_range()

            xmin <- axis_range["xmin"]

            xmax <- axis_range["xmax"]


            x <- seq(

                xmin,

                xmax,

                length.out = 1000

            )


            prior_density <- dnorm(

                x,

                prior_mean,

                prior_sd

            )


            # -----------------------------------------
            # Colours
            # -----------------------------------------

            prior_colour <- "#E76F51"

            prior_line_colour <- "#9B2D20"

            data_colour <- "#7B9ACC"

            data_line_colour <- "#5A6FA3"

            true_colour <- "#2A9D8F"


            # -----------------------------------------
            # Plot
            # -----------------------------------------

            plot <- ggplot(

                data.frame(

                    x = x,

                    density = prior_density

                ),

                aes(

                    x = x,

                    y = density

                )

            ) +

                geom_line(

                    colour = prior_colour,

                    linewidth = 0.9

                ) +

                geom_vline(

                    xintercept = prior_mean,

                    colour = prior_line_colour,

                    linetype = "dashed",

                    linewidth = 0.6

                )


            # -----------------------------------------
            # Observed data
            # -----------------------------------------

            if (length(y) > 0) {

                plot <- plot +

                    geom_rug(

                        data = data.frame(

                            x = y

                        ),

                        aes(

                            x = x

                        ),

                        inherit.aes = FALSE,

                        sides = "b",

                        colour = data_colour,

                        linewidth = 0.7,

                        length = unit(

                            0.08,

                            "npc"

                        )

                    )

            }


            # -----------------------------------------
            # Data mean
            # -----------------------------------------

            if (

                length(y) > 0 &&

                isTRUE(input$show_data_mean)

            ) {

                plot <- plot +

                    geom_vline(

                        xintercept = data_mean,

                        colour = data_line_colour,

                        linetype = "dashed",

                        linewidth = 0.6

                    )

            }


            # -----------------------------------------
            # True mean
            # -----------------------------------------

            if (isTRUE(input$show_true)) {

                plot <- plot +

                    geom_vline(

                        xintercept = true_mu,

                        colour = true_colour,

                        linetype = "dashed",

                        linewidth = 0.6

                    )

            }


            plot +

                labs(

                    title = "Prior distribution",

                    subtitle = if (length(y) == 0)

                        "Adjust the prior settings before simulating data"

                    else

                        "Prior distribution with observed data",

                    x = "Value",

                    y = "Density"

                ) +

                coord_cartesian(

                    xlim = c(

                        xmin,

                        xmax

                    )

                ) +

                theme_minimal(

                    base_size = 15

                ) +

                theme(

                    plot.title = element_text(

                        size = 14,

                        face = "bold"

                    ),

                    plot.subtitle = element_text(

                        size = 12

                    ),

                    panel.grid.minor = element_blank()

                )

        })


        # =================================================
        # BAYESIAN POSTERIOR PLOT
        # =================================================

        output$bayes_posterior_plot <- renderPlot({

            req(posterior_ready())

            values <- data_values()

            req(values)


            post <- posterior()


            y <- values$y

            true_mu <- values$true_mu

            post_mean <- post$mean

            post_sd <- post$sd

            data_mean <- post$data_mean


            # -----------------------------------------
            # Same x-axis as top plot
            # -----------------------------------------

            axis_range <- bayes_x_range()

            xmin <- axis_range["xmin"]

            xmax <- axis_range["xmax"]


            x <- seq(

                xmin,

                xmax,

                length.out = 1000

            )


            prior_density <- dnorm(

                x,

                values$prior_mean,

                values$prior_sd

            )


            posterior_density <- dnorm(

                x,

                post_mean,

                post_sd

            )


            # -----------------------------------------
            # Colours
            # -----------------------------------------

            prior_colour <- "#E76F51"

            posterior_colour <- "#6A4C93"

            posterior_line_colour <- "#49336A"

            data_line_colour <- "#5A6FA3"

            true_colour <- "#2A9D8F"


            # -----------------------------------------
            # Plot
            # -----------------------------------------

            if (isTRUE(input$overlay_bayes)) {

                plot <- ggplot(

                    data.frame(

                        x = x,

                        prior = prior_density,

                        posterior = posterior_density

                    ),

                    aes(

                        x = x

                    )

                ) +

                    geom_line(

                        aes(

                            y = prior,

                            colour = "Prior"

                        ),

                        linewidth = 0.9

                    ) +

                    geom_line(

                        aes(

                            y = posterior,

                            colour = "Posterior"

                        ),

                        linewidth = 0.9

                    ) +

                    scale_colour_manual(

                        name = NULL,

                        values = c(

                            "Prior" = prior_colour,

                            "Posterior" = posterior_colour

                        )

                    )

            } else {

                plot <- ggplot(

                    data.frame(

                        x = x,

                        density = posterior_density

                    ),

                    aes(

                        x = x,

                        y = density

                    )

                ) +

                    geom_line(

                        colour = posterior_colour,

                        linewidth = 0.9

                    )

            }


            # -----------------------------------------
            # Posterior mean
            # -----------------------------------------

            plot <- plot +

                geom_vline(

                    xintercept = post_mean,

                    colour = posterior_line_colour,

                    linetype = "dashed",

                    linewidth = 0.6

                )


            # -----------------------------------------
            # Data mean
            # -----------------------------------------

            if (isTRUE(input$show_data_mean)) {

                plot <- plot +

                    geom_vline(

                        xintercept = data_mean,

                        colour = data_line_colour,

                        linetype = "dashed",

                        linewidth = 0.6

                    )

            }


            # -----------------------------------------
            # True mean
            # -----------------------------------------

            if (isTRUE(input$show_true)) {

                plot <- plot +

                    geom_vline(

                        xintercept = true_mu,

                        colour = true_colour,

                        linetype = "dashed",

                        linewidth = 0.6

                    )

            }


            plot +

                labs(

                    title = "Posterior distribution",

                    subtitle = paste0(

                        "Posterior mean = ",

                        round(post_mean, 3),

                        ", posterior SD = ",

                        round(post_sd, 3)

                    ),

                    x = "Value",

                    y = "Density"

                ) +

                coord_cartesian(

                    xlim = c(

                        xmin,

                        xmax

                    )

                ) +

                theme_minimal(

                    base_size = 15

                ) +

                theme(

                    plot.title = element_text(

                        size = 14,

                        face = "bold"

                    ),

                    plot.subtitle = element_text(

                        size = 12

                    ),

                    panel.grid.minor = element_blank(),

                    legend.position = "top"

                )

        })


        # =================================================
        # START OVER
        # =================================================

        observeEvent(

            input$start_over,

            {

                current_example <- input$example


                # -----------------------------------------
                # Clear stored data and state
                # -----------------------------------------

                binom_values(NULL)

                data_values(NULL)

                posterior_ready(FALSE)

                binom_locked(FALSE)

                bayes_data_locked(FALSE)

                bayes_posterior_locked(FALSE)


                # -----------------------------------------
                # Reset binomial settings
                # -----------------------------------------

                updateSliderInput(

                    session,

                    "n",

                    value = 20

                )

                updateSliderInput(

                    session,

                    "p1",

                    value = 0.4

                )

                updateSliderInput(

                    session,

                    "nsim",

                    value = 100

                )

                updateNumericInput(

                    session,

                    "binom_seed",

                    value = sample(

                        1:999,

                        1

                    )

                )


                # -----------------------------------------
                # Reset binomial PMF and likelihood settings
                # -----------------------------------------

                likelihood_m(NULL)
                shinyjs::enable("m_lik")
                shinyjs::enable("set_m_lik")

                updateNumericInput(
                    session,
                    "m_lik",
                    value = 100
                )

                updateSliderInput(
                    session,
                    "x_lik",
                    min = 0,
                    max = 100,
                    value = 15
                )

                updateSliderInput(
                    session,
                    "theta_lik",
                    value = 1/6
                )


                # -----------------------------------------
                # Reset Bayesian settings
                # -----------------------------------------

                updateSliderInput(

                    session,

                    "true_mu",

                    value = 3

                )

                updateSliderInput(

                    session,

                    "prior_mean",

                    value = 0

                )

                updateSliderInput(

                    session,

                    "prior_sd",

                    value = 2

                )

                updateSliderInput(

                    session,

                    "n_bayes",

                    value = 10

                )

                updateSliderInput(

                    session,

                    "sigma",

                    value = 2

                )

                updateNumericInput(

                    session,

                    "bayes_seed",

                    value = sample(

                        1:999,

                        1

                    )

                )


                # -----------------------------------------
                # Reset checkboxes
                # -----------------------------------------

                updateCheckboxInput(

                    session,

                    "show_true",

                    value = FALSE

                )

                updateCheckboxInput(

                    session,

                    "show_data_mean",

                    value = FALSE

                )

                updateCheckboxInput(

                    session,

                    "overlay_binom",

                    value = FALSE

                )

                updateCheckboxInput(

                    session,

                    "overlay_bayes",

                    value = FALSE

                )

                updateCheckboxInput(

                    session,

                    "bayes_posterior_ready",

                    value = FALSE

                )


                # -----------------------------------------
                # Restore binomial controls
                # -----------------------------------------

                set_slider_state(
                    "n",
                    TRUE
                )

                set_slider_state(
                    "p1",
                    TRUE
                )

                set_slider_state(
                    "nsim",
                    TRUE
                )

                shinyjs::enable("binom_seed")

                shinyjs::enable("generate_binom")


                # -----------------------------------------
                # Restore Bayesian controls
                # -----------------------------------------

                set_slider_state(
                    "true_mu",
                    TRUE
                )

                set_slider_state(
                    "prior_mean",
                    TRUE
                )

                set_slider_state(
                    "prior_sd",
                    TRUE
                )

                set_slider_state(
                    "n_bayes",
                    TRUE
                )

                set_slider_state(
                    "sigma",
                    TRUE
                )

                shinyjs::enable("bayes_seed")


                # -----------------------------------------
                # BAYESIAN INITIAL BUTTON STATE
                #
                # Simulate new data = ENABLED
                # Obtain posterior = DISABLED
                # -----------------------------------------

                shinyjs::enable("generate_bayes")

                shinyjs::disable("obtain_posterior")


                # -----------------------------------------
                # Keep current example
                # -----------------------------------------

                updateRadioButtons(

                    session,

                    "example",

                    selected = current_example

                )

            }

        )


        # =================================================
        # DYNAMIC R CODE
        # =================================================

        output$code <- renderText({

            if (input$example == "binom") {

                values <- binom_values()


                if (is.null(values)) {

                    n <- input$n

                    p <- input$p1

                    nsim <- input$nsim

                    seed <- input$binom_seed

                } else {

                    n <- values$n

                    p <- values$p

                    nsim <- values$nsim

                    seed <- values$seed

                }


                paste0(

                    "# Binomial model\n\n",

                    "n <- ", n, "\n",

                    "p <- ", p, "\n",

                    "nsim <- ", nsim, "\n",

                    "seed <- ", seed, "\n\n",


                    "# Set random seed\n\n",

                    "set.seed(seed)\n\n",


                    "# Generate simulated data\n\n",

                    "simulated <- rbinom(\n",

                    "    n = nsim,\n",

                    "    size = n,\n",

                    "    prob = p\n",

                    ")\n\n",


                    "# Theoretical probabilities\n\n",

                    "x <- 0:n\n\n",

                    "theoretical <- dbinom(\n",

                    "    x,\n",

                    "    size = n,\n",

                    "    prob = p\n",

                    ")\n\n",


                    "# Simulated frequencies\n\n",

                    "frequencies <- table(\n",

                    "    factor(\n",

                    "        simulated,\n",

                    "        levels = 0:n\n",

                    "    )\n",

                    ")\n\n",


                    "# Simulated frequency density\n\n",

                    "frequency_density <- frequencies / nsim\n\n",


                    "# Theoretical mean and standard deviation\n\n",

                    "theoretical_mean <- n * p\n",

                    "theoretical_sd <- sqrt(\n",

                    "    n * p * (1 - p)\n",

                    ")\n\n",


                    "# Simulated mean and standard deviation\n\n",

                    "simulated_mean <- mean(simulated)\n",

                    "simulated_sd <- sd(simulated)"

                )

            } else if (input$example == "likelihood") {

                m <- likelihood_m()
                if (is.null(m)) {
                    return("# Press Set m to display the binomial PMF and likelihood code.")
                }
                x_obs <- min(input$x_lik, m)
                theta <- input$theta_lik

                paste0(
                    "# Binomial model and observed data\n\n",
                    "m <- ", m, "\n",
                    "x <- ", x_obs, "\n",
                    "theta <- ", format(theta, digits = 4), "\n\n",
                    "# Probability mass function\n\n",
                    "x_values <- 0:m\n",
                    "pmf <- dbinom(x_values, size = m, prob = theta)\n\n",
                    "# Likelihood function\n\n",
                    "theta_values <- seq(0, 1, length.out = 1001)\n",
                    "likelihood <- dbinom(x, size = m, prob = theta_values)"
                )

            } else {

                values <- data_values()


                if (is.null(values)) {

                    true_mu <- input$true_mu

                    prior_mean <- input$prior_mean

                    prior_sd <- input$prior_sd

                    n_bayes <- input$n_bayes

                    sigma <- input$sigma

                    seed <- input$bayes_seed

                } else {

                    true_mu <- values$true_mu

                    prior_mean <- values$prior_mean

                    prior_sd <- values$prior_sd

                    n_bayes <- values$n_bayes

                    sigma <- values$sigma

                    seed <- values$seed

                }


                paste0(

                    "# Random seed\n\n",

                    "seed <- ", seed, "\n",

                    "set.seed(seed)\n\n",


                    "# Generate observations\n\n",

                    "y <- rnorm(\n",

                    "    n = ", n_bayes, ",\n",

                    "    mean = ", true_mu, ",\n",

                    "    sd = ", sigma, "\n",

                    ")\n\n",


                    "# Prior\n\n",

                    "prior_mean <- ", prior_mean, "\n",

                    "prior_sd <- ", prior_sd, "\n",

                    "prior_var <- prior_sd^2\n\n",


                    "# Posterior variance\n\n",

                    "posterior_var <- 1 / (\n",

                    "    1 / prior_var +\n",

                    "    length(y) / ", sigma, "^2\n",

                    ")\n\n",


                    "# Posterior mean\n\n",

                    "posterior_mean <- posterior_var * (\n",

                    "    prior_mean / prior_var +\n",

                    "    length(y) * mean(y) / ", sigma, "^2\n",

                    ")\n\n",


                    "# Posterior SD\n\n",

                    "posterior_sd <- sqrt(posterior_var)\n\n",


                    "# Posterior density\n\n",

                    "dnorm(\n",

                    "    x,\n",

                    "    mean = posterior_mean,\n",

                    "    sd = posterior_sd\n",

                    ")"

                )

            }

        })

    })
}

