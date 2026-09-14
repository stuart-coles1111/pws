# =========================================================
# CHAPTER 4
# Bayesian Updating + Quiz Score Explorer
# =========================================================


# =========================================================
# UI
# =========================================================

chapter4_ui <- function(id){

    ns <- NS(id)


    # =====================================================
    # SIDEBAR
    # =====================================================


    sidebar_controls <- sidebar(

        h4("Chapter 4 Explorer"),

        # Only ONE explorer can be selected
        radioButtons(
            ns("explorers"),
            "Explore",
            choices = c(
                "Bayesian updating" = "bayes",
                "Quiz score explorer" = "score"
            ),
            selected = "bayes"
        ),

        hr(),


        # =================================================
        # BAYESIAN CONTROLS
        # =================================================

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'bayes'",
                ns("explorers")
            ),

            # -------------------------------------------------
            # TRUE VALUE
            # -------------------------------------------------

            h5("True value"),

            sliderInput(
                ns("true_mu"),
                "True mean",
                min = -10,
                max = 10,
                value = 3,
                step = 0.5
            ),

            hr(),

            # -------------------------------------------------
            # PRIOR
            # -------------------------------------------------

            h5("Prior"),

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

            # -------------------------------------------------
            # DATA
            # -------------------------------------------------

            h5("Data"),

            sliderInput(
                ns("n"),
                "Number of observations",
                min = 1,
                max = 100,
                value = 10,
                step = 1
            ),

            sliderInput(
                ns("sigma"),
                "Observation SD",
                min = 0.5,
                max = 5,
                value = 2,
                step = 0.5
            ),

            actionButton(
                ns("generate"),
                "Generate new data",
                class = "btn-primary"
            ),

            hr(),

            checkboxInput(
                ns("show_true"),
                "Show true value",
                value = TRUE
            )
        ),


        # =================================================
        # SCORE CONTROLS
        # =================================================

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'score'",
                ns("explorers")
            ),

            h5("Quiz Score Explorer"),

            radioButtons(
                ns("view_mode"),
                "Display",
                choices = c(
                    "Single score" = "single",
                    "Score as error varies" = "error",
                    "Score as uncertainty varies" = "uncertainty"
                ),
                selected = "single"
            ),

            hr(),


            # -------------------------------------------------
            # SINGLE PREDICTION
            # -------------------------------------------------

            conditionalPanel(

                condition = sprintf(
                    "input['%s'] == 'single'",
                    ns("view_mode")
                ),

                sliderInput(
                    ns("Theta"),
                    "True value (T)",
                    min = -10,
                    max = 10,
                    value = 1,
                    step = 0.1
                ),

                sliderInput(
                    ns("G"),
                    "Guess (G)",
                    min = -10,
                    max = 10,
                    value = 0,
                    step = 0.1
                ),

                sliderInput(
                    ns("S"),
                    "Uncertainty (S)",
                    min = 0.1,
                    max = 10,
                    value = 1,
                    step = 0.1
                ),

                checkboxInput(
                    ns("lines"),
                    "Show true value and score",
                    value = TRUE
                )
            ),


            # -------------------------------------------------
            # SCORE VS ERROR
            # -------------------------------------------------

            conditionalPanel(

                condition = sprintf(
                    "input['%s'] == 'error'",
                    ns("view_mode")
                ),

                sliderInput(
                    ns("fixed_S"),
                    "Fixed uncertainty (S)",
                    min = 0.1,
                    max = 10,
                    value = 1,
                    step = 0.1
                ),

                checkboxInput(
                    ns("show_error_zero"),
                    "Show optimal response (δ = 0)",
                    value = FALSE
                )
            ),


            # -------------------------------------------------
            # SCORE VS UNCERTAINTY
            # -------------------------------------------------

            conditionalPanel(

                condition = sprintf(
                    "input['%s'] == 'uncertainty'",
                    ns("view_mode")
                ),

                sliderInput(
                    ns("fixed_error"),
                    "Guess error (Θ − G)",
                    min = -50,
                    max = 50,
                    value = 2,
                    step = 0.5
                ),

                checkboxInput(
                    ns("show_optimum"),
                    "Show optimal uncertainty",
                    value = FALSE
                )
            )
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

            card_header(
                div(
                    "Chapter 4: Uncertainty and Bayesian Updating",
                    style = "
                    font-size: 1.4rem;
                    font-weight: 700;
                    color: #2c3e50;
                    "
                )
            ),

            p(
                strong("This chapter provides two interactive explorations.")
            ),

            p(
                "The Bayesian updating explorer provides a simple visual
                demonstration of how prior information and new data can be
                combined. The quiz score explorer investigates how a prediction
                score depends on both the accuracy of a prediction and the
                uncertainty attached to it."
            ),

            hr(),

            h5("Bayesian updating"),

            p(
                "Bayesian statistics provides a way of combining information
                we already have with new information from observed data."
            ),

            p(
                "In this experiment, the quantity of interest is a single unknown
                value, represented by ",
                tags$em("\u03bc"),
                ". Before seeing any data, we describe our beliefs about this
                value using a normal distribution."
            ),

            withMathJax(
                p(
                    "\\(\\mu \\sim N(\\mu_0,\\sigma_0^2)\\)"
                )
            ),

            p(
                "We then generate observations from a normal distribution centred
                on the true value. The final distribution combines the information
                from the prior with the information contained in the observations."
            ),

            hr(),

            h5("How to use the Bayesian explorer"),

            tags$ol(

                tags$li(
                    "Choose a prior mean and prior uncertainty."
                ),

                tags$li(
                    "Choose a true value and the uncertainty associated with each observation."
                ),

                tags$li(
                    "Choose how many observations to generate."
                ),

                tags$li(
                    "Press ",
                    strong("Generate new data"),
                    " to simulate a new sample."
                ),

                tags$li(
                    "Compare the prior, the observations and the resulting posterior distribution."
                )
            ),

            hr(),

            h5("Quiz score explorer"),

            p(
                "Activity 4 of Playing With Statistics asks participants to
                provide both a best guess (G) and a measure of uncertainty (S)."
            ),

            p(
                "The score depends on how close the guess is to the true value
                and on whether the stated uncertainty is appropriate."
            ),

            p(
                "The score explorer can be used to investigate an individual
                score, the effect of prediction error, or the effect of changing
                the uncertainty attached to a prediction."
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
                        "Does the scoring system treat over-estimates and under-estimates differently?"
                    ),

                    tags$li(
                        "What happens if S is very large relative to the error in an answer?"
                    ),

                    tags$li(
                        "What happens if S is very small relative to the error in an answer?"
                    ),

                    tags$li(
                        "What happens when the prior is very uncertain?"
                    ),

                    tags$li(
                        "What happens when more observations are collected?"
                    ),

                    tags$li(
                        "How does the posterior change when the prior and the data disagree?"
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
        # BAYESIAN RESULTS
        # =================================================

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'bayes'",
                ns("explorers")
            ),

            card(

                card_header("Bayesian updating"),

                p(
                    "The three panels show the prior distribution, the
                    observed data, and the resulting posterior distribution."
                ),

                plotOutput(
                    ns("bayes_plot"),
                    height = "850px"
                )
            )
        ),


        # =================================================
        # SCORE RESULTS
        # =================================================

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'score'",
                ns("explorers")
            ),

            layout_columns(

                col_widths = c(9, 3),

                card(

                    card_header("Response analysis plot"),

                    plotOutput(
                        ns("plot"),
                        height = 450
                    )
                ),

                uiOutput(
                    ns("score_panel")
                )
            )
        )
    )


    # =====================================================
    # PAGE
    # =====================================================

    chapter_page_ui(

        id = id,

        title = "Chapter 4: Uncertainty",

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
        # BAYESIAN DATA
        # =================================================

        data_values <- reactiveVal(NULL)

        observeEvent(

            input$generate,

            {

                data_values(

                    rnorm(
                        input$n,
                        mean = input$true_mu,
                        sd = input$sigma
                    )
                )

            },

            ignoreInit = FALSE
        )


        # =================================================
        # BAYESIAN POSTERIOR
        # =================================================

        posterior <- reactive({

            y <- data_values()

            req(y)

            prior_mean <- input$prior_mean

            prior_var <- input$prior_sd^2

            data_mean <- mean(y)

            data_var <- input$sigma^2

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
        # BAYESIAN PLOT
        # =================================================

        output$bayes_plot <- renderPlot({

            y <- data_values()

            post <- posterior()

            req(y)


            prior_mean <- input$prior_mean

            prior_sd <- input$prior_sd

            post_mean <- post$mean

            post_sd <- post$sd


            # -------------------------------------------------
            # COMMON X AXIS
            # -------------------------------------------------

            xmin <- min(

                y,

                prior_mean - 4 * prior_sd,

                post_mean - 4 * post_sd,

                input$true_mu - 4 * input$sigma

            )


            xmax <- max(

                y,

                prior_mean + 4 * prior_sd,

                post_mean + 4 * post_sd,

                input$true_mu + 4 * input$sigma

            )


            x <- seq(

                xmin - 1,

                xmax + 1,

                length.out = 1000

            )


            prior_density <- dnorm(

                x,

                prior_mean,

                prior_sd

            )


            posterior_density <- dnorm(

                x,

                post_mean,

                post_sd

            )


            # =================================================
            # OBSERVED DATA
            # =================================================

            data_plot <- ggplot() +

                geom_point(

                    data = data.frame(
                        x = y,
                        y = 0
                    ),

                    aes(x, y),

                    colour = "#7B9ACC",

                    size = 3

                ) +

                geom_density(

                    data = data.frame(x = y),

                    aes(x),

                    colour = "#7B9ACC",

                    linewidth = 1.2,

                    adjust = 1.5

                )


            if (input$show_true) {

                data_plot <- data_plot +

                    geom_vline(

                        xintercept = input$true_mu,

                        colour = "#2A9D8F",

                        linewidth = 1.2

                    )
            }


            data_plot <- data_plot +

                labs(

                    title = "Observed data",

                    x = NULL,

                    y = NULL

                ) +

                theme_minimal(base_size = 15) +

                theme(

                    axis.text.y = element_blank(),

                    axis.ticks.y = element_blank(),

                    panel.grid.minor = element_blank()

                )


            # =================================================
            # PRIOR
            # =================================================

            prior_plot <- ggplot(

                data.frame(

                    x = x,

                    density = prior_density

                ),

                aes(x, density)

            ) +

                geom_line(

                    colour = "#E76F51",

                    linewidth = 1.5

                ) +

                geom_vline(

                    xintercept = prior_mean,

                    colour = "#E76F51",

                    linetype = "dashed"

                )


            if (input$show_true) {

                prior_plot <- prior_plot +

                    geom_vline(

                        xintercept = input$true_mu,

                        colour = "#2A9D8F",

                        linewidth = 1.2

                    )
            }


            prior_plot <- prior_plot +

                labs(

                    title = "Prior distribution",

                    x = NULL,

                    y = "Density"

                ) +

                theme_minimal(base_size = 15) +

                theme(

                    panel.grid.minor = element_blank()

                )


            # =================================================
            # POSTERIOR
            # =================================================

            posterior_plot <- ggplot(

                data.frame(

                    x = x,

                    density = posterior_density

                ),

                aes(x, density)

            ) +

                geom_line(

                    colour = "#7B9ACC",

                    linewidth = 1.5

                ) +

                geom_vline(

                    xintercept = post_mean,

                    colour = "#7B9ACC",

                    linetype = "dashed",

                    linewidth = 1

                )


            if (input$show_true) {

                posterior_plot <- posterior_plot +

                    geom_vline(

                        xintercept = input$true_mu,

                        colour = "#2A9D8F",

                        linewidth = 1.2

                    )
            }


            posterior_plot <- posterior_plot +

                labs(

                    title = "Posterior distribution",

                    x = "Value",

                    y = "Density"

                ) +

                theme_minimal(base_size = 15) +

                theme(

                    panel.grid.minor = element_blank()

                )


            # =================================================
            # COMBINE
            # =================================================

            prior_plot /

                data_plot /

                posterior_plot

        })


        # =================================================
        # SCORE OBJECT
        # =================================================

        score_obj <- reactive({

            req(

                input$G,

                input$S,

                input$Theta

            )


            activity4_response_score(

                G = input$G,

                S = input$S,

                Theta = input$Theta,

                alpha = 0.95,

                dp = 3

            )
        })


        # =================================================
        # SCORE PLOT
        # =================================================

        output$plot <- renderPlot({

            req(input$view_mode)


            # =================================================
            # SINGLE SCORE
            # =================================================

            if (input$view_mode == "single") {

                pws::activity4_response_analysis(

                    G = input$G,

                    S = input$S,

                    Theta = input$Theta,

                    dp = 3,

                    lines = input$lines,

                    final_score_only = FALSE

                )


                # =================================================
                # SCORE VS ERROR
                # =================================================

            } else if (input$view_mode == "error") {


                error_grid <- seq(

                    -10,

                    10,

                    length.out = 500

                )


                score_vals <- sapply(

                    error_grid,

                    function(e) {

                        activity4_response_score(

                            G = e,

                            S = input$fixed_S,

                            Theta = 0

                        )$scores

                    }

                )


                p <- ggplot(

                    data.frame(

                        error = error_grid,

                        score = score_vals

                    ),

                    aes(error, score)

                ) +

                    geom_line(

                        colour = "#7B9ACC",

                        linewidth = 1.4

                    ) +

                    labs(

                        x = expression(delta == T - G),

                        y = "Score"

                    ) +

                    theme_minimal(

                        base_size = 16

                    ) +

                    theme(

                        axis.title = element_text(size = 16),

                        axis.text = element_text(size = 14)

                    )


                if (input$show_error_zero) {

                    p <- p +

                        geom_vline(

                            xintercept = 0,

                            linetype = "dashed",

                            colour = "#E76F51",

                            linewidth = 1

                        )
                }


                p


                # =================================================
                # SCORE VS UNCERTAINTY
                # =================================================

            } else {


                s_opt <- abs(input$fixed_error) *

                    qnorm(0.975)


                s_min <- max(

                    0.1,

                    s_opt / 5

                )


                s_max <- max(

                    20,

                    s_opt * 5

                )


                s_grid <- seq(

                    s_min,

                    s_max,

                    length.out = 1000

                )


                score_vals <- sapply(

                    s_grid,

                    function(s) {

                        activity4_response_score(

                            G = input$fixed_error,

                            S = s,

                            Theta = 0

                        )$scores

                    }

                )


                p <- ggplot(

                    data.frame(

                        S = s_grid,

                        score = score_vals

                    ),

                    aes(S, score)

                ) +

                    geom_line(

                        colour = "#E76F51",

                        linewidth = 1.5

                    ) +

                    labs(

                        x = "Uncertainty (S)",

                        y = "Score"

                    ) +

                    theme_minimal(

                        base_size = 16

                    ) +

                    theme(

                        axis.title = element_text(size = 18),

                        axis.text = element_text(size = 15)

                    )


                if (input$show_optimum) {

                    p <- p +

                        geom_vline(

                            xintercept = s_opt,

                            linetype = "dashed",

                            colour = "#7B9ACC",

                            linewidth = 1

                        ) +

                        annotate(

                            "text",

                            x = s_opt,

                            y = max(score_vals),

                            label = "Optimal S",

                            hjust = -0.4,

                            size = 5

                        )
                }


                p
            }

        })


        # =================================================
        # SCORE DISPLAY
        # =================================================

        output$score <- renderText({

            result <- score_obj()

            req(result)

            # activity4_response_score() returns the score
            # in the $scores element.

            req(

                is.numeric(result$scores),

                length(result$scores) > 0

            )


            round(

                result$scores,

                2

            )

        })


        # =================================================
        # SCORE SIDE PANEL
        # =================================================

        output$score_panel <- renderUI({

            req(input$view_mode)


            if (input$view_mode == "single") {

                card(

                    card_header("Score"),

                    div(

                        style = "
                        font-size: 1.5rem;
                        font-weight: 700;
                        text-align: center;
                        padding-top: 150px;
                        ",

                        textOutput(

                            session$ns("score")

                        )

                    )

                )


            } else if (input$view_mode == "error") {

                card(

                    card_header("Interpretation"),

                    p(

                        "For a fixed uncertainty S, the score is maximised when ",

                        strong("δ = Θ − G = 0"),

                        ", meaning the prediction is exactly correct."

                    )

                )


            } else {

                card(

                    card_header("Optimal uncertainty"),

                    div(

                        style = "
                        font-size: 1.5rem;
                        font-weight: 700;
                        text-align: center;
                        padding-top: 100px;
                        ",

                        paste0(

                            "Optimal S = ",

                            round(

                                abs(input$fixed_error) *

                                    qnorm(0.975),

                                2

                            )

                        )

                    )

                )

            }

        })


        # =================================================
        # DYNAMIC R CODE
        # =================================================

        output$code <- renderText({

            req(input$explorers)


            # =================================================
            # BAYESIAN CODE
            # =================================================

            if (input$explorers == "bayes") {

                paste0(

                    "# Generate observations\n",

                    "y <- rnorm(\n",

                    "    n = ", input$n, ",\n",

                    "    mean = ", input$true_mu, ",\n",

                    "    sd = ", input$sigma, "\n",

                    ")\n\n",


                    "# Prior\n",

                    "prior_mean <- ", input$prior_mean, "\n",

                    "prior_sd <- ", input$prior_sd, "\n",

                    "prior_var <- prior_sd^2\n\n",


                    "# Posterior variance\n",

                    "posterior_var <- 1 / (\n",

                    "    1 / prior_var +\n",

                    "    length(y) / ", input$sigma, "^2\n",

                    ")\n\n",


                    "# Posterior mean\n",

                    "posterior_mean <- posterior_var * (\n",

                    "    prior_mean / prior_var +\n",

                    "    length(y) * mean(y) / ", input$sigma, "^2\n",

                    ")\n\n",


                    "# Posterior SD\n",

                    "posterior_sd <- sqrt(posterior_var)\n\n",


                    "# Posterior density\n",

                    "dnorm(\n",

                    "    x,\n",

                    "    mean = posterior_mean,\n",

                    "    sd = posterior_sd\n",

                    ")"

                )


                # =================================================
                # SCORE CODE
                # =================================================

            } else {


                if (input$view_mode == "single") {

                    paste0(

                        "pws::activity4_response_analysis(\n",

                        "    G = ", input$G, ",\n",

                        "    S = ", input$S, ",\n",

                        "    Theta = ", input$Theta, ",\n",

                        "    dp = 3,\n",

                        "    lines = ", input$lines, ",\n",

                        "    final_score_only = FALSE\n",

                        ")"

                    )


                } else if (input$view_mode == "error") {

                    paste0(

                        "# Create a grid of prediction errors\n",

                        "error_grid <- seq(-10, 10, length.out = 500)\n\n",


                        "# Calculate the score at each error\n",

                        "score_vals <- sapply(\n",

                        "    error_grid,\n",

                        "    function(e) {\n",

                        "        activity4_response_score(\n",

                        "            G = e,\n",

                        "            S = ", input$fixed_S, ",\n",

                        "            Theta = 0\n",

                        "        )$scores\n",

                        "    }\n",

                        ")\n\n",


                        "# Plot the relationship\n",

                        "ggplot(\n",

                        "    data.frame(\n",

                        "        error = error_grid,\n",

                        "        score = score_vals\n",

                        "    ),\n",

                        "    aes(error, score)\n",

                        ") +\n",

                        "    geom_line()"

                    )


                } else {

                    paste0(

                        "# Calculate the optimal uncertainty\n",

                        "s_opt <- abs(",

                        input$fixed_error,

                        ") * qnorm(0.975)\n\n",


                        "# Create a grid of uncertainty values\n",

                        "s_grid <- seq(\n",

                        "    max(0.1, s_opt / 5),\n",

                        "    max(20, s_opt * 5),\n",

                        "    length.out = 1000\n",

                        ")\n\n",


                        "# Calculate the score for each uncertainty\n",

                        "score_vals <- sapply(\n",

                        "    s_grid,\n",

                        "    function(s) {\n",

                        "        activity4_response_score(\n",

                        "            G = ", input$fixed_error, ",\n",

                        "            S = s,\n",

                        "            Theta = 0\n",

                        "        )$scores\n",

                        "    }\n",

                        ")\n\n",


                        "# Plot the relationship\n",

                        "ggplot(\n",

                        "    data.frame(\n",

                        "        S = s_grid,\n",

                        "        score = score_vals\n",

                        "    ),\n",

                        "    aes(S, score)\n",

                        ") +\n",

                        "    geom_line()"

                    )

                }

            }

        })

    })
}
