# =========================================================
# CHAPTER 4
# Bayesian Updating Explorer
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

        h4("Model Settings"),

        # -------------------------------------------------
        # TRUE VALUE
        # -------------------------------------------------

        h5("True population mean"),

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
            "Show true population mean",
            value = FALSE
        ),

        checkboxInput(
            ns("show_data_mean"),
            "Show mean of data",
            value = FALSE
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
                    "Module 4: Bayesian Updating",
                    style = "
                    font-size: 1.4rem;
                    font-weight: 700;
                    color: #2c3e50;
                    "
                )
            ),

            p(
                strong(
                    "This module provides an interactive exploration of Bayesian updating."
                )
            ),

            p(
                "The Bayesian updating explorer provides a visual demonstration
                of how prior information and new data can be combined to produce
                a posterior distribution."
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
                on the true value. The posterior distribution combines the
                information from the prior with the information contained in
                the observations."
            ),

            hr(),

            h5("How to use the Bayesian explorer"),

            tags$ol(

                tags$li(
                    "Choose a prior mean and prior uncertainty."
                ),

                tags$li(
                    "Choose a true population mean and the uncertainty associated with each observation."
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
                        "What happens when the prior is very uncertain?"
                    ),

                    tags$li(
                        "What happens when more observations are collected?"
                    ),

                    tags$li(
                        "What happens when the prior and the data disagree?"
                    ),

                    tags$li(
                        "How does the posterior distribution compare with the prior?"
                    ),

                    tags$li(
                        "How does the posterior become more concentrated as information accumulates?"
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
    )


    # =====================================================
    # PAGE
    # =====================================================

    chapter_page_ui(

        id = id,

        title = "📉 Module 4: Bayesian Updating",

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



        # =========================================================
        # BAYESIAN PLOT
        # =========================================================

        output$bayes_plot <- renderPlot({

            y <- data_values()

            post <- posterior()

            req(y)


            prior_mean <- input$prior_mean

            prior_sd <- input$prior_sd

            post_mean <- post$mean

            post_sd <- post$sd

            data_mean <- post$data_mean


            # ---------------------------------------------------------
            # COLOURS
            # ---------------------------------------------------------

            prior_colour <- "#E76F51"

            prior_line_colour <- "#9B2D20"

            data_colour <- "#7B9ACC"

            data_line_colour <- "#F4A261"

            true_colour <- "#2A9D8F"

            posterior_colour <- "#7B9ACC"

            posterior_line_colour <- "#34495E"


            # ---------------------------------------------------------
            # LINE WIDTHS
            # ---------------------------------------------------------

            curve_width <- 1.5

            reference_width <- 0.8


            # ---------------------------------------------------------
            # COMMON X AXIS
            # ---------------------------------------------------------

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


            # =========================================================
            # PRIOR
            # =========================================================

            prior_plot <- ggplot(

                data.frame(

                    x = x,

                    density = prior_density

                ),

                aes(x, density)

            ) +

                # Prior distribution
                geom_line(

                    colour = prior_colour,

                    linewidth = curve_width

                ) +

                # Prior mean
                geom_vline(

                    xintercept = prior_mean,

                    colour = prior_line_colour,

                    linetype = "dashed",

                    linewidth = reference_width

                )


            # True value
            if (input$show_true) {

                prior_plot <- prior_plot +

                    geom_vline(

                        xintercept = input$true_mu,

                        colour = true_colour,

                        linetype = "dashed",

                        linewidth = reference_width

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


            # =========================================================
            # PRIOR + OBSERVED DATA
            # =========================================================

            data_plot <- ggplot(

                data.frame(

                    x = x,

                    density = prior_density

                ),

                aes(x, density)

            ) +

                # Same prior distribution as in the top panel
                geom_line(

                    colour = prior_colour,

                    linewidth = curve_width

                ) +

                # Observed data
                geom_rug(

                    data = data.frame(x = y),

                    aes(x = x),

                    inherit.aes = FALSE,

                    sides = "b",

                    colour = data_colour,

                    linewidth = 1.0,

                    length = unit(0.08, "npc")

                ) +

                # Prior mean
                geom_vline(

                    xintercept = prior_mean,

                    colour = prior_line_colour,

                    linetype = "dashed",

                    linewidth = reference_width

                )


            # Mean of observed data
            if (input$show_data_mean) {

                data_plot <- data_plot +

                    geom_vline(

                        xintercept = data_mean,

                        colour = data_line_colour,

                        linetype = "dashed",

                        linewidth = reference_width

                    )
            }


            # True value
            if (input$show_true) {

                data_plot <- data_plot +

                    geom_vline(

                        xintercept = input$true_mu,

                        colour = true_colour,

                        linetype = "dashed",

                        linewidth = reference_width

                    )
            }


            data_plot <- data_plot +

                labs(

                    title = "Prior distribution with observed data",

                    x = NULL,

                    y = "Density"

                ) +

                theme_minimal(base_size = 15) +

                theme(

                    panel.grid.minor = element_blank()

                )


            # =========================================================
            # POSTERIOR
            # =========================================================

            posterior_plot <- ggplot(

                data.frame(

                    x = x,

                    density = posterior_density

                ),

                aes(x, density)

            ) +

                # Posterior distribution
                geom_line(

                    colour = posterior_colour,

                    linewidth = curve_width

                ) +

                # Posterior mean
                geom_vline(

                    xintercept = post_mean,

                    colour = posterior_line_colour,

                    linetype = "dashed",

                    linewidth = reference_width

                )


            # Mean of observed data
            if (input$show_data_mean) {

                posterior_plot <- posterior_plot +

                    geom_vline(

                        xintercept = data_mean,

                        colour = data_line_colour,

                        linetype = "dashed",

                        linewidth = reference_width

                    )
            }


            # True value
            if (input$show_true) {

                posterior_plot <- posterior_plot +

                    geom_vline(

                        xintercept = input$true_mu,

                        colour = true_colour,

                        linetype = "dashed",

                        linewidth = reference_width

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


            # =========================================================
            # COMBINE
            # =========================================================

            prior_plot /

                data_plot /

                posterior_plot

        })



        # =================================================
        # DYNAMIC R CODE
        # =================================================

        output$code <- renderText({

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

        })

    })
}

