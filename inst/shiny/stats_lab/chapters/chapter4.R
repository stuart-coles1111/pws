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
                "Binomial comparison" = "binom",
                "Bayesian updating" = "bayes"
            ),
            selected = "binom"
        ),

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'binom'",
                ns("example")
            ),

            h5("Model settings"),

            sliderInput(
                ns("n"),
                "Number of trials",
                min = 5,
                max = 50,
                value = 20,
                step = 1
            ),

            sliderInput(
                ns("p1"),
                "Probability of winning: Distribution 1",
                min = 0.05,
                max = 0.95,
                value = 0.4,
                step = 0.05
            ),

            sliderInput(
                ns("p2"),
                "Probability of winning: Distribution 2",
                min = 0.05,
                max = 0.95,
                value = 0.1,
                step = 0.05
            ),

            sliderInput(
                ns("nsim"),
                "Number of simulations",
                min = 10,
                max = 1000,
                value = 100,
                step = 10
            ),

            actionButton(
                ns("generate_binom"),
                "Generate new simulation",
                class = "btn-primary"
            )
        ),

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'bayes'",
                ns("example")
            ),

            h5("Model settings"),

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

            h5("Data"),

            sliderInput(
                ns("n_bayes"),
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
                ns("generate_bayes"),
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
        ),

        hr(),

        # =================================================
        # START OVER
        # =================================================

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
                        "This example provides an interactive comparison of two binomial distributions."
                    )
                ),

                p(
                    "The binomial distribution describes the number of successes
                    obtained in a fixed number of independent trials, when each
                    trial has the same probability of success."
                ),

                hr(),

                h5("Theoretical distributions"),

                p(
                    "The top row shows the theoretical probability distributions
                    for the number of wins under the two specified probabilities.
                    The mean and standard deviation are shown for each distribution."
                ),

                h5("Simulation"),

                p(
                    "The bottom row shows one simulated sample from each distribution.
                    Each time you press ",
                    strong("Generate new simulation"),
                    ", a new sample is generated."
                ),

                p(
                    "Comparing the simulated samples with the theoretical distributions
                    illustrates the difference between a probability model and a
                    particular sample generated from that model."
                ),

                hr(),

                h5("How to use the Explorer"),

                tags$ol(

                    tags$li(
                        "Choose the number of trials."
                    ),

                    tags$li(
                        "Choose the probability of winning for each distribution."
                    ),

                    tags$li(
                        "Choose the number of simulated samples."
                    ),

                    tags$li(
                        "Press ",
                        strong("Generate new simulation"),
                        " to generate a new sample."
                    ),

                    tags$li(
                        "Compare the simulated results with the corresponding theoretical distributions."
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
                            "How do the two theoretical distributions differ?"
                        ),

                        tags$li(
                            "What happens when the probability of winning is increased?"
                        ),

                        tags$li(
                            "How closely does a simulated sample resemble its theoretical distribution?"
                        ),

                        tags$li(
                            "What happens when the number of simulations is increased?"
                        ),

                        tags$li(
                            "How are the means and standard deviations related to the probability of winning?"
                        )
                    )
                )
            ),


            # =====================================================
            # BAYESIAN UPDATING
            # =====================================================

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
                    tags$em("\u03bc"),
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
                    "We then generate observations from a normal distribution centred
                    on a chosen true value. The posterior distribution combines the
                    information from the prior with the information contained in
                    the observations."
                ),

                hr(),

                h5("How to use the Explorer"),

                tags$ol(

                    tags$li(
                        "Choose the true population mean."
                    ),

                    tags$li(
                        "Choose the mean and uncertainty of the prior distribution."
                    ),

                    tags$li(
                        "Choose the number of observations and their standard deviation."
                    ),

                    tags$li(
                        "Press ",
                        strong("Generate new data"),
                        " to generate a new sample."
                    ),

                    tags$li(
                        "Compare the prior distribution, the observed data and the resulting posterior distribution."
                    )
                ),

                p(
                    "You can also choose whether to display the true population
                    mean and the mean of the observed data."
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
                            "How does the posterior distribution compare with the prior?"
                        ),

                        tags$li(
                            "How does the posterior become more concentrated as information accumulates?"
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

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'binom'",
                ns("example")
            ),

            card(

                card_header("Binomial comparison"),

                p(
                    "The first row shows the theoretical distributions.
                    The second row shows one simulated sample from each distribution."
                ),

                plotOutput(
                    ns("binom_plot"),
                    height = "650px"
                )
            )
        ),

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'bayes'",
                ns("example")
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
        # LOCK STATES
        # =================================================

        binom_locked <- reactiveVal(FALSE)

        bayes_locked <- reactiveVal(FALSE)


        # =================================================
        # BINOMIAL SIMULATION
        # =================================================

        binom_values <- reactiveVal(NULL)

        observeEvent(

            input$generate_binom,

            {

                # Store both the simulation and the parameters
                # used to generate it.

                n <- input$n
                p1 <- input$p1
                p2 <- input$p2
                nsim <- input$nsim

                s1 <- rbinom(
                    nsim,
                    n,
                    p1
                )

                s2 <- rbinom(
                    nsim,
                    n,
                    p2
                )

                binom_values(

                    list(
                        s1 = s1,
                        s2 = s2,
                        n = n,
                        p1 = p1,
                        p2 = p2,
                        nsim = nsim
                    )

                )

                binom_locked(TRUE)

            }

        )


        # =================================================
        # LOCK BINOMIAL CONTROLS
        # =================================================

        observe({

            shinyjs::toggleState(
                id = session$ns("n"),
                condition = !binom_locked()
            )

            shinyjs::toggleState(
                id = session$ns("p1"),
                condition = !binom_locked()
            )

            shinyjs::toggleState(
                id = session$ns("p2"),
                condition = !binom_locked()
            )

            shinyjs::toggleState(
                id = session$ns("nsim"),
                condition = !binom_locked()
            )

        })


        # =================================================
        # BINOMIAL PLOT
        # =================================================

        output$binom_plot <- renderPlot({

            values <- binom_values()

            req(values)

            n <- values$n
            p1 <- values$p1
            p2 <- values$p2
            nsim <- values$nsim

            s1 <- values$s1
            s2 <- values$s2


            # -------------------------------------------------
            # THEORETICAL DISTRIBUTIONS
            # -------------------------------------------------

            x <- 0:n

            pr1 <- dbinom(
                x,
                n,
                p1
            )

            pr2 <- dbinom(
                x,
                n,
                p2
            )

            theoretical_max <- max(
                c(pr1, pr2)
            )


            g1 <- ggplot(

                data.frame(
                    x = x,
                    probability = pr1
                ),

                aes(
                    x = x,
                    y = probability
                )

            ) +

                geom_bar(
                    stat = "identity",
                    fill = "lightblue",
                    width = 0.8
                ) +

                geom_point(
                    colour = "steelblue",
                    size = 2
                ) +

                ylim(
                    0,
                    theoretical_max
                ) +

                labs(
                    title = paste0(
                        "Distribution 1: p = ",
                        p1,
                        ", Mean = ",
                        round(n * p1, 3),
                        ", SD = ",
                        round(
                            sqrt(
                                n * p1 * (1 - p1)
                            ),
                            3
                        )
                    ),
                    x = "Number of Wins",
                    y = "Probability"
                ) +

                theme_minimal(
                    base_size = 14
                ) +

                theme(
                    plot.title = element_text(
                        size = 11,
                        face = "bold"
                    )
                )


            g2 <- ggplot(

                data.frame(
                    x = x,
                    probability = pr2
                ),

                aes(
                    x = x,
                    y = probability
                )

            ) +

                geom_bar(
                    stat = "identity",
                    fill = "lightblue",
                    width = 0.8
                ) +

                geom_point(
                    colour = "steelblue",
                    size = 2
                ) +

                ylim(
                    0,
                    theoretical_max
                ) +

                labs(
                    title = paste0(
                        "Distribution 2: p = ",
                        p2,
                        ", Mean = ",
                        round(n * p2, 3),
                        ", SD = ",
                        round(
                            sqrt(
                                n * p2 * (1 - p2)
                            ),
                            3
                        )
                    ),
                    x = "Number of Wins",
                    y = "Probability"
                ) +

                theme_minimal(
                    base_size = 14
                ) +

                theme(
                    plot.title = element_text(
                        size = 11,
                        face = "bold"
                    )
                )


            # -------------------------------------------------
            # SIMULATED DISTRIBUTIONS
            # -------------------------------------------------

            s1_tab <- table(
                factor(
                    s1,
                    levels = 0:n
                )
            ) |>
                as.data.frame()

            s2_tab <- table(
                factor(
                    s2,
                    levels = 0:n
                )
            ) |>
                as.data.frame()


            simulation_max <- max(
                c(
                    s1_tab$Freq,
                    s2_tab$Freq
                )
            )


            g3 <- ggplot(

                s1_tab,

                aes(
                    x = Var1,
                    y = Freq
                )

            ) +

                geom_bar(
                    stat = "identity",
                    fill = "lightblue",
                    width = 0.8
                ) +

                scale_x_discrete(
                    drop = FALSE,
                    breaks = seq(
                        0,
                        n,
                        by = 5
                    ),
                    labels = seq(
                        0,
                        n,
                        by = 5
                    )
                ) +

                ylim(
                    0,
                    simulation_max
                ) +

                labs(
                    title = paste0(
                        "Simulation 1: n = ",
                        nsim,
                        ", Mean = ",
                        round(mean(s1), 3),
                        ", SD = ",
                        round(sd(s1), 3)
                    ),
                    x = "Number of Wins",
                    y = "Frequency"
                ) +

                theme_minimal(
                    base_size = 14
                ) +

                theme(
                    plot.title = element_text(
                        size = 11,
                        face = "bold"
                    )
                )


            g4 <- ggplot(

                s2_tab,

                aes(
                    x = Var1,
                    y = Freq
                )

            ) +

                geom_bar(
                    stat = "identity",
                    fill = "lightblue",
                    width = 0.8
                ) +

                scale_x_discrete(
                    drop = FALSE,
                    breaks = seq(
                        0,
                        n,
                        by = 5
                    ),
                    labels = seq(
                        0,
                        n,
                        by = 5
                    )
                ) +

                ylim(
                    0,
                    simulation_max
                ) +

                labs(
                    title = paste0(
                        "Simulation 2: n = ",
                        nsim,
                        ", Mean = ",
                        round(mean(s2), 3),
                        ", SD = ",
                        round(sd(s2), 3)
                    ),
                    x = "Number of Wins",
                    y = "Frequency"
                ) +

                theme_minimal(
                    base_size = 14
                ) +

                theme(
                    plot.title = element_text(
                        size = 11,
                        face = "bold"
                    )
                )


            # -------------------------------------------------
            # COMBINE
            # -------------------------------------------------

            (g1 | g2) /

                (g3 | g4)

        })


        # =================================================
        # BAYESIAN DATA
        # =================================================

        data_values <- reactiveVal(NULL)

        observeEvent(

            input$generate_bayes,

            {

                # Store both the generated data and all
                # parameters used to generate it.

                true_mu <- input$true_mu
                prior_mean <- input$prior_mean
                prior_sd <- input$prior_sd
                n_bayes <- input$n_bayes
                sigma <- input$sigma

                y <- rnorm(
                    n_bayes,
                    mean = true_mu,
                    sd = sigma
                )

                data_values(

                    list(
                        y = y,
                        true_mu = true_mu,
                        prior_mean = prior_mean,
                        prior_sd = prior_sd,
                        n_bayes = n_bayes,
                        sigma = sigma
                    )

                )

                bayes_locked(TRUE)

            }

        )


        # =================================================
        # LOCK BAYESIAN CONTROLS
        # =================================================

        observe({

            shinyjs::toggleState(
                id = session$ns("true_mu"),
                condition = !bayes_locked()
            )

            shinyjs::toggleState(
                id = session$ns("prior_mean"),
                condition = !bayes_locked()
            )

            shinyjs::toggleState(
                id = session$ns("prior_sd"),
                condition = !bayes_locked()
            )

            shinyjs::toggleState(
                id = session$ns("n_bayes"),
                condition = !bayes_locked()
            )

            shinyjs::toggleState(
                id = session$ns("sigma"),
                condition = !bayes_locked()
            )

        })


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
        # BAYESIAN PLOT
        # =================================================

        output$bayes_plot <- renderPlot({

            values <- data_values()

            req(values)

            y <- values$y

            post <- posterior()


            prior_mean <- values$prior_mean

            prior_sd <- values$prior_sd

            true_mu <- values$true_mu

            sigma <- values$sigma

            post_mean <- post$mean

            post_sd <- post$sd

            data_mean <- post$data_mean


            prior_colour <- "#E76F51"

            prior_line_colour <- "#9B2D20"

            data_colour <- "#7B9ACC"

            data_line_colour <- "#F4A261"

            true_colour <- "#2A9D8F"

            posterior_colour <- "#7B9ACC"

            posterior_line_colour <- "#34495E"


            curve_width <- 1.5

            reference_width <- 0.8


            xmin <- min(

                y,

                prior_mean - 4 * prior_sd,

                post_mean - 4 * post_sd,

                true_mu - 4 * sigma

            )


            xmax <- max(

                y,

                prior_mean + 4 * prior_sd,

                post_mean + 4 * post_sd,

                true_mu + 4 * sigma

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

                    colour = prior_colour,

                    linewidth = curve_width

                ) +

                geom_vline(

                    xintercept = prior_mean,

                    colour = prior_line_colour,

                    linetype = "dashed",

                    linewidth = reference_width

                )


            if (input$show_true) {

                prior_plot <- prior_plot +

                    geom_vline(

                        xintercept = true_mu,

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

                theme_minimal(

                    base_size = 15

                ) +

                theme(

                    panel.grid.minor = element_blank()

                )


            # =================================================
            # PRIOR + OBSERVED DATA
            # =================================================

            data_plot <- ggplot(

                data.frame(

                    x = x,

                    density = prior_density

                ),

                aes(x, density)

            ) +

                geom_line(

                    colour = prior_colour,

                    linewidth = curve_width

                ) +

                geom_rug(

                    data = data.frame(x = y),

                    aes(x = x),

                    inherit.aes = FALSE,

                    sides = "b",

                    colour = data_colour,

                    linewidth = 1.0,

                    length = unit(

                        0.08,

                        "npc"

                    )

                ) +

                geom_vline(

                    xintercept = prior_mean,

                    colour = prior_line_colour,

                    linetype = "dashed",

                    linewidth = reference_width

                )


            if (input$show_data_mean) {

                data_plot <- data_plot +

                    geom_vline(

                        xintercept = data_mean,

                        colour = data_line_colour,

                        linetype = "dashed",

                        linewidth = reference_width

                    )

            }


            if (input$show_true) {

                data_plot <- data_plot +

                    geom_vline(

                        xintercept = true_mu,

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

                theme_minimal(

                    base_size = 15

                ) +

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

                    colour = posterior_colour,

                    linewidth = curve_width

                ) +

                geom_vline(

                    xintercept = post_mean,

                    colour = posterior_line_colour,

                    linetype = "dashed",

                    linewidth = reference_width

                )


            if (input$show_data_mean) {

                posterior_plot <- posterior_plot +

                    geom_vline(

                        xintercept = data_mean,

                        colour = data_line_colour,

                        linetype = "dashed",

                        linewidth = reference_width

                    )

            }


            if (input$show_true) {

                posterior_plot <- posterior_plot +

                    geom_vline(

                        xintercept = true_mu,

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

                theme_minimal(

                    base_size = 15

                ) +

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
        # START OVER
        # =================================================

        observeEvent(

            input$start_over,

            {

                # Clear simulations

                binom_values(NULL)

                data_values(NULL)


                # Unlock controls

                binom_locked(FALSE)

                bayes_locked(FALSE)


                # Reset example

                updateRadioButtons(
                    session,
                    "example",
                    selected = "binom"
                )


                # Reset binomial controls

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
                    "p2",
                    value = 0.1
                )

                updateSliderInput(
                    session,
                    "nsim",
                    value = 100
                )


                # Reset Bayesian controls

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


                # Reset checkboxes

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
                    p1 <- input$p1
                    p2 <- input$p2
                    nsim <- input$nsim

                } else {

                    n <- values$n
                    p1 <- values$p1
                    p2 <- values$p2
                    nsim <- values$nsim

                }


                paste0(

                    "# Generate two binomial samples\n",

                    "n <- ", n, "\n",

                    "p1 <- ", p1, "\n",

                    "p2 <- ", p2, "\n",

                    "nsim <- ", nsim, "\n\n",

                    "s1 <- rbinom(nsim, n, p1)\n",

                    "s2 <- rbinom(nsim, n, p2)\n\n",

                    "# Theoretical probabilities\n",

                    "x <- 0:n\n\n",

                    "prob1 <- dbinom(x, n, p1)\n",

                    "prob2 <- dbinom(x, n, p2)\n\n",

                    "# Simulated means and standard deviations\n",

                    "mean(s1)\n",

                    "sd(s1)\n\n",

                    "mean(s2)\n",

                    "sd(s2)"

                )

            } else {

                values <- data_values()

                if (is.null(values)) {

                    true_mu <- input$true_mu
                    prior_mean <- input$prior_mean
                    prior_sd <- input$prior_sd
                    n_bayes <- input$n_bayes
                    sigma <- input$sigma

                } else {

                    true_mu <- values$true_mu
                    prior_mean <- values$prior_mean
                    prior_sd <- values$prior_sd
                    n_bayes <- values$n_bayes
                    sigma <- values$sigma

                }


                paste0(

                    "# Generate observations\n",

                    "y <- rnorm(\n",

                    "    n = ", n_bayes, ",\n",

                    "    mean = ", true_mu, ",\n",

                    "    sd = ", sigma, "\n",

                    ")\n\n",

                    "# Prior\n",

                    "prior_mean <- ", prior_mean, "\n",

                    "prior_sd <- ", prior_sd, "\n",

                    "prior_var <- prior_sd^2\n\n",

                    "# Posterior variance\n",

                    "posterior_var <- 1 / (\n",

                    "    1 / prior_var +\n",

                    "    length(y) / ", sigma, "^2\n",

                    ")\n\n",

                    "# Posterior mean\n",

                    "posterior_mean <- posterior_var * (\n",

                    "    prior_mean / prior_var +\n",

                    "    length(y) * mean(y) / ", sigma, "^2\n",

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

            }

        })

    })
}

