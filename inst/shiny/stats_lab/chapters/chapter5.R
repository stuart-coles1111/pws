# =========================================================

# Chapter 5 — Inference & Regression

# =========================================================

# =========================================================

# Colours

# =========================================================

pal_blue <- "#7B9ACC"
pal_lav  <- "#CDB4DB"
pal_red  <- "#D9534F"
pal_blue_soft <- "#A9BFE3"


# Button colours
pal_roll    <- "#5B8DB8"   # soft blue
pal_est     <- "#6FB286"   # soft green
pal_sim     <- "#9B72B0"   # soft purple
pal_ci      <- "#6F777D"   # soft charcoal
pal_reveal  <- "#D99452"   # soft orange
pal_restart <- "#C96B68"   # soft red

# =========================================================

# UI

# =========================================================

chapter5_ui <- function(id){


    ns <- NS(id)
    useShinyjs()


    # =====================================================
    # Sidebar
    # =====================================================

    sidebar_controls <- sidebar(

        numericInput(
            ns("seed"),
            "Random seed",
            value = sample(1:999, 1),
            min = 1,
            max = 999,
            step = 1
        ),

        radioButtons(
            ns("topic"),
            "Choose topic",
            choices = c(
                "One dice game" = "Inference",
                "Regression" = "Regression"
            ),
            selected = "Inference"
        ),


        # =================================================
        # Inference controls
        # =================================================

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Inference'",
                ns("topic")
            ),

            hr(),

            h5("The One-Dice game"),

            numericInput(
                ns("n"),
                "Number of dice rolls",
                value = 1000,
                min = 10
            ),

            radioButtons(
                ns("prob_mode"),
                "Probability of rolling a six:",
                choices = c(
                    "Chosen manually" = "fixed",
                    "Randomised" = "random"
                ),
                selected = "fixed"
            ),

            conditionalPanel(
                condition = sprintf(
                    "input['%s']=='fixed'",
                    ns("prob_mode")
                ),

                sliderInput(
                    ns("p_true"),
                    "True probability, p, of rolling a 6",
                    min = 0.05,
                    max = 0.50,
                    value = 0.167,
                    step = 0.01
                )
            ),

            conditionalPanel(
                condition = sprintf(
                    "input['%s']=='random'",
                    ns("prob_mode")
                ),

                p(
                    style = "
                    font-size: 0.9em;
                    color: #777;
                    margin-top: 5px;
                ",
                    "The true probability of rolling a 6 will be chosen randomly."
                )
            ),

            actionButton(
                ns("roll"),
                "Roll Dice",
                class = "btn-primary",
                style = paste0(
                    "background-color:", pal_roll,
                    "; border-color:", pal_roll, ";"
                )
            ),

            hr(),

            actionButton(
                ns("estimate"),
                "Estimate probability",
                class = "btn-success",
                style = paste0(
                    "background-color:", pal_est,
                    "; border-color:", pal_est, ";"
                )
            ),

            hr(),

            selectInput(
                ns("boot_method"),
                "Simulation method:",
                choices = c(
                    "Process simulation" = "est_p",
                    "Resampling" = "resample"
                )
            ),

            numericInput(
                ns("B"),
                "Number of simulated estimates of p",
                value = 5000,
                min = 100
            ),

            actionButton(
                ns("bootstrap"),
                "Simulate Estimates",
                class = "btn-info",
                style = paste0(
                    "background-color:", pal_sim,
                    "; border-color:", pal_sim, ";"
                )
            ),

            hr(),

            sliderInput(
                ns("conf"),
                "Confidence level",
                min = 0.80,
                max = 0.999,
                value = 0.95,
                step = 0.001
            ),

            br(),

            actionButton(
                ns("ci"),
                "Confidence Interval for p",
                class = "btn-secondary",
                style = paste0(
                    "background-color:", pal_ci,
                    "; border-color:", pal_ci, ";"
                )
            ),

            br(),
            br(),

            actionButton(
                ns("reveal"),
                "Reveal true probability, p",
                class = "btn-warning",
                style = paste0(
                    "background-color:", pal_reveal,
                    "; border-color:", pal_reveal,
                    "; color:white;"
                )
            ),

            hr(),

            actionButton(
                ns("restart"),
                "Restart Experiment",
                class = "btn-danger",
                style = paste0(
                    "background-color:", pal_restart,
                    "; border-color:", pal_restart, ";"
                )
            )
        ),


        # =================================================
        # Regression controls
        # =================================================

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Regression'",
                ns("topic")
            ),

            h4("Regression Fitting"),

            selectInput(
                ns("start_season"),
                "First season included",
                choices = unique(
                    pws::PL_points$season
                ),
                selected = head(
                    unique(
                        pws::PL_points$season
                    ),
                    1
                )
            ),

            selectInput(
                ns("end_season"),
                "Final season included",
                choices = unique(
                    pws::PL_points$season
                ),
                selected = tail(
                    unique(
                        pws::PL_points$season
                    ),
                    1
                )
            ),

            sliderInput(
                ns("conf_reg"),
                "Confidence level",
                min = 0.80,
                max = 0.99,
                value = 0.95,
                step = 0.01
            ),

            checkboxInput(
                ns("show_diagonal"),
                "Show diagonal reference line (y = x)",
                value = FALSE
            ),

            hr(),

            h4("Second half of season prediction"),

            sliderInput(
                ns("x_split"),
                "Points in first half of season",
                min = min(
                    pws::PL_points$points_half1,
                    na.rm = TRUE
                ),
                max = max(
                    pws::PL_points$points_half1,
                    na.rm = TRUE
                ),
                value = median(
                    pws::PL_points$points_half1,
                    na.rm = TRUE
                ),
                step = 1
            ),
        )
    )


    # =====================================================
    # Overview
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
            # STATISTICAL INFERENCE
            # =================================================

            conditionalPanel(

                condition = sprintf(
                    "input['%s'] == 'Inference'",
                    ns("topic")
                ),

                card_header(
                    div(
                        "Module 5: Statistical inference",
                        style = "
                        font-size: 1.4rem;
                        font-weight: 700;
                        color: #2c3e50;
                    "
                    )
                ),

                p(
                    strong(
                        "This example uses the one-dice game to explore how we can learn about an unknown probability from observed data."
                    )
                ),

                p(
                    "The underlying probability of rolling a six is not necessarily known.
                We observe a sample of dice rolls and use those observations to estimate
                the probability."
                ),

                hr(),

                h5("The one-dice experiment"),

                p(
                    "You begin by choosing the number of dice rolls and deciding whether
                the true probability of rolling a six should be chosen by you or
                generated randomly. When you press ",
                    strong("Roll Dice"),
                    ", the app generates one observed sample."
                ),

                p(
                    "The true probability is kept hidden when the random option is used.
                This allows you to make an estimate without knowing the value that
                generated the data."
                ),

                h5("Estimating the probability"),

                p(
                    "After observing the dice, you can calculate the estimate of the
                probability of rolling a six. The estimate is simply the number of
                sixes divided by the total number of rolls."
                ),

                h5("Simulating estimates"),

                p(
                    "Once the probability has been estimated, the app can generate many
                new estimates using two different approaches:"
                ),

                tags$ul(

                    tags$li(
                        strong("Approximate process simulation: "),
                        "new samples are generated using the probability estimated from the observed data."
                    ),

                    tags$li(
                        strong("Resampling: "),
                        "new samples are created by sampling with replacement from the observed data."
                    )
                ),

                p(
                    "The resulting estimates are displayed as a distribution. This gives
                a visual way to investigate how much estimates vary from one sample
                to another."
                ),

                hr(),

                h5("Confidence intervals"),

                p(
                    "The simulated distribution can also be used to construct a confidence
                interval for the probability estimated from the observed data."
                ),

                p(
                    "Changing the confidence level changes the width of the interval.
                The simulation therefore provides an opportunity to explore the
                relationship between confidence and uncertainty."
                ),

                hr(),

                div(

                    style = "
                    background-color:#f8f9fa;
                    border-left:5px solid #7B9ACC;
                    padding:12px;
                    border-radius:8px;
                ",

                    h5("Questions to investigate"),

                    tags$ul(

                        tags$li(
                            "How much does the estimated probability vary between samples?"
                        ),

                        tags$li(
                            "What happens to the distribution of estimates when the number of observations is increased?"
                        ),

                        tags$li(
                            "How do the two methods of generating new estimates differ?"
                        ),

                        tags$li(
                            "How does the standard error change as the number of simulated estimates increases?"
                        ),

                        tags$li(
                            "What happens to the confidence interval when the confidence level is increased?"
                        ),

                        tags$li(
                            "Does a higher confidence level necessarily give a narrower interval?"
                        ),

                        tags$li(
                            "How close is the estimated probability to the true probability?"
                        )
                    )
                )
            ),


            # =================================================
            # REGRESSION
            # =================================================

            conditionalPanel(

                condition = sprintf(
                    "input['%s'] == 'Regression'",
                    ns("topic")
                ),

                card_header(
                    div(
                        "Module 5: Regression",
                        style = "
                        font-size: 1.4rem;
                        font-weight: 700;
                        color: #2c3e50;
                    "
                    )
                ),

                p(
                    strong(
                        "This example uses Premier League points to explore how a regression model can describe and predict the relationship between two variables."
                    )
                ),

                p(
                    "The analysis examines the relationship between the number of points
                a team has accumulated in the first half of a season and the number
                of points accumulated in the second half."
                ),

                hr(),

                h5("Choosing the seasons"),

                p(
                    "You can choose the first and final seasons included in the analysis.
                The final season is automatically restricted to be the same as or
                later than the first season."
                ),

                p(
                    "This allows you to investigate how the fitted relationship changes
                when different periods of Premier League history are considered."
                ),

                h5("Fitting a regression model"),

                p(
                    "A linear regression model is fitted to the selected seasons.
                The model describes the relationship between points in the first
                half and points in the second half."
                ),

                h5("Predictions"),

                p(
                    "The prediction tool allows you to choose a particular number of
                first-half points and obtain the corresponding predicted number
                of second-half points."
                ),

                p(
                    "The prediction is shown graphically on the scatter plot, together
                with a confidence interval for the mean response at the selected
                prediction point."
                ),

                h5("Confidence bands"),

                p(
                    "The fitted regression line is surrounded by a confidence band.
                The width of this band reflects uncertainty in the estimated
                mean relationship."
                ),

                p(
                    "You can change the confidence level and observe how this affects
                the band. You can also display the diagonal line ",
                    tags$em("y = x"),
                    " as a reference for comparing first-half and second-half points."
                ),

                hr(),

                div(

                    style = "
                    background-color:#f8f9fa;
                    border-left:5px solid #7B9ACC;
                    padding:12px;
                    border-radius:8px;
                ",

                    h5("Questions to investigate"),

                    tags$ul(

                        tags$li(
                            "How strong is the relationship between first-half and second-half points?"
                        ),

                        tags$li(
                            "What happens to the fitted regression line when fewer seasons are included?"
                        ),

                        tags$li(
                            "How does changing the selected time period affect the fitted relationship?"
                        ),

                        tags$li(
                            "How does changing the confidence level affect the confidence band?"
                        ),

                        tags$li(
                            "Why is the confidence band not necessarily the same width across the plot?"
                        ),

                        tags$li(
                            "How does the predicted value change as the prediction point is moved?"
                        ),

                        tags$li(
                            "What can the diagonal reference line tell us when it is displayed?"
                        )
                    )
                )
            )
        )
    )


    # =====================================================
    # Generated Code
    # =====================================================

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

                textOutput(
                    ns("generated_code")
                )
            )
        )
    )


    # =====================================================
    # Results
    # =====================================================

    results_panel <- div(

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Inference'",
                ns("topic")
            ),

            fluidRow(

                column(
                    6,

                    card(
                        card_header(
                            "Observed data"
                        ),

                        plotOutput(
                            ns("dice_plot"),
                            height = 350
                        )
                    )
                ),

                column(
                    6,

                    card(
                        card_header(
                            "Simulated estimates of p"
                        ),

                        plotOutput(
                            ns("bootstrap_plot"),
                            height = 350
                        )
                    )
                )
            ),

            br(),

            uiOutput(
                ns("inference_results")
            )
        ),

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Regression'",
                ns("topic")
            ),

            card(

                card_header(
                    "Regression model"
                ),

                plotOutput(
                    ns("reg_plot"),
                    height = 450
                )
            ),

            br(),

            uiOutput(
                ns("regression_results")
            )
        )
    )


    # =====================================================
    # Build chapter
    # =====================================================

    chapter_page_ui(

        id = id,

        title = "📊 Module 5: Statistics",

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

chapter5_server <- function(id){


    moduleServer(id, function(input, output, session){


        # =====================================================
        # Regression season list
        # =====================================================

        seasons <- unique(
            pws::PL_points$season
        )


        # =====================================================
        # Keep final season >= first season
        # =====================================================

        observeEvent(
            input$start_season,
            {

                req(
                    input$start_season
                )

                start_index <- match(
                    input$start_season,
                    seasons
                )

                req(
                    !is.na(start_index)
                )

                valid_end_seasons <- seasons[
                    start_index:length(seasons)
                ]

                current_end <- input$end_season

                if (
                    is.null(current_end) ||
                    !(current_end %in% valid_end_seasons)
                ) {

                    current_end <- tail(
                        valid_end_seasons,
                        1
                    )
                }

                updateSelectInput(

                    session,

                    "end_season",

                    choices = valid_end_seasons,

                    selected = current_end
                )
            },

            ignoreInit = FALSE
        )


        # =====================================================
        # Reactive state
        # =====================================================

        rv <- reactiveValues(

            dice = NULL,

            p_true_used = NULL,

            p_hat = NULL,

            x = NULL,

            n = NULL,

            bootstrap_p = NULL,

            se = NULL,

            ci_active = FALSE,

            reveal_true = FALSE
        )


        # =====================================================
        # Button states
        # =====================================================

        observe({

            req(
                input$topic == "Inference"
            )

            if (
                is.null(rv$dice)
            ) {

                enable("roll")

                disable("estimate")

                disable("bootstrap")

                disable("ci")

                disable("reveal")

                disable("restart")

                return()
            }

            if (
                is.null(rv$p_hat)
            ) {

                disable("roll")

                enable("estimate")

                disable("bootstrap")

                disable("ci")

                disable("reveal")

                enable("restart")

                return()
            }

            if (
                is.null(rv$bootstrap_p)
            ) {

                disable("roll")

                disable("estimate")

                enable("bootstrap")

                disable("ci")

                enable("reveal")

                enable("restart")

                return()
            }

            disable("roll")

            disable("estimate")

            enable("bootstrap")

            enable("ci")

            enable("reveal")

            enable("restart")
        })


        # =====================================================
        # Roll Dice
        # =====================================================

        observeEvent(input$roll, {

            req(
                input$seed
            )

            set.seed(
                input$seed
            )

            if (
                input$prob_mode == "fixed"
            ) {

                rv$p_true_used <- input$p_true

            } else {

                rv$p_true_used <- runif(
                    1,
                    min = 0.05,
                    max = 0.50
                )
            }

            rv$dice <- sample(

                1:6,

                size = input$n,

                replace = TRUE,

                prob = c(

                    rep(
                        (1 - rv$p_true_used) / 5,
                        5
                    ),

                    rv$p_true_used
                )
            )

            rv$n <- length(
                rv$dice
            )

            rv$x <- sum(
                rv$dice == 6
            )

            rv$p_hat <- NULL

            rv$bootstrap_p <- NULL

            rv$se <- NULL

            rv$ci_active <- FALSE

            rv$reveal_true <- FALSE
        })


        # =====================================================
        # Estimate probability
        # =====================================================

        observeEvent(input$estimate, {

            req(
                rv$dice
            )

            rv$x <- sum(
                rv$dice == 6
            )

            rv$n <- length(
                rv$dice
            )

            rv$p_hat <- rv$x / rv$n

            rv$bootstrap_p <- NULL

            rv$se <- NULL

            rv$ci_active <- FALSE
        })


        # =====================================================
        # Restart Experiment
        # =====================================================

        observeEvent(input$restart, {

            new_seed <- sample(
                1:999,
                1
            )

            updateNumericInput(
                session,
                "seed",
                value = new_seed
            )

            rv$dice <- NULL

            rv$p_true_used <- NULL

            rv$p_hat <- NULL

            rv$x <- NULL

            rv$n <- NULL

            rv$bootstrap_p <- NULL

            rv$se <- NULL

            rv$ci_active <- FALSE

            rv$reveal_true <- FALSE
        })


        # =====================================================
        # Reveal true probability
        # =====================================================

        observeEvent(input$reveal, {

            req(
                rv$p_true_used
            )

            rv$reveal_true <- TRUE
        })


        # =====================================================
        # Simulate Estimates
        # =====================================================

        observeEvent(input$bootstrap, {

            req(
                rv$dice,
                rv$p_hat
            )

            n <- length(
                rv$dice
            )

            rv$bootstrap_p <- replicate(

                input$B,

                {

                    if (
                        input$boot_method == "est_p"
                    ) {

                        d <- sample(

                            1:6,

                            size = n,

                            replace = TRUE,

                            prob = c(

                                rep(
                                    (1 - rv$p_hat) / 5,
                                    5
                                ),

                                rv$p_hat
                            )
                        )

                    } else {

                        d <- sample(

                            rv$dice,

                            size = n,

                            replace = TRUE
                        )
                    }

                    mean(
                        d == 6
                    )
                }
            )

            rv$se <- sd(
                rv$bootstrap_p
            )

            rv$ci_active <- FALSE
        })


        # =====================================================
        # Changing simulation method
        # =====================================================

        observeEvent(input$boot_method, {

            req(
                rv$dice,
                rv$p_hat
            )

            rv$bootstrap_p <- NULL

            rv$se <- NULL

            rv$ci_active <- FALSE
        })


        # =====================================================
        # Changing number of simulations
        # =====================================================

        observeEvent(input$B, {

            req(
                rv$dice,
                rv$p_hat
            )

            rv$bootstrap_p <- NULL

            rv$se <- NULL

            rv$ci_active <- FALSE
        })


        # =====================================================
        # Confidence Interval button
        # =====================================================

        observeEvent(input$ci, {

            req(
                rv$bootstrap_p
            )

            rv$ci_active <- TRUE
        })


        # =====================================================
        # Confidence interval calculation
        # =====================================================

        ci_inference <- reactive({

            req(
                rv$bootstrap_p,
                rv$p_hat,
                rv$se,
                rv$ci_active
            )

            z <- qnorm(
                1 - (1 - input$conf) / 2
            )

            c(

                rv$p_hat - z * rv$se,

                rv$p_hat + z * rv$se
            )
        })


        # =====================================================
        # Generated R code
        # =====================================================

        output$generated_code <- renderText({

            if (
                input$topic == "Inference"
            ) {

                if (
                    !is.null(rv$p_true_used)
                ) {

                    p_used <- rv$p_true_used

                } else if (
                    input$prob_mode == "fixed"
                ) {

                    p_used <- input$p_true

                } else {

                    p_used <- 0.25
                }

                if (
                    !is.null(rv$n)
                ) {

                    n_used <- rv$n

                } else {

                    n_used <- input$n
                }

                if (
                    input$prob_mode == "random"
                ) {

                    probability_code <- paste0(

                        "# Randomly choose the true probability\n",

                        "p_true <- runif(1, 0.05, 0.50)\n\n"
                    )

                } else {

                    probability_code <- paste0(

                        "# Choose the true probability\n",

                        "p_true <- ", input$p_true, "\n\n"
                    )
                }

                if (
                    input$boot_method == "est_p"
                ) {

                    bootstrap_code <- paste0(

                        "# Approximate process simulation\n\n",

                        "p_hat <- mean(dice == 6)\n\n",

                        "bootstrap_p <- replicate(\n",

                        "    ", input$B, ",\n",

                        "    {\n",

                        "        d <- sample(\n",

                        "            1:6,\n",

                        "            size = length(dice),\n",

                        "            replace = TRUE,\n",

                        "            prob = c(\n",

                        "                rep((1 - p_hat)/5, 5),\n",

                        "                p_hat\n",

                        "            )\n",

                        "        )\n",

                        "        mean(d == 6)\n",

                        "    }\n",

                        ")"
                    )

                } else {

                    bootstrap_code <- paste0(

                        "# Resampling\n\n",

                        "bootstrap_p <- replicate(\n",

                        "    ", input$B, ",\n",

                        "    mean(sample(dice, replace = TRUE) == 6)\n",

                        ")"
                    )
                }

                code <- paste0(

                    "## One-dice inference investigation\n\n",

                    "set.seed(", input$seed, ")\n\n",

                    probability_code,

                    "# Generate observed dice rolls\n",

                    "dice <- sample(\n",

                    "    1:6,\n",

                    "    size = ", n_used, ",\n",

                    "    replace = TRUE,\n",

                    "    prob = c(\n",

                    "        rep((1 - p_true)/5, 5),\n",

                    "        p_true\n",

                    "    )\n",

                    ")\n\n",

                    "# Estimate probability of rolling a six\n",

                    "x <- sum(dice == 6)\n",

                    "n <- length(dice)\n",

                    "p_hat <- x / n\n\n",

                    bootstrap_code,

                    "\n\n",

                    "# Bootstrap standard error\n",

                    "se <- sd(bootstrap_p)\n\n",

                    "# Confidence interval\n",

                    "z <- qnorm(1 - (1 - ", input$conf, ")/2)\n\n",

                    "c(\n",

                    "    p_hat - z * se,\n",

                    "    p_hat + z * se\n",

                    ")"
                )

            } else {

                code <- paste0(

                    "## Regression investigation\n\n",

                    "# Select seasons\n",

                    "data <- subset(\n",

                    "    pws::PL_points,\n",

                    "    season >= '", input$start_season, "' &\n",

                    "    season <= '", input$end_season, "'\n",

                    ")\n\n",

                    "# Fit regression model\n\n",

                    "model <- lm(\n",

                    "    points_half2 ~ points_half1,\n",

                    "    data = data\n",

                    ")\n\n",

                    "# Prediction at selected point\n\n",

                    "predict(\n",

                    "    model,\n",

                    "    newdata = data.frame(\n",

                    "        points_half1 = ", input$x_split, "\n",

                    "    ),\n",

                    "    interval = 'confidence',\n",

                    "    level = ", input$conf_reg, "\n",

                    ")"
                )
            }

            code
        })


        # =====================================================
        # Dice plot
        # =====================================================

        output$dice_plot <- renderPlot({

            req(
                rv$dice
            )

            df <- data.frame(

                face = factor(
                    rv$dice,
                    levels = 1:6
                )
            )

            ggplot(
                df,
                aes(face)
            ) +

                geom_bar(

                    aes(
                        fill = face == "6"
                    ),

                    colour = "white",

                    linewidth = 0.4
                ) +

                scale_fill_manual(

                    values = c(

                        "FALSE" = pal_blue_soft,

                        "TRUE" = pal_red
                    ),

                    guide = "none"
                ) +

                theme_minimal(
                    base_size = 14
                ) +

                labs(

                    x = "Face",

                    y = "Frequency"
                )
        })


        # =====================================================
        # Simulated estimates plot
        # =====================================================

        output$bootstrap_plot <- renderPlot({

            req(
                rv$bootstrap_p
            )

            df <- data.frame(
                p = rv$bootstrap_p
            )

            p <- ggplot(
                df,
                aes(p)
            ) +

                geom_histogram(

                    bins = 30,

                    fill = pal_lav,

                    colour = "white"
                ) +

                theme_minimal(
                    base_size = 14
                ) +

                labs(

                    x = expression(hat(p)),

                    y = "Frequency"
                )

            if (
                isTRUE(rv$ci_active)
            ) {

                ci <- ci_inference()

                p <- p +

                    annotate(

                        "rect",

                        xmin = ci[1],

                        xmax = ci[2],

                        ymin = 0,

                        ymax = Inf,

                        alpha = 0.15,

                        fill = pal_red
                    ) +

                    geom_vline(

                        xintercept = ci,

                        colour = pal_red,

                        linewidth = 1.2
                    )
            }

            if (
                isTRUE(rv$reveal_true)
            ) {

                p <- p +

                    geom_vline(

                        xintercept = rv$p_true_used,

                        colour = pal_reveal,

                        linewidth = 1.3,

                        linetype = "dashed"
                    ) +

                    annotate(

                        "text",

                        x = rv$p_true_used,

                        y = Inf,

                        label = paste0(
                            "True p = ",
                            round(
                                rv$p_true_used,
                                3
                            )
                        ),

                        colour = pal_reveal,

                        vjust = 1.5,

                        hjust = -0.05,

                        fontface = "bold"
                    )
            }

            p
        })


        # =====================================================
        # Inference summary
        # =====================================================

        output$inference_results <- renderUI({

            req(
                input$topic == "Inference"
            )

            if (
                is.null(rv$dice)
            ) {

                return(NULL)
            }

            result_items <- list()

            if (
                !is.null(rv$p_hat)
            ) {

                result_items <- append(

                    result_items,

                    list(

                        p(

                            strong(
                                "Estimate p: "
                            ),

                            rv$x,

                            " / ",

                            rv$n,

                            " = ",

                            round(
                                rv$p_hat,
                                4
                            )
                        )
                    )
                )
            }

            if (
                !is.null(rv$se)
            ) {

                result_items <- append(

                    result_items,

                    list(

                        p(

                            strong(
                                "Simulation SE: "
                            ),

                            round(
                                rv$se,
                                4
                            )
                        )
                    )
                )
            }

            if (
                isTRUE(rv$ci_active)
            ) {

                ci <- ci_inference()

                result_items <- append(

                    result_items,

                    list(

                        p(

                            strong(
                                "Confidence Interval: "
                            ),

                            paste0(

                                "[",

                                round(
                                    ci[1],
                                    4
                                ),

                                ", ",

                                round(
                                    ci[2],
                                    4
                                ),

                                "]"
                            )
                        )
                    )
                )
            }

            if (
                isTRUE(rv$reveal_true)
            ) {

                result_items <- append(

                    result_items,

                    list(

                        p(

                            strong(
                                "True probability: "
                            ),

                            round(
                                rv$p_true_used,
                                4
                            )
                        )
                    )
                )
            }

            card(

                card_header(
                    "Inference Summary"
                ),

                result_items
            )
        })


        # =====================================================
        # Regression data
        # =====================================================

        reg_data <- reactive({

            req(
                input$start_season,
                input$end_season
            )

            req(
                input$start_season %in% seasons,
                input$end_season %in% seasons
            )

            start_index <- match(
                input$start_season,
                seasons
            )

            end_index <- match(
                input$end_season,
                seasons
            )

            req(
                start_index <= end_index
            )

            pws::PL_points[
                pws::PL_points$season %in%
                    seasons[start_index:end_index],
                ,
                drop = FALSE
            ]
        })


        # =====================================================
        # Regression model
        # =====================================================

        reg_fit <- reactive({

            lm(

                points_half2 ~ points_half1,

                data = reg_data()
            )
        })


        # =====================================================
        # Prediction
        # =====================================================

        prediction <- reactive({

            predict(

                reg_fit(),

                newdata = data.frame(

                    points_half1 =
                        input$x_split
                ),

                interval = "confidence",

                level = input$conf_reg
            )
        })


        # =====================================================
        # Regression prediction grid
        # =====================================================

        plot_predictions <- reactive({

            fit <- reg_fit()

            df <- reg_data()

            grid <- data.frame(

                points_half1 = seq(

                    min(
                        df$points_half1,
                        na.rm = TRUE
                    ),

                    max(
                        df$points_half1,
                        na.rm = TRUE
                    ),

                    length.out = 100
                )
            )

            preds <- predict(

                fit,

                newdata = grid,

                interval = "confidence",

                level = input$conf_reg
            )

            cbind(
                grid,
                preds
            )
        })


        # =====================================================
        # Regression plot
        # =====================================================

        output$reg_plot <- renderPlot({

            df <- reg_data()

            plot_df <- plot_predictions()

            pr <- prediction()

            p <- ggplot(

                df,

                aes(
                    points_half1,
                    points_half2
                )

            ) +

                geom_point(
                    colour = pal_blue
                ) +

                geom_ribbon(

                    data = plot_df,

                    aes(

                        x = points_half1,

                        ymin = lwr,

                        ymax = upr
                    ),

                    fill = pal_lav,

                    alpha = 0.20,

                    inherit.aes = FALSE
                )

            if (
                isTRUE(input$show_diagonal)
            ) {

                p <- p +

                    geom_abline(

                        slope = 1,

                        intercept = 0,

                        colour = "#777777",

                        linetype = "dashed",

                        linewidth = 0.9
                    )
            }

            p <- p +

                geom_line(

                    data = plot_df,

                    aes(

                        x = points_half1,

                        y = fit
                    ),

                    colour = pal_lav,

                    linewidth = 1.2,

                    inherit.aes = FALSE
                ) +

                geom_vline(

                    xintercept = input$x_split,

                    colour = pal_red,

                    linetype = "dashed",

                    linewidth = 0.8
                ) +

                geom_hline(

                    yintercept =
                        as.numeric(
                            pr[1, "fit"]
                        ),

                    colour = pal_red,

                    linetype = "dashed",

                    linewidth = 0.8
                ) +

                annotate(

                    "point",

                    x = input$x_split,

                    y = as.numeric(
                        pr[1, "fit"]
                    ),

                    colour = pal_red,

                    size = 4
                ) +

                theme_minimal(
                    base_size = 14
                ) +

                labs(

                    x = "Points (Half 1)",

                    y = "Points (Half 2)"
                )

            p
        })


        # =====================================================
        # Regression summary
        # =====================================================


        output$regression_results <- renderUI({

            fit <- reg_fit()

            pr <- prediction()

            card(

                style = "
        background-color: #F5F7FB;
        border: none;
        border-radius: 12px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.05);
    ",

                card_header(
                    "Regression Summary",
                    style = "
            background-color: #EEF2F8;
            border-bottom: none;
            font-weight: 600;
        "
                ),

                p(
                    strong("Seasons: "),
                    input$start_season,
                    " to ",
                    input$end_season,
                    ",    ",
                    strong("Observations: "),
                    nrow(reg_data())
                ),

                p(
                    strong("Intercept: "),
                    round(coef(fit)[1], 1),
                    ",    ",
                    strong("Slope: "),
                    round(coef(fit)[2], 3)
                ),

                p(
                    strong("Prediction for second half of season points: "),
                    round(pr[1, "fit"], 1)
                ),

                p(
                    strong("Confidence Interval: "),
                    paste0(
                        "[",
                        round(pr[1, "lwr"], 1),
                        ", ",
                        round(pr[1, "upr"], 1),
                        "]"
                    )
                )
            )
        })





    })


}
