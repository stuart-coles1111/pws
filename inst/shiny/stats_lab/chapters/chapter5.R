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
pal_roll    <- "#5B8DB8"
pal_est     <- "#6FB286"
pal_sim     <- "#9B72B0"
pal_ci      <- "#6F777D"
pal_reveal  <- "#D99452"
pal_restart <- "#C96B68"


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

        width = 270,

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
        # One-Dice activities
        # =================================================

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Inference'",
                ns("topic")
            ),

            hr(),

            h5("The One-Dice game"),

            radioButtons(
                ns("dice_activity"),
                "Choose activity:",
                choices = c(
                    "Inference from observed data" = "inference",
                    "Sampling distribution of the estimator" = "sampling"
                ),
                selected = "inference"
            ),


            # =================================================
            # INFERENCE ACTIVITY
            # =================================================

            conditionalPanel(
                condition = sprintf(
                    "input['%s']=='inference'",
                    ns("dice_activity")
                ),

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
                        "True probability, θ, of rolling a 6",
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
                        "The true probability of rolling a six will be chosen randomly."
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

                numericInput(
                    ns("B"),
                    "Number of bootstrap samples",
                    value = 5000,
                    min = 100
                ),

                actionButton(
                    ns("bootstrap"),
                    "Bootstrap",
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
                    "Confidence Interval for θ",
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
                    "Reveal true probability, θ",
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
            # SAMPLING DISTRIBUTION ACTIVITY
            # =================================================

            conditionalPanel(
                condition = sprintf(
                    "input['%s']=='sampling'",
                    ns("dice_activity")
                ),

                sliderInput(
                    ns("sampling_p"),
                    "True probability, θ, of rolling a 6",
                    min = 0.05,
                    max = 0.50,
                    value = 0.167,
                    step = 0.001
                ),

                sliderInput(
                    ns("sampling_n"),
                    "Sample size, n",
                    min = 10,
                    max = 1000,
                    value = 100,
                    step = 10,
                    sep = ","
                ),

                sliderInput(
                    ns("sampling_M"),
                    "Number of simulated samples",
                    min = 100,
                    max = 20000,
                    value = 5000,
                    step = 100,
                    sep = ","
                ),

                actionButton(
                    ns("sampling_simulate"),
                    "Simulate sampling distribution",
                    class = "btn-info",
                    style = paste0(
                        "background-color:", pal_sim,
                        "; border-color:", pal_sim, ";"
                    )
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
            )
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

                h5("Sampling distribution of the estimator"),

                p(
                    "The sampling distribution activity illustrates what happens when
                    we repeatedly collect samples from a population for which the true
                    probability is known. Each simulated sample gives a new estimate of
                    the probability."
                ),

                p(
                    "By examining many such estimates, you can investigate two important
                    properties of the estimator: whether its distribution is centred on
                    the true probability, and whether its distribution is approximately
                    Normal."
                ),

                h5("Bootstrap"),

                p(
                    "Once the probability has been estimated, the app uses bootstrap
                    resampling to investigate how much the estimate might vary from
                    sample to sample."
                ),

                p(
                    "A bootstrap sample is created by sampling with replacement from
                    the observed dice rolls. The probability of rolling a six is then
                    estimated for this new sample. Repeating this process many times
                    produces a distribution of bootstrap estimates."
                ),

                p(
                    "The resulting distribution gives a visual picture of the
                    sampling variability of the estimated probability."
                ),

                hr(),

                h5("Confidence intervals"),

                p(
                    "The bootstrap distribution can also be used to construct a
                    confidence interval for the probability estimated from the
                    observed data."
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
                            "What happens to the distribution of the estimator when the number of observations is increased?"
                        ),

                        tags$li(
                            "Is the mean of the simulated estimates close to the true probability?"
                        ),

                        tags$li(
                            "How does the shape of the sampling distribution change when the sample size is increased?"
                        ),

                        tags$li(
                            "What happens when the number of simulated samples is increased?"
                        ),

                        tags$li(
                            "What does the bootstrap distribution tell us about the uncertainty in the estimated probability?"
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

        # =================================================
        # OBSERVED-DATA INFERENCE
        # =================================================

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Inference' &&
                 input['%s']=='inference'",
                ns("topic"),
                ns("dice_activity")
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
                            "Bootstrap estimates of θ"
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


        # =================================================
        # SAMPLING DISTRIBUTION
        # =================================================

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Inference' &&
                 input['%s']=='sampling'",
                ns("topic"),
                ns("dice_activity")
            ),

            card(

                card_header(
                    "Sampling distribution of the estimator"
                ),

                plotOutput(
                    ns("sampling_plot"),
                    height = 500
                )
            ),

            br(),

            uiOutput(
                ns("sampling_results")
            )
        ),


        # =================================================
        # REGRESSION
        # =================================================

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
        # Formatting helper
        # =====================================================

        fmt3 <- function(x) {

            format(
                x,
                digits = 3,
                trim = TRUE,
                scientific = FALSE
            )
        }


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

            # -----------------------------------------------
            # Original inference experiment
            # -----------------------------------------------

            dice = NULL,

            p_true_used = NULL,

            p_hat = NULL,

            x = NULL,

            n = NULL,

            bootstrap_p = NULL,

            se = NULL,

            ci_active = FALSE,

            reveal_true = FALSE,


            # -----------------------------------------------
            # Sampling distribution activity
            # -----------------------------------------------

            sampling_p = NULL,

            sampling_n = NULL,

            sampling_M = NULL,

            sampling_estimates = NULL,

            # Seed that actually generated the currently
            # displayed sampling distribution.
            #
            # This is kept separately from input$seed because
            # input$seed is changed after each simulation.

            sampling_seed_used = NULL
        )


        # =====================================================
        # Button states
        # =====================================================

        observe({

            req(
                input$topic == "Inference",
                input$dice_activity == "inference"
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
        # Bootstrap
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

                mean(
                    sample(
                        rv$dice,
                        size = n,
                        replace = TRUE
                    ) == 6
                )
            )

            rv$se <- sd(
                rv$bootstrap_p
            )

            rv$ci_active <- FALSE
        })


        # =====================================================
        # Changing number of bootstrap samples
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
        # Sampling distribution simulation
        # =====================================================

        observeEvent(input$sampling_simulate, {

            req(
                input$sampling_p,
                input$sampling_n,
                input$sampling_M,
                input$seed
            )

            # -------------------------------------------------
            # Store the seed that will actually generate this
            # simulation.
            # -------------------------------------------------

            simulation_seed <- input$seed

            set.seed(
                simulation_seed
            )

            rv$sampling_p <- input$sampling_p

            rv$sampling_n <- as.numeric(
                input$sampling_n
            )

            rv$sampling_M <- as.numeric(
                input$sampling_M
            )

            rv$sampling_seed_used <- simulation_seed


            # -------------------------------------------------
            # Simulate the sampling distribution.
            #
            # Each simulated sample contains n Bernoulli
            # observations indicating whether a six was rolled.
            #
            # The estimator is the proportion of sixes:
            #
            #       p_hat = X / n
            #
            # This is equivalent to sampling from a binomial
            # distribution and dividing by n.
            # -------------------------------------------------

            rv$sampling_estimates <- replicate(

                rv$sampling_M,

                mean(
                    rbinom(
                        rv$sampling_n,
                        size = 1,
                        prob = rv$sampling_p
                    )
                )
            )


            # -------------------------------------------------
            # Generate a different seed for the next
            # simulation.
            #
            # The displayed seed therefore changes every time
            # the simulation button is pressed.
            # -------------------------------------------------

            possible_seeds <- setdiff(
                1:999,
                simulation_seed
            )

            next_seed <- sample(
                possible_seeds,
                1
            )

            updateNumericInput(
                session,
                "seed",
                value = next_seed
            )
        })


        # =====================================================
        # Generated R code
        # =====================================================

        output$generated_code <- renderText({

            # =================================================
            # SAMPLING DISTRIBUTION ACTIVITY
            # =================================================

            if (
                input$topic == "Inference" &&
                input$dice_activity == "sampling"
            ) {

                # -------------------------------------------------
                # Use the seed that actually generated the currently
                # displayed simulation.
                #
                # If no simulation has yet been run, use the seed
                # currently shown in the input box.
                # -------------------------------------------------

                if (
                    !is.null(rv$sampling_seed_used)
                ) {

                    seed_used <- rv$sampling_seed_used

                } else {

                    seed_used <- input$seed
                }

                code <- paste0(

                    "## Sampling distribution of the estimator\n\n",

                    "set.seed(",
                    seed_used,
                    ")\n\n",

                    "# True probability of rolling a six\n",

                    "p_true <- ",
                    input$sampling_p,
                    "\n\n",

                    "# Sample size\n",

                    "n <- ",
                    input$sampling_n,
                    "\n\n",

                    "# Number of simulated samples\n",

                    "M <- ",
                    input$sampling_M,
                    "\n\n",

                    "# Simulate the sampling distribution\n",

                    "sampling_estimates <- replicate(\n",

                    "    M,\n",

                    "    mean(\n",

                    "        rbinom(\n",

                    "            n,\n",

                    "            size = 1,\n",

                    "            prob = p_true\n",

                    "        )\n",

                    "    )\n",

                    ")\n\n",

                    "# Mean of the simulated estimates\n",

                    "mean(sampling_estimates)"
                )


                # =================================================
                # ORIGINAL INFERENCE ACTIVITY
                # =================================================

            } else if (
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

                        "p_true <- ",
                        input$p_true,
                        "\n\n"
                    )
                }

                bootstrap_code <- paste0(

                    "# Bootstrap resampling\n\n",

                    "bootstrap_p <- replicate(\n",

                    "    ", input$B, ",\n",

                    "    mean(\n",

                    "        sample(\n",

                    "            dice,\n",

                    "            size = length(dice),\n",

                    "            replace = TRUE\n",

                    "        ) == 6\n",

                    "    )\n",

                    ")"
                )

                code <- paste0(

                    "## One-dice bootstrap investigation\n\n",

                    "set.seed(",
                    input$seed,
                    ")\n\n",

                    probability_code,

                    "# Generate observed dice rolls\n",

                    "dice <- sample(\n",

                    "    1:6,\n",

                    "    size = ",
                    n_used,
                    ",\n",

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

                    "z <- qnorm(1 - (1 - ",
                    input$conf,
                    ")/2)\n\n",

                    "c(\n",

                    "    p_hat - z * se,\n",

                    "    p_hat + z * se\n",

                    ")"
                )
            }


            # =================================================
            # REGRESSION
            # =================================================

            else {

                code <- paste0(

                    "## Regression investigation\n\n",

                    "# Select seasons\n",

                    "data <- subset(\n",

                    "    pws::PL_points,\n",

                    "    season >= '",
                    input$start_season,
                    "' &\n",

                    "    season <= '",
                    input$end_season,
                    "'\n\n",

                    "# Fit regression model\n\n",

                    "model <- lm(\n",

                    "    points_half2 ~ points_half1,\n",

                    "    data = data\n",

                    ")\n\n",

                    "# Prediction at selected point\n\n",

                    "predict(\n",

                    "    model,\n",

                    "    newdata = data.frame(\n",

                    "        points_half1 = ",
                    input$x_split,
                    "\n",

                    "    ),\n",

                    "    interval = 'confidence',\n",

                    "    level = ",
                    input$conf_reg,
                    "\n",

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

                    x = "Score",

                    y = "Frequency"
                )
        })


        # =====================================================
        # Bootstrap estimates plot
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
                    x = expression(hat(theta)),
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
                            "True θ = ",
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
                input$topic == "Inference",
                input$dice_activity == "inference"
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
                                "Estimate of θ: "
                            ),

                            rv$x,

                            " / ",

                            rv$n,

                            " = ",

                            fmt3(
                                rv$p_hat
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
                                "Bootstrap SE: "
                            ),

                            fmt3(
                                rv$se
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
                                paste0(
                                    fmt3(input$conf * 100),
                                    "% confidence interval: "
                                )
                            ),

                            paste0(

                                "[",

                                fmt3(
                                    ci[1]
                                ),

                                ", ",

                                fmt3(
                                    ci[2]
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

                            fmt3(
                                rv$p_true_used
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
        # Sampling distribution plot
        # =====================================================

        output$sampling_plot <- renderPlot({

            req(
                rv$sampling_estimates,
                rv$sampling_p,
                rv$sampling_n
            )

            estimates <- rv$sampling_estimates

            p_true <- rv$sampling_p

            n <- rv$sampling_n

            M <- length(
                estimates
            )


            # -------------------------------------------------
            # Mean of simulated estimates
            # -------------------------------------------------

            simulated_mean <- mean(
                estimates
            )


            # -------------------------------------------------
            # Theoretical standard deviation is used only to
            # draw the Normal approximation.
            #
            # It is NOT reported to the user.
            # -------------------------------------------------

            theoretical_se <- sqrt(
                p_true * (1 - p_true) / n
            )


            # -------------------------------------------------
            # Create a discrete frequency distribution.
            #
            # p_hat can only take values:
            #
            # 0, 1/n, 2/n, ..., 1
            #
            # so an ordinary histogram can create misleading
            # binning artefacts.
            # -------------------------------------------------

            plot_df <- as.data.frame(
                table(
                    estimates
                ),
                stringsAsFactors = FALSE
            )

            names(plot_df) <- c(
                "p_hat",
                "frequency"
            )

            plot_df$p_hat <- as.numeric(
                as.character(
                    plot_df$p_hat
                )
            )

            plot_df$frequency <- as.numeric(
                plot_df$frequency
            )


            # -------------------------------------------------
            # Normal approximation
            #
            # Convert density to frequency scale.
            #
            # There are approximately n possible p_hat
            # intervals per unit of p, so multiply the density
            # by M/n to put the curve on the frequency scale.
            # -------------------------------------------------

            x_min <- max(
                0,
                min(
                    estimates,
                    p_true - 4 * theoretical_se
                )
            )

            x_max <- min(
                1,
                max(
                    estimates,
                    p_true + 4 * theoretical_se
                )
            )

            x_grid <- seq(
                x_min,
                x_max,
                length.out = 500
            )

            normal_df <- data.frame(

                x = x_grid,

                y =
                    dnorm(
                        x_grid,
                        mean = p_true,
                        sd = theoretical_se
                    ) *
                    M / n
            )


            # -------------------------------------------------
            # Set sensible annotation heights
            # -------------------------------------------------

            y_max <- max(
                plot_df$frequency,
                na.rm = TRUE
            )


            # -------------------------------------------------
            # Plot
            # -------------------------------------------------

            ggplot() +

                # ---------------------------------------------
            # Discrete simulated distribution
            # ---------------------------------------------

            geom_col(

                data = plot_df,

                aes(
                    x = p_hat,
                    y = frequency
                ),

                width = 0.8 / n,

                fill = pal_lav,

                colour = "white",

                linewidth = 0.3
            ) +

                # ---------------------------------------------
            # Normal approximation
            # ---------------------------------------------

            geom_line(

                data = normal_df,

                aes(
                    x = x,
                    y = y
                ),

                colour = pal_blue,

                linewidth = 1.3
            ) +

                # ---------------------------------------------
            # True p
            # ---------------------------------------------

            geom_vline(

                xintercept = p_true,

                colour = pal_reveal,

                linewidth = 1.2,

                linetype = "dashed"
            ) +

                # ---------------------------------------------
            # Simulated mean
            # ---------------------------------------------

            geom_vline(

                xintercept = simulated_mean,

                colour = pal_red,

                linewidth = 1.2
            ) +

                # ---------------------------------------------
            # True p label
            # ---------------------------------------------

            annotate(

                "text",

                x = p_true,

                y = y_max * 0.98,

                label = paste0(
                    "True θ = ",
                    round(
                        p_true,
                        3
                    )
                ),

                colour = pal_reveal,

                fontface = "bold",

                hjust = -0.05,

                vjust = 0
            ) +

                # ---------------------------------------------
            # Mean label
            # ---------------------------------------------

            annotate(

                "text",

                x = simulated_mean,

                y = y_max * 0.90,

                label = paste0(
                    "Mean = ",
                    round(
                        simulated_mean,
                        3
                    )
                ),

                colour = pal_red,

                fontface = "bold",

                hjust = -0.05,

                vjust = 0
            ) +

                # ---------------------------------------------
            # Theme
            # ---------------------------------------------

            theme_minimal(
                base_size = 14
            ) +

                labs(

                    title = expression(
                        "Sampling distribution of " ~ hat(theta)
                    ),

                    subtitle = paste0(
                        "n = ",
                        n,
                        ",  M = ",
                        format(
                            M,
                            big.mark = ","
                        )
                    ),

                    x = expression(hat(theta)),

                    y = "Frequency"
                )
        })


        # =====================================================
        # Sampling distribution summary
        # =====================================================

        output$sampling_results <- renderUI({

            req(
                rv$sampling_estimates,
                rv$sampling_p,
                rv$sampling_n
            )

            estimates <- rv$sampling_estimates

            p_true <- rv$sampling_p

            n <- rv$sampling_n

            simulated_mean <- mean(
                estimates
            )

            mean_difference <-
                simulated_mean - p_true


            card(

                style = "
                background-color: #F5F7FB;
                border: none;
                border-radius: 12px;
                box-shadow: 0 2px 8px rgba(0,0,0,0.05);
                ",

                card_header(
                    "Simulation summary"
                ),

                p(
                    strong("True probability: "),
                    fmt3(
                        p_true
                    )
                ),

                p(
                    strong("Mean of simulated estimates: "),
                    fmt3(
                        simulated_mean
                    )
                ),

                p(
                    strong("Difference from true θ: "),
                    fmt3(
                        mean_difference
                    )

                )
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
                    fmt3(
                        coef(fit)[1]
                    ),
                    ",    ",
                    strong("Slope: "),
                    fmt3(
                        coef(fit)[2]
                    )
                ),

                p(
                    strong(
                        "Prediction for second half of season points: "
                    ),
                    fmt3(
                        pr[1, "fit"]
                    )
                ),

                p(
                    strong(
                        paste0(
                            fmt3(input$conf_reg * 100),
                            "% confidence interval: "
                        )
                    ),

                    paste0(

                        "[",

                        fmt3(
                            pr[1, "lwr"]
                        ),

                        ", ",

                        fmt3(
                            pr[1, "upr"]
                        ),

                        "]"
                    )
                )
            )
        })
    })
}
