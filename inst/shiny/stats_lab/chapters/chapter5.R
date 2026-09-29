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

        h4("Statistics"),

        numericInput(
            ns("seed"),
            "Random seed",
            value = sample(1:999, 1),
            min = 1,
            max = 999
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


        # -------------------------------------------------
        # Inference controls
        # -------------------------------------------------

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

            sliderInput(
                ns("p_true"),
                "True probability of rolling a 6",
                min = 0.05,
                max = 0.50,
                value = 0.167,
                step = 0.01
            ),

            actionButton(
                ns("roll"),
                "Roll Dice",
                class = "btn-primary"
            ),

            hr(),

            selectInput(
                ns("boot_method"),
                "How should new samples be generated?",
                choices = c(
                    "Exact process simulation" = "true_p",
                    "Approximate process simulation" = "est_p",
                    "Resampling" = "resample"
                )
            ),

            numericInput(
                ns("B"),
                "Number of simulated estimates",
                value = 5000,
                min = 100
            ),

            actionButton(
                ns("bootstrap"),
                "Simulate Estimates",
                class = "btn-info"
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
                "Confidence Interval",
                class = "btn-secondary"
            ),

            hr(),

            actionButton(
                ns("restart"),
                "Restart Experiment",
                class = "btn-warning"
            ),

            br(),
            br(),

            p(
                style = "
                    font-size: 0.85em;
                    color: #777;
                    margin-top: 5px;
                ",
                "The observed dice remain fixed while you repeat the simulation."
            )
        ),


        # -------------------------------------------------
        # Regression controls
        # -------------------------------------------------

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Regression'",
                ns("topic")
            ),

            h4("Regression Fitting"),

            selectInput(
                ns("end_season"),
                "Final season included",
                choices = unique(pws::PL_points$season),
                selected = tail(
                    unique(pws::PL_points$season),
                    1
                )
            ),

            sliderInput(
                ns("x_split"),
                "Prediction point",
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
                    "You begin by choosing the number of dice rolls and the true probability
            of rolling a six. When you press ",
                    strong("Roll Dice"),
                    ", the app generates one observed sample."
                ),

                p(
                    "The observed sample is then kept fixed while you investigate how
            estimates of the probability behave under repeated simulation."
                ),

                h5("Simulating estimates"),

                p(
                    "The app can generate many new estimates using three different approaches:"
                ),

                tags$ul(

                    tags$li(
                        strong("Exact process simulation: "),
                        "new samples are generated using the probability that produced the observed data."
                    ),

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
                            "How do the three methods of generating new estimates differ?"
                        ),

                        tags$li(
                            "How does the standard error change as the number of simulated estimates increases?"
                        ),

                        tags$li(
                            "What happens to the confidence interval when the confidence level is increased?"
                        ),

                        tags$li(
                            "Does a higher confidence level necessarily give a narrower interval?"
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

                h5("Fitting a regression model"),

                p(
                    "A linear regression model is fitted to the available seasons.
            The model describes the relationship between points in the first
            half and points in the second half."
                ),

                p(
                    "You can choose how many seasons are included in the analysis.
            This makes it possible to investigate how the fitted relationship
            changes when the amount of data is changed."
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
                            "Observed data — fixed until experiment is restarted"
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
                            "Simulated estimates"
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
        # Reactive state
        # =====================================================

        rv <- reactiveValues(

            # -------------------------------------------------
            # Observed data
            # -------------------------------------------------
            #
            # This is the experiment itself.
            # It remains fixed until Restart Experiment
            # followed by Roll Dice.
            #

            dice = NULL,


            # -------------------------------------------------
            # True probability used for the current experiment
            # -------------------------------------------------

            p_true_used = NULL,


            # -------------------------------------------------
            # Current simulated estimator distribution
            # -------------------------------------------------

            bootstrap_p = NULL,


            # -------------------------------------------------
            # Estimate from the observed data
            # -------------------------------------------------

            p_hat = NULL,


            # -------------------------------------------------
            # Standard error from the current simulation
            # -------------------------------------------------

            se = NULL,


            # -------------------------------------------------
            # Whether the confidence interval is displayed
            # -------------------------------------------------

            ci_active = FALSE
        )



        # =====================================================
        # Button states
        # =====================================================

        observe({

            req(
                input$topic == "Inference"
            )

            if (is.null(rv$dice)) {

                # ---------------------------------------------
                # No experiment yet
                # ---------------------------------------------

                enable("roll")
                disable("bootstrap")
                disable("ci")
                disable("restart")

            } else {

                # ---------------------------------------------
                # An experiment exists
                # ---------------------------------------------

                # The dice cannot be rerolled until the
                # experiment is restarted.
                disable("roll")

                # A new simulation can always be performed.
                enable("bootstrap")

                # The experiment can be restarted at any time.
                enable("restart")

                # CI is only available after estimates have
                # been simulated.
                if (is.null(rv$bootstrap_p)) {

                    disable("ci")

                } else {

                    enable("ci")
                }
            }
        })



        # =====================================================
        # Inference: Roll Dice
        # =====================================================

        observeEvent(input$roll, {

            # -------------------------------------------------
            # Generate a seed for this particular experiment
            # -------------------------------------------------

            new_seed <- sample(
                1:999,
                1
            )

            updateNumericInput(
                session,
                "seed",
                value = new_seed
            )

            set.seed(
                new_seed
            )


            # -------------------------------------------------
            # Store the true probability used for THIS
            # experiment.
            #
            # This value remains fixed even if the user later
            # moves the p_true slider.
            # -------------------------------------------------

            rv$p_true_used <- input$p_true


            # -------------------------------------------------
            # Generate observed dice
            # -------------------------------------------------

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


            # -------------------------------------------------
            # Calculate the estimate from the observed data
            #
            # This belongs to the observed sample and does
            # not depend on any simulation method.
            # -------------------------------------------------

            rv$p_hat <- mean(
                rv$dice == 6
            )


            # -------------------------------------------------
            # Clear anything belonging to a previous
            # simulation.
            # -------------------------------------------------

            rv$bootstrap_p <- NULL
            rv$se <- NULL
            rv$ci_active <- FALSE
        })



        # =====================================================
        # Restart Experiment
        # =====================================================

        observeEvent(input$restart, {

            # -------------------------------------------------
            # Clear the observed experiment
            # -------------------------------------------------

            rv$dice <- NULL

            rv$p_true_used <- NULL

            # -------------------------------------------------
            # Clear the simulated results
            # -------------------------------------------------

            rv$bootstrap_p <- NULL

            rv$p_hat <- NULL

            rv$se <- NULL

            rv$ci_active <- FALSE
        })



        # =====================================================
        # Simulate Estimates
        # =====================================================

        observeEvent(input$bootstrap, {

            req(
                rv$dice
            )

            n <- length(
                rv$dice
            )


            rv$bootstrap_p <- replicate(

                input$B,

                {

                    # =========================================
                    # Exact process simulation
                    # =========================================
                    #
                    # Generate a new sample from the actual
                    # process, using the true probability that
                    # generated the observed data.
                    #

                    if (
                        input$boot_method == "true_p"
                    ) {

                        d <- sample(

                            1:6,

                            size = n,

                            replace = TRUE,

                            prob = c(

                                rep(
                                    (1 - rv$p_true_used) / 5,
                                    5
                                ),

                                rv$p_true_used
                            )
                        )


                        # =========================================
                        # Approximate process simulation
                        # =========================================
                        #
                        # Generate a new sample from a process
                        # whose probability has been estimated
                        # from the observed data.
                        #

                    } else if (
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


                        # =========================================
                        # Resampling
                        # =========================================
                        #
                        # Generate a new sample by sampling with
                        # replacement from the observed data.
                        #

                    } else {

                        d <- sample(

                            rv$dice,

                            size = n,

                            replace = TRUE
                        )
                    }


                    # -----------------------------------------
                    # Estimate p from the simulated sample
                    # -----------------------------------------

                    mean(
                        d == 6
                    )
                }
            )


            # -------------------------------------------------
            # Standard error of the simulated estimates
            # -------------------------------------------------

            rv$se <- sd(
                rv$bootstrap_p
            )


            # -------------------------------------------------
            # A new simulation means that any old CI should
            # no longer be displayed.
            # -------------------------------------------------

            rv$ci_active <- FALSE
        })



        # =====================================================
        # Changing the simulation method
        # =====================================================

        observeEvent(input$boot_method, {

            req(
                rv$dice
            )

            # -------------------------------------------------
            # Keep the observed data!
            #
            # Only clear the current simulated distribution.
            # -------------------------------------------------

            rv$bootstrap_p <- NULL

            rv$se <- NULL

            rv$ci_active <- FALSE
        })



        # =====================================================
        # Changing the number of simulations
        # =====================================================

        observeEvent(input$B, {

            req(
                rv$dice
            )

            # -------------------------------------------------
            # If B changes, the existing simulation no longer
            # corresponds to the selected number of simulations.
            #
            # The observed data remain unchanged.
            # -------------------------------------------------

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
                rv$se
            )

            req(
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

                # -------------------------------------------------
                # Use the probability that actually generated
                # the current experiment.
                # -------------------------------------------------

                if (
                    !is.null(rv$p_true_used)
                ) {

                    p_used <- rv$p_true_used

                } else {

                    p_used <- input$p_true
                }


                # -------------------------------------------------
                # Use the number of observations in the actual
                # experiment if one exists.
                # -------------------------------------------------

                if (
                    !is.null(rv$dice)
                ) {

                    n_used <- length(
                        rv$dice
                    )

                } else {

                    n_used <- input$n
                }


                # -------------------------------------------------
                # Simulation code
                # -------------------------------------------------

                if (
                    input$boot_method == "true_p"
                ) {

                    bootstrap_code <- paste0(

                        "# Exact process simulation\n",

                        "bootstrap_p <- replicate(\n",

                        "    ", input$B, ",\n",

                        "    {\n",

                        "        d <- sample(\n",

                        "            1:6,\n",

                        "            size = length(dice),\n",

                        "            replace = TRUE,\n",

                        "            prob = c(\n",

                        "                rep((1 - ", p_used, ")/5, 5),\n",

                        "                ", p_used, "\n",

                        "            )\n",

                        "        )\n",

                        "        mean(d == 6)\n",

                        "    }\n",

                        ")"
                    )


                } else if (
                    input$boot_method == "est_p"
                ) {

                    bootstrap_code <- paste0(

                        "# Approximate process simulation\n",

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

                        "# Resampling\n",

                        "bootstrap_p <- replicate(\n",

                        "    ", input$B, ",\n",

                        "    mean(sample(dice, replace = TRUE) == 6)\n",

                        ")"
                    )
                }


                # -------------------------------------------------
                # Complete generated code
                # -------------------------------------------------

                code <- paste0(

                    "## One-dice inference investigation\n\n",

                    "# Generate observed dice rolls\n",

                    "set.seed(", input$seed, ")\n\n",

                    "dice <- sample(\n",

                    "    1:6,\n",

                    "    size = ", n_used, ",\n",

                    "    replace = TRUE,\n",

                    "    prob = c(\n",

                    "        rep((1 - ", p_used, ")/5, 5),\n",

                    "        ", p_used, "\n",

                    "    )\n",

                    ")\n\n",

                    "# Estimate probability of rolling a six\n",

                    "p_hat <- mean(dice == 6)\n\n",

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

                # =================================================
                # Regression code
                # =================================================

                code <- paste0(

                    "## Regression investigation\n\n",

                    "# Select seasons\n",

                    "data <- subset(\n",

                    "    pws::PL_points,\n",

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

                        "FALSE" = "#A9BFE3",

                        "TRUE" = "#D9534F"
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


            # -------------------------------------------------
            # Add confidence interval only after the user
            # presses the CI button.
            # -------------------------------------------------

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


            p
        })



        # =====================================================
        # Inference summary
        # =====================================================

        output$inference_results <- renderUI({

            req(
                input$topic == "Inference"
            )


            # -------------------------------------------------
            # No experiment yet
            # -------------------------------------------------

            if (
                is.null(rv$dice)
            ) {

                return(NULL)
            }


            # -------------------------------------------------
            # The estimate is always available once the dice
            # have been rolled.
            # -------------------------------------------------

            card(

                card_header(
                    "Inference Summary"
                ),


                p(

                    strong(
                        "Estimated p: "
                    ),

                    round(
                        rv$p_hat,
                        3
                    )
                ),


                # -------------------------------------------------
                # Standard error appears once estimates have
                # been simulated.
                # -------------------------------------------------

                if (
                    !is.null(rv$se)
                ) {

                    p(

                        strong(
                            "Simulation SE: "
                        ),

                        round(
                            rv$se,
                            4
                        )
                    )
                },


                # -------------------------------------------------
                # CI appears only after CI button is pressed.
                # -------------------------------------------------

                if (
                    isTRUE(rv$ci_active)
                ) {

                    p(

                        strong(
                            "Confidence Interval: "
                        ),

                        paste0(

                            "[",

                            round(
                                ci_inference()[1],
                                3
                            ),

                            ", ",

                            round(
                                ci_inference()[2],
                                3
                            ),

                            "]"
                        )
                    )
                }
            )
        })



        # =====================================================
        # Regression
        # =====================================================

        reg_data <- reactive({

            seasons <- unique(
                pws::PL_points$season
            )


            end_index <- match(

                input$end_season,

                seasons
            )


            pws::PL_points[

                pws::PL_points$season %in%
                    seasons[1:end_index],

            ]
        })


        reg_fit <- reactive({

            lm(

                points_half2 ~ points_half1,

                data = reg_data()
            )
        })


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


            # ---------------------------------------------
            # Observations
            # ---------------------------------------------

            geom_point(
                colour = pal_blue
            ) +


            # ---------------------------------------------
            # Confidence band
            # ---------------------------------------------

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


            # ---------------------------------------------
            # Optional diagonal reference line
            # ---------------------------------------------

            if (isTRUE(input$show_diagonal)) {

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


            # ---------------------------------------------
            # Regression line
            # ---------------------------------------------

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


            # ---------------------------------------------
            # Vertical prediction line
            # ---------------------------------------------

            geom_vline(

                xintercept = input$x_split,

                colour = pal_red,

                linetype = "dashed",

                linewidth = 0.8
            ) +


            # ---------------------------------------------
            # Horizontal prediction line
            # ---------------------------------------------

            geom_hline(

                yintercept =
                    as.numeric(
                        pr[1, "fit"]
                    ),

                colour = pal_red,

                linetype = "dashed",

                linewidth = 0.8
            ) +


            # ---------------------------------------------
            # Prediction point
            # ---------------------------------------------

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



        output$regression_results <- renderUI({

            fit <- reg_fit()

            pr <- prediction()


            card(

                card_header(
                    "Regression Summary"
                ),


                p(

                    strong(
                        "Observations: "
                    ),

                    nrow(
                        reg_data()
                    )
                ),


                p(

                    strong(
                        "Slope: "
                    ),

                    round(
                        coef(fit)[2],
                        3
                    )
                ),


                p(

                    strong(
                        "Intercept: "
                    ),

                    round(
                        coef(fit)[1],
                        1
                    )
                ),


                p(

                    strong(
                        "Prediction: "
                    ),

                    round(
                        pr[1, "fit"],
                        1
                    )
                ),


                p(

                    strong(
                        "Confidence Interval: "
                    ),

                    paste0(

                        "[",

                        round(
                            pr[1, "lwr"],
                            1
                        ),

                        ", ",

                        round(
                            pr[1, "upr"],
                            1
                        ),

                        "]"
                    )
                )
            )
        })

    })
}
