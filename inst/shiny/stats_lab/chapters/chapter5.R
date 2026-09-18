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

            card_header(
                div(
                    "📊 Basics of statistical inference",
                    style = "
                        font-size: 1.4rem;
                        font-weight: 700;
                        color: #2c3e50;
                    "
                )
            ),

            p(
                strong("Main idea: "),
                "Chapter 5 of Playing With Statistics discusses general principles of statistics, focusing on some of the main themes of statistical inference and modelling."
            ),

            hr(),

            h5("Topics"),

            p(
                "The module provides tools to explore two topics from Chapter 5 of Playing With Statistics:"
            ),

            tags$ul(
                tags$li(
                    "Statistical inference (using the one-dice game for illustration)"
                ),
                tags$li(
                    "Regression (using points in the Premier League as an example)"
                )
            ),

            hr(),

            h5("Your options"),

            p(
                "Choose to explore either statistical inference or regression."
            ),

            p(
                "If you choose statistical inference, you are guided through a simulation and analysis of the one-dice game, including:"
            ),

            tags$ul(
                tags$li("Simulation of observed data"),
                tags$li("Resampling of probability estimates"),
                tags$li("Calculation of confidence intervals")
            ),

            p(
                "Sidebar options allow you to choose the sample size, the method used to generate new estimates, and the confidence level used for intervals."
            ),

            p(
                "The display includes:"
            ),

            tags$ul(
                tags$li(
                    "A bar chart of the simulated data"
                ),
                tags$li(
                    "A histogram of the simulated estimates based on those data"
                ),
                tags$li(
                    "A summary of the inference"
                )
            ),

            p(
                "If you choose regression, you calculate a linear regression relationship between points scored in the first and second halves of Premier League seasons. The display shows:"
            ),

            tags$ul(
                tags$li(
                    "A graphical representation of the linear model overlaid on the data"
                ),
                tags$li(
                    "Predictions at any chosen point"
                ),
                tags$li(
                    "A confidence band around the fitted regression line"
                ),
                tags$li(
                    "A numerical summary of the regression model"
                )
            ),

            p(
                "Sidebar options allow you to vary the subset of data analysed, the prediction point, and the confidence level used for the confidence band."
            ),

            hr(),

            h5("What to observe"),

            p(
                "For each example, notice how the size of confidence intervals or bands are affected by the choice of sample size and the level of confidence chosen."
            ),

            p(
                "Notice also whether the numerical summaries of analyses are more or less effective in conveying information than the graphical summaries."
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

                p(
                    strong("For statistical inference:")
                ),

                tags$ul(
                    tags$li(
                        "How does the accuracy of estimates depend on the number of simulations?"
                    ),
                    tags$li(
                        "Does the confidence interval always contain the true probability?"
                    ),
                    tags$li(
                        "Does the method used to generate new estimates affect the results?"
                    )
                ),

                p(
                    strong("For regression:")
                ),

                tags$ul(
                    tags$li(
                        "What is the effect of fitting the regression model to smaller datasets?"
                    ),
                    tags$li(
                        "How does changing the confidence level affect the confidence bands?"
                    ),
                    tags$li(
                        "Are confidence intervals the same width at all prediction points?"
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

        title = "📊 Chapter 5: Statistics",

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


            ggplot(

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
            ) +


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
