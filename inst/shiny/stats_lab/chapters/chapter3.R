# =========================================================
# Chapter 3 — Expectation
# =========================================================


# =========================================================
# Colours
# =========================================================

pal_c3_blue <- "#7B9ACC"
pal_c3_red  <- "#D9534F"
pal_c3_soft <- "#A9BFE3"
pal_c3_lav  <- "#CDB4DB"


# =========================================================
# UI
# =========================================================

chapter3_ui <- function(id){

    ns <- NS(id)


    # =====================================================
    # Sidebar
    # =====================================================

    sidebar_controls <- sidebar(

        h4("Expectation"),

        numericInput(
            ns("seed"),
            "Random seed",
            value = sample(1:999, 1),
            min = 1,
            max = 999
        ),

        radioButtons(
            ns("topic"),
            "Choose example",
            choices = c(
                "Rolling means of dice" = "Dice",
                "Expected goals (xG)" = "xG"
            ),
            selected = "Dice"
        ),


        # -------------------------------------------------
        # Dice controls
        # -------------------------------------------------

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Dice'",
                ns("topic")
            ),

            hr(),

            h5("Dice simulation"),

            numericInput(
                ns("n_rolls"),
                "Number of rolls per run",
                value = 1000,
                min = 10,
                max = 100000
            ),

            numericInput(
                ns("n_runs"),
                "Number of runs",
                value = 4,
                min = 1,
                max = 25
            ),

            actionButton(
                ns("dice_run"),
                "Generate rolling means",
                class = "btn-primary"
            )
        ),


        # -------------------------------------------------
        # xG controls
        # -------------------------------------------------

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='xG'",
                ns("topic")
            ),

            hr(),

            h5("xG simulation"),

            actionButton(
                ns("randomise"),
                "Randomise model parameters",
                class = "btn-secondary"
            ),

            numericInput(
                ns("n_data"),
                "Number of shots",
                value = 5000,
                min = 100
            ),

            actionButton(
                ns("run"),
                "Generate data",
                class = "btn-primary"
            ),

            hr(),

            actionButton(
                ns("fit"),
                "Fit model",
                class = "btn-info"
            ),

            hr(),

            h5("Prediction tool"),

            numericInput(
                ns("x"),
                "x coordinate",
                5
            ),

            numericInput(
                ns("y"),
                "y coordinate",
                10
            ),

            selectInput(
                ns("body"),
                "Shot type",
                choices = c(
                    "Head" = "Head",
                    "Foot" = "Foot"
                )
            ),

            actionButton(
                ns("predict"),
                "Predict",
                class = "btn-success"
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
                    "⚖ Understanding expectation",
                    style = "
                        font-size: 1.4rem;
                        font-weight: 700;
                        color: #2c3e50;
                    "
                )
            ),

            p(
                strong("Main idea: "),
                "Expectation describes the average outcome we would anticipate over many repetitions of a random process. It provides a link between a probability model and the long-run behaviour of data."
            ),

            p(
                "This module explores the idea in two different settings. The first is deliberately simple: repeated rolls of a fair die. The second applies the same ideas to expected goals (xG) in football."
            ),

            hr(),

            h5("1. Rolling means of dice"),

            p(
                "A fair six-sided die has theoretical expectation 3.5. No individual roll can equal 3.5, but the average of a large number of rolls tends to get closer to 3.5."
            ),

            p(
                "The simulation shows several independent runs of dice rolls. In each run, the rolling mean is calculated after every roll and compared with the theoretical expectation."
            ),

            p(
                "This provides a visual way of exploring the relationship between a theoretical expectation and the averages that arise from actual random data."
            ),

            hr(),

            h5("2. Expected goals (xG)"),

            p(
                "Expected goals provides an applied example. For an individual chance, an xG value represents the estimated probability that the chance results in a goal. For a collection of chances, the xG values can be added to obtain an expected number of goals."
            ),

            p(
                "The simulation uses a simple model in which scoring probability depends on the location and type of a chance. Data are generated from the model, the model is then fitted to those data, and the fitted model can be used to obtain predictions for new chances."
            ),

            hr(),

            h5("Your options"),

            p(
                "Choose either example using the controls in the sidebar."
            ),

            tags$ul(

                tags$li(
                    strong("Rolling means of dice: "),
                    "investigate how sample averages behave as the number of observations increases."
                ),

                tags$li(
                    strong("Expected goals: "),
                    "investigate how probabilities can be combined and estimated using a statistical model."
                )

            ),

            hr(),

            h5("What to observe"),

            p(
                "For the dice example, pay particular attention to what happens to the rolling means as the number of rolls increases. Notice both the tendency towards the theoretical expectation and the fact that random variation does not disappear completely."
            ),

            p(
                "For the xG example, compare the patterns in the simulated outcomes with the fitted model. Consider how the amount of data affects the accuracy of the estimated parameters and the resulting predictions."
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
                    strong("Rolling means of dice:")
                ),

                tags$ul(

                    tags$li(
                        "How quickly do the rolling means move towards 3.5?"
                    ),

                    tags$li(
                        "Do all runs behave in the same way?"
                    ),

                    tags$li(
                        "What happens if the number of rolls is much smaller?"
                    ),

                    tags$li(
                        "Does the rolling mean ever become exactly equal to 3.5?"
                    )

                ),

                p(
                    strong("Expected goals:")
                ),

                tags$ul(

                    tags$li(
                        "How accurately can the model parameters be estimated?"
                    ),

                    tags$li(
                        "How does estimation accuracy depend on sample size?"
                    ),

                    tags$li(
                        "Do the fitted heatmaps reflect the patterns in the observed data?"
                    ),

                    tags$li(
                        "How should an xG value be interpreted for an individual chance?"
                    )

                )

            )

        )

    )


    # =====================================================
    # Generated R Code
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

    results_panel <- tagList(


        # -------------------------------------------------
        # Dice results
        # -------------------------------------------------

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Dice'",
                ns("topic")
            ),

            card(

                card_header("Rolling means of dice rolls"),

                plotOutput(
                    ns("dice_plot"),
                    height = 650
                )
            ),

            br(),

            uiOutput(
                ns("dice_summary")
            )
        ),


        # -------------------------------------------------
        # xG results
        # -------------------------------------------------

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='xG'",
                ns("topic")
            ),

            fluidRow(

                column(
                    4,

                    card(

                        card_header("Model parameters"),

                        tableOutput(
                            ns("model")
                        ),

                        br(),

                        h5("Prediction"),

                        div(
                            style = "
                                background-color: #f8f9fa;
                                border-left: 5px solid #28a745;
                                padding: 12px;
                                border-radius: 8px;
                                margin-top: 10px;
                            ",

                            h5("⚽ Goal prediction"),

                            textOutput(
                                ns("pred")
                            )
                        )
                    )
                ),

                column(
                    8,

                    card(

                        card_header("xG model"),

                        h4(
                            "Observed Values",
                            style = "text-align:center;"
                        ),

                        fluidRow(

                            column(
                                6,
                                plotOutput(
                                    ns("plot1"),
                                    height = 300
                                )
                            ),

                            column(
                                6,
                                plotOutput(
                                    ns("plot2"),
                                    height = 300
                                )
                            )

                        ),

                        br(),

                        h4(
                            "Fitted Model Heatmap",
                            style = "text-align:center;"
                        ),

                        fluidRow(

                            column(
                                6,
                                plotOutput(
                                    ns("plot3"),
                                    height = 300
                                )
                            ),

                            column(
                                6,
                                plotOutput(
                                    ns("plot4"),
                                    height = 300
                                )
                            )

                        )
                    )
                )
            )
        )
    )


    # =====================================================
    # What should you have learned?
    # =====================================================

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


            # -------------------------------------------------
            # Dice learning points
            # -------------------------------------------------

            conditionalPanel(
                condition = sprintf(
                    "input['%s']=='Dice'",
                    ns("topic")
                ),

                h5("1. Expectation describes long-run behaviour"),

                p(
                    "The expectation of a fair die is 3.5. This does not mean that a roll of the die will produce 3.5. Rather, it describes the value towards which the average of many independent rolls tends to move."
                ),

                hr(),

                h5("2. Averages vary from sample to sample"),

                p(
                    "Different runs of the same random process produce different results. Even after many rolls, the rolling means will not be identical."
                ),

                hr(),

                h5("3. More data generally gives a more stable average"),

                p(
                    "As the number of rolls increases, the rolling means tend to fluctuate less dramatically around the theoretical expectation."
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
                        strong(
                            "An expectation is not a prediction of what will happen next."
                        ),
                        br(),
                        "It describes the average behaviour we would expect over many repetitions."
                    )
                )
            ),


            # -------------------------------------------------
            # xG learning points
            # -------------------------------------------------

            conditionalPanel(
                condition = sprintf(
                    "input['%s']=='xG'",
                    ns("topic")
                ),

                h5("1. xG is a probability for an individual chance"),

                p(
                    "An xG value represents the estimated probability that a particular chance results in a goal. An xG of 0.3 does not mean that the chance will produce 0.3 goals."
                ),

                hr(),

                h5("2. Expected goals combine probabilities"),

                p(
                    "When xG values are added across a collection of chances, the result represents the expected number of goals from those chances."
                ),

                hr(),

                h5("3. Expectations can be estimated from models"),

                p(
                    "The xG model uses information about the characteristics of a chance to estimate its scoring probability. The parameters of the model are themselves estimated from data."
                ),

                hr(),

                h5("4. Estimates vary because data vary"),

                p(
                    "Even when the underlying model is fixed, different samples produce different parameter estimates. Larger datasets generally provide more stable estimates."
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
                        strong(
                            "Expectation connects probability models to average outcomes."
                        ),
                        br(),
                        "It provides a way of translating probabilities for individual events into statements about what we expect to see across many events."
                    )
                )
            )
        )
    )


    # =====================================================
    # Build chapter
    # =====================================================

    chapter_page_ui(

        id = id,

        title = "⚖ Chapter 3: Expectation",

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

chapter3_server <- function(id){

    moduleServer(
        id,
        function(input, output, session){


            # =================================================
            # Default xG parameters
            # =================================================

            default_params <- list(

                intercept = 0.25,

                bodyHead = -0.05,

                distance = -0.10,

                angle_trans = -0.80,

                bodyHead_distance = -0.25

            )


            true_params <- reactiveVal(
                default_params
            )


            # =================================================
            # App state
            # =================================================

            state <- reactiveValues(

                # Dice
                dice = NULL,

                # xG
                xg_data = NULL,
                xg_model = NULL,
                xg_plots = NULL,
                xg_pred = NULL

            )


            # =================================================
            # RESET
            # =================================================

            observeEvent(
                input$reset,
                {

                    new_seed <- sample(
                        1:999,
                        1
                    )

                    updateNumericInput(
                        session,
                        "seed",
                        value = new_seed
                    )

                    updateRadioButtons(
                        session,
                        "topic",
                        selected = "Dice"
                    )

                    updateNumericInput(
                        session,
                        "n_rolls",
                        value = 1000
                    )

                    updateNumericInput(
                        session,
                        "n_runs",
                        value = 9
                    )

                    updateNumericInput(
                        session,
                        "n_data",
                        value = 5000
                    )

                    updateNumericInput(
                        session,
                        "x",
                        value = 5
                    )

                    updateNumericInput(
                        session,
                        "y",
                        value = 10
                    )

                    updateSelectInput(
                        session,
                        "body",
                        selected = "Head"
                    )

                    true_params(
                        default_params
                    )

                    state$dice <- NULL

                    state$xg_data <- NULL
                    state$xg_model <- NULL
                    state$xg_plots <- NULL
                    state$xg_pred <- NULL
                }
            )



            # =================================================
            # DICE SIMULATION
            # =================================================

            observeEvent(
                input$dice_run,
                {

                    # Generate a new random seed
                    new_seed <- sample(
                        1:999,
                        1
                    )

                    # Update the displayed seed
                    updateNumericInput(
                        session,
                        "seed",
                        value = new_seed
                    )

                    # Use the new seed for this simulation
                    set.seed(
                        new_seed
                    )

                    state$dice <- pws::dice_mean_series(

                        n_rolls =
                            input$n_rolls,

                        nrep =
                            input$n_runs
                    )
                }
            )




            # =================================================
            # DICE PLOT
            # =================================================

            output$dice_plot <- renderPlot({

                req(
                    state$dice
                )

                # Keep the package-generated plot and layout.
                # Only change the facet labels to "Run 1",
                # "Run 2", etc.

                state$dice +
                    ggplot2::facet_wrap(
                        ggplot2::vars(run),
                        labeller = ggplot2::as_labeller(
                            function(x) {
                                paste0("Run ", x)
                            }
                        )
                    )
            })


            # =================================================
            # DICE SUMMARY
            # =================================================

            output$dice_summary <- renderUI({

                req(
                    state$dice
                )


                # -------------------------------------------------
                # Extract the data from the generated simulation
                # -------------------------------------------------

                df <- state$dice$data


                # Find the final roll actually present in the
                # generated data, rather than using the current
                # input$n_rolls value.

                final_roll <- max(
                    df$Roll,
                    na.rm = TRUE
                )


                final_means <- df$Mean[
                    df$Roll == final_roll
                ]


                card(

                    card_header(
                        "Summary"
                    ),

                    p(
                        strong(
                            "Theoretical expectation: "
                        ),
                        "3.5"
                    ),

                    p(
                        strong(
                            "Number of rolls in generated data: "
                        ),
                        final_roll
                    ),

                    p(
                        strong(
                            "Average of the final rolling means: "
                        ),
                        round(
                            mean(final_means),
                            3
                        )
                    ),

                    p(
                        strong(
                            "Range of final rolling means: "
                        ),
                        paste(
                            round(
                                range(final_means),
                                3
                            ),
                            collapse = " to "
                        )
                    ),

                    div(
                        style = "
                background-color:#f8f9fa;
                border-left:5px solid #7B9ACC;
                padding:12px;
                border-radius:8px;
            ",

                        p(
                            "The final rolling means are generally closer to 3.5 than the means based on only a few rolls, but they are not identical. Random variation remains even when the number of observations is large."
                        )
                    )
                )
            })




            # =================================================
            # RANDOMISE xG PARAMETERS
            # =================================================

            observeEvent(
                input$randomise,
                {

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

                    true_params(
                        list(

                            intercept =
                                runif(
                                    1,
                                    0.10,
                                    0.50
                                ),

                            bodyHead =
                                runif(
                                    1,
                                    -0.10,
                                    -0.025
                                ),

                            distance =
                                runif(
                                    1,
                                    -0.20,
                                    -0.05
                                ),

                            angle_trans =
                                runif(
                                    1,
                                    -1.00,
                                    -0.50
                                ),

                            bodyHead_distance =
                                runif(
                                    1,
                                    -0.50,
                                    0.00
                                )
                        )
                    )


                    # Clear results based on old parameters

                    state$xg_data <- NULL

                    state$xg_model <- NULL

                    state$xg_plots <- NULL

                    state$xg_pred <- NULL
                }
            )


            # =================================================
            # xG DATA GENERATION
            # =================================================

            observeEvent(
                input$run,
                {

                    pars <- true_params()


                    # Clear old fitted results first

                    state$xg_model <- NULL

                    state$xg_pred <- NULL


                    state$xg_data <- pws::xGsim(

                        n_data =
                            input$n_data,

                        bodypar =
                            0.2,

                        distpar =
                            0.2,

                        anglepar =
                            3,

                        intercept =
                            pars$intercept,

                        dist_coeff =
                            pars$distance,

                        dist_body_inter =
                            pars$bodyHead_distance,

                        body_coeff =
                            pars$bodyHead,

                        angle_coeff =
                            pars$angle_trans,

                        seed =
                            input$seed
                    )


                    state$xg_plots <- pws::xGplot(
                        state$xg_data
                    )
                }
            )


            # =================================================
            # xG MODEL FITTING
            # =================================================

            observeEvent(
                input$fit,
                {

                    req(
                        state$xg_data
                    )

                    state$xg_model <- pws::xGfit(
                        state$xg_data
                    )

                    state$xg_pred <- NULL
                }
            )


            # =================================================
            # xG PREDICTION
            # =================================================

            observeEvent(
                input$predict,
                {

                    req(
                        state$xg_model
                    )

                    state$xg_pred <- pws::xGpred(

                        state$xg_model,

                        input$x,

                        input$y,

                        input$body
                    )
                }
            )


            # =================================================
            # xG MODEL TABLE
            # =================================================

            output$model <- renderTable({

                pars <- true_params()


                row_names <- c(

                    "Intercept",

                    "Header",

                    "Distance",

                    "Angle",

                    "Distance-Header Interaction"

                )


                truth <- data.frame(

                    check.names = FALSE,

                    "True Parameters" = c(

                        pars$intercept,

                        pars$bodyHead,

                        pars$distance,

                        pars$angle_trans,

                        pars$bodyHead_distance
                    ),

                    row.names = row_names
                )


                # No fitted model yet

                if (
                    is.null(
                        state$xg_model
                    )
                ) {

                    return(
                        round(
                            truth,
                            3
                        )
                    )
                }


                tbl <- state$xg_model$summary


                colnames(tbl) <- c(

                    "True Parameters",

                    "Estimates"

                )


                rownames(tbl) <- row_names


                tbl[, 1] <- truth[, 1]


                round(
                    tbl,
                    3
                )

            }, rownames = TRUE)


            # =================================================
            # xG PLOTS
            # =================================================

            output$plot1 <- renderPlot({

                req(
                    state$xg_plots
                )

                state$xg_plots[[1]]
            })


            output$plot2 <- renderPlot({

                req(
                    state$xg_plots
                )

                state$xg_plots[[2]]
            })


            output$plot3 <- renderPlot({

                req(
                    state$xg_plots,
                    state$xg_model
                )

                state$xg_plots[[3]]
            })


            output$plot4 <- renderPlot({

                req(
                    state$xg_plots,
                    state$xg_model
                )

                state$xg_plots[[4]]
            })


            # =================================================
            # xG PREDICTION OUTPUT
            # =================================================

            output$pred <- renderText({

                req(
                    state$xg_pred
                )

                sprintf(
                    "P(goal) = %.3f",
                    state$xg_pred
                )
            })



            # =================================================
            # GENERATED R CODE
            # =================================================

            output$generated_code <- renderText({

                # -------------------------------------------------
                # Dice
                # -------------------------------------------------

                if (input$topic == "Dice") {

                    # Nothing generated yet
                    if (is.null(state$dice)) {

                        return(
                            paste0(
                                "## Rolling means of dice rolls\n\n",

                                "# No simulation has been generated yet.\n",

                                "# Press 'Generate rolling means' to generate data."
                            )
                        )
                    }


                    # Get the actual seed and number of rolls used
                    # by the generated simulation.

                    used_seed <- input$seed

                    used_rolls <- max(
                        state$dice$data$Roll,
                        na.rm = TRUE
                    )

                    used_runs <- length(
                        unique(state$dice$data$run)
                    )


                    code <- paste0(

                        "## Rolling means of dice rolls\n\n",

                        "set.seed(",
                        used_seed,
                        ")\n\n",

                        "pws::dice_mean_series(\n",

                        "  n_rolls = ",
                        used_rolls,
                        ",\n",

                        "  nrep = ",
                        used_runs,
                        "\n",

                        ")"
                    )


                    return(
                        code
                    )
                }


                # -------------------------------------------------
                # xG
                # -------------------------------------------------

                pars <- true_params()


                # No data generated yet

                if (is.null(state$xg_data)) {

                    return(
                        paste0(

                            "## Expected goals (xG) investigation\n\n",

                            "# No data have been generated yet.\n",

                            "# Press 'Generate data' to generate simulated shots."
                        )
                    )
                }


                # -------------------------------------------------
                # Generate xG data
                # -------------------------------------------------

                code <- paste0(

                    "## Expected goals (xG) investigation\n\n",

                    "# Generate simulated shot data\n",

                    "shots <- pws::xGsim(\n",

                    "  n_data = ",
                    input$n_data,
                    ",\n",

                    "  bodypar = 0.2,\n",

                    "  distpar = 0.2,\n",

                    "  anglepar = 3,\n",

                    "  intercept = ",
                    round(
                        pars$intercept,
                        4
                    ),
                    ",\n",

                    "  dist_coeff = ",
                    round(
                        pars$distance,
                        4
                    ),
                    ",\n",

                    "  dist_body_inter = ",
                    round(
                        pars$bodyHead_distance,
                        4
                    ),
                    ",\n",

                    "  body_coeff = ",
                    round(
                        pars$bodyHead,
                        4
                    ),
                    ",\n",

                    "  angle_coeff = ",
                    round(
                        pars$angle_trans,
                        4
                    ),
                    ",\n",

                    "  seed = ",
                    input$seed,
                    "\n",

                    ")\n\n"
                )


                # -------------------------------------------------
                # Fit model
                # -------------------------------------------------

                if (!is.null(state$xg_model)) {

                    code <- paste0(

                        code,

                        "# Fit xG model\n",

                        "model <- pws::xGfit(shots)\n\n"
                    )
                }


                # -------------------------------------------------
                # Prediction
                # -------------------------------------------------

                if (!is.null(state$xg_pred)) {

                    code <- paste0(

                        code,

                        "# Predict scoring probability\n",

                        "pws::xGpred(\n",

                        "  model,\n",

                        "  x = ",
                        input$x,
                        ",\n",

                        "  y = ",
                        input$y,
                        ",\n",

                        "  body = \"",
                        input$body,
                        "\"\n",

                        ")"
                    )
                }


                code
            })



        }
    )
}
