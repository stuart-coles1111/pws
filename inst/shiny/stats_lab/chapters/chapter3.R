chapter3_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("xG simulation explorer"),
        p("Simulate football shots, fit an xG model, and make predictions."),

        numericInput(
            ns("seed"),
            "Random seed",
            value = sample(1:999, 1),
            min = 1,
            max = 999
        ),

        actionButton(ns("randomise"), "Randomise model parameters"),

        numericInput(ns("n_data"), "Number of shots", value = 5000, min = 100),

        actionButton(ns("run"), "Generate data"),

        hr(),

        actionButton(ns("fit"), "Fit model"),

        hr(),

        h4("Prediction tool"),

        numericInput(ns("x"), "x coordinate", 5),
        numericInput(ns("y"), "y coordinate", 10),

        selectInput(ns("body"), "Shot type", choices = c("Head", "Foot")),

        actionButton(ns("predict"), "Predict"),

        hr(),

        actionButton(
            ns("reset"),
            "Repeat From Start",
            style = "
        background-color: #e74c3c;
        color: white;
        border: none;
    "
        )
    )

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
                    "⚖ Understanding Expected Goals (xG)",
                    style = "
            font-size: 1.4rem;
            font-weight: 700;
            color: #2c3e50;
        "
                )
            ),

            p(
                strong("Main idea: "),
                "Expected goals (xG) have become a widely used statistic for summarising football matches. They are defined as the number of goals a team would be expected to score, given the chances it created in a game. This module uses simulation to explore estimation and prediction from an xG model."
            ),

            hr(),

            h5("xG model simulation and estimation"),

            p(
                "This module is based on a simple xG model in which the probability of scoring from a chance depends on:"
            ),

            tags$ul(
                tags$li("the distance to goal"),
                tags$li("the angle to the centre of the goal"),
                tags$li("the type of attempt (shot or header)")
            ),

            p(
                "The detailed way in which each of these factors affects xG depends on a set of numerical parameters. Either default values or randomised values can be used."
            ),

            p(
                "The same model is used for both simulation and estimation. Simulated data are generated from the model and then used to estimate its parameters. The estimated model can subsequently be used for predicting xG from new chance information."
            ),

            hr(),

            h5("Your options"),

            p(
                "Using either the default or randomised model parameters, generate a dataset of chances and goals, then fit an xG model to the simulated data."
            ),

            p("Experiment with:"),

            tags$ul(
                tags$li("Different model parameter values"),
                tags$li("Different dataset sizes")
            ),

            hr(),

            h5("Interpreting the output"),

            p("Once a model has been fitted, you will see:"),

            tags$ul(
                tags$li(
                    "A comparison of the true and estimated model parameters"
                ),
                tags$li(
                    "Maps showing the observed outcomes (goal or miss) for all simulated chances, displayed separately for shots and headers"
                ),
                tags$li(
                    "Heatmaps showing the fitted xG surface for shots and headers"
                )
            ),

            p(
                "Additionally, you can request the estimated xG value at any location on the pitch for either a shot or a header."
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
                        "How accurate is the estimation procedure?"
                    ),
                    tags$li(
                        "How does estimation accuracy depend on sample size and parameter values?"
                    ),
                    tags$li(
                        "Do the fitted heatmaps reflect the patterns visible in the observed goal/miss maps?"
                    ),
                    tags$li(
                        "How well can the estimated model predict xG at new locations?"
                    )
                )
            )
        )

    )


    code_panel <- div(
        card(
            card_header("Generated R Code"),
            tags$pre(textOutput(ns("generated_code")))
        )
    )

    results_panel <- tagList(

        fluidRow(

            column(
                4,

                card(
                    card_header("Model parameters"),

                    tableOutput(ns("model")),

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
                    card_header("xG plot"),

                    h4(
                        "Observed Values",
                        style = "text-align:center;"
                    ),

                    fluidRow(
                        column(6, plotOutput(ns("plot1"), height = 300)),
                        column(6, plotOutput(ns("plot2"), height = 300))
                    ),

                    br(),

                    h4(
                        "Fitted Model Heatmap",
                        style = "text-align:center;"
                    ),

                    fluidRow(
                        column(6, plotOutput(ns("plot3"), height = 300)),
                        column(6, plotOutput(ns("plot4"), height = 300))
                    )
                )
            )
        )
    )
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

            tags$div(

                h5("1. Expected goals are probabilities"),

                p(
                    "An xG value is the estimated probability that a shot
                 results in a goal. It is not a prediction that a goal
                 will definitely be scored."
                ),

                hr(),

                h5("2. Context influences success"),

                p(
                    "Distance, shooting angle and body part all affect
                 scoring probability. Better shooting situations
                 generally produce larger xG values."
                ),

                hr(),

                h5("3. Models learn from data"),

                p(
                    "The logistic regression model estimates relationships
                 between shot characteristics and outcomes using
                 historical observations."
                ),

                hr(),

                h5("4. Estimates contain uncertainty"),

                p(
                    "Even when the true model is known, fitted parameter
                 estimates vary from sample to sample because the data
                 contain random variation."
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
                        strong("Expected goals quantify chance, not certainty."),
                        br(),
                        "Statistical models convert information about shot
                     quality into probabilities that help explain and
                     predict football performance."
                    )
                )
            )
        )
    )

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

chapter3_server <- function(id){

    moduleServer(id, function(input, output, session){

        output$generated_code <- renderText({

            code <- paste0(

                "# Generate simulated shot data\n",
                "shots <- xGsim(\n",
                "  n_data = ", input$n_data, ",\n",
                "  seed = ", input$seed, "\n",
                ")\n\n"
            )

            if (!is.null(state$model)) {

                code <- paste0(
                    code,
                    "# Fit xG model\n",
                    "model <- xGfit(shots)\n\n"
                )
            }

            if (!is.null(state$pred)) {

                code <- paste0(
                    code,
                    "# Predict scoring probability\n",
                    "xGpred(\n",
                    "  model,\n",
                    "  x = ", input$x, ",\n",
                    "  y = ", input$y, ",\n",
                    "  body = \"", input$body, "\"\n",
                    ")\n"
                )
            }

            code
        })

        # -----------------------------------------------------
        # TRUE PARAMETERS
        # -----------------------------------------------------
        true_params <- reactiveVal(list(
            intercept = 0.25,
            bodyHead = -0.05,
            distance = -0.10,
            angle_trans = -0.80,
            bodyHead_distance = -0.25
        ))

        # -----------------------------------------------------
        # APP STATE (THE ONLY SOURCE OF TRUTH)
        # -----------------------------------------------------
        state <- reactiveValues(
            data = NULL,
            model = NULL,
            plots = NULL,
            pred = NULL
        )

        # -----------------------------------------------------
        # RANDOMISE PARAMETERS
        # -----------------------------------------------------
        observeEvent(input$randomise, {

            new_seed <- sample(1:999, 1)

            updateNumericInput(
                session,
                "seed",
                value = new_seed
            )

            set.seed(new_seed)

            true_params(list(
                intercept = runif(1, 0.10, 0.50),
                bodyHead = runif(1, -0.10, -0.025),
                distance = runif(1, -0.20, -0.05),
                angle_trans = runif(1, -1.00, -0.50),
                bodyHead_distance = runif(1, -0.50, 0.00)
            ))
        })

        # -----------------------------------------------------
        # RESET (FULL CLEAN STATE)
        # -----------------------------------------------------
        observeEvent(input$reset, {

            updateNumericInput(session, "seed", value = sample(1:999, 1))
            updateNumericInput(session, "n_data", value = 5000)

            updateNumericInput(session, "x", value = 5)
            updateNumericInput(session, "y", value = 10)
            updateSelectInput(session, "body", selected = "Head")

            true_params(list(
                intercept = 0.25,
                bodyHead = -0.05,
                distance = -0.10,
                angle_trans = -0.80,
                bodyHead_distance = -0.25
            ))

            # CLEAR ALL OUTPUT STATE
            state$data <- NULL
            state$model <- NULL
            state$plots <- NULL
            state$pred <- NULL
        })

        # -----------------------------------------------------
        # DATA GENERATION
        # -----------------------------------------------------
        observeEvent(input$run, {

            pars <- true_params()

            state$data <- pws::xGsim(
                n_data = input$n_data,
                bodypar = 0.2,
                distpar = 0.2,
                anglepar = 3,
                intercept = pars$intercept,
                dist_coeff = pars$distance,
                dist_body_inter = pars$bodyHead_distance,
                body_coeff = pars$bodyHead,
                angle_coeff = pars$angle_trans,
                seed = input$seed
            )

            state$plots <- pws::xGplot(state$data)
        })

        # -----------------------------------------------------
        # MODEL FITTING
        # -----------------------------------------------------
        observeEvent(input$fit, {

            req(state$data)

            state$model <- pws::xGfit(state$data)
        })

        # -----------------------------------------------------
        # PREDICTION
        # -----------------------------------------------------
        observeEvent(input$predict, {

            req(state$model)

            state$pred <- pws::xGpred(
                state$model,
                input$x,
                input$y,
                input$body
            )
        })

        # -----------------------------------------------------
        # MODEL TABLE
        # -----------------------------------------------------
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

            # BEFORE MODEL FIT
            if (is.null(state$model)) {
                return(round(truth, 3))
            }

            tbl <- state$model$summary

            colnames(tbl) <- c("True Parameters", "Estimates")
            rownames(tbl) <- row_names

            tbl[, 1] <- truth[, 1]

            round(tbl, 3)

        }, rownames = TRUE)
        # -----------------------------------------------------
        # PLOTS
        # -----------------------------------------------------
        output$plot1 <- renderPlot({
            req(state$plots)
            state$plots[[1]]
        })

        output$plot2 <- renderPlot({
            req(state$plots)
            state$plots[[2]]
        })

        output$plot3 <- renderPlot({
            req(state$plots, state$model)
            state$plots[[3]]
        })

        output$plot4 <- renderPlot({
            req(state$plots, state$model)
            state$plots[[4]]
        })

        # -----------------------------------------------------
        # PREDICTION OUTPUT
        # -----------------------------------------------------
        output$pred <- renderText({

            req(state$pred)

            sprintf(
                "P(goal) = %.3f",
                state$pred
            )
        })
    })
}
