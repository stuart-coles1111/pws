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
            max = 999,
            step = 1
        ),

        actionButton(
            ns("randomise"),
            "Randomise model parameters"
        ),

        numericInput(
            ns("n_data"),
            "Number of shots",
            value = 5000,
            min = 100
        ),

        actionButton(
            ns("run"),
            "Generate data"
        ),

        hr(),

        actionButton(
            ns("fit"),
            "Fit model"
        ),

        hr(),

        h4("Prediction tool"),

        numericInput(ns("x"), "x coordinate", 5),
        numericInput(ns("y"), "y coordinate", 10),

        selectInput(
            ns("body"),
            "Body part",
            choices = c("Head", "Foot")
        ),

        actionButton(
            ns("predict"),
            "Predict"
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
                    "⚖ Understanding Prediction Models",
                    style = "
                    font-size: 1.4rem;
                    font-weight: 700;
                    color: #2c3e50;
                "
                )
            ),

            p(
                strong("Main idea: "),
                "Statistical models can learn patterns from historical data
             and use those patterns to estimate the probability of future outcomes."
            ),

            hr(),

            h5("The problem"),

            p(
                "Not all football shots are equally likely to result in a goal.
             A shot taken close to goal is usually more dangerous than one
             taken from distance, and other factors may also influence success."
            ),

            p(
                "Expected goals (xG) models attempt to quantify this by assigning
             a probability to every shot."
            ),

            hr(),

            h5("What happens in this chapter?"),

            tags$div(
                style = "margin-left: 10px;",

                p("① Generate a dataset of football shots."),

                p("② Each shot has characteristics such as distance, angle and body part."),

                p("③ Fit a statistical model to learn how those characteristics influence scoring."),

                p("④ Use the fitted model to predict the probability of a goal.")
            ),

            hr(),

            h5("Your job"),

            tags$ul(
                tags$li("Generate different datasets"),
                tags$li("Fit the xG model"),
                tags$li("Compare the true relationships with the estimated relationships"),
                tags$li("Use the model to make predictions for new shots")
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
                        "Which shot characteristics appear most important?"
                    ),
                    tags$li(
                        "How close are the estimated effects to the true effects?"
                    ),
                    tags$li(
                        "Does increasing the amount of data improve estimation?"
                    ),
                    tags$li(
                        "How does changing a shot's location affect its predicted xG?"
                    )
                )
            )
        )
    )

    code_panel <- card(
        card_header("R code"),
        tags$pre(textOutput(ns("code")))
    )

    results_panel <- tagList(

        card(
            card_header("Model parameters"),
            tableOutput(ns("model"))
        ),

        br(),

        card(
            card_header("xG plot"),

            fluidRow(
                column(
                    6,
                    plotOutput(ns("plot1"), height = 300)
                ),
                column(
                    6,
                    plotOutput(ns("plot2"), height = 300)
                )
            ),

            fluidRow(
                column(
                    6,
                    plotOutput(ns("plot3"), height = 300)
                ),
                column(
                    6,
                    plotOutput(ns("plot4"), height = 300)
                )
            )
        ),

        br(),

        card(
            card_header("Prediction"),
            h3(textOutput(ns("pred")))
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

            h5("1. Statistical models learn from data"),

            p(
                "Rather than being programmed with exact answers,
             statistical models estimate relationships using observed data."
            ),

            hr(),

            h5("2. Prediction is based on patterns"),

            p(
                "The model identifies how variables such as distance,
             angle and body part are associated with the probability
             of scoring."
            ),

            hr(),

            h5("3. Models estimate, they do not know"),

            p(
                "The fitted model does not recover the true relationships perfectly.
             It uses limited information and therefore produces estimates
             rather than exact values."
            ),

            hr(),

            h5("4. More data generally improves estimation"),

            p(
                "Larger datasets provide more evidence about the underlying
             relationships and typically lead to more stable predictions."
            ),

            hr(),

            h5("5. Predictions are probabilities"),

            p(
                "An xG value is not a prediction that a goal will definitely
             be scored. It is an estimate of how likely a goal is."
            ),

            hr(),

            div(
                style = "
                background-color: #f8f9fa;
                border-left: 5px solid #28a745;
                padding: 12px;
                border-radius: 8px;
            ",

                h5("Key takeaway"),

                p(
                    strong("Statistical models turn data into predictions."),
                    br(),
                    "By learning relationships from historical observations,
                 they allow us to estimate the probabilities of future events,
                 even when individual outcomes remain uncertain."
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

        # -----------------------------------------------------
        # TRUE PARAMETERS
        # -----------------------------------------------------

        true_params <- reactiveVal(
            list(
                intercept = 0.25,
                bodyHead = -0.05,
                distance = -0.10,
                angle_trans = -0.80,
                bodyHead_distance = -0.25
            )
        )

        observeEvent(input$randomise, {

            set.seed(input$seed)

            true_params(
                list(
                    intercept = runif(1, 0.10, 0.50),
                    bodyHead = runif(1, -0.10, -0.025),
                    distance = runif(1, -0.20, -0.05),
                    angle_trans = runif(1, -1.00, -0.50),
                    bodyHead_distance = runif(1, -0.50, 0.00)
                )
            )

        })



        # -----------------------------------------------------
        # DATA GENERATION
        # -----------------------------------------------------

        xG_data <- eventReactive(input$run, {

            pars <- true_params()

            pws::xGsim(
                n_data = input$n_data,
                bodypar = 0.2,
                distpar = 0.2,
                anglepar = 3,
                intercept = pars$intercept,
                dist_coeff = pars$distance,
                dist_body_inter = pars$bodyHead_distance,
                body_coeff = pars$bodyHead,
                angle_coeff = pars$angle_trans,
                seed = input$seed)
        })

        # -----------------------------------------------------
        # MODEL FITTING
        # -----------------------------------------------------

        model <- eventReactive(input$fit, {

            req(xG_data())

            pws::xGfit(xG_data())
        })

        # -----------------------------------------------------
        # PLOTS
        # -----------------------------------------------------

        plots <- reactive({

            req(xG_data())

            pws::xGplot(xG_data())
        })

        # -----------------------------------------------------
        # PREDICTION
        # -----------------------------------------------------

        pred <- eventReactive(input$predict, {

            req(model())

            pws::xGpred(
                model(),
                x = input$x,
                y = input$y,
                body = input$body
            )
        })

        # -----------------------------------------------------
        # CODE
        # -----------------------------------------------------

        output$code <- renderText({

            paste(
                "xG_data <- xGsim(n_data = ...)",
                "model <- xGfit(xG_data)",
                "xGpred(model, x, y, body)",
                sep = "\n\n"
            )
        })

        # -----------------------------------------------------
        # MODEL PARAMETERS
        # -----------------------------------------------------

        output$model <- renderTable({

            pars <- true_params()

            truth <- data.frame(
                "True Parameters" = c(
                    `(Intercept)` = pars$intercept,
                    bodyHead = pars$bodyHead,
                    distance = pars$distance,
                    angle_trans = pars$angle_trans,
                    `bodyHead:distance` = pars$bodyHead_distance
                )
            )

            if (input$fit == 0) {

                return(
                    format(
                        round(truth, 3),
                        nsmall = 3
                    )
                )
            }

            req(model())

            tbl <- model()$summary

            colnames(tbl) <- c(
                "True Parameters",
                "Estimates"
            )

            tbl[, 1] <- truth[, 1]

            format(
                round(tbl, 3),
                nsmall = 3
            )

        }, rownames = TRUE)

        # -----------------------------------------------------
        # FIRST ROW: SHOWN AFTER GENERATE DATA
        # -----------------------------------------------------

        output$plot1 <- renderPlot({

            req(plots())

            plots()[[1]]
        })

        output$plot2 <- renderPlot({

            req(plots())

            plots()[[2]]
        })

        # -----------------------------------------------------
        # SECOND ROW: SHOWN AFTER FIT MODEL
        # -----------------------------------------------------

        output$plot3 <- renderPlot({

            req(model())

            plots()[[3]]
        })

        output$plot4 <- renderPlot({

            req(model())

            plots()[[4]]
        })

        # -----------------------------------------------------
        # PREDICTION
        # -----------------------------------------------------

        output$pred <- renderText({

            req(pred())

            sprintf(
                "P(goal) = %.3f",
                pred()
            )
        })
    })
}
