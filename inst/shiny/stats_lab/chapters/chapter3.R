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

    overview_panel <- card(
        card_header("What this chapter explores"),
        tags$p("This chapter introduces a statistical model for expected goals (xG)."),
        tags$ul(
            tags$li("Shots are simulated using a known data-generating process."),
            tags$li("A logistic regression model is fitted to recover structure."),
            tags$li("We compare true vs estimated relationships."),
            tags$li("We use the model to predict shot success probability.")
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

    learn_panel <- card(
        card_header("Key ideas"),
        tags$ul(
            tags$li("xG is a probability model for scoring a shot."),
            tags$li("Logistic regression links features to scoring probability."),
            tags$li("Distance, angle, and body part all matter."),
            tags$li("We can simulate data where the truth is known.")
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
