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

        selectInput(ns("body"), "Body part", choices = c("Head", "Foot")),

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

    results_panel <- tagList(

        card(
            card_header("Model parameters"),
            tableOutput(ns("model"))
        ),

        br(),

        card(
            card_header("xG plot"),

            h4("Observed Values", style = "text-align:center;"),

            fluidRow(
                column(6, plotOutput(ns("plot1"), height = 300)),
                column(6, plotOutput(ns("plot2"), height = 300))
            ),

            br(),

            h4("Fitted Model Heatmap", style = "text-align:center;"),

            fluidRow(
                column(6, plotOutput(ns("plot3"), height = 300)),
                column(6, plotOutput(ns("plot4"), height = 300))
            )
        ),

        br(),

        card(
            card_header("Prediction"),
            h3(textOutput(ns("pred")))
        )
    )

    chapter_page_ui(
        id = id,
        title = "⚖ Chapter 3: Expectation",
        sidebar = sidebar_controls,
        overview = NULL,
        code = NULL,
        results = results_panel,
        learn = NULL
    )
}

chapter3_server <- function(id){

    moduleServer(id, function(input, output, session){

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

            set.seed(input$seed)

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

            sprintf("P(goal) = %.3f", state$pred)
        })
    })
}
