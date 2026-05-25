chapter3_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("xG simulation explorer"),

        p("Simulate football shots, fit an xG model, and make predictions."),

        numericInput(
            ns("n_data"),
            "Number of shots",
            value = 5000,
            min = 100
        ),

        actionButton(ns("run"), "Generate data"),

        hr(),

        h4("Prediction tool"),

        numericInput(ns("x"), "x coordinate", 5),
        numericInput(ns("y"), "y coordinate", 10),

        selectInput(ns("body"),
                    "Body part",
                    choices = c("Head", "Foot"))
    )

    # -----------------------------------------------------
    # OVERVIEW
    # -----------------------------------------------------

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

    # -----------------------------------------------------
    # CODE
    # -----------------------------------------------------

    code_panel <- card(

        card_header("R code"),

        tags$pre(textOutput(ns("code")))
    )

    # -----------------------------------------------------
    # RESULTS
    # -----------------------------------------------------

    results_panel <- tagList(

        card(
            card_header("Model summary"),
            tableOutput(ns("model"))
        ),

        br(),

        card(
            card_header("xG plot"),
            plotOutput(ns("plot"), height = 400)
        ),

        br(),

        card(
            card_header("Prediction"),
            h2(textOutput(ns("pred")))
        )
    )

    # -----------------------------------------------------
    # LEARN
    # -----------------------------------------------------

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
        # simulate data (event-driven like Chapter 2)
        # -----------------------------------------------------

        xG_data <- eventReactive(input$run, {

            pws::xGsim(
                n_data = input$n_data,
                seed = 123
            )
        })

        # -----------------------------------------------------
        # fit model
        # -----------------------------------------------------

        model <- reactive({

            req(xG_data())

            pws::xGfit(xG_data())
        })

        # -----------------------------------------------------
        # prediction
        # -----------------------------------------------------

        pred <- reactive({

            req(model())

            pws::xGpred(
                model(),
                x = input$x,
                y = input$y,
                body = input$body
            )
        })

        # -----------------------------------------------------
        # CODE (mirrors computation)
        # -----------------------------------------------------

        output$code <- renderText({

            "xG_data <- xGsim(n_data = ...)\n
model <- xGfit(xG_data)\n
xGpred(model, x, y, body)"
        })

        # -----------------------------------------------------
        # MODEL TABLE
        # -----------------------------------------------------

        output$model <- renderTable({

            req(model())

            round(model()$summary, 3)
        })

        # -----------------------------------------------------
        # PLOT
        # -----------------------------------------------------

        output$plot <- renderPlot({

            req(xG_data())

            pws::xGplot(xG_data())
        })

        # -----------------------------------------------------
        # PREDICTION
        # -----------------------------------------------------

        output$pred <- renderText({

            req(pred())

            round(pred(), 3)
        })
    })
}
