chapter4_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("A Quiz Score Explorer"),

        p("Explore how a guess (G) and uncertainty (S) determine a score when the true value is Θ."),

        sliderInput(
            ns("Theta"),
            "True value (Θ)",
            min = -10, max = 10, value = 1, step = 0.1
        ),

        sliderInput(
            ns("G"),
            "Guess (G)",
            min = -10, max = 10, value = 0, step = 0.1
        ),

        sliderInput(
            ns("S"),
            "Uncertainty (S)",
            min = 0.1, max = 10, value = 1, step = 0.1
        ),

        checkboxInput(ns("lines"), "Show true value and score", value = TRUE),

        checkboxInput(ns("final_only"), "Show only final score plot", value = FALSE)
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
                    "📉 Understanding Uncertainty",
                    style = "
                    font-size: 1.4rem;
                    font-weight: 700;
                    color: #2c3e50;
                "
                )
            ),

            p(
                strong("Main idea: "),
                "A good prediction should communicate both what you expect
             to happen and how certain you are about that expectation."
            ),

            hr(),

            h5("The problem"),

            p(
                "Suppose you are asked to predict an unknown quantity.
             Giving a single number is often not enough because
             uncertainty is part of every prediction."
            ),

            p(
                "A useful prediction therefore combines two things:"
            ),

            tags$ul(
                tags$li(
                    strong("A best guess"),
                    " about the true value"
                ),
                tags$li(
                    strong("A measure of uncertainty"),
                    " describing how confident you are"
                )
            ),

            hr(),

            h5("What happens in this chapter?"),

            tags$div(
                style = "margin-left: 10px;",

                p("① Choose a guess for the true value."),

                p("② Specify how uncertain you are."),

                p("③ Reveal the true value."),

                p("④ Evaluate the prediction using a scoring rule.")
            ),

            hr(),

            h5("Your job"),

            tags$ul(
                tags$li("Explore different combinations of guesses and uncertainty"),
                tags$li("Investigate how scores change when predictions are accurate or inaccurate"),
                tags$li("Compare cautious predictions with confident predictions"),
                tags$li("Discover what kinds of predictions receive the best scores")
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
                        "What happens when the guess is correct but uncertainty is very small?"
                    ),
                    tags$li(
                        "What happens when the guess is wrong but uncertainty is large?"
                    ),
                    tags$li(
                        "Can being overconfident hurt your score?"
                    ),
                    tags$li(
                        "What level of uncertainty seems appropriate for different situations?"
                    )
                )
            )
        )
    )

    code_panel <- card(

        card_header("Generated R code"),

        tags$pre(
            textOutput(ns("code"))
        )
    )

    results_panel <- tagList(

        card(
            card_header("Response analysis plot"),
            plotOutput(ns("plot"), height = 450)
        ),

        card(
            card_header("Score"),
            div(
                style = "font-size: 2rem; font-weight: 700;",
                textOutput(ns("score"))
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

            h5("1. Every prediction contains uncertainty"),

            p(
                "Even when we provide a best estimate, we are rarely
             completely certain that it is correct."
            ),

            hr(),

            h5("2. Good predictions balance accuracy and confidence"),

            p(
                "A useful prediction should be close to the truth while
             also expressing an appropriate level of uncertainty."
            ),

            hr(),

            h5("3. Overconfidence can be costly"),

            p(
                "Very narrow predictions may perform well when they are correct,
             but they are heavily penalised when they are wrong."
            ),

            hr(),

            h5("4. Uncertainty is information"),

            p(
                "Reporting uncertainty is not a weakness.
             It provides valuable information about how reliable
             a prediction is likely to be."
            ),

            hr(),

            h5("5. Scoring rules reward honest forecasting"),

            p(
                "Well-designed scoring systems encourage people to report
             both their best estimate and their genuine uncertainty."
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
                    strong("The goal is not to appear certain."),
                    br(),
                    "The goal is to describe uncertainty as accurately as possible.
                 Good statistical predictions combine a best guess with an honest
                 assessment of how confident we should be."
                )
            )
        )
    )


    chapter_page_ui(
        id = id,
        title = "📉 Chapter 4: Uncertainty",
        sidebar = sidebar_controls,
        overview = overview_panel,
        code = code_panel,
        results = results_panel,
        learn = learn_panel
    )
}

chapter4_server <- function(id){

    moduleServer(id, function(input, output, session){


        score_obj <- reactive({

            activity4_response_score(
                G = input$G,
                S = input$S,
                Theta = input$Theta,
                alpha = 0.95,
                dp = 3
            )
        })

        output$score <- renderText({
            score_obj()$scores
        })

        output$plot <- renderPlot({

            pws::activity4_response_analysis(
                G = input$G,
                S = input$S,
                Theta = input$Theta,
                dp = 3,
                lines = input$lines,
                final_score_only = input$final_only
            )
        })

        output$code <- renderText({

            paste0(
                "activity4_response_score(",
                "G = ", input$G,
                ", S = ", input$S,
                ", Theta = ", input$Theta,
                ", alpha = 0.95, ",
                "dp = 3",
                ")"
            )
        })

    })
}
