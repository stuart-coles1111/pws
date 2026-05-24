chapter4_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("Response scoring explorer"),

        p("Explore how a guess (G) and uncertainty (S) determine a score when the true value is Θ."),

        numericInput(
            ns("G"),
            "Guess (G)",
            value = 0
        ),

        numericInput(
            ns("S"),
            "Uncertainty (S)",
            value = 1,
            min = 0.01
        ),

        numericInput(
            ns("Theta"),
            "True value (Θ)",
            value = 1
        ),

        numericInput(
            ns("dp"),
            "Decimal places",
            value = 2,
            min = 0,
            max = 10
        ),

        checkboxInput(
            ns("lines"),
            "Show detailed curves",
            value = TRUE
        ),

        checkboxInput(
            ns("final_only"),
            "Show only final score plot",
            value = FALSE
        )
    )

    overview_panel <- card(

        card_header("What this chapter explores"),

        tags$p("This chapter introduces how probabilistic scores arise from a normal model centred on a guess."),

        tags$ul(
            tags$li("A guess (G) defines a centre of belief"),
            tags$li("Uncertainty (S) defines spread"),
            tags$li("True value (Θ) determines score quality"),
            tags$li("Scores are log densities under a normal model")
        )
    )

    code_panel <- card(

        card_header("R code"),

        tags$pre(
            textOutput(ns("code"))
        )
    )

    results_panel <- tagList(

        card(
            card_header("Score"),

            h2(textOutput(ns("score")))
        ),

        br(),

        card(
            card_header("Response analysis plot"),

            plotOutput(ns("plot"), height = 450)
        )
    )

    learn_panel <- card(

        card_header("Key ideas"),

        tags$ul(
            tags$li("A sharper (smaller S) prediction is penalised more when wrong."),
            tags$li("Log density converts probability into additive scores."),
            tags$li("Distance from Θ relative to S determines performance."),
            tags$li("The same error can be good or bad depending on uncertainty.")
        )
    )

    chapter_page_ui(
        id = id,
        title = "📈 Chapter 4: Response Scoring",
        sidebar = sidebar_controls,
        overview = overview_panel,
        code = code_panel,
        results = results_panel,
        learn = learn_panel
    )
}

chapter4_server <- function(id){

    moduleServer(id, function(input, output, session){

        output$score <- renderText({

            res <- pws::activity4_response_analysis(
                G = input$G,
                S = input$S,
                Theta = input$Theta,
                dp = input$dp,
                lines = FALSE,
                final_score_only = TRUE
            )

            # function prints; we just re-run score directly if needed
            "See plot"
        })

        output$plot <- renderPlot({

            pws::activity4_response_analysis(
                G = input$G,
                S = input$S,
                Theta = input$Theta,
                dp = input$dp,
                lines = input$lines,
                final_score_only = input$final_only
            )

        })

        output$code <- renderText({

            paste0(
                "activity4_response_analysis(G = ", input$G,
                ", S = ", input$S,
                ", Theta = ", input$Theta,
                ", dp = ", input$dp,
                ")"
            )
        })

    })
}
