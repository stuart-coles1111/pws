chapter4_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("Response scoring explorer"),

        p("Explore how a guess (G) and uncertainty (S) determine a score when the true value is Θ."),

        numericInput(ns("G"), "Guess (G)", value = 0),

        numericInput(ns("S"), "Uncertainty (S)", value = 1, min = 0.01),

        numericInput(ns("Theta"), "True value (Θ)", value = 1),

        checkboxInput(ns("lines"), "Show true value and score", value = TRUE),

        checkboxInput(ns("final_only"), "Show only final score plot", value = FALSE)
    )

    overview_panel <- card(

        card_header("What this chapter explores"),

        tags$p(
            "This chapter introduces how probabilistic scores arise from a normal model centred on a guess."
        ),

        tags$ul(
            tags$li("A guess (G) defines the centre of belief"),
            tags$li("Uncertainty (S) defines interval width (not SD)"),
            tags$li("True value (Θ) evaluates the prediction"),
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
            tags$li("Uncertainty S is converted to standard deviation internally."),
            tags$li("Sharper predictions are more strongly penalised."),
            tags$li("Scores are log densities of a Gaussian model."),
            tags$li("Performance depends on distance relative to uncertainty.")
        )
    )

    # =========================================================
    # ACTIVITY PANEL
    # =========================================================


    activity_panel <- div(

        card(

            card_header("Interactive Activity"),

            p(
                "Launch the companion activity for this chapter."
            ),

            actionButton(
                ns("launch_activity"),
                "Launch Activity",
                class = "btn-success"
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
        learn = learn_panel,
        activity = activity_panel
    )
}

chapter4_server <- function(id){

    moduleServer(id, function(input, output, session){

        observeEvent(input$launch_activity, {

            url <- pws:::launch_activity_from_lab(4)

            shinyjs::runjs(
                sprintf(
                    "window.open('%s', '_blank');",
                    url
                )
            )
        })

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
