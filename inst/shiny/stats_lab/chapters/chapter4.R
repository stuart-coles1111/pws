chapter4_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("Quiz Score Explorer"),

        radioButtons(
            ns("view_mode"),
            "Display",
            choices = c(
                "Single score" = "single",
                "Score as error varies" = "error",
                "Score as uncertainty varies" = "uncertainty"
            ),
            selected = "single"
        ),

        hr(),

        # =====================================================
        # SINGLE PREDICTION
        # =====================================================

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'single'",
                ns("view_mode")
            ),

            sliderInput(
                ns("Theta"),
                "True value (T)",
                min = -10,
                max = 10,
                value = 1,
                step = 0.1
            ),

            sliderInput(
                ns("G"),
                "Guess (G)",
                min = -10,
                max = 10,
                value = 0,
                step = 0.1
            ),

            sliderInput(
                ns("S"),
                "Uncertainty (S)",
                min = 0.1,
                max = 10,
                value = 1,
                step = 0.1
            ),

            checkboxInput(
                ns("lines"),
                "Show true value and score",
                value = TRUE
            )
        ),

        # =====================================================
        # SCORE VS ERROR
        # =====================================================

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'error'",
                ns("view_mode")
            ),

            sliderInput(
                ns("fixed_S"),
                "Fixed uncertainty (S)",
                min = 0.1,
                max = 10,
                value = 1,
                step = 0.1
            ),

            checkboxInput(
                ns("show_error_zero"),
                "Show optimal response (δ = 0)",
                value = FALSE
            )
        ),

        # =====================================================
        # SCORE VS UNCERTAINTY
        # =====================================================

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'uncertainty'",
                ns("view_mode")
            ),

            sliderInput(
                ns("fixed_error"),
                "Guess error (Θ − G)",
                min = -50,
                max = 50,
                value = 2,
                step = 0.5
            ),

            checkboxInput(
                ns("show_optimum"),
                "Show optimal uncertainty",
                value = FALSE
            )
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
                "This module provides tools to explore the quiz score function used in Chapter 4 of Playing With Statistics."
            ),

            hr(),

            h5("Background"),

            p(
                "Activity 4 of Playing With Statistics presents a quiz that requires participants to provide not just their best guess (G)
                to an answer, but also a measure of uncertainty (S) that defines how confident they are in their guess."
            ),

            p(
                "This is defined so that if the true answer is T, P(G - S <= T <= G + S) = 95%")
            ),

        p(
            "In other words, particpants choose S such that they are 95% confident that the true answer is less than a distance S from their guess G"
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

    results_panel <- layout_columns(

        col_widths = c(9, 3),

        card(
            card_header("Response analysis plot"),
            plotOutput(ns("plot"), height = 450)
        ),

        uiOutput(ns("score_panel"))

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
            round(score_obj()$scores, 2)
        })

        output$plot <- renderPlot({

            if (input$view_mode == "single") {

                pws::activity4_response_analysis(
                    G = input$G,
                    S = input$S,
                    Theta = input$Theta,
                    dp = 3,
                    lines = input$lines,
                    final_score_only = FALSE
                )

            } else if (input$view_mode == "error") {

                error_grid <- seq(-10, 10, length.out = 500)

                score_vals <- sapply(
                    error_grid,
                    function(e)
                        activity4_response_score(
                            G = e,
                            S = input$fixed_S,
                            Theta = 0
                        )$scores
                )

                p <- ggplot(
                    data.frame(
                        error = error_grid,
                        score = score_vals
                    ),
                    aes(error, score)
                ) +
                    geom_line(
                        colour = "#7B9ACC",
                        linewidth = 1.4
                    ) +
                    labs(
                        x = expression(delta == T - G),
                        y = "Score"
                    ) +
                    theme_minimal(base_size = 16) +
                    theme(
                        axis.title = element_text(size = 16),
                        axis.text = element_text(size = 14)
                    )

                if (input$show_error_zero) {

                    p <- p +
                        geom_vline(
                            xintercept = 0,
                            linetype = "dashed",
                            colour = "#E76F51",
                            linewidth = 1
                        )
                }

                p

            } else {

                error <- abs(input$fixed_error)

                s_opt <- abs(input$fixed_error) * qnorm(0.975)

                s_min <- max(0.1, s_opt/5)
                s_max <- max(20, s_opt*5)

                s_grid <- seq(
                    s_min,
                    s_max,
                    length.out = 1000
                )

                score_vals <- sapply(
                    s_grid,
                    function(s)
                        activity4_response_score(
                            G = input$fixed_error,
                            S = s,
                            Theta = 0
                        )$scores
                )

                p <- ggplot(
                    data.frame(
                        S = s_grid,
                        score = score_vals
                    ),
                    aes(S, score)
                ) +
                    geom_line(
                        colour = "#E76F51",
                        linewidth = 1.5
                    ) +
                    labs(
                        x = "Uncertainty (S)",
                        y = "Score"
                    ) +
                    theme_minimal(base_size = 16) +
                    theme(
                        axis.title = element_text(size = 18),
                        axis.text = element_text(size = 15)
                    )

                if (input$show_optimum) {

                    p <- p +
                        geom_vline(
                            xintercept = s_opt,
                            linetype = "dashed",
                            colour = "#7B9ACC",
                            linewidth = 1
                        ) +
                        annotate(
                            "text",
                            x = s_opt,
                            y = max(score_vals),
                            label = "Optimal S",
                            hjust = -0.4,
                            size = 5
                        )
                }

                p
            }
        })

        output$code <- renderText({

            if (input$view_mode == "single") {

                paste0(
                    "pws::activity4_response_analysis(\n",
                    "    G = ", input$G, ",\n",
                    "    S = ", input$S, ",\n",
                    "    Theta = ", input$Theta, ",\n",
                    "    dp = 3,\n",
                    "    lines = ", input$lines, "\n",
                    ")"
                )

            } else if (input$view_mode == "error") {

                paste0(
                    "error_grid <- seq(-10, 10, length.out = 500)\n\n",

                    "score_vals <- sapply(\n",
                    "    error_grid,\n",
                    "    function(e)\n",
                    "        activity4_response_score(\n",
                    "            G = e,\n",
                    "            S = ", input$fixed_S, ",\n",
                    "            Theta = 0\n",
                    "        )$scores\n",
                    ")\n\n",

                    "ggplot(\n",
                    "    data.frame(\n",
                    "        error = error_grid,\n",
                    "        score = score_vals\n",
                    "    ),\n",
                    "    aes(error, score)\n",
                    ") +\n",
                    "    geom_line()\n"
                )

            } else {

                paste0(
                    "s_opt <- abs(error) * qnorm(0.975)\n\n",

                    "s_grid <- seq(\n",
                    "    max(0.1, s_opt/5),\n",
                    "    max(20, s_opt*5),\n",
                    "    length.out = 1000\n",
                    ")\n\n",

                    "score_vals <- sapply(\n",
                    "    s_grid,\n",
                    "    function(s)\n",
                    "        activity4_response_score(\n",
                    "            G = ", input$fixed_error, ",\n",
                    "            S = s,\n",
                    "            Theta = 0\n",
                    "        )$scores\n",
                    ")\n\n",

                    "ggplot(\n",
                    "    data.frame(\n",
                    "        S = s_grid,\n",
                    "        score = score_vals\n",
                    "    ),\n",
                    "    aes(S, score)\n",
                    ") +\n",
                    "    geom_line()\n"
                )
            }
        })
        output$score_panel <- renderUI({

            if (input$view_mode == "single") {

                card(
                    card_header("Score"),

                    div(
                        style = "
                    font-size: 1.5rem;
                    font-weight: 700;
                    text-align: center;
                    padding-top: 150px;
                ",
                        textOutput(session$ns("score"))
                    )
                )

            } else if (input$view_mode == "error") {

                card(
                    card_header("Interpretation"),

                    p(
                        "For a fixed uncertainty S, the score is maximised when ",
                        strong("δ = Θ − G = 0"),
                        ", meaning the prediction is exactly correct."
                    )
                )

            } else {

                card(
                    card_header("Optimal uncertainty"),

                    div(
                        style = "
                    font-size: 1.5rem;
                    font-weight: 700;
                    text-align: center;
                    padding-top: 100px;
                ",
                        paste0(
                            "Optimal S = ",
                            round(abs(input$fixed_error) * qnorm(0.975), 2)
                        )
                    )
                )
            }

        })
        })
}
