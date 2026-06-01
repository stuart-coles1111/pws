library(shiny)
library(ggplot2)

# =========================================================
# Chapter 5 UI
# =========================================================

chapter5_ui <- function(id){

    ns <- NS(id)

    # =========================================================
    # SIDEBAR
    # =========================================================

    sidebar_controls <- sidebar(

        h4("Sampling & inference lab"),
        p("Explore how estimates vary across repeated samples."),

        # MODE
        selectInput(
            ns("mode"),
            "Experiment",
            choices = c(
                "Sampling variability",
                "Bootstrap standard error",
                "Regression prediction"
            )
        ),

        hr(),

        # ONLY SHOW FOR SIMULATION-BASED EXPERIMENTS
        conditionalPanel(
            condition = sprintf(
                "input['%s'] != 'Regression prediction'",
                ns("mode")
            ),

            numericInput(ns("sample_size"), "Sample size (n)", 1000, min = 10),
            numericInput(ns("m"), "Number of simulations", 1000, min = 100)
        ),

        hr(),

        # REGRESSION OPTIONS ONLY
        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'Regression prediction'",
                ns("mode")
            ),

            numericInput(ns("x_value"), "Points in first half", 18),
            numericInput(ns("level"), "Confidence level", 0.95, min = 0.5, max = 0.99)
        ),

        hr(),

        actionButton(ns("run"), "Repeat experiment", icon = icon("refresh"))
    )

    # =========================================================
    # OVERVIEW PANEL
    # =========================================================

    overview_panel <- card(
        card_header("What this chapter explores"),

        tags$p("This chapter introduces sampling variability and statistical inference."),

        tags$ul(
            tags$li("Repeated samples produce different estimates"),
            tags$li("Standard error measures typical variation"),
            tags$li("Bootstrap approximates uncertainty from data"),
            tags$li("Regression provides predictions with uncertainty intervals")
        )
    )

    # =========================================================
    # CODE PANEL
    # =========================================================

    code_panel <- card(
        card_header("R code"),
        tags$pre(textOutput(ns("code")))
    )

    # =========================================================
    # RESULTS PANEL
    # =========================================================

    results_panel <- tagList(

        card(
            card_header("Result summary"),
            h2(textOutput(ns("result")))
        ),

        br(),

        card(
            card_header("Sampling distribution"),
            plotOutput(ns("plot"), height = 350)
        ),

        br(),

        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'Regression prediction'",
                ns("mode")
            ),

            card(
                card_header("Prediction details"),
                verbatimTextOutput(ns("regression_details"))
            )
        )
    )

    # =========================================================
    # LEARN PANEL
    # =========================================================

    learn_panel <- card(
        card_header("Key ideas"),

        tags$ul(
            tags$li("Estimates vary because samples vary."),
            tags$li("Standard error quantifies this variation."),
            tags$li("Bootstrap approximates sampling variability from data."),
            tags$li("Regression predictions include uncertainty, not certainty.")
        )
    )

    # =========================================================
    # PAGE
    # =========================================================

    chapter_page_ui(
        id = id,
        title = "📊 Chapter 5: Sampling & Inference",
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

chapter5_server <- function(id){

    moduleServer(id, function(input, output, session){

        # =====================================================
        # DATA
        # =====================================================

        goal_diff_data <- subset(
            pws::PL_goals,
            season == "2020-2021"
        )$goal_diff

        regression_model <- lm(
            points_half2 ~ points_half1,
            data = pws::PL_points
        )

        # =====================================================
        # SAMPLING VARIABILITY (EVENT-BASED)
        # =====================================================

        estimates <- eventReactive(input$run, {

            dice_probs <- c(rep((1 - 0.5/6)/5, 5), 0.5/6)

            replicate(input$m, {

                x <- sample(
                    1:6,
                    size = input$sample_size,
                    replace = TRUE,
                    prob = dice_probs
                )

                mean(x == 6)
            })

        }, ignoreInit = FALSE)

        # =====================================================
        # BOOTSTRAP (EVENT-BASED)
        # =====================================================

        bootstrap_estimates <- eventReactive(input$run, {

            replicate(input$m, {

                sample(
                    goal_diff_data,
                    input$sample_size,
                    replace = TRUE
                ) |> mean()

            })

        }, ignoreInit = FALSE)

        # =====================================================
        # REGRESSION PREDICTION (EVENT-BASED)
        # =====================================================

        prediction <- eventReactive(input$run, {

            predict(
                regression_model,
                newdata = data.frame(points_half1 = input$x_value),
                interval = "confidence",
                level = input$level
            )

        }, ignoreInit = FALSE)

        # =====================================================
        # RESULT SUMMARY
        # =====================================================

        output$result <- renderText({

            switch(

                input$mode,

                "Sampling variability" =
                    paste(
                        "Average estimated probability:",
                        round(mean(estimates()), 3)
                    ),

                "Bootstrap standard error" =
                    paste(
                        "Bootstrap standard error:",
                        round(sd(bootstrap_estimates()), 3)
                    ),

                "Regression prediction" =
                    paste(
                        "Predicted second-half points:",
                        round(prediction()[, "fit"], 2)
                    )
            )
        })

        # =====================================================
        # CODE DISPLAY
        # =====================================================

        output$code <- renderText({

            switch(
                input$mode,

                "Sampling variability" =
                    "replicate(m, sample(...)) → mean(x == 6)",

                "Bootstrap standard error" =
                    "replicate(m, sample(data, replace = TRUE) → mean())",

                "Regression prediction" =
                    "lm(...) + predict(..., interval = 'confidence')"
            )
        })

        # =====================================================
        # REGRESSION DETAILS
        # =====================================================

        output$regression_details <- renderText({

            req(input$mode == "Regression prediction")

            pred <- prediction()

            paste0(
                "Predicted value: ", round(pred[, "fit"], 2), "\n\n",
                round(100 * input$level), "% confidence interval:\n",
                round(pred[, "lwr"], 2), " to ", round(pred[, "upr"], 2)
            )
        })

        # =====================================================
        # PLOT
        # =====================================================

        output$plot <- renderPlot({

            if(input$mode == "Sampling variability"){

                ggplot(data.frame(x = estimates()), aes(x)) +
                    geom_histogram(fill = "#7B9ACC", bins = 30) +
                    theme_minimal(base_size = 14) +
                    labs(x = "Estimate", y = "Frequency")

            } else if(input$mode == "Bootstrap standard error"){

                ggplot(data.frame(x = bootstrap_estimates()), aes(x)) +
                    geom_histogram(fill = "#CDB4DB", bins = 30) +
                    theme_minimal(base_size = 14) +
                    labs(x = "Bootstrap mean", y = "Frequency")

            } else {

                ggplot(pws::PL_points,
                       aes(points_half1, points_half2)) +
                    geom_point() +
                    geom_smooth(method = "lm", se = TRUE) +
                    geom_vline(xintercept = input$x_value, linetype = 2) +
                    theme_minimal(base_size = 14) +
                    labs(x = "First half points", y = "Second half points")

            }
        })

    })
}
