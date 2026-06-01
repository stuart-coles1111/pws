library(shiny)
library(ggplot2)
library(DT)

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

        # -----------------------------------------------------
        # MODE SELECTOR
        # -----------------------------------------------------

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

        # -----------------------------------------------------
        # GLOBAL SIMULATION CONTROLS
        # -----------------------------------------------------

        numericInput(ns("sample_size"), "Sample size (n)", 1000, min = 10),
        numericInput(ns("m"), "Number of simulations", 1000, min = 100),

        hr(),

        # -----------------------------------------------------
        # REGRESSION OPTIONS ONLY
        # -----------------------------------------------------

        conditionalPanel(
            condition = sprintf("input['%s'] == 'Regression prediction'", ns("mode")),

            numericInput(ns("x_value"), "Points in first half", 18),
            numericInput(ns("level"), "Confidence level", 0.95, min = 0.5, max = 0.99)
        )
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

        tags$pre(
            textOutput(ns("code"))
        )
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

        card(
            card_header("Simulation / inference table"),
            DTOutput(ns("table"))
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
    # BUILD PAGE
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
# Chapter 5 SERVER
# =========================================================

chapter5_server <- function(id){

    moduleServer(id, function(input, output, session){

        # =====================================================
        # SAFE PACKAGE DATA ACCESS
        # =====================================================

        # Always explicitly namespace package data
        goal_diff_data <- subset(
            pws::PL_goals,
            season == "2020-2021"
        )$goal_diff

        regression_model <- lm(
            points_half2 ~ points_half1,
            data = pws::PL_points
        )

        # =====================================================
        # SAMPLING VARIABILITY
        # =====================================================

        estimates <- reactive({

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
        })

        # =====================================================
        # BOOTSTRAP
        # =====================================================

        bootstrap_estimates <- reactive({

            replicate(input$m, {

                sample(
                    goal_diff_data,
                    input$sample_size,
                    replace = TRUE
                ) |> mean()

            })

        })

        # =====================================================
        # REGRESSION PREDICTION
        # =====================================================

        prediction <- reactive({

            predict(
                regression_model,
                newdata = data.frame(points_half1 = input$x_value),
                interval = "confidence",
                level = input$level
            )

        })

        # =====================================================
        # MAIN RESULT
        # =====================================================

        result <- reactive({

            switch(
                input$mode,

                "Sampling variability" = mean(estimates()),

                "Bootstrap standard error" = sd(bootstrap_estimates()),

                "Regression prediction" = prediction()[, "fit"]
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
        # RESULT OUTPUT
        # =====================================================

        output$result <- renderText({
            round(result(), 3)
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

        # =====================================================
        # TABLE
        # =====================================================

        output$table <- renderDT({

            if(input$mode == "Sampling variability"){

                data.frame(
                    estimate = round(estimates(), 4)
                )

            } else if(input$mode == "Bootstrap standard error"){

                data.frame(
                    bootstrap_mean = round(bootstrap_estimates(), 4)
                )

            } else {

                as.data.frame(
                    prediction()
                )

            }

        })

    })
}
