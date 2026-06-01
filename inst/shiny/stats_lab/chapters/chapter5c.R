library(shiny)
library(ggplot2)

# =========================================================
# UI
# =========================================================

chapter5_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("Statistics Lab"),

        selectInput(
            ns("mode"),
            "Experiment",
            choices = c(
                "Inference",
                "Regression prediction"
            )
        ),

        hr(),

        # =====================================================
        # INFERENCE CONTROLS
        # =====================================================

        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'Inference'",
                ns("mode")
            ),

            sliderInput(
                ns("true_p"),
                "True probability of a six",
                min = 0.01,
                max = 0.50,
                value = 1/6,
                step = 0.01
            ),

            numericInput(
                ns("sample_size"),
                "Number of rolls",
                value = 100,
                min = 10
            ),

            numericInput(
                ns("m"),
                "Bootstrap samples",
                value = 1000,
                min = 100
            ),

            selectInput(
                ns("bootstrap_method"),
                "Inference method",
                choices = c(
                    "Known true probability",
                    "Estimated probability",
                    "Resample observed data"
                )
            ),

            sliderInput(
                ns("conf_level"),
                "Confidence level",
                min = 0.80,
                max = 0.99,
                value = 0.95,
                step = 0.01
            ),

            checkboxInput(
                ns("show_truth"),
                "Show true probability",
                FALSE
            )
        ),

        # =====================================================
        # REGRESSION CONTROLS
        # =====================================================

        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'Regression prediction'",
                ns("mode")
            ),

            numericInput(
                ns("x_value"),
                "Points in first half",
                value = 18
            ),

            sliderInput(
                ns("level"),
                "Confidence level",
                min = 0.80,
                max = 0.99,
                value = 0.95,
                step = 0.01
            )
        ),

        hr(),

        actionButton(
            ns("run"),
            "Repeat experiment",
            icon = icon("refresh")
        )
    )

    overview_panel <- card(

        card_header("What this chapter explores"),

        tags$ul(
            tags$li("Samples produce estimates."),
            tags$li("Estimates are uncertain."),
            tags$li("Bootstrap methods estimate uncertainty."),
            tags$li("Regression predicts outcomes with confidence intervals.")
        )
    )

    code_panel <- card(
        card_header("R code"),
        tags$pre(textOutput(ns("code")))
    )

    results_panel <- tagList(

        card(
            card_header("Results"),
            verbatimTextOutput(ns("result"))
        ),

        br(),

        card(
            card_header("Sampling distribution"),
            plotOutput(ns("plot"), height = 400)
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

    learn_panel <- card(

        card_header("Key ideas"),

        tags$ul(
            tags$li("Different samples give different estimates."),
            tags$li("Bootstrap simulates repeated sampling."),
            tags$li("Standard errors measure uncertainty."),
            tags$li("Confidence intervals summarise plausible values."),
            tags$li("Regression predicts with uncertainty.")
        )
    )

    chapter_page_ui(
        id = id,
        title = "📊 Chapter 5: Statistics",
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
        # REGRESSION MODEL
        # =====================================================

        regression_model <- lm(
            points_half2 ~ points_half1,
            data = pws::PL_points
        )

        # =====================================================
        # INFERENCE SAMPLE
        # =====================================================

        sample_result <- eventReactive(input$run, {

            x <- rbinom(
                1,
                size = input$sample_size,
                prob = input$true_p
            )

            list(
                successes = x,
                p_hat = x / input$sample_size
            )

        }, ignoreInit = FALSE)

        # =====================================================
        # BOOTSTRAP DISTRIBUTION
        # =====================================================

        bootstrap_dist <- reactive({

            req(input$mode == "Inference")

            x <- sample_result()$successes
            n <- input$sample_size
            p_hat <- sample_result()$p_hat
            m <- input$m

            if(input$bootstrap_method ==
               "Known true probability"){

                replicate(
                    m,
                    rbinom(
                        1,
                        size = n,
                        prob = input$true_p
                    ) / n
                )

            } else if(input$bootstrap_method ==
                      "Estimated probability"){

                replicate(
                    m,
                    rbinom(
                        1,
                        size = n,
                        prob = p_hat
                    ) / n
                )

            } else {

                observed_data <- c(
                    rep(1, x),
                    rep(0, n - x)
                )

                replicate(
                    m,
                    mean(
                        sample(
                            observed_data,
                            size = n,
                            replace = TRUE
                        )
                    )
                )
            }
        })

        # =====================================================
        # CI
        # =====================================================

        inference_summary <- reactive({

            req(input$mode == "Inference")

            p_hat <- sample_result()$p_hat

            se <- sd(bootstrap_dist())

            z <- qnorm(
                1 - (1 - input$conf_level)/2
            )

            ci <- c(
                p_hat - z * se,
                p_hat + z * se
            )

            ci <- pmax(ci, 0)
            ci <- pmin(ci, 1)

            list(
                p_hat = p_hat,
                se = se,
                ci = ci
            )
        })

        # =====================================================
        # REGRESSION PREDICTION
        # =====================================================

        prediction <- eventReactive(input$run, {

            predict(
                regression_model,
                newdata = data.frame(
                    points_half1 = input$x_value
                ),
                interval = "confidence",
                level = input$level
            )

        }, ignoreInit = FALSE)

        # =====================================================
        # RESULTS
        # =====================================================

        output$result <- renderText({

            if(input$mode == "Inference"){

                s <- inference_summary()

                paste0(
                    "Observed sixes: ",
                    sample_result()$successes,
                    "\n\nEstimated probability: ",
                    round(s$p_hat, 3),
                    "\n\nBootstrap SE: ",
                    round(s$se, 4),
                    "\n\n",
                    round(100 * input$conf_level),
                    "% confidence interval:\n(",
                    round(s$ci[1], 3),
                    ", ",
                    round(s$ci[2], 3),
                    ")"
                )

            } else {

                paste(
                    "Predicted second-half points:",
                    round(prediction()[,"fit"],2)
                )
            }
        })

        # =====================================================
        # CODE PANEL
        # =====================================================

        output$code <- renderText({

            if(input$mode == "Inference"){

                switch(

                    input$bootstrap_method,

                    "Known true probability" =
                        "replicate(m, rbinom(1, n, p_true)/n)",

                    "Estimated probability" =
                        "replicate(m, rbinom(1, n, p_hat)/n)",

                    "Resample observed data" =
                        "replicate(m, mean(sample(observed_data, replace=TRUE)))"
                )

            } else {

                "lm(...) + predict(..., interval='confidence')"
            }
        })

        # =====================================================
        # REGRESSION DETAILS
        # =====================================================

        output$regression_details <- renderText({

            req(input$mode == "Regression prediction")

            pred <- prediction()

            paste0(
                "Predicted value: ",
                round(pred[, "fit"], 2),
                "\n\n",
                round(100 * input$level),
                "% confidence interval:\n",
                round(pred[, "lwr"], 2),
                " to ",
                round(pred[, "upr"], 2)
            )
        })

        # =====================================================
        # PLOT
        # =====================================================

        output$plot <- renderPlot({

            if(input$mode == "Inference"){

                s <- inference_summary()

                p <- ggplot(
                    data.frame(x = bootstrap_dist()),
                    aes(x)
                ) +
                    geom_histogram(
                        bins = 30,
                        fill = "#7B9ACC",
                        colour = "white"
                    ) +
                    geom_vline(
                        xintercept = s$p_hat,
                        linewidth = 1.2,
                        linetype = 2
                    ) +
                    geom_vline(
                        xintercept = s$ci,
                        colour = "red",
                        linewidth = 1.2
                    ) +
                    labs(
                        x = "Estimated probability",
                        y = "Frequency"
                    ) +
                    theme_minimal(base_size = 14)

                if(input$show_truth){

                    p <- p +
                        geom_vline(
                            xintercept = input$true_p,
                            colour = "darkgreen",
                            linewidth = 1.3
                        )
                }

                p

            } else {

                ggplot(
                    pws::PL_points,
                    aes(points_half1, points_half2)
                ) +
                    geom_point() +
                    geom_smooth(
                        method = "lm",
                        se = TRUE
                    ) +
                    geom_vline(
                        xintercept = input$x_value,
                        linetype = 2
                    ) +
                    theme_minimal(base_size = 14) +
                    labs(
                        x = "First-half points",
                        y = "Second-half points"
                    )
            }
        })

    })
}
