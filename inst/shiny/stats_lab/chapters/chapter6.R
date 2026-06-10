bp <- function(n) {
    p <- 1
    if (n > 1) {
        for (i in 1:n)
            p <- p * (365 - i + 1) / 365
    }
    1 - p
}

# =========================================================
# Chapter 6 UI
# =========================================================

chapter6_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("Choose experiment"),

        selectInput(
            ns("demo"),
            "Experiment",
            choices = c(
                "Birthday Problem",
                "Difference in Proportions",
                "Data Dredging"
            )
        ),

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'Birthday Problem'",
                ns("demo")
            ),

            numericInput(
                ns("p_level"),
                "Probability threshold (p)",
                value = 0.5,
                min = 0.1,
                max = 0.99,
                step = 0.01
            )
        ),

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'Difference in Proportions'",
                ns("demo")
            ),

            numericInput(ns("count1"), "Successes group 1", 21),
            numericInput(ns("trial1"), "Trials group 1", 31),

            numericInput(ns("count2"), "Successes group 2", 9),
            numericInput(ns("trial2"), "Trials group 2", 23),

            sliderInput(
                ns("alpha"),
                "Confidence level",
                min = 0.80,
                max = 0.99,
                value = 0.95,
                step = 0.01
            ),

            numericInput(
                ns("nsim"),
                "Simulations",
                value = 10000,
                min = 1000
            ),

            numericInput(
                ns("seed"),
                "Random seed",
                value = sample.int(999, 1),
                min = 1,
                max = 999
            )
        ),

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'Data Dredging'",
                ns("demo")
            ),

            numericInput(
                ns("n_data"),
                "Observations",
                100
            ),

            numericInput(
                ns("n_var"),
                "Candidate predictors",
                50
            ),

            numericInput(
                ns("seed"),
                "Random seed",
                value = sample.int(999, 1),
                min = 1,
                max = 999
            )
        )
    )

    # =======================================================
    # Overview
    # =======================================================

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
                    "📐 When randomness looks like structure",
                    style = "
                    font-size: 1.4rem;
                    font-weight: 700;
                    color: #2c3e50;
                "
                )
            ),

            p(
                strong("Main idea: "),
                "Random data can easily produce patterns that look meaningful,
             even when no real underlying relationship exists."
            ),

            hr(),

            h5("The problem"),

            p(
                "In real-world data analysis, we are constantly searching for patterns.
             The danger is that randomness itself can create patterns that appear real."
            ),

            hr(),

            h5("What happens in this chapter?"),

            tags$div(
                style = "margin-left: 10px;",

                p("① Explore how unlikely events can still occur (Birthday Problem)."),

                p("② Estimate differences between groups using simulated data."),

                p("③ Study how often random data produces ‘significant’ results."),

                p("④ Observe how regression can find relationships in pure noise.")
            ),

            hr(),

            h5("Your job"),

            tags$ul(
                tags$li("Investigate how often random data appears structured"),
                tags$li("Compare true effects with apparent effects"),
                tags$li("Explore how sample size affects false discoveries"),
                tags$li("Learn how easily regression can be misled")
            ),

            hr(),

            div(
                style = "
                background-color:#f8f9fa;
                border-left:5px solid #7B9ACC;
                padding:12px;
                border-radius:8px;
            ",

                h5("Questions to investigate"),

                tags$ul(
                    tags$li("Why do rare events become inevitable with enough trials?"),
                    tags$li("How often do confidence intervals mislead us?"),
                    tags$li("Can regression find patterns in pure noise?"),
                    tags$li("When should we distrust a ‘significant’ result?")
                )
            )
        )
    )

    # =======================================================
    # Code
    # =======================================================

    code_panel <- div(

        card(
            card_header("Generated R code"),

            tags$pre(
                style="
                    background:#F8F9FA;
                    padding:15px;
                    border-radius:10px;
                ",
                textOutput(ns("generated_code"))
            )
        )
    )

    # =======================================================
    # Results
    # =======================================================

    results_panel <- div(

        card(
            card_header("Visualisation"),
            plotOutput(ns("plot"), height = 450)
        ),

        br(),

        uiOutput(ns("results_panel"))
    )

    # =======================================================
    # Learn
    # =======================================================

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

            h5("1. Rare events are not impossible"),

            p(
                "Even very unlikely events will occur if we repeat experiments enough times."
            ),

            hr(),

            h5("2. Statistical methods always produce answers"),

            p(
                "Even when there is no real signal, methods like confidence intervals and regression will still produce results that look meaningful."
            ),

            hr(),

            h5("3. ‘Significance’ does not guarantee truth"),

            p(
                "A statistically significant result can still arise from random variation rather than a real effect."
            ),

            hr(),

            h5("4. Searching creates false discoveries"),

            p(
                "The more hypotheses or patterns we test, the more likely we are to find something that looks important by chance alone."
            ),

            hr(),

            h5("5. Regression can be fooled by noise"),

            p(
                "With enough variables, regression will almost always find relationships—even in purely random data."
            ),

            hr(),

            div(
                style = "
                background-color:#f8f9fa;
                border-left:5px solid #dc3545;
                padding:12px;
                border-radius:8px;
            ",

                h5("Key takeaway"),

                p(
                    strong("Structure can be an illusion."),
                    br(),
                    "Statistical tools are powerful, but they do not distinguish between real patterns and patterns created by randomness.
                 Interpretation matters as much as calculation."
                )
            )
        )
    )
    # =======================================================
    # Activity Panel
    # =======================================================

    activity_panel <- div(

        card(

            style = "
            border-radius: 16px;
            box-shadow: 0 4px 12px rgba(0,0,0,0.08);
            border: none;
            padding: 10px;
            font-family: 'Inter', sans-serif;
        ",

            card_header(
                div(
                    "Activity 6",
                    style = "
                    font-size: 1.4rem;
                    font-weight: 700;
                    color: #2c3e50;
                "
                )
            ),


            actionButton(
                ns("launch_activity"),
                "Launch Activity 6: Breaking Records",
                class = "btn-success"
            )
        )
    )

    chapter_page_ui(
        id = id,
        title = "📐 Chapter 6: Design",
        sidebar = sidebar_controls,
        overview = overview_panel,
        code = code_panel,
        results = results_panel,
        learn = learn_panel,
        activity = activity_panel
    )
}

# =========================================================
# Chapter 6 Server
# =========================================================

chapter6_server <- function(id){

    moduleServer(id, function(input, output, session){


        observeEvent(input$launch_activity, {

            url <- pws:::launch_activity_from_lab(6)

            shinyjs::runjs(
                sprintf(
                    "window.open('%s', '_blank');",
                    url
                )
            )
        })

        # -------------------------------------------------
        # Auto-switch to Results tab on experiment change
        # -------------------------------------------------


        observeEvent(input$demo, {

            updateTabsetPanel(
                session,
                "chapter_tab",
                selected = "Results"
            )


            if (input$demo != "Birthday Problem") {

                updateNumericInput(
                    session,
                    "seed",
                    value = sample.int(999, 1)
                )

            }

        }, ignoreInit = TRUE)

        # -------------------------------------------------
        # Reactive analysis (no Run button)
        # -------------------------------------------------

        analysis <- reactive({

            # =================================================
            # Birthday Problem
            # =================================================

            if(input$demo == "Birthday Problem"){

                nmax <- 60

                df <- data.frame(
                    N = 1:nmax,
                    P = sapply(1:nmax, bp)
                )

                required_n <- df$N[which(df$P >= input$p_level)[1]]

                list(
                    type = "birthday",
                    data = df,
                    required_n = required_n
                )
            }

            # =================================================
            # Difference in Proportions
            # =================================================

            else if(input$demo == "Difference in Proportions"){

                set.seed(input$seed)

                p1 <- input$count1 / input$trial1
                p2 <- input$count2 / input$trial2

                s1 <- rbinom(input$nsim, input$trial1, p1) / input$trial1
                s2 <- rbinom(input$nsim, input$trial2, p2) / input$trial2

                d <- s1 - s2

                se <- sd(d)
                m <- mean(d)

                qv <- qnorm((1 + input$alpha)/2)

                ci <- c(m - qv * se, m + qv * se)

                list(
                    type = "prop",
                    d = d,
                    se = se,
                    ci = ci,
                    estimate = p1 - p2
                )
            }

            # =================================================
            # Data dredging
            # =================================================

            else {

                set.seed(input$seed)

                y <- rnorm(input$n_data, 0, 5)

                x <- matrix(
                    rnorm(input$n_var * input$n_data, 0, 10),
                    nrow = input$n_var
                )

                pvals <- sapply(1:input$n_var, function(i){
                    summary(lm(y ~ x[i, ]))$coeff[2, 4]
                })

                best <- which.min(pvals)
                xx <- x[best, ]

                fit <- lm(y ~ xx)

                list(
                    type = "dredge",
                    x = xx,
                    y = y,
                    coef = summary(fit)$coefficients,
                    minp = min(pvals)
                )
            }

        })

        # =====================================================
        # Code display
        # =====================================================

        output$generated_code <- renderText({

            switch(
                input$demo,

                "Birthday Problem" =
                    "plot_bp(p = p_level, nmax = 60)",

                "Difference in Proportions" =
                    paste0(
                        "prop.diff(counts = c(",
                        input$count1, ",", input$count2,
                        "), trials = c(",
                        input$trial1, ",", input$trial2,
                        "))"
                    ),

                "Data Dredging" =
                    paste0(
                        "data_dredge(n_data = ",
                        input$n_data,
                        ", n_var = ",
                        input$n_var,
                        ")"
                    )
            )
        })

        # =====================================================
        # Plot
        # =====================================================

        output$plot <- renderPlot({

            a <- analysis()

            if(a$type == "birthday"){

                ggplot(a$data, aes(N, P)) +
                    geom_line(colour = "#7B9ACC", linewidth = 1) +
                    geom_point(colour = "#7B9ACC") +
                    geom_hline(
                        yintercept = input$p_level,
                        colour = "red",
                        linetype = "dashed"
                    ) +
                    geom_vline(
                        xintercept = a$required_n,
                        colour = "darkred",
                        linetype = "dotted"
                    ) +
                    theme_minimal(base_size = 14)
            }

            else if(a$type == "prop"){

                ggplot(data.frame(d = a$d), aes(d)) +
                    geom_histogram(
                        bins = 20,
                        fill = "#7B9ACC",
                        colour = "white"
                    ) +
                    geom_vline(
                        xintercept = a$ci,
                        colour = "red",
                        linetype = "dashed"
                    ) +
                    theme_minimal(base_size = 14)
            }

            else {

                ggplot(
                    data.frame(x = a$x, y = a$y),
                    aes(x, y)
                ) +
                    geom_point(colour = "#7B9ACC") +
                    geom_smooth(method = "lm", colour = "red") +
                    theme_minimal(base_size = 14)
            }
        })

        # =====================================================
        # Results panel
        # =====================================================

        output$results_panel <- renderUI({

            a <- analysis()

            if(a$type == "birthday"){

                card(
                    card_header("Key result"),
                    h3(sprintf("Required n = %d", a$required_n))
                )

            } else if(a$type == "prop"){

                inside <- 0 >= a$ci[1] && 0 <= a$ci[2]

                sig_level <- 100 * (1 - input$alpha)

                conclusion <- if (inside) {
                    sprintf(
                        "No evidence against common proportions at the %.1f%% significance level.",
                        sig_level
                    )
                } else {
                    sprintf(
                        "Evidence against common proportions at the %.1f%% significance level.",
                        sig_level
                    )
                }

                card(
                    card_header("Difference in proportions"),

                    p(sprintf(
                        "Estimated difference (p1 - p2): %.3f",
                        a$estimate
                    )),

                    p(sprintf(
                        "SE: %.4f",
                        a$se
                    )),

                    p(sprintf(
                        "%.0f%% CI: %.3f to %.3f",
                        100 * input$alpha,
                        a$ci[1],
                        a$ci[2]
                    )),

                    hr(),

                    strong(conclusion)
                )

            } else {

                card(
                    card_header("Regression result"),
                    p(sprintf("Smallest p-value: %.5f", a$minp)),
                    p(sprintf("Gradient: %.3f", a$coef[2,1])),
                    p(sprintf("Std error: %.3f", a$coef[2,2]))
                )
            }
        })

    })
}
