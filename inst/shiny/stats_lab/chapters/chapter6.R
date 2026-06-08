library(shiny)
library(ggplot2)

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
            card_header("What this chapter explores"),

            p("This chapter introduces probability,
              confidence intervals and the dangers
              of searching for patterns in random data."),

            tags$ul(
                tags$li("The Birthday Problem"),
                tags$li("Comparing two proportions"),
                tags$li("Data dredging and false discoveries")
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
            card_header("Key ideas"),

            tags$ul(
                tags$li("Probability can be surprising."),
                tags$li("Confidence intervals quantify uncertainty."),
                tags$li("Large numbers of tests create false positives."),
                tags$li("Significant results are not always meaningful.")
            )
        ),

        br(),

        card(
            card_header("Big idea"),

            tags$blockquote(
                style="
                    font-size:22px;
                    font-weight:700;
                    color:#7B9ACC;
                    border-left:5px solid #CDB4DB;
                    padding-left:18px;
                ",
                "The more we search random data, the easier it becomes to find apparently important patterns."
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
        learn = learn_panel
    )
}

# =========================================================
# Chapter 6 Server
# =========================================================

chapter6_server <- function(id){

    moduleServer(id, function(input, output, session){

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
