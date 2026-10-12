
# =========================================================
# Chapter 6 UI
# =========================================================

chapter6_ui <- function(id) {

    ns <- NS(id)

    sidebar_controls <- sidebar(

        selectInput(
            ns("demo"),
            "Example:",
            choices = c(
                "Birthday Problem",
                "Assessing the ITV jinx",
                "Data Dredging"
            )
        ),

        # -------------------------------------------------------
        # Birthday Problem
        # -------------------------------------------------------

        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'Birthday Problem'", ns("demo")
            ),

            radioButtons(
                ns("birthday_type"),
                "Choose birthday investigation",
                choices = c(
                    "Classic birthday problem" = "classic",
                    "Non-Classic birthday problem" = "context"
                ),
                selected = "classic"
            ),

            conditionalPanel(
                condition = sprintf(
                    "input['%s'] == 'classic'", ns("birthday_type")
                ),

                sliderInput(
                    ns("p_level"),
                    "Probability threshold (p)",
                    min = 0.01, max = 0.99,
                    value = 0.50, step = 0.01
                )
            ),

            conditionalPanel(
                condition = sprintf(
                    "input['%s'] == 'context'", ns("birthday_type")
                ),

                sliderInput(
                    ns("match_size"),
                    "Number sharing birthday",
                    min = 2, max = 10, value = 4
                ),

                sliderInput(
                    ns("team_small"),
                    "Team size",
                    min = 5, max = 100, value = 20
                ),

                sliderInput(
                    ns("team_large"),
                    "Company size",
                    min = 10, max = 500, value = 100
                ),

                radioButtons(
                    ns("birthday_method"),
                    "Calculation method",
                    choices = c(
                        "Dynamic programming (exact)" = "dp",
                        "Poisson approximation (fast)" = "poisson"
                    ),
                    selected = "poisson"
                ),

                radioButtons(
                    ns("birthday_scale"),
                    "Display scale",
                    choices = c(
                        "Probability" = "prob",
                        "Log probability" = "log"
                    ),
                    selected = "log"
                )
            )
        ),

        # -------------------------------------------------------
        # Assessing the ITV jinx
        # -------------------------------------------------------

        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'Assessing the ITV jinx'", ns("demo")
            ),

            h5("BBC"),

            numericInput(
                ns("trial1"), "Number of Matches", 31, min = 1
            ),

            numericInput(
                ns("count1"), "England Wins", 21, min = 0
            ),

            hr(),

            h5("ITV"),

            numericInput(
                ns("trial2"), "Number of Matches", 23, min = 1
            ),

            numericInput(
                ns("count2"), "England Wins", 9, min = 0
            ),

            sliderInput(
                ns("alpha"),
                "Confidence level",
                min = 0.80, max = 0.99,
                value = 0.95, step = 0.01
            ),

            numericInput(
                ns("seed"),
                "Random seed",
                value = 123, min = 1, max = 999999
            )
        ),

        # -------------------------------------------------------
        # Data Dredging
        # -------------------------------------------------------

        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'Data Dredging'", ns("demo")
            ),

            p(
                "Keep X fixed and generate a new Y for every regression."
            ),

            numericInput(
                ns("seed_dredge"),
                "Random seed",
                value = 123,
                min = 1,
                max = 999999
            ),

            sliderInput(
                ns("n_data"),
                "Observations per regression (n)",
                min = 10, max = 500,
                value = 100, step = 10
            ),

            sliderInput(
                ns("n_var"),
                "Number of regressions (m)",
                min = 5, max = 200,
                value = 50, step = 5
            ),

            actionButton(
                ns("run_analysis"),
                "Analysis",
                icon = icon("play"),
                class = "btn-primary"
            ),

            br(),
            br(),

            actionButton(
                ns("show_summary"),
                "Summary",
                icon = icon("book-open")
            ),

            br(),

            helpText(
                "Changing the controls does not rerun the analysis. ",
                "Press Analysis when you are ready to run it."
            )
        )
    )


    # =========================================================
    # Overview
    # =========================================================

    overview_panel <- div(

        card(
            card_header("Module 6: Statistics in Context"),

            conditionalPanel(
                condition = sprintf(
                    "input['%s'] == 'Birthday Problem'", ns("demo")
                ),

                h4("The birthday problem"),

                p(
                    "The probability of a shared birthday depends on how ",
                    "the event is defined. The classic problem asks how ",
                    "large a group must be for the probability of at least ",
                    "one shared birthday to reach a specified threshold."
                ),

                p(
                    "The non-classic version considers whether any group ",
                    "of a specified size shares a birthday. Compare group ",
                    "sizes, calculation methods and probability scales."
                )
            ),

            conditionalPanel(
                condition = sprintf(
                    "input['%s'] == 'Assessing the ITV jinx'", ns("demo")
                ),

                h4("Assessing the ITV jinx"),

                p(
                    "This experiment compares England's observed win ",
                    "proportions for BBC and ITV broadcasts. Simulated ",
                    "differences illustrate how much results can vary ",
                    "through sampling variation."
                ),

                p(
                    "An observed difference does not establish that the ",
                    "broadcaster caused the difference. The matches shown ",
                    "and other factors may also matter."
                )
            ),

            conditionalPanel(
                condition = sprintf(
                    "input['%s'] == 'Data Dredging'", ns("demo")
                ),

                h4("Data dredging"),

                p(
                    "This experiment demonstrates how searching across ",
                    "many regressions can produce an apparently striking ",
                    "relationship even when the variables are independent."
                ),

                p(
                    "A single set of explanatory observations X is ",
                    "generated and held fixed. The response variable Y ",
                    "is generated independently again for every regression."
                ),

                p(
                    "The experiment selects the regression with the largest ",
                    "absolute gradient. Increasing m gives random variation ",
                    "more opportunities to produce an extreme result."
                ),

                h5("How to use the experiment"),

                tags$ol(
                    tags$li("Choose the number of observations n."),
                    tags$li("Choose the number of regressions m."),
                    tags$li("Choose a random seed."),
                    tags$li("Press Analysis to run the simulation."),
                    tags$li(
                        "Inspect the selected regression and its 95% confidence band."
                    ),
                    tags$li(
                        "Press Summary to inspect the results across all regressions."
                    ),
                    tags$li(
                        "Change the controls and press Analysis again to start a new simulation."
                    )
                ),

                h5("Questions to investigate"),

                tags$ul(
                    tags$li(
                        "How does the largest absolute gradient change as m increases?"
                    ),
                    tags$li(
                        "Can the selected regression have a small p-value when X and Y are independent?"
                    ),
                    tags$li(
                        "How does increasing n affect the gradients?"
                    ),
                    tags$li(
                        "Why does selecting the most extreme result create a risk of false discoveries?"
                    )
                )
            )
        )
    )


    # =========================================================
    # Generated R code
    # =========================================================

    code_panel <- div(
        card(
            card_header("Generated R code"),

            p(
                "The code below reproduces the currently displayed analysis."
            ),

            tags$pre(
                style = paste(
                    "background:#F8F9FA;",
                    "padding:15px;",
                    "border-radius:10px;",
                    "white-space:pre-wrap;"
                ),
                textOutput(ns("generated_code"))
            )
        )
    )


    # =========================================================
    # Results
    # =========================================================

    results_panel <- div(
        card(
            card_header("Regression Analysis"),
            plotOutput(ns("plot"), height = 450)
        ),

        br(),

        uiOutput(ns("results_panel"))
    )


    # =========================================================
    # Learning outcomes
    # =========================================================

    learn_panel <- div(
        card(
            card_header("What should you have learned?"),

            h5("1. Rare events are not impossible"),

            p(
                "Repeated experiments give unlikely events more opportunities to occur."
            ),

            hr(),

            h5("2. Random data can look meaningful"),

            p(
                "Regression will estimate a relationship even when the variables are independent."
            ),

            hr(),

            h5("3. Selection matters"),

            p(
                "Choosing the most extreme result from many analyses can exaggerate the apparent evidence."
            ),

            hr(),

            h5("4. More tests mean more opportunities for false discoveries"),

            p(
                "The number of analyses performed matters when interpreting a selected result."
            ),

            hr(),

            h5("5. Statistical results need context"),

            p(
                "A small p-value or an impressive graph is not, by itself, proof of a genuine relationship."
            )
        )
    )


    # =========================================================
    # Assemble page
    # =========================================================

    chapter_page_ui(
        id = id,
        title = "🔍 Module 6: Context",
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

chapter6_server <- function(id) {

    moduleServer(id, function(input, output, session) {

        summary_visible <- reactiveVal(FALSE)


        # -------------------------------------------------------
        # Birthday control: keep company size above team size
        # -------------------------------------------------------

        observeEvent(input$team_small, {

            req(input$team_small)

            required_min <- input$team_small + 1

            if (input$team_large < required_min) {

                updateSliderInput(
                    session,
                    "team_large",
                    min = required_min,
                    value = required_min
                )

            } else {

                updateSliderInput(
                    session,
                    "team_large",
                    min = required_min
                )
            }
        })


        # -------------------------------------------------------
        # Switching experiments hides the optional summary
        # -------------------------------------------------------

        observeEvent(input$demo, {
            summary_visible(FALSE)
        }, ignoreInit = TRUE)


        # -------------------------------------------------------
        # Non-dredging analysis
        #
        # Birthday and ITV continue to respond to their inputs.
        # Data Dredging is handled separately below.
        # -------------------------------------------------------

        other_analysis <- reactive({

            req(input$demo != "Data Dredging")

            # =====================================================
            # Birthday Problem
            # =====================================================

            if (input$demo == "Birthday Problem") {

                if (input$birthday_type == "classic") {

                    nmax <- 60

                    df <- data.frame(
                        N = seq_len(nmax),
                        P = vapply(
                            seq_len(nmax),
                            bp,
                            numeric(1)
                        )
                    )

                    eligible <- which(df$P >= input$p_level)

                    required_n <- if (length(eligible)) {
                        df$N[eligible[1]]
                    } else {
                        NA_integer_
                    }

                    return(list(
                        type = "birthday",
                        data = df,
                        required_n = required_n
                    ))
                }

                m <- input$match_size
                n1 <- input$team_small
                n2 <- max(input$team_large, n1 + 1)

                if (input$birthday_method == "dp") {

                    p1 <- birthday_dp(n1, m)
                    p2 <- birthday_dp(n2, m)

                } else {

                    p1 <- birthday_poisson(n1, m)
                    p2 <- birthday_poisson(n2, m)
                }

                scenarios <- c(
                    paste(m, "specific people,\nspecific date"),
                    paste(m, "specific people,\nany date"),
                    paste("Any", m, "in group of", n1),
                    paste("Any", m, "in group of", n2)
                )

                probabilities <- c(
                    365^(-m),
                    365^(1 - m),
                    p1,
                    p2
                )

                return(list(
                    type = "birthday_context",

                    data = data.frame(
                        Scenario = factor(
                            scenarios,
                            levels = scenarios
                        ),
                        k = c(
                            365^m,
                            365^(m - 1),
                            1 / pmax(p1, .Machine$double.xmin),
                            1 / pmax(p2, .Machine$double.xmin)
                        )
                    ),

                    probabilities = probabilities
                ))
            }


            # =====================================================
            # Assessing the ITV jinx
            # =====================================================

            req(
                input$trial1 > 0,
                input$trial2 > 0,
                input$count1 >= 0,
                input$count2 >= 0,
                input$count1 <= input$trial1,
                input$count2 <= input$trial2
            )

            set.seed(input$seed)

            p1 <- input$count1 / input$trial1
            p2 <- input$count2 / input$trial2

            nsim <- 5000

            s1 <- rbinom(
                nsim,
                input$trial1,
                p1
            ) / input$trial1

            s2 <- rbinom(
                nsim,
                input$trial2,
                p2
            ) / input$trial2

            d <- s1 - s2

            se <- sd(d)
            estimate <- p1 - p2

            qv <- qnorm((1 + input$alpha) / 2)

            ci <- c(
                estimate - qv * se,
                estimate + qv * se
            )

            list(
                type = "prop",
                d = d,
                se = se,
                ci = ci,
                estimate = estimate
            )
        })


        # -------------------------------------------------------
        # Data Dredging: run only when Analysis is pressed
        # -------------------------------------------------------

        dredge_analysis <- eventReactive(
            input$run_analysis,
            {

                n <- input$n_data
                m <- input$n_var
                seed_used <- input$seed_dredge

                set.seed(seed_used)

                # Generate X exactly once for this analysis.
                x <- rnorm(n, mean = 0, sd = 10)

                slopes <- numeric(m)
                pvalues <- numeric(m)
                standard_errors <- numeric(m)

                responses <- vector("list", m)

                # Generate an independent Y for every regression.
                for (j in seq_len(m)) {

                    y <- rnorm(n, mean = 0, sd = 5)

                    fit <- lm(y ~ x)
                    coefs <- summary(fit)$coefficients

                    slopes[j] <- coefs[2, 1]
                    standard_errors[j] <- coefs[2, 2]
                    pvalues[j] <- coefs[2, 4]

                    responses[[j]] <- y
                }

                # Select the largest absolute gradient.
                best <- which.max(abs(slopes))

                y_best <- responses[[best]]

                fit_best <- lm(y_best ~ x)

                list(
                    type = "dredge",

                    x = x,
                    y = y_best,

                    coef = summary(fit_best)$coefficients,

                    selected = best,
                    selected_slope = slopes[best],
                    selected_se = standard_errors[best],
                    selected_p = pvalues[best],

                    slopes = slopes,
                    pvalues = pvalues,
                    standard_errors = standard_errors,

                    n = n,
                    m = m,
                    seed = seed_used
                )

            },
            ignoreInit = TRUE
        )


        # Hide the optional summary when a new analysis is requested.
        observeEvent(input$run_analysis, {

            updateNumericInput(
                session,
                "seed_dredge",
                value = sample.int(999, 1)
            )

        }, ignoreInit = TRUE)


        # -------------------------------------------------------
        # Show optional summary
        # -------------------------------------------------------

        observeEvent(input$show_summary, {
            summary_visible(TRUE)
        })


        # -------------------------------------------------------
        # Current analysis
        # -------------------------------------------------------

        analysis <- reactive({

            if (input$demo == "Data Dredging") {
                return(dredge_analysis())
            }

            other_analysis()
        })


        # =======================================================
        # Generated R code
        # =======================================================

        output$generated_code <- renderText({

            a <- analysis()

            if (is.null(a)) {
                return(
                    paste(
                        "No Data Dredging analysis has been run yet.",
                        "Choose n, m and a random seed, then press Analysis."
                    )
                )
            }

            # -----------------------------------------------------
            # Birthday
            # -----------------------------------------------------

            if (a$type == "birthday") {

                return(paste0(
                    "birthday_classic(\n",
                    "  threshold = ", input$p_level, "\n",
                    ")"
                ))
            }

            if (a$type == "birthday_context") {

                return(paste0(
                    "birthday_context(\n",
                    "  matches = ", input$match_size, ",\n",
                    "  groups = c(",
                    input$team_small, ", ", input$team_large, "),\n",
                    "  method = \"", input$birthday_method, "\"\n",
                    ")"
                ))
            }

            # -----------------------------------------------------
            # ITV
            # -----------------------------------------------------

            if (a$type == "prop") {

                return(paste0(
                    "difference_in_proportions(\n",
                    "  wins = c(", input$count1, ", ", input$count2, "),\n",
                    "  matches = c(", input$trial1, ", ", input$trial2, "),\n",
                    "  confidence_level = ", input$alpha, ",\n",
                    "  seed = ", input$seed, "\n",
                    ")"
                ))
            }

            # -----------------------------------------------------
            # Data Dredging
            #
            # IMPORTANT: use the submitted analysis settings stored
            # in a, not the current unsubmitted slider values.
            # -----------------------------------------------------

            paste0(
                "# Data dredging: fixed X, repeated independent Y\n\n",

                "set.seed(", a$seed, ")\n\n",

                "n <- ", a$n, "\n",
                "m <- ", a$m, "\n\n",

                "# Generate X once\n",
                "x <- rnorm(n, mean = 0, sd = 10)\n\n",

                "# Store regression results\n",
                "slopes <- numeric(m)\n",
                "standard_errors <- numeric(m)\n",
                "pvalues <- numeric(m)\n",
                "responses <- vector('list', m)\n\n",

                "for (j in seq_len(m)) {\n",
                "  y <- rnorm(n, mean = 0, sd = 5)\n",
                "  fit <- lm(y ~ x)\n",
                "  coefs <- summary(fit)$coefficients\n",
                "  slopes[j] <- coefs[2, 1]\n",
                "  standard_errors[j] <- coefs[2, 2]\n",
                "  pvalues[j] <- coefs[2, 4]\n",
                "  responses[[j]] <- y\n",
                "}\n\n",

                "# Select the most extreme gradient\n",
                "best <- which.max(abs(slopes))\n",
                "y_best <- responses[[best]]\n",
                "fit_best <- lm(y_best ~ x)\n\n",

                "# Plot the selected regression\n",
                "plot(x, y_best,\n",
                "     xlab = 'Explanatory variable (X)',\n",
                "     ylab = 'Response variable (Y)')\n",
                "abline(h = mean(y_best), lty = 2, col = 'grey40')\n",
                "abline(fit_best, col = 'red', lwd = 2)\n\n",

                "# Add a 95% confidence band\n",
                "xx <- seq(min(x), max(x), length.out = 200)\n",
                "pred <- predict(\n",
                "  fit_best,\n",
                "  newdata = data.frame(x = xx),\n",
                "  interval = 'confidence',\n",
                "  level = 0.95\n",
                ")\n",
                "polygon(\n",
                "  c(xx, rev(xx)),\n",
                "  c(pred[, 'lwr'], rev(pred[, 'upr'])),\n",
                "  col = adjustcolor('grey70', alpha.f = 0.5),\n",
                "  border = NA\n",
                ")\n",
                "points(x, y_best)\n",
                "abline(h = mean(y_best), lty = 2, col = 'grey40')\n",
                "lines(xx, pred[, 'fit'], col = 'red', lwd = 2)\n\n",

                "# Summary\n",
                "c(\n",
                "  selected_regression = best,\n",
                "  selected_gradient = slopes[best],\n",
                "  selected_p_value = pvalues[best],\n",
                "  significant_at_5_percent = sum(pvalues < 0.05),\n",
                "  number_of_regressions = m\n",
                ")"
            )
        })


        # =======================================================
        # Plot
        # =======================================================

        output$plot <- renderPlot({

            a <- analysis()

            # Empty until Analysis is pressed for Data Dredging.
            if (is.null(a)) {
                plot.new()
                return(invisible(NULL))
            }


            # -----------------------------------------------------
            # Classic birthday problem
            # -----------------------------------------------------

            if (a$type == "birthday") {

                req(!is.na(a$required_n))

                print(
                    ggplot(a$data, aes(N, P)) +
                        geom_line(
                            colour = "#7B9ACC",
                            linewidth = 1
                        ) +
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
                        theme_minimal(base_size = 14) +
                        labs(
                            x = "Number of people",
                            y = "Probability of a shared birthday"
                        )
                )

                # -----------------------------------------------------
                # Context birthday problem
                # -----------------------------------------------------

            } else if (a$type == "birthday_context") {

                plot_data <- a$data

                if (input$birthday_scale == "prob") {
                    plot_data$value <- a$probabilities
                    ylab <- "Probability"
                } else {
                    plot_data$value <- log10(
                        pmax(a$probabilities, .Machine$double.xmin)
                    )
                    ylab <- "log10(Probability)"
                }

                print(
                    ggplot(plot_data, aes(Scenario, value)) +
                        geom_point(
                            size = 4,
                            colour = "#7B9ACC"
                        ) +
                        geom_line(
                            aes(group = 1),
                            colour = "#7B9ACC"
                        ) +
                        labs(x = NULL, y = ylab) +
                        theme_minimal(base_size = 14) +
                        theme(
                            axis.text.x = element_text(
                                angle = 30,
                                hjust = 1
                            )
                        )
                )

                # -----------------------------------------------------
                # ITV jinx
                # -----------------------------------------------------

            } else if (a$type == "prop") {

                print(
                    ggplot(data.frame(d = a$d), aes(d)) +
                        geom_histogram(
                            bins = 30,
                            fill = "#7B9ACC",
                            colour = "white"
                        ) +
                        geom_vline(
                            xintercept = a$estimate,
                            colour = "darkred",
                            linewidth = 1
                        ) +
                        geom_vline(
                            xintercept = a$ci,
                            colour = "red",
                            linetype = "dashed"
                        ) +
                        theme_minimal(base_size = 14) +
                        labs(
                            x = "Simulated difference in proportions",
                            y = "Frequency"
                        )
                )

                # -----------------------------------------------------
                # Data Dredging
                # -----------------------------------------------------

            } else if (a$type == "dredge") {

                plot_data <- data.frame(
                    x = a$x,
                    y = a$y
                )

                suppressWarnings({
                print(
                    ggplot(plot_data, aes(x, y)) +

                        # Mean of the selected simulated response.
                        geom_hline(
                            yintercept = mean(a$y),
                            colour = "grey40",
                            linetype = "dashed",
                            linewidth = 0.7
                        ) +

                        # Fitted regression and 95% confidence band.
                        geom_smooth(
                            method = "lm",
                            formula = y ~ x,
                            se = TRUE,
                            level = 0.95,
                            colour = "red",
                            fill = "grey70",
                            alpha = 0.4
                        ) +

                        geom_point(
                            colour = "#7B9ACC"
                        ) +

                        labs(
                            title = paste0(
                                "Selected regression ", a$selected,
                                " of ", a$m
                            ),
                            subtitle = paste0(
                                "n = ", a$n,
                                "; gradient = ",
                                sprintf("%.4f", a$selected_slope)
                            ),
                            x = "Explanatory variable (X)",
                            y = "Response variable (Y)"
                        ) +

                        theme_minimal(base_size = 14)
                )

                })
            }
        })


        # =======================================================
        # Results panel
        # =======================================================

        output$results_panel <- renderUI({

            a <- analysis()

            if (is.null(a)) {
                return(NULL)
            }


            # -----------------------------------------------------
            # Classic birthday
            # -----------------------------------------------------

            if (a$type == "birthday") {

                return(
                    card(
                        card_header("Key result"),

                        p(
                            sprintf(
                                paste0(
                                    "The minimum group size for a probability ",
                                    "of at least %.0f%% is %d."
                                ),
                                100 * input$p_level,
                                a$required_n
                            )
                        )
                    )
                )
            }


            # -----------------------------------------------------
            # Context birthday
            # -----------------------------------------------------

            if (a$type == "birthday_context") {

                return(
                    card(
                        card_header("Birthday probabilities"),

                        tags$table(
                            class = "table",

                            tags$tr(
                                tags$th("Scenario"),
                                tags$th("Probability"),
                                tags$th("Reciprocal probability")
                            ),

                            lapply(seq_len(nrow(a$data)), function(i) {
                                tags$tr(
                                    tags$td(as.character(a$data$Scenario[i])),
                                    tags$td(
                                        format(
                                            a$probabilities[i],
                                            scientific = TRUE,
                                            digits = 5
                                        )
                                    ),
                                    tags$td(
                                        format(
                                            round(a$data$k[i]),
                                            big.mark = ",",
                                            scientific = FALSE
                                        )
                                    )
                                )
                            })
                        )
                    )
                )
            }


            # -----------------------------------------------------
            # ITV jinx
            # -----------------------------------------------------

            if (a$type == "prop") {

                inside <- (
                    a$ci[1] <= 0 &&
                        a$ci[2] >= 0
                )

                conclusion <- if (inside) {
                    "The interval includes zero."
                } else {
                    "The interval excludes zero."
                }

                return(
                    card(
                        card_header("Difference in proportions"),

                        p(sprintf(
                            "Estimated difference (BBC - ITV): %.4f",
                            a$estimate
                        )),

                        p(sprintf("Standard error: %.4f", a$se)),

                        p(sprintf(
                            "%.0f%% interval: [%.4f, %.4f]",
                            100 * input$alpha,
                            a$ci[1],
                            a$ci[2]
                        )),

                        strong(conclusion)
                    )
                )
            }


            # -----------------------------------------------------
            # Data Dredging
            # -----------------------------------------------------

            if (!summary_visible()) {
                return(NULL)
            }

            significant <- a$pvalues < 0.05

            card(
                card_header("Data dredging: summary"),

                h5("Selected regression"),

                p(sprintf(
                    "The most extreme absolute gradient occurred in regression %d of %d.",
                    a$selected,
                    a$m
                )),

                tags$ul(
                    tags$li(sprintf(
                        "Gradient estimate: %.5f",
                        a$selected_slope
                    )),

                    tags$li(sprintf(
                        "Standard error: %.5f",
                        a$selected_se
                    )),

                    tags$li(sprintf(
                        "P-value: %.6f",
                        a$selected_p
                    ))

                ),

                hr(),

                p(
                    "The X variables were generated once and held fixed throughout this ",
                    "analysis. Each regression used a newly generated set of Y variables. ",
                    "The selected regression is the one with the largest ",
                    "absolute gradient, not a randomly chosen regression."
                )
            )
        })
    })
}
