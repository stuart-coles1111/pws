
# =========================================================
# Chapter 6 UI
# =========================================================

chapter6_ui <- function(id) {

    ns <- NS(id)

    sidebar_controls <- sidebar(

        selectInput(
            ns("demo"),
            "Experiment:",
            choices = c(
                "Birthday Problem",
                "Assessing the ITV jinx",
                "Data Dredging"
            )
        ),

        # -------------------------------------------------
        # Birthday problem
        # -------------------------------------------------

        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'Birthday Problem'",
                ns("demo")
            ),

            radioButtons(
                ns("birthday_type"),
                "Choose birthday investigation",
                choices = c(
                    "Classic birthday problem" = "classic",
                    "Non-classic birthday problem" = "context"
                ),
                selected = "classic"
            ),

            conditionalPanel(
                condition = sprintf(
                    "input['%s'] == 'classic'",
                    ns("birthday_type")
                ),

                sliderInput(
                    ns("p_level"),
                    "Probability threshold (p)",
                    min = 0.01,
                    max = 0.99,
                    value = 0.50,
                    step = 0.01
                )
            ),

            conditionalPanel(
                condition = sprintf(
                    "input['%s'] == 'context'",
                    ns("birthday_type")
                ),

                sliderInput(
                    ns("match_size"),
                    "Number sharing birthday",
                    min = 2,
                    max = 10,
                    value = 4
                ),

                sliderInput(
                    ns("team_small"),
                    "Team size",
                    min = 5,
                    max = 100,
                    value = 20
                ),

                sliderInput(
                    ns("team_large"),
                    "Company size",
                    min = 10,
                    max = 500,
                    value = 100
                ),

                radioButtons(
                    ns("birthday_method"),
                    "Calculation method",
                    choices = c(
                        "Dynamic programming (exact)" = "dp",
                        "Poisson approximation" = "poisson"
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

        # -------------------------------------------------
        # ITV jinx
        # -------------------------------------------------

        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'Assessing the ITV jinx'",
                ns("demo")
            ),

            h5("BBC"),

            numericInput(
                ns("trial1"),
                "Number of matches",
                value = 31,
                min = 1
            ),

            numericInput(
                ns("count1"),
                "England wins",
                value = 21,
                min = 0
            ),

            hr(),

            h5("ITV"),

            numericInput(
                ns("trial2"),
                "Number of matches",
                value = 23,
                min = 1
            ),

            numericInput(
                ns("count2"),
                "England wins",
                value = 9,
                min = 0
            ),

            sliderInput(
                ns("alpha"),
                "Confidence level",
                min = 0.80,
                max = 0.99,
                value = 0.95,
                step = 0.01
            ),

            numericInput(
                ns("seed"),
                "Random seed",
                value = sample.int(999, 1),
                min = 1,
                max = 999
            )
        ),

        # -------------------------------------------------
        # Data dredging
        # -------------------------------------------------

        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'Data Dredging'",
                ns("demo")
            ),

            numericInput(
                ns("seed_dredge"),
                "Random seed",
                value = sample.int(999, 1),
                min = 1,
                max = 999
            ),

            sliderInput(
                ns("n_data"),
                "Observations per regression (n)",
                min = 10,
                max = 500,
                value = 100,
                step = 10
            ),

            sliderInput(
                ns("n_var"),
                "Number of regressions (m)",
                min = 5,
                max = 200,
                value = 50,
                step = 5
            )
        ),

        hr(),

        actionButton(
            ns("analyse"),
            "Analysis",
            icon = icon("play"),
            class = "btn-primary"
        ),

        actionButton(
            ns("show_summary"),
            "Summary",
            icon = icon("book-open")
        ),

        p(
            style = "color: #666; font-size: 0.9em;",
            "Changing the controls does not rerun the analysis. ",
            "Press Analysis to use the current settings."
        )
    )


    # =====================================================
    # Overview
    # =====================================================

    overview_panel <- div(

        card(

            style = paste(
                "border-radius: 16px;",
                "border: none;",
                "box-shadow: 0 4px 12px rgba(0,0,0,0.08);",
                "padding: 10px;"
            ),

            # -------------------------------------------------
            # Birthday problem
            # -------------------------------------------------

            conditionalPanel(
                condition = sprintf(
                    "input['%s'] == 'Birthday Problem'",
                    ns("demo")
                ),

                card_header("Module 6: The birthday problem"),

                p(
                    strong(
                        "How does the probability of a shared birthday ",
                        "depend on how the question is defined?"
                    )
                ),

                h5("Classic birthday problem"),

                p(
                    "The classic problem asks how many people are needed ",
                    "before there is a specified probability that at least ",
                    "two people share a birthday."
                ),

                p(
                    "Assuming 365 equally likely birthdays, we calculate ",
                    "the probability that everybody has a different birthday ",
                    "and subtract it from 1."
                ),

                p(
                    "Use the probability threshold to investigate how ",
                    "the required group size changes."
                ),

                hr(),

                h5("Non-classic birthday problem"),

                p(
                    "We can instead ask whether any group contains a ",
                    "specified number of people sharing a birthday. ",
                    "This is different from asking whether a particular ",
                    "set of people share a particular date."
                ),

                p(
                    "Compare the probabilities for different group sizes ",
                    "and numbers of people sharing a birthday."
                ),

                p(
                    "Dynamic programming calculates the probability more ",
                    "directly, while the Poisson method provides an approximation."
                )
            ),

            # -------------------------------------------------
            # ITV jinx
            # -------------------------------------------------

            conditionalPanel(
                condition = sprintf(
                    "input['%s'] == 'Assessing the ITV jinx'",
                    ns("demo")
                ),

                card_header("Module 6: Assessing the ITV jinx"),

                p(
                    strong(
                        "Could an apparent difference in England's win rates ",
                        "under two broadcasters arise through random variation?"
                    )
                ),

                p(
                    "The example compares England's observed win proportions ",
                    "for matches shown by the BBC and ITV."
                ),

                p(
                    "An observed difference does not necessarily mean that ",
                    "the underlying probabilities differ. Samples vary, and ",
                    "some differences arise by chance."
                ),

                h5("Interpreting the results"),

                p(
                    "The Explorer simulates repeated samples using the ",
                    "observed win proportions and displays the resulting ",
                    "differences."
                ),

                p(
                    "The confidence interval describes uncertainty under ",
                    "the simulation model. It does not establish why any ",
                    "observed difference occurred."
                ),

                p(
                    "The choice of matches, the timing of games and the ",
                    "process determining which broadcaster shows a match ",
                    "may all matter when interpreting the results."
                )
            ),

            # -------------------------------------------------
            # Data dredging
            # -------------------------------------------------

            conditionalPanel(
                condition = sprintf(
                    "input['%s'] == 'Data Dredging'",
                    ns("demo")
                ),

                card_header("Module 6: Data dredging"),

                p(
                    strong(
                        "Repeated analyses can produce apparently strong ",
                        "relationships even when the variables are independent."
                    )
                ),

                p(
                    "The experiment generates a single set of observations ",
                    "for the explanatory variable X. These observations ",
                    "remain fixed throughout the experiment."
                ),

                p(
                    "A new response variable Y is generated independently ",
                    "of X. A regression of Y on X is fitted, and its gradient ",
                    "is recorded. This process is repeated m times, with ",
                    "the same X observations and a newly generated Y each time."
                ),

                hr(),

                h5("Selecting an extreme result"),

                p(
                    "The Explorer selects the regression with the largest ",
                    "absolute gradient. This may be a positive or negative ",
                    "gradient."
                ),

                p(
                    "By performing many regressions and selecting the most ",
                    "extreme result, we give random variation repeated ",
                    "opportunities to produce an apparently striking relationship."
                ),

                hr(),

                h5("Confidence interval for the gradient"),

                p(
                    "The summary includes a conventional 95% confidence ",
                    "interval for the selected gradient."),

                p(
                    "This is the conventional interval for a regression ",
                    "slope. It does not account for the fact that the ",
                    "regression was selected because its gradient was ",
                    "the most extreme among m attempts."
                ),

                p(
                    "Consequently, the interval should not be interpreted ",
                    "as having guaranteed 95% coverage after selection. ",
                    "This is an important part of the data-dredging experiment."
                ),

                hr(),

                h5("How to use the Explorer"),

                tags$ol(
                    tags$li(
                        "Choose the number of observations, n, per regression."
                    ),
                    tags$li(
                        "Choose the number of regressions, m."
                    ),
                    tags$li(
                        "Press Analysis to run the experiment."
                    ),
                    tags$li(
                        "Inspect the selected regression and its confidence band."
                    ),
                    tags$li(
                        "Press Summary to examine the selected gradient, its ",
                        "confidence interval and the results across all regressions."
                    ),
                    tags$li(
                        "Change the settings and press Analysis again."
                    )
                )
            )
        )
    )


    # =====================================================
    # Generated R code
    # =====================================================

    code_panel <- div(

        card(
            card_header("Generated R code"),

            p(
                style = "color: #666;",
                "This code reproduces the most recently completed analysis. ",
                "For data dredging, it includes the confidence interval ",
                "for the selected gradient."
            ),

            tags$pre(
                style = paste(
                    "background: #F8F9FA;",
                    "padding: 15px;",
                    "border-radius: 10px;",
                    "white-space: pre-wrap;"
                ),
                textOutput(ns("generated_code"))
            )
        )
    )


    # =====================================================
    # Results
    # =====================================================

    results_panel <- div(

        card(
            card_header("Visualisation"),

            plotOutput(
                ns("plot"),
                height = 450
            )
        ),

        br(),

        uiOutput(ns("results_panel"))
    )


    # =====================================================
    # Learn
    # =====================================================

    learn_panel <- div(

        card(

            style = paste(
                "border-radius: 16px;",
                "border: none;",
                "box-shadow: 0 4px 12px rgba(0,0,0,0.08);",
                "padding: 10px;"
            ),

            card_header("What should you have learned?"),

            h5("1. Rare events are not impossible"),

            p(
                "Events that are unlikely in a single experiment can ",
                "occur when experiments are repeated many times."
            ),

            hr(),

            h5("2. Statistical methods always produce answers"),

            p(
                "Regression and confidence intervals produce numerical ",
                "results even when there is no underlying relationship."
            ),

            hr(),

            h5("3. Significance does not guarantee truth"),

            p(
                "An apparently significant result can arise from random ",
                "variation rather than a real effect."
            ),

            hr(),

            h5("4. Searching creates false discoveries"),

            p(
                "The more analyses we perform, the more opportunities ",
                "we create to find a pattern that looks important by chance."
            ),

            hr(),

            h5("5. Selection affects interpretation"),

            p(
                "A confidence interval calculated after selecting the ",
                "most extreme result does not retain its usual nominal ",
                "coverage automatically."
            ),

            hr(),

            div(
                style = paste(
                    "background-color: #f8f9fa;",
                    "border-left: 5px solid #dc3545;",
                    "padding: 12px;",
                    "border-radius: 8px;"
                ),

                h5("Key takeaway"),

                p(
                    strong("Structure can be an illusion."),
                    " Statistical tools can reveal genuine patterns, ",
                    "but repeated searching can also make random data ",
                    "appear meaningful. How a result was obtained matters ",
                    "as much as the result itself."
                )
            )
        )
    )


    # =====================================================
    # Assemble chapter page
    # =====================================================

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

        # -------------------------------------------------
        # Stored results
        # -------------------------------------------------

        analysis_result <- reactiveVal(NULL)

        summary_visible <- reactiveVal(FALSE)


        # -------------------------------------------------
        # Helpers
        # -------------------------------------------------

        make_dredge_analysis <- function(n, m, seed) {

            set.seed(seed)

            # Generate X once; hold it fixed for all regressions.

            x <- rnorm(
                n,
                mean = 0,
                sd = 10
            )

            slopes <- numeric(m)
            pvalues <- numeric(m)
            standard_errors <- numeric(m)

            responses <- vector("list", m)

            for (j in seq_len(m)) {

                y <- rnorm(
                    n,
                    mean = 0,
                    sd = 5
                )

                fit <- lm(y ~ x)

                coefs <- summary(fit)$coefficients

                slopes[j] <- coefs[2, 1]
                standard_errors[j] <- coefs[2, 2]

                # Two-sided P-value using the regression t-statistic.

                pvalues[j] <- 2 * pt(
                    -abs(slopes[j] / standard_errors[j]),
                    df = n - 2
                )

                responses[[j]] <- y
            }

            # Select the most extreme gradient.

            best <- which.max(abs(slopes))

            y_best <- responses[[best]]

            fit_best <- lm(y_best ~ x)

            coef_best <- summary(fit_best)$coefficients

            selected_slope <- slopes[best]
            selected_se <- standard_errors[best]

            # Conventional 95% confidence interval.

            t_critical <- qt(
                0.975,
                df = n - 2
            )

            selected_ci <- selected_slope +
                c(-1, 1) * t_critical * selected_se

            list(
                type = "dredge",

                x = x,
                y = y_best,

                coef = coef_best,

                selected = best,
                selected_slope = selected_slope,
                selected_se = selected_se,
                selected_p = pvalues[best],
                selected_ci = selected_ci,

                slopes = slopes,
                pvalues = pvalues,
                standard_errors = standard_errors,

                n = n,
                m = m,
                seed = seed
            )
        }


        # -------------------------------------------------
        # Birthday analysis
        # -------------------------------------------------

        make_birthday_analysis <- function() {

            if (input$birthday_type == "classic") {

                nmax <- 60

                df <- data.frame(
                    N = seq_len(nmax),
                    P = sapply(seq_len(nmax), bp)
                )

                eligible <- which(df$P >= input$p_level)

                required_n <- if (length(eligible)) {
                    df$N[eligible[1]]
                } else {
                    NA_integer_
                }

                return(
                    list(
                        type = "birthday",
                        data = df,
                        required_n = required_n,
                        threshold = input$p_level
                    )
                )
            }

            m <- input$match_size

            n1 <- input$team_small

            n2 <- max(
                input$team_large,
                n1 + 1
            )

            if (input$birthday_method == "dp") {

                p1 <- birthday_dp(n1, m)
                p2 <- birthday_dp(n2, m)

            } else {

                p1 <- birthday_poisson(n1, m)
                p2 <- birthday_poisson(n2, m)
            }

            scenario_names <- c(
                paste(m, "specific people,\nspecific date"),
                paste(m, "specific people,\nany date"),
                paste("Any", m, "in group of", n1),
                paste("Any", m, "in group of", n2)
            )

            probabilities <- c(
                1 / 365^m,
                1 / 365^(m - 1),
                p1,
                p2
            )

            data <- data.frame(
                Scenario = factor(
                    scenario_names,
                    levels = scenario_names
                ),
                k = 1 / probabilities
            )

            list(
                type = "birthday_context",
                data = data,
                probabilities = probabilities,
                method = input$birthday_method,
                scale = input$birthday_scale
            )
        }


        # -------------------------------------------------
        # ITV analysis
        # -------------------------------------------------

        make_prop_analysis <- function() {

            validate(
                need(input$trial1 >= 1, "BBC matches must be at least 1."),
                need(input$trial2 >= 1, "ITV matches must be at least 1."),
                need(input$count1 >= 0 && input$count1 <= input$trial1,
                     "BBC wins must be between 0 and the number of matches."),
                need(input$count2 >= 0 && input$count2 <= input$trial2,
                     "ITV wins must be between 0 and the number of matches.")
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
                mean(d) - qv * se,
                mean(d) + qv * se
            )

            list(
                type = "prop",
                d = d,
                se = se,
                ci = ci,
                estimate = estimate,
                alpha = input$alpha,
                count1 = input$count1,
                count2 = input$count2,
                trial1 = input$trial1,
                trial2 = input$trial2,
                seed = input$seed
            )
        }


        # -------------------------------------------------
        # Run analysis only when Analysis is pressed
        # -------------------------------------------------

        observeEvent(input$analyse, {

            summary_visible(FALSE)

            # Snapshot the current settings.
            # Subsequent slider changes do not alter this run.

            result <- isolate({

                if (input$demo == "Birthday Problem") {

                    make_birthday_analysis()

                } else if (
                    input$demo == "Assessing the ITV jinx"
                ) {

                    make_prop_analysis()

                } else {

                    make_dredge_analysis(
                        n = input$n_data,
                        m = input$n_var,
                        seed = input$seed_dredge
                    )
                }
            })

            # Save the completed result before changing the seed.

            analysis_result(result)

            # Change the seed control for the NEXT analysis.
            # The completed analysis keeps its original seed.

            if (result$type == "dredge") {

                updateNumericInput(
                    session,
                    "seed_dredge",
                    value = sample(
                        setdiff(1:999, result$seed),
                        1
                    )
                )

            } else if (result$type == "prop") {

                updateNumericInput(
                    session,
                    "seed",
                    value = sample(
                        setdiff(1:999, result$seed),
                        1
                    )
                )
            }

        }, ignoreInit = TRUE)


        # -------------------------------------------------
        # Summary button
        # -------------------------------------------------

        observeEvent(input$show_summary, {

            if (!is.null(analysis_result())) {
                summary_visible(TRUE)
            }

        }, ignoreInit = TRUE)


        # -------------------------------------------------
        # Experiment change
        # -------------------------------------------------

        observeEvent(input$demo, {

            analysis_result(NULL)
            summary_visible(FALSE)

            updateTabsetPanel(
                session,
                "chapter_tab",
                selected = "Overview"
            )

        }, ignoreInit = TRUE)


        # -------------------------------------------------
        # Generated R code
        # -------------------------------------------------

        output$generated_code <- renderText({

            a <- analysis_result()

            if (is.null(a)) {
                return(
                    paste(
                        "No analysis has been run yet.",
                        "Choose the settings and press Analysis."
                    )
                )
            }

            if (a$type == "dredge") {

                return(
                    paste0(
                        "# Data dredging: fixed X, repeated Y\n\n",

                        "set.seed(", a$seed, ")\n\n",

                        "n <- ", a$n, "\n",
                        "m <- ", a$m, "\n\n",

                        "# Generate X once\n",
                        "x <- rnorm(n, mean = 0, sd = 10)\n\n",

                        "# Store regression results\n",
                        "slopes <- numeric(m)\n",
                        "pvalues <- numeric(m)\n",
                        "standard_errors <- numeric(m)\n",
                        "responses <- vector('list', m)\n\n",

                        "# Generate a new Y for every regression\n",
                        "for (j in seq_len(m)) {\n",
                        "    y <- rnorm(n, mean = 0, sd = 5)\n",
                        "    fit <- lm(y ~ x)\n",
                        "    coefs <- summary(fit)$coefficients\n",
                        "    slopes[j] <- coefs[2, 1]\n",
                        "    standard_errors[j] <- coefs[2, 2]\n",
                        "    pvalues[j] <- 2 * pt(-abs(slopes[j] / standard_errors[j]), df = n - 2)\n",
                        "    responses[[j]] <- y\n",
                        "}\n\n",

                        "# Select the largest absolute gradient\n",
                        "best <- which.max(abs(slopes))\n\n",

                        "# Selected regression\n",
                        "y_best <- responses[[best]]\n",
                        "fit_best <- lm(y_best ~ x)\n",
                        "selected_slope <- slopes[best]\n",
                        "selected_se <- standard_errors[best]\n\n",

                        "# 95% confidence interval\n",
                        "selected_ci <- selected_slope + ",
                        "c(-1, 1) * qt(0.975, df = n - 2) * selected_se\n\n",

                        "# Numerical results\n",
                        "list(\n",
                        "    selected_regression = best,\n",
                        "    selected_gradient = selected_slope,\n",
                        "    standard_error = selected_se,\n",
                        "    confidence_interval = selected_ci,\n",
                        "    p_value = pvalues[best],\n",
                        "    significant_at_5_percent = sum(pvalues < 0.05),\n",
                        "    number_of_regressions = m\n",
                        ")\n\n",

                        "# Plot the selected regression\n",
                        "plot(x, y_best,\n",
                        "     xlab = 'Explanatory variable (X)',\n",
                        "     ylab = 'Response variable (Y)')\n",
                        "abline(fit_best, col = 'red', lwd = 2)\n",
                        "abline(h = mean(y_best), col = 'grey40', lty = 2)\n\n",

                        "# Pointwise 95% confidence band for the mean response\n",
                        "newdata <- data.frame(\n",
                        "    x = seq(min(x), max(x), length.out = 200)\n",
                        ")\n",
                        "pred <- predict(fit_best, newdata, se.fit = TRUE)\n",
                        "t_critical <- qt(0.975, df = n - 2)\n",
                        "lines(newdata$x, pred$fit + t_critical * pred$se.fit,\n",
                        "      col = 'red', lty = 2)\n",
                        "lines(newdata$x, pred$fit - t_critical * pred$se.fit,\n",
                        "      col = 'red', lty = 2)\n\n",

                        "# Note: the interval is not adjusted for selecting\n",
                        "# the most extreme regression from m attempts."
                    )
                )
            }

            if (a$type == "birthday") {

                return(
                    paste0(
                        "# Classic birthday problem\n\n",
                        "threshold <- ", a$threshold, "\n",
                        "n <- 1:60\n",
                        "probability <- sapply(n, bp)\n",
                        "min(n[probability >= threshold])"
                    )
                )
            }

            if (a$type == "birthday_context") {

                return(
                    paste0(
                        "# Non-classic birthday problem\n\n",
                        "matches <- ", input$match_size, "\n",
                        "groups <- c(",
                        input$team_small, ", ",
                        input$team_large, ")\n",
                        "method <- '", a$method, "'\n\n",
                        "# Use the selected birthday calculation function\n",
                        "if (method == 'dp') {\n",
                        "    probabilities <- sapply(groups, function(n) ",
                        "birthday_dp(n, matches))\n",
                        "} else {\n",
                        "    probabilities <- sapply(groups, function(n) ",
                        "birthday_poisson(n, matches))\n",
                        "}\n",
                        "probabilities"
                    )
                )
            }

            paste0(
                "# Difference in proportions\n\n",
                "set.seed(", a$seed, ")\n",
                "wins <- c(", a$count1, ", ", a$count2, ")\n",
                "matches <- c(", a$trial1, ", ", a$trial2, ")\n",
                "alpha <- ", a$alpha, "\n\n",
                "p <- wins / matches\n",
                "nsim <- 5000\n",
                "s1 <- rbinom(nsim, matches[1], p[1]) / matches[1]\n",
                "s2 <- rbinom(nsim, matches[2], p[2]) / matches[2]\n",
                "d <- s1 - s2\n",
                "se <- sd(d)\n",
                "ci <- mean(d) + c(-1, 1) * ",
                "qnorm((1 + alpha) / 2) * se\n",
                "ci"
            )
        })


        # -------------------------------------------------
        # Plot
        # -------------------------------------------------

        output$plot <- renderPlot({

            a <- analysis_result()

            # Leave the plot blank until Analysis is pressed.

            if (is.null(a)) {
                return(NULL)
            }

            if (a$type == "birthday") {

                return(
                    ggplot(
                        a$data,
                        aes(N, P)
                    ) +
                        geom_line(
                            colour = "#7B9ACC",
                            linewidth = 1
                        ) +
                        geom_point(
                            colour = "#7B9ACC"
                        ) +
                        geom_hline(
                            yintercept = a$threshold,
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
            }

            if (a$type == "birthday_context") {

                plot_data <- a$data

                if (a$scale == "prob") {

                    plot_data$value <- a$probabilities
                    ylab <- "Probability"

                } else {

                    plot_data$value <- log10(
                        pmax(
                            a$probabilities,
                            .Machine$double.xmin
                        )
                    )

                    ylab <- "log10(Probability)"
                }

                return(
                    ggplot(
                        plot_data,
                        aes(Scenario, value)
                    ) +
                        geom_point(
                            size = 4,
                            colour = "#7B9ACC"
                        ) +
                        geom_line(
                            aes(group = 1),
                            colour = "#7B9ACC"
                        ) +
                        theme_minimal(base_size = 14) +
                        theme(
                            axis.text.x = element_text(
                                angle = 30,
                                hjust = 1
                            )
                        ) +
                        labs(
                            x = NULL,
                            y = ylab
                        )
                )
            }

            if (a$type == "prop") {

                return(
                    ggplot(
                        data.frame(d = a$d),
                        aes(d)
                    ) +
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
                        theme_minimal(base_size = 14) +
                        labs(
                            x = "Difference in simulated proportions",
                            y = "Frequency"
                        )
                )
            }

            # -------------------------------------------------
            # Data dredging
            # -------------------------------------------------

            plot_data <- data.frame(
                x = a$x,
                y = a$y
            )

            ggplot(
                plot_data,
                aes(x, y)
            ) +

                # Horizontal reference line at mean of selected Y

                geom_hline(
                    yintercept = mean(a$y),
                    colour = "grey40",
                    linetype = "dashed",
                    linewidth = 0.7
                ) +

                # Pointwise 95% confidence band for the mean response.
                # geom_smooth(method = "lm") uses the regression t-based interval.

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

                theme_minimal(base_size = 14) +

                labs(
                    x = "Explanatory variable (X)",
                    y = "Response variable (Y)",
                    title = paste(
                        "Selected regression:",
                        a$selected,
                        "of",
                        a$m
                    )
                )
        })


        # -------------------------------------------------
        # Results and summary
        # -------------------------------------------------

        output$results_panel <- renderUI({

            a <- analysis_result()

            if (is.null(a)) {
                return(
                    p(
                        "Press Analysis to generate results."
                    )
                )
            }

            if (a$type == "birthday") {

                return(
                    card(
                        card_header("Key result"),

                        p(
                            sprintf(
                                paste0(
                                    "Minimum number of invitees for a ",
                                    "%.0f%% probability of a shared birthday: %d."
                                ),
                                100 * a$threshold,
                                a$required_n
                            )
                        )
                    )
                )
            }

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

                            lapply(
                                seq_along(a$probabilities),

                                function(i) {

                                    tags$tr(
                                        tags$td(
                                            as.character(a$data$Scenario[i])
                                        ),
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
                                                big.mark = ","
                                            )
                                        )
                                    )
                                }
                            )
                        )
                    )
                )
            }

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

                        p(
                            sprintf(
                                "Estimated difference (BBC - ITV): %.3f",
                                a$estimate
                            )
                        ),

                        p(
                            sprintf(
                                "Standard error: %.4f",
                                a$se
                            )
                        ),

                        p(
                            sprintf(
                                "%.0f%% confidence interval: [%.3f, %.3f]",
                                100 * a$alpha,
                                a$ci[1],
                                a$ci[2]
                            )
                        ),

                        hr(),

                        strong(conclusion)
                    )
                )
            }

            # Data-dredging results are shown only when requested.

            if (!summary_visible()) {
                return(
                    p(
                        "Press Summary to display the numerical results."
                    )
                )
            }

            significant <- a$pvalues < 0.05

            card(
                card_header("Data dredging: summary"),

                h5("Selected regression"),

                p(
                    sprintf(
                        "The regression with the largest absolute gradient was repetition %d of %d.",
                        a$selected,
                        a$m
                    )
                ),

                tags$ul(
                    tags$li(
                        sprintf(
                            "Gradient estimate: %.4f",
                            a$selected_slope
                        )
                    ),

                    tags$li(
                        sprintf(
                            "Standard error: %.4f",
                            a$selected_se
                        )
                    ),

                    tags$li(
                        sprintf(
                            paste0(
                                "95%% confidence interval ",
                                "for the gradient: [%.4f, %.4f]"
                            ),
                            a$selected_ci[1],
                            a$selected_ci[2]
                        )
                    ),

                    tags$li(
                        sprintf(
                            "P-value: %.5f",
                            a$selected_p
                        )
                    )
                )
            )
        })
    })
}
