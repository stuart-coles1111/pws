# =========================================================
# Chapter 2 UI
# =========================================================

chapter2_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("Understanding Probability Distributions"),

        p("Explore the Binomial, Poisson, and Normal distributions"),

        hr(),

        # -----------------------------------------------------
        # DISTRIBUTION SELECTOR
        # -----------------------------------------------------

        selectInput(
            ns("dist"),
            "Distribution",
            choices = c("Binomial", "Poisson", "Normal")
        ),

        hr(),

        # =====================================================
        # BINOMIAL
        # =====================================================

        conditionalPanel(
            condition = sprintf("input['%s'] == 'Binomial'", ns("dist")),

            sliderInput(ns("n"), "n (trials)", min = 1, max = 100, value = 10, step = 1),
            sliderInput(ns("p"), "p (success probability)", min = 0, max = 1, value = 0.5, step = 0.01),
            sliderInput(
                ns("x_bin"),
                "x (successes)",
                min = 0,
                max = 10,  # initial value, updated reactively
                value = 7,
                step = 1
            ),
            checkboxInput(
                ns("show_comparison"),
                "Overlay Normal approximation",
                FALSE
            )
            ),

        # =====================================================
        # POISSON
        # =====================================================

        conditionalPanel(
            condition = sprintf("input['%s'] == 'Poisson'", ns("dist")),

            sliderInput(
                ns("lambda"),
                "λ (rate)",
                min = 0,
                max = 50,  # initial value, updated reactively
                value = 10,
                step = 0.1
            ),
            sliderInput(
                ns("x_pois"),
                "x (events)",
                min = 0,
                max = 25,  # initial value, updated reactively
                value = 10,
                step = 1
            ),
            checkboxInput(
                ns("show_comparison"),
                "Overlay Normal approximation",
                FALSE
            )
        ),

        # =====================================================
        # NORMAL
        # =====================================================

        conditionalPanel(
            condition = sprintf("input['%s'] == 'Normal'", ns("dist")),

            sliderInput(
                ns("mean"),
                "μ (mean)",
                min = -20,
                max = 20,
                value = 1,
                step = 0.1
            ),

            sliderInput(
                ns("sd"),
                "σ (standard deviation)",
                min = 0.1,
                max = 10,
                value = 3,
                step = 0.1
            ),

            sliderInput(
                ns("interval"),
                "Interval for X",
                min = -10,
                max = 10,
                value = c(-1, 2),
                step = 0.1
            )
        )
    )

    # =========================================================
    # OVERVIEW PANEL
    # =========================================================
    overview_panel <- div(

        card(

            style = "
        border-radius: 16px;
        border: none;
        box-shadow: 4px 12px 12px rgba(0,0,0,0.08);
        padding: 10px;
    ",

            card_header(
                div(
                    "🌗 Exploring Probability Distributions",
                    style = "
                font-size: 1.4rem;
                font-weight: 700;
                color: #2c3e50;
            "
                )
            ),

            p(
                strong("Main idea: "),
                "This module extends the statistical toolkit by allowing exploration of, ",
                "and calculation from, the standard probability distributions introduced ",
                "in Playing With Statistics."
            ),

            hr(),

            h5("The available distributions"),

            tags$ul(

                tags$li(
                    strong("Binomial distribution: "),
                    "the number of successes in a fixed number of independent trials, ",
                    "where each trial has the same probability of success."
                ),

                tags$li(
                    strong("Poisson distribution: "),
                    "the number of events occurring in a fixed interval of time or space."
                ),

                tags$li(
                    strong("Normal distribution: "),
                    "a continuous distribution used to model measurements that vary around ",
                    "an average value."
                )

            ),

            p(
                "Each of these distributions is discussed in detail in Chapter 2 ",
                "of Playing With Statistics."
            ),

            hr(),

            h5("Your options"),

            p(
                "You choose the distribution you wish to explore. Depending on your choice, ",
                "you then select the relevant distribution parameters:"
            ),

            tags$ul(

                tags$li(
                    strong("Binomial distribution: "),
                    "n, the number of trials; p, the probability of success in each trial."
                ),

                tags$li(
                    strong("Poisson distribution: "),
                    "λ, the average rate of events over the fixed time interval."
                ),

                tags$li(
                    strong("Normal distribution: "),
                    "μ, the mean; σ, the standard deviation."
                )

            ),

            p(
                "For the Binomial and Poisson distributions, you choose a particular value ",
                "of x and calculate its probability. For the Normal distribution, you specify ",
                "an interval [a,b] and calculate the probability that X lies within that range."
            ),

            p(
                "For the Binomial and Poisson distributions, you can optionally overlay a ",
                "Normal approximation with the same mean and standard deviation. This allows ",
                "you to investigate when a continuous model provides a good approximation ",
                "to a discrete distribution."
            ),

            hr(),

            h5("Exploring the distributions"),

            p(
                "The module can be used as a simple probability calculator. Choose a ",
                "distribution, select its parameters, and calculate probabilities for ",
                "particular values or intervals."
            ),

            p(
                "It can also be used for experimentation. Adjust the parameters and ",
                "observe how the shape and spread of each distribution change."
            ),

            tags$ul(

                tags$li(
                    "Adjust the parameters and observe how the distribution changes."
                ),

                tags$li(
                    "For Binomial and Poisson distributions, try overlaying a Normal ",
                    "distribution and compare the shapes."
                )

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
                        "How does increasing the probability of success affect the shape ",
                        "of the Binomial distribution?"
                    ),

                    tags$li(
                        "How does changing the event rate affect the shape of the ",
                        "Poisson distribution?"
                    ),

                    tags$li(
                        "How do the mean and standard deviation affect a Normal distribution?"
                    ),

                    tags$li(
                        "In which situations does the Normal distribution provide a ",
                        "reasonable approximation to the Binomial or Poisson distributions?"
                    )

                )
            )
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
            card_header("Distribution plot"),
            plotOutput(ns("plot"), height = 350)
        ),

        br(),

        card(
            card_header("Result"),
            h2(textOutput(ns("result")))
        )
    )




    # =========================================================
    # BUILD PAGE
    # =========================================================

    chapter_page_ui(
        id = id,
        title = "🌗 Chapter 2: Probability",
        sidebar = sidebar_controls,
        overview = overview_panel,
        code = code_panel,
        results = results_panel,
        learn = learn_panel
        )
}

# =========================================================
# Chapter 2 SERVER
# =========================================================


chapter2_server <- function(id){

    moduleServer(id, function(input, output, session){

        # =====================================================
        # helper
        # =====================================================

        fmt <- function(x){
            sprintf("%.3f", round(x, 3))
        }

        bounds <- reactive({
            req(input$interval)

            list(
                a = input$interval[1],
                b = input$interval[2]
            )
        })

        # =====================================================
        # main result
        # =====================================================

        result <- reactive({

            switch(

                input$dist,

                "Binomial" = dbinom(
                    input$x_bin,
                    input$n,
                    input$p
                ),

                "Poisson" = {
                    req(input$lambda > 0)

                    dpois(
                        input$x_pois,
                        input$lambda
                    )
                },

                "Normal" = pnorm(
                    bounds()$b,
                    input$mean,
                    input$sd
                ) -
                    pnorm(
                        bounds()$a,
                        input$mean,
                        input$sd
                    )
            )
        })


        observeEvent(input$n, {
            updateSliderInput(
                session,
                "x_bin",
                min = 0,
                max = input$n,
                value = min(input$x_bin, input$n)
            )
        })



        observeEvent(input$lambda, {

            req(input$lambda > 0)

            upper <- qpois(0.999, input$lambda)

            updateSliderInput(
                session,
                "x_pois",
                min = 0,
                max = upper,
                value = min(input$x_pois, upper)
            )
        })

        observe({

            lower <- floor(input$mean - 4 * input$sd)
            upper <- ceiling(input$mean + 4 * input$sd)

            current <- input$interval

            updateSliderInput(
                session,
                "interval",
                min = lower,
                max = upper,
                value = c(
                    max(current[1], lower),
                    min(current[2], upper)
                )
            )
        })

        # =====================================================
        # CODE OUTPUT
        # =====================================================

        output$code <- renderText({

            switch(

                input$dist,

                "Binomial" = {

                    code <- paste0(
                        "dbinom(",
                        input$x_bin,
                        ", ",
                        input$n,
                        ", ",
                        input$p,
                        ")"
                    )

                    if (isTRUE(input$show_comparison)) {

                        code <- paste(
                            code,
                            "",
                            "# Normal approximation",
                            paste0(
                                "dnorm(x, mean = ",
                                input$n * input$p,
                                ", sd = ",
                                round(
                                    sqrt(
                                        input$n *
                                            input$p *
                                            (1 - input$p)
                                    ),
                                    3
                                ),
                                ")"
                            ),
                            sep = "\n"
                        )
                    }

                    code
                },

                "Poisson" = {

                    code <- paste0(
                        "dpois(",
                        input$x_pois,
                        ", ",
                        input$lambda,
                        ")"
                    )

                    if (isTRUE(input$show_comparison)) {

                        code <- paste(
                            code,
                            "",
                            "# Normal approximation",
                            paste0(
                                "dnorm(x, mean = ",
                                input$lambda,
                                ", sd = ",
                                round(
                                    sqrt(input$lambda),
                                    3
                                ),
                                ")"
                            ),
                            sep = "\n"
                        )
                    }

                    code
                },

                "Normal" = paste0(
                    "pnorm(",
                    bounds()$b,
                    ", ",
                    input$mean,
                    ", ",
                    input$sd,
                    ") - ",
                    "pnorm(",
                    bounds()$a,
                    ", ",
                    input$mean,
                    ", ",
                    input$sd,
                    ")"
                )
            )
        })

        # =====================================================
        # RESULT DISPLAY
        # =====================================================

        output$result <- renderText({

            validate(
                need(
                    !(input$dist == "Poisson" && input$lambda <= 0),
                    "λ must be strictly positive"
                )
            )

            switch(

                input$dist,

                "Binomial" = paste0(
                    "P(X = ",
                    input$x_bin,
                    ") = ",
                    fmt(result())
                ),

                "Poisson" = paste0(
                    "P(X = ",
                    input$x_pois,
                    ") = ",
                    fmt(result())
                ),

                "Normal" = paste0(
                    "P(",
                    input$interval[1],
                    " ≤ X ≤ ",
                    input$interval[2],
                    ") = ",
                    fmt(result())
                )
            )
        })

        # =====================================================
        # PLOT
        # =====================================================

        output$plot <- renderPlot({

            if(input$dist == "Binomial"){

                x <- 0:input$n
                y <- dbinom(x, input$n, input$p)

                p <- ggplot(data.frame(x, y), aes(x, y)) +
                    geom_col(fill = "#7B9ACC") +
                    geom_col(
                        data = data.frame(
                            x = input$x_bin,
                            y = dbinom(
                                input$x_bin,
                                input$n,
                                input$p
                            )
                        ),
                        fill = "#E76F51"
                    ) +
                    theme_minimal(base_size = 14) +
                    labs(
                        x = "Number of successes",
                        y = "Probability"
                    )

                if (isTRUE(input$show_comparison)) {

                    mu <- input$n * input$p

                    sigma <- sqrt(
                        input$n *
                            input$p *
                            (1 - input$p)
                    )

                    p <- p +
                        stat_function(
                            fun = dnorm,
                            args = list(
                                mean = mu,
                                sd = sigma
                            ),
                            linewidth = 1.2,
                            colour = "#E76F51"
                        )
                }

                p

            } else if(input$dist == "Poisson"){

                req(input$lambda > 0)

                upper <- qpois(0.999, input$lambda)

                x <- 0:upper
                y <- dpois(x, input$lambda)

                p <- ggplot(data.frame(x, y), aes(x, y)) +
                    geom_col(fill = "#CDB4DB") +
                    geom_col(
                        data = data.frame(
                            x = input$x_pois,
                            y = dpois(
                                input$x_pois,
                                input$lambda
                            )
                        ),
                        fill = "#E76F51"
                    ) +
                    theme_minimal(base_size = 14) +
                    labs(
                        x = "Number of events",
                        y = "Probability"
                    )

                if (isTRUE(input$show_comparison)) {

                    p <- p +
                        stat_function(
                            fun = dnorm,
                            args = list(
                                mean = input$lambda,
                                sd = sqrt(input$lambda)
                            ),
                            linewidth = 1.2,
                            colour = "#7B9ACC"
                        )
                }

                p

            } else {

                x <- seq(
                    input$mean - 4 * input$sd,
                    input$mean + 4 * input$sd,
                    length.out = 300
                )

                y <- dnorm(
                    x,
                    input$mean,
                    input$sd
                )

                df <- data.frame(x, y)

                ggplot(df, aes(x, y)) +
                    geom_line(
                        color = "#7B9ACC",
                        linewidth = 1.2
                    ) +
                    geom_area(
                        data = subset(
                            df,
                            x >= bounds()$a & x <= bounds()$b
                        ),
                        fill = "#CDB4DB",
                        alpha = 0.6
                    ) +
                    theme_minimal(base_size = 14) +
                    labs(
                        x = "x",
                        y = "Density"
                    )
            }
        })
    })
}

