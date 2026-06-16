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
                min = 0.1,
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
            box-shadow: 0 4px 12px rgba(0,0,0,0.08);
            padding: 10px;
        ",

            card_header(
                div(
                    "🌗 Understanding Probability Distributions",
                    style = "
                    font-size: 1.4rem;
                    font-weight: 700;
                    color: #2c3e50;
                "
                )
            ),

            p(
                strong("Main idea: "),
                "A probability distribution describes how likely different
             outcomes are before we observe what actually happens."
            ),

            hr(),

            h5("What is a probability distribution?"),

            p(
                "Probability distributions provide a mathematical description
             of uncertainty. They tell us which outcomes are likely,
             which are unlikely, and how probability is distributed
             across all possible values."
            ),

            hr(),

            h5("Distributions to explore"),

            tags$ul(

                tags$li(
                    strong("Binomial distribution: "),
                    "the number of successes in a fixed number of attempts."
                ),

                tags$li(
                    strong("Poisson distribution: "),
                    "the number of events occurring in a period of time or space."
                ),

                tags$li(
                    strong("Normal distribution: "),
                    "continuous measurements that vary around an average value."
                )
            ),

            hr(),

            h5("Your job"),

            p(
                "Adjust the parameters and investigate how the shape of each
             distribution changes."
            ),

            tags$ul(
                tags$li("Which outcomes are most likely?"),
                tags$li("Which outcomes become less likely?"),
                tags$li("How does changing the parameters alter uncertainty?"),
                tags$li("How are discrete and continuous distributions different?")
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
                        "How does increasing the probability of success affect a Binomial distribution?"
                    ),
                    tags$li(
                        "How does the event rate affect a Poisson distribution?"
                    ),
                    tags$li(
                        "How does the standard deviation affect a Normal distribution?"
                    ),
                    tags$li(
                        "What differences do you notice between discrete and continuous models?"
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
    # LEARN PANEL
    # =========================================================

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

            h5("1. Probability distributions describe uncertainty"),

            p(
                "Rather than predicting a single outcome, a distribution
             describes the range of possible outcomes and their
             probabilities."
            ),

            hr(),

            h5("2. Different problems require different distributions"),

            p(
                "No single probability model works for every situation.
             Binomial, Poisson and Normal distributions are designed
             for different types of data and questions."
            ),

            hr(),

            h5("3. Parameters determine the shape of a distribution"),

            p(
                "Changing the parameter values changes the probabilities.
             The same mathematical model can describe many different
             real-world situations."
            ),

            hr(),

            h5("4. Discrete and continuous uncertainty are different"),

            p(
                "Some variables can take only specific values
             (such as counts of goals or events), while others can vary
             continuously across a range of values."
            ),

            hr(),

            h5("5. Probability is a long-run concept"),

            p(
                "A probability does not guarantee what will happen next.
             Instead, it describes what tends to happen when the same
             process is repeated many times."
            ),

            hr(),

            div(
                style = "
                background-color: #f8f9fa;
                border-left: 5px solid #28a745;
                padding: 12px;
                border-radius: 8px;
            ",

                h5("Key takeaway"),

                p(
                    strong("Probability distributions are models of uncertainty."),
                    br(),
                    "They allow us to quantify how likely different outcomes
                 are and form the foundation of statistical reasoning."
                )
            )
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

                "Poisson" = dpois(
                    input$x_pois,
                    input$lambda
                ),

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

                "Binomial" = paste0(
                    "dbinom(",
                    input$x_bin,
                    ", ",
                    input$n,
                    ", ",
                    input$p,
                    ")"
                ),

                "Poisson" = paste0(
                    "dpois(",
                    input$x_pois,
                    ", ",
                    input$lambda,
                    ")"
                ),

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

                ggplot(data.frame(x, y), aes(x, y)) +
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

            } else if(input$dist == "Poisson"){

                upper <- qpois(0.999, input$lambda)

                x <- 0:upper
                y <- dpois(x, input$lambda)

                ggplot(data.frame(x, y), aes(x, y)) +
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

