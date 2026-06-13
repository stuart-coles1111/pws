# =========================================================
# Chapter 2 UI
# =========================================================

chapter2_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("Distribution explorer"),

        p("Explore binomial, poisson, and normal probabilities."),

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

            numericInput(ns("n"), "n (trials)", 10, min = 1),
            numericInput(ns("p"), "p (success probability)", 0.5, min = 0, max = 1),
            numericInput(ns("x_bin"), "x (successes)", 7, min = 0)
        ),

        # =====================================================
        # POISSON
        # =====================================================

        conditionalPanel(
            condition = sprintf("input['%s'] == 'Poisson'", ns("dist")),

            numericInput(ns("lambda"), "λ (rate)", 2.5, min = 0.1),
            numericInput(ns("x_pois"), "x (events)", 3, min = 0)
        ),

        # =====================================================
        # NORMAL
        # =====================================================

        conditionalPanel(
            condition = sprintf("input['%s'] == 'Normal'", ns("dist")),

            numericInput(ns("mean"), "Mean", 1),
            numericInput(ns("sd"), "SD", 3, min = 0.001),

            numericInput(ns("a"), "Lower bound", -1),
            numericInput(ns("b"), "Upper bound", 2)
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
    # ACTIVITY PANEL
    # =========================================================


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
                    "Activity 2",
                    style = "
                    font-size: 1.4rem;
                    font-weight: 700;
                    color: #2c3e50;
                "
                )
            ),

            uiOutput(ns("launch_activity_ui"))
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
        learn = learn_panel,
        activity = activity_panel
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
                    input$b,
                    input$mean,
                    input$sd
                ) -
                    pnorm(
                        input$a,
                        input$mean,
                        input$sd
                    )
            )
        })

        activity_url <- reactiveVal(NULL)

        observe({

            if (is.null(activity_url())) {

                activity_url(
                    pws:::run_activity(2)
                )

            }

        })

        output$launch_activity_ui <- renderUI({

            req(activity_url())

            tags$a(
                href = activity_url(),
                target = "_blank",
                class = "btn btn-success",
                "Launch Activity 2: Who Wants to be a Danish Millionaire?"
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
                    input$b,
                    ", ",
                    input$mean,
                    ", ",
                    input$sd,
                    ") - ",
                    "pnorm(",
                    input$a,
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
                    input$a,
                    " ≤ X ≤ ",
                    input$b,
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

                x <- 0:max(15, input$x_pois + 10)
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
                            x >= input$a & x <= input$b
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

