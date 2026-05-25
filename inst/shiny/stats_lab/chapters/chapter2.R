library(shiny)
library(ggplot2)
library(DT)

# =========================================================
# Chapter 2 UI
# =========================================================

chapter2_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("Distribution explorer"),

        p("Explore binomial, poisson, and normal probabilities."),

        # -----------------------------------------------------
        # GLOBAL ACCURACY CONTROL
        # -----------------------------------------------------

        numericInput(
            ns("digits"),
            "Accuracy (decimal places)",
            value = 3,
            min = 0,
            max = 10
        ),

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
            numericInput(ns("sd"), "SD", 3),

            numericInput(ns("a"), "Lower bound", -1),
            numericInput(ns("b"), "Upper bound", 2)
        )
    )

    # =========================================================
    # OVERVIEW PANEL
    # =========================================================

    overview_panel <- card(

        card_header("What this chapter explores"),

        tags$p("This chapter introduces probability distributions."),

        tags$ul(
            tags$li("Binomial distribution: counts of successes"),
            tags$li("Poisson distribution: counts of events"),
            tags$li("Normal distribution: continuous variation via intervals")
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
            card_header("Result"),

            h2(textOutput(ns("result")))
        ),

        br(),

        card(
            card_header("Distribution plot"),
            plotOutput(ns("plot"), height = 350)
        ),

        br(),

        card(
            card_header("Values / probabilities"),
            DTOutput(ns("table"))
        )
    )

    # =========================================================
    # LEARN PANEL
    # =========================================================

    learn_panel <- card(

        card_header("Key ideas"),

        tags$ul(
            tags$li("Binomial and Poisson are discrete probability models."),
            tags$li("Normal distribution is continuous."),
            tags$li("Normal probabilities are always intervals (areas)."),
            tags$li("The same probability function depends on parameter values.")
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

        fmt <- function(x) round(x, input$digits)

        # =====================================================
        # main result
        # =====================================================

        result <- reactive({

            switch(

                input$dist,

                "Binomial" = dbinom(input$x_bin, input$n, input$p),

                "Poisson" = dpois(input$x_pois, input$lambda),

                "Normal" = pnorm(input$b, input$mean, input$sd) -
                    pnorm(input$a, input$mean, input$sd)
            )
        })

        # =====================================================
        # CODE OUTPUT
        # =====================================================

        output$code <- renderText({

            switch(

                input$dist,

                "Binomial" = paste0(
                    "dbinom(", input$x_bin, ", ", input$n, ", ", input$p, ")"
                ),

                "Poisson" = paste0(
                    "dpois(", input$x_pois, ", ", input$lambda, ")"
                ),

                "Normal" = paste0(
                    "pnorm(", input$b, ", ", input$mean, ", ", input$sd, ") - ",
                    "pnorm(", input$a, ", ", input$mean, ", ", input$sd, ")"
                )
            )
        })

        # =====================================================
        # RESULT DISPLAY
        # =====================================================

        output$result <- renderText({
            fmt(result())
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
                    theme_minimal(base_size = 14) +
                    labs(x = "x", y = "Probability")

            } else if(input$dist == "Poisson"){

                x <- 0:max(15, input$x_pois + 10)
                y <- dpois(x, input$lambda)

                ggplot(data.frame(x, y), aes(x, y)) +
                    geom_col(fill = "#CDB4DB") +
                    theme_minimal(base_size = 14) +
                    labs(x = "x", y = "Probability")

            } else {

                x <- seq(input$mean - 4*input$sd,
                         input$mean + 4*input$sd,
                         length.out = 300)

                y <- dnorm(x, input$mean, input$sd)

                df <- data.frame(x, y)

                ggplot(df, aes(x, y)) +
                    geom_line(color = "#7B9ACC", linewidth = 1.2) +
                    geom_area(
                        data = subset(df, x >= input$a & x <= input$b),
                        fill = "#CDB4DB",
                        alpha = 0.5
                    ) +
                    theme_minimal(base_size = 14) +
                    labs(x = "x", y = "Density")
            }
        })

        # =====================================================
        # TABLE OUTPUT
        # =====================================================

        output$table <- renderDT({

            if(input$dist == "Binomial"){

                data.frame(
                    x = 0:input$n,
                    prob = round(dbinom(0:input$n, input$n, input$p), input$digits)
                )

            } else if(input$dist == "Poisson"){

                x <- 0:15

                data.frame(
                    x = x,
                    prob = round(dpois(x, input$lambda), input$digits)
                )

            } else {

                # NORMAL: ONLY interval probability (no density table)
                data.frame(
                    interval = paste0("[", input$a, ", ", input$b, "]"),
                    probability = round(
                        pnorm(input$b, input$mean, input$sd) -
                            pnorm(input$a, input$mean, input$sd),
                        input$digits
                    )
                )
            }
        })
    })
}
