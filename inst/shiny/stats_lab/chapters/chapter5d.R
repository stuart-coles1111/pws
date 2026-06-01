

# =========================================================
# Chapter 5 — Inference (Dice) + Regression (PL_points)
# =========================================================

library(shiny)
library(ggplot2)
library(pws)

# =========================================================
# Shared palette (consistent across chapters)
# =========================================================

pal_blue <- "#7B9ACC"
pal_lav  <- "#CDB4DB"
pal_red  <- "#D9534F"

# =========================================================
# UI
# =========================================================

chapter5_ui <- function(id){

    ns <- NS(id)



    tagList(

        # =====================================================
        # SIDEBAR MODE SELECTOR (global control)
        # =====================================================

        div(
            class = "card-style",

            selectInput(
                ns("mode"),
                "Choose experiment:",
                choices = c(
                    "🎲 Inference (Dice)" = "inference",
                    "📈 Regression (PL_points)" = "regression"
                )
            )
        ),

        # =====================================================
        # INFERENCE TAB CONTENT
        # =====================================================

        conditionalPanel(
            condition = sprintf("input['%s'] == 'inference'", ns("mode")),

            sidebarLayout(

                sidebarPanel(

                    h4("Dice experiment controls"),

                    numericInput(ns("n"), "Number of dice rolls:", 50, min = 10),

                    sliderInput(
                        ns("p_true"),
                        "True probability of rolling a 6:",
                        0.05, 0.5, 0.167, step = 0.01
                    ),

                    numericInput(ns("B"), "Bootstrap simulations:", 1000, min = 100),

                    sliderInput(
                        ns("conf"),
                        "Confidence level:",
                        0.80, 0.99, 0.95, step = 0.01
                    ),

                    selectInput(
                        ns("boot_method"),
                        "Bootstrap method:",
                        choices = c(
                            "Simulation (true p)" = "true_p",
                            "Simulation (estimated p)" = "est_p",
                            "Resampling from data" = "resample"
                        )
                    ),

                    div(
                        style = "display:flex; flex-direction:column; gap:10px; margin-top:12px;",

                        actionButton(ns("roll"), "Roll Dice", class = "btn-primary"),
                        actionButton(ns("bootstrap"), "Bootstrap", class = "btn-primary"),
                        actionButton(ns("ci"), "Confidence Interval", class = "btn-primary")
                    )
                ),

                mainPanel(

                    div(
                        class = "card-style",

                        h3("🎲 Inference — Dice experiment"),

                        p("Estimate probabilities, simulate sampling variation, and construct confidence intervals.")
                    ),

                    plotOutput(ns("dice_plot"), height = "250px"),
                    hr(),
                    plotOutput(ns("bootstrap_plot"), height = "320px"),
                    hr(),
                    uiOutput(ns("results"))
                )
            )
        ),

        # =====================================================
        # REGRESSION TAB CONTENT
        # =====================================================

        conditionalPanel(
            condition = sprintf("input['%s'] == 'regression'", ns("mode")),

            sidebarLayout(

                sidebarPanel(

                    h4("Regression controls"),

                    sliderInput(
                        ns("x_split"),
                        "Prediction point (points_half1):",
                        min = min(pws::PL_points$points_half1, na.rm = TRUE),
                        max = max(pws::PL_points$points_half1, na.rm = TRUE),
                        value = median(pws::PL_points$points_half1, na.rm = TRUE),
                        step = 1
                    ),

                    sliderInput(
                        ns("conf_reg"),
                        "Confidence level:",
                        min = 0.80,
                        max = 0.99,
                        value = 0.95,
                        step = 0.01
                    )
                ),

                mainPanel(

                    div(
                        class = "card-style",

                        h3("📈 Regression — PL_points model"),

                        p("Fit a linear model and explore prediction uncertainty.")
                    ),

                    plotOutput(ns("reg_plot"), height = "420px"),
                    hr(),
                    uiOutput(ns("reg_results"))
                )
            )
        )
    )
}

# =========================================================
# SERVER
# =========================================================

chapter5_server <- function(id){

    moduleServer(id, function(input, output, session){

        rv <- reactiveValues(
            dice = NULL,
            bootstrap_p = NULL,
            p_hat = NULL,
            se = NULL,
            ci = NULL,
            reg_data = NULL
        )

        # =====================================================
        # INFERENCE
        # =====================================================

        observeEvent(input$roll, {

            rv$dice <- sample(
                1:6,
                input$n,
                replace = TRUE,
                prob = c(rep((1 - input$p_true)/5, 5), input$p_true)
            )

            rv$bootstrap_p <- NULL
            rv$ci <- NULL
        })

        output$dice_plot <- renderPlot({

            req(rv$dice)

            ggplot(
                data.frame(face = factor(rv$dice, levels = 1:6)),
                aes(face)
            ) +
                geom_bar(fill = pal_blue) +
                theme_minimal()
        })

        observeEvent(input$bootstrap, {

            req(rv$dice)

            B <- input$B
            n <- length(rv$dice)

            rv$bootstrap_p <- replicate(B, {

                if (input$boot_method == "resample") {

                    d <- sample(rv$dice, n, replace = TRUE)

                } else {

                    p_hat <- mean(rv$dice == 6)

                    d <- sample(
                        1:6, n, replace = TRUE,
                        prob = c(rep((1 - p_hat)/5, 5), p_hat)
                    )
                }

                mean(d == 6)
            })

            rv$p_hat <- mean(rv$dice == 6)
            rv$se <- sd(rv$bootstrap_p)
        })

        observeEvent(input$ci, {

            req(rv$bootstrap_p)

            k <- qnorm(1 - (1 - input$conf)/2)

            rv$ci <- c(
                rv$p_hat - k * rv$se,
                rv$p_hat + k * rv$se
            )
        })

        output$bootstrap_plot <- renderPlot({

            req(rv$bootstrap_p)

            df <- data.frame(p = rv$bootstrap_p)

            p <- ggplot(df, aes(p)) +
                geom_histogram(bins = 30, fill = pal_lav) +
                theme_minimal() +
                labs(x = "Bootstrap p̂", y = "Frequency")

            if (!is.null(rv$ci)) {

                p <- p +
                    annotate(
                        "rect",
                        xmin = rv$ci[1], xmax = rv$ci[2],
                        ymin = 0, ymax = Inf,
                        alpha = 0.15,
                        fill = pal_red
                    ) +
                    geom_vline(xintercept = rv$ci[1], color = pal_red, linewidth = 1.1) +
                    geom_vline(xintercept = rv$ci[2], color = pal_red, linewidth = 1.1)
            }

            p
        })

        output$results <- renderUI({

            req(rv$bootstrap_p)

            HTML(paste0(
                "<div class='message-panel'>",
                "<h4>Inference Summary</h4>",
                "<p><b>p̂:</b> ", round(rv$p_hat, 3), "</p>",
                "<p><b>SE:</b> ", round(rv$se, 4), "</p>",
                if (!is.null(rv$ci)) {
                    paste0("<p><b>CI:</b> [",
                           round(rv$ci[1], 3), ", ",
                           round(rv$ci[2], 3), "]</p>")
                } else {
                    "<p><i>CI not computed</i></p>"
                },
                "</div>"
            ))
        })

        # =====================================================
        # REGRESSION
        # =====================================================

        observe({
            rv$reg_data <- pws::PL_points
        })

        reg_fit <- reactive({
            req(rv$reg_data)
            lm(points_half2 ~ points_half1, data = rv$reg_data)
        })

        pred <- reactive({

            req(reg_fit())

            predict(
                reg_fit(),
                newdata = data.frame(points_half1 = input$x_split),
                interval = "confidence",
                level = input$conf_reg
            )
        })

        output$reg_plot <- renderPlot({

            req(rv$reg_data)

            df <- rv$reg_data
            fit <- reg_fit()

            grid <- data.frame(
                points_half1 = seq(min(df$points_half1),
                                   max(df$points_half1),
                                   length.out = 100)
            )

            preds <- predict(fit, newdata = grid, interval = "confidence")

            plot_df <- cbind(grid, preds)

            p <- ggplot(df, aes(points_half1, points_half2)) +
                geom_point(color = pal_blue) +

                geom_line(
                    data = plot_df,
                    aes(points_half1, fit),
                    color = pal_lav,
                    linewidth = 1.2,
                    inherit.aes = FALSE
                ) +

                geom_ribbon(
                    data = plot_df,
                    aes(points_half1, ymin = lwr, ymax = upr),
                    alpha = 0.2,
                    fill = pal_lav,
                    inherit.aes = FALSE
                ) +

                theme_minimal(base_size = 14)

            pr <- pred()

            p +
                annotate(
                    "point",
                    x = input$x_split,
                    y = pr[1, "fit"],
                    color = pal_red,
                    size = 4
                )
        })

        output$reg_results <- renderUI({

            req(reg_fit())

            pr <- pred()

            HTML(paste0(
                "<div class='message-panel'>",
                "<h4>Regression Summary</h4>",
                "<p><b>Slope:</b> ",
                round(coef(reg_fit())[2], 3), "</p>",
                "<p><b>Intercept:</b> ",
                round(coef(reg_fit())[1], 3), "</p>",
                "<p><b>Prediction:</b> ",
                round(pr[1, "fit"], 3), "</p>",
                "<p><b>CI:</b> [",
                round(pr[1, "lwr"], 3), ", ",
                round(pr[1, "upr"], 3), "]</p>",
                "</div>"
            ))
        })
    })
}
