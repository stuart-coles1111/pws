# =========================================================
# Chapter 5 — Inference & Regression
# =========================================================


# =========================================================
# Colours
# =========================================================

pal_blue <- "#7B9ACC"
pal_lav  <- "#CDB4DB"
pal_red  <- "#D9534F"
pal_blue_soft <- "#A9BFE3"

# =========================================================
# UI
# =========================================================

chapter5_ui <- function(id){

    ns <- NS(id)
    useShinyjs()

    # =====================================================
    # Sidebar
    # =====================================================

    sidebar_controls <- sidebar(

        h4("Statistics"),

        numericInput(
            ns("seed"),
            "Random seed",
            value = sample(1:999, 1),
            min = 1,
            max = 999
        ),

        radioButtons(
            ns("topic"),
            "Choose topic",
            choices = c(
                "One dice game" = "Inference",
                "Regression" = "Regression"
            ),
            selected = "Inference"
        ),

        # -------------------------------------------------
        # Inference controls
        # -------------------------------------------------

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Inference'",
                ns("topic")
            ),

            hr(),

            h5("The One-Dice game"),

            numericInput(
                ns("n"),
                "Number of dice rolls",
                value = 50,
                min = 10
            ),

            sliderInput(
                ns("p_true"),
                "True probability of rolling a 6",
                min = 0.05,
                max = 0.50,
                value = 0.167,
                step = 0.01
            ),

            numericInput(
                ns("B"),
                "Number of simulations for inference",
                value = 1000,
                min = 100
            ),

            sliderInput(
                ns("conf"),
                "Confidence level",
                min = 0.80,
                max = 0.999,
                value = 0.95,
                step = 0.001
            ),

            selectInput(
                ns("boot_method"),
                "Estimate simulation method",
                choices = c(
                    "Process Simulation (exact)" = "true_p",
                    "Process Simulation (approx)" = "est_p",
                    "Resampling" = "resample"
                )
            ),

            actionButton(
                ns("roll"),
                "Roll Dice",
                class = "btn-primary"
            ),

            actionButton(
                ns("bootstrap"),
                "Simulate Estimates",
                class = "btn-info"
            ),

            actionButton(
                ns("ci"),
                "Confidence Interval",
                class = "btn-secondary"
            )
        ),

        # -------------------------------------------------
        # Regression controls
        # -------------------------------------------------

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Regression'",
                ns("topic")
            ),

            h4("Regression Fitting"),

            selectInput(
                ns("end_season"),
                "Final season included",
                choices = unique(pws::PL_points$season),
                selected = tail(
                    unique(pws::PL_points$season),
                    1
                )
            ),

            sliderInput(
                ns("x_split"),
                "Prediction point",
                min = min(
                    pws::PL_points$points_half1,
                    na.rm = TRUE
                ),
                max = max(
                    pws::PL_points$points_half1,
                    na.rm = TRUE
                ),
                value = median(
                    pws::PL_points$points_half1,
                    na.rm = TRUE
                ),
                step = 1
            ),

            sliderInput(
                ns("conf_reg"),
                "Confidence level",
                min = 0.80,
                max = 0.99,
                value = 0.95,
                step = 0.01
            )
        )
    )

    # =====================================================
    # Overview
    # =====================================================

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
                    "📊 Basics of statistical inference",
                    style = "
                font-size: 1.4rem;
                font-weight: 700;
                color: #2c3e50;
                "
                )
            ),

            p(
                strong("Main idea: "),
                "Chapter 5 of Playing With Statistics discusses general principles of statistics, focusing on some of the main themes of statistical inference and modelling."
            ),

            hr(),

            h5("Topics"),

            p(
                "The module provides tools to explore two topics from Chapter 5 of Playing With Statistics:"
            ),

            tags$ul(
                tags$li(
                    "Statistical inference (using the one-dice game for illustration)"
                ),
                tags$li(
                    "Regression (using points in the Premier League as an example)"
                )
            ),

            hr(),

            h5("Your options"),

            p(
                "Choose to explore either statistical inference or regression."
            ),

            p(
                "If you choose statistical inference, you are guided through a simulation and analysis of the one-dice game, including:"
            ),

            tags$ul(
                tags$li("Simulation of observed data"),
                tags$li("Resampling of probability estimates"),
                tags$li("Calculation of confidence intervals")
            ),

            p(
                "Sidebar options allow you to choose the sample size, the method used to generate new estimates, and the confidence level used for intervals."
            ),

            p(
                "The display includes:
                "
            ),

            tags$ul(
                tags$li(
                    "A bar chart of the simulated data"
                ),
                tags$li(
                    "A histogram of the simulated estimates based on those data"
                ),
                tags$li(
                    "A summary of the inference"
                ),
            ),

            p(
                "If you choose regression, you calculate a linear regression relationship between points scored in the first and second halves of Premier League seasons. The display shows:"
            ),

            tags$ul(
                tags$li(
                    "A graphical representation of the linear model overlaid on the data"
                ),
                tags$li(
                    "Predictions at any chosen point"
                ),
                tags$li(
                    "A confidence band around the fitted regression line"
                ),
                tags$li(
                    "A numerical summary of the regression model"
                )
            ),

            p(
                "Sidebar options allow you to vary the subset of data analysed, the prediction point, and the confidence level used for the confidence band."
            ),
            hr(),

            h5("What to observe"),

            p("For each example, notice how the size of confidence intervals or bands are affected by the choie of sample size and the level of
              confidence chosen."),

            p("Notice also whether the numerical summaries of analyses are more or less effective in conveying information than the graphical summaries."),

            hr(),

            div(

                style = "
            background-color:#f8f9fa;
            border-left:5px solid #7B9ACC;
            padding:12px;
            border-radius:8px;
            ",

                h5("Questions to investigate"),

                p(
                    strong("For statistical inference:")
                ),

                tags$ul(
                    tags$li(
                        "How does the accuracy of estimates depend on the number of simulations?"
                    ),
                    tags$li(
                        "Does the confidence interval always contain the true probability?"
                    ),
                    tags$li(
                        "Does the method used to generate new estimates affect the results?"
                    )
                ),

                p(
                    strong("For regression:")
                ),

                tags$ul(
                    tags$li(
                        "What is the effect of fitting the regression model to smaller datasets?"
                    ),
                    tags$li(
                        "How does changing the confidence level affect the confidence bands?"
                    ),
                    tags$li(
                        "Are confidence intervals the same width at all prediction points?"
                    )
                )

            )

        )

    )

    # =====================================================
    # Generated Code
    # =====================================================

    code_panel <- div(

        card(

            card_header("Generated R code"),

            tags$pre(
                style = "
                background:#F8F9FA;
                padding:15px;
                border-radius:10px;
                font-size:15px;
                ",

                textOutput(ns("generated_code"))
            )
        )
    )

    # =====================================================
    # Results
    # =====================================================

    results_panel <- div(

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Inference'",
                ns("topic")
            ),

            fluidRow(

                column(
                    6,

                    card(
                        card_header("Observed dice outcomes"),

                        plotOutput(
                            ns("dice_plot"),
                            height = 350
                        )
                    )
                ),

                column(
                    6,

                    card(
                        card_header("Estimator distribution"),

                        plotOutput(
                            ns("bootstrap_plot"),
                            height = 350
                        )
                    )
                )
            ),

            br(),

            uiOutput(ns("inference_results"))
        ),

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Regression'",
                ns("topic")
            ),

            card(
                card_header("Regression model"),

                plotOutput(
                    ns("reg_plot"),
                    height = 450
                )
            ),

            br(),

            uiOutput(ns("regression_results"))
        )
    )





    # =====================================================
    # Build chapter
    # =====================================================

    chapter_page_ui(

        id = id,

        title = "📊 Chapter 5: Statistics",

        sidebar = sidebar_controls,

        overview = overview_panel,

        code = code_panel,

        results = results_panel,

        learn = learn_panel,

        activity = activity_panel
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
            ci_active = FALSE,
            stage = "new"
        )

        observe({

            req(input$topic == "Inference")

            if (rv$stage == "new") {

                enable("roll")
                disable("bootstrap")
                disable("ci")

            } else if (rv$stage == "rolled") {

                disable("roll")
                enable("bootstrap")
                disable("ci")

            } else if (rv$stage == "simulated") {

                disable("roll")
                disable("bootstrap")
                enable("ci")

            } else if (rv$stage == "complete") {

                enable("roll")
                disable("bootstrap")
                disable("ci")

            }

        })
        # =====================================================
        # Inference: roll dice
        # =====================================================

        observeEvent(input$roll, {

            new_seed <- sample(1:999, 1)

            updateNumericInput(
                session,
                "seed",
                value = new_seed
            )

            set.seed(new_seed)

            rv$dice <- sample(
                1:6,
                size = input$n,
                replace = TRUE,
                prob = c(
                    rep((1-input$p_true)/5,5),
                    input$p_true
                )
            )

            rv$bootstrap_p <- NULL
            rv$p_hat <- NULL
            rv$se <- NULL
            rv$ci_active <- FALSE

            rv$stage <- "rolled"

        })


        # =====================================================
        # Bootstrap
        # =====================================================

        observeEvent(input$bootstrap, {

            req(rv$dice)

            n <- length(rv$dice)

            rv$bootstrap_p <- replicate(input$B, {

                if (input$boot_method == "true_p") {

                    d <- sample(
                        1:6,
                        size = n,
                        replace = TRUE,
                        prob = c(rep((1 - input$p_true)/5, 5), input$p_true)
                    )

                } else if (input$boot_method == "est_p") {

                    p_hat <- mean(rv$dice == 6)

                    d <- sample(
                        1:6,
                        size = n,
                        replace = TRUE,
                        prob = c(rep((1 - p_hat)/5, 5), p_hat)
                    )

                } else {

                    d <- sample(rv$dice, size = n, replace = TRUE)
                }

                mean(d == 6)
            })

            rv$p_hat <- mean(rv$dice == 6)
            rv$se <- sd(rv$bootstrap_p)

            rv$stage <- "simulated"
        })

        # =====================================================
        # CI activation button
        # =====================================================

        observeEvent(input$ci, {

            req(rv$bootstrap_p)

            rv$ci_active <- TRUE

            rv$stage <- "complete"

        })

        # =====================================================
        # Reactive CI (only if activated)
        # =====================================================

        ci_inference <- reactive({

            req(rv$bootstrap_p, rv$p_hat, rv$se)
            req(rv$ci_active)

            z <- qnorm(1 - (1 - input$conf) / 2)

            c(
                rv$p_hat - z * rv$se,
                rv$p_hat + z * rv$se
            )
        })

        # =====================================================
        # Outputs
        # =====================================================

        output$p_hat_display <- renderText({
            req(rv$p_hat)
            round(rv$p_hat, 3)
        })

        output$se_display <- renderText({
            req(rv$se)
            round(rv$se, 4)
        })

        output$conf_display <- renderText({
            paste0(round(100 * input$conf), "%")
        })

        # =====================================================
        # Generated R code panel
        # =====================================================

        output$generated_code <- renderText({

            if (input$topic == "Inference") {

                code <- paste0(
                    "## One-dice inference investigation

# Generate observed dice rolls
set.seed(", input$seed, ")

dice <- sample(
    1:6,
    size = ", input$n, ",
    replace = TRUE,
    prob = c(
        rep((1 - ", input$p_true, ")/5, 5),
        ", input$p_true, "
    )
)

# Estimate probability of rolling a six
p_hat <- mean(dice == 6)

# Bootstrap distribution
bootstrap_p <- replicate(
    ", input$B, ",
    mean(sample(dice, replace = TRUE) == 6)
)

# Bootstrap standard error
se <- sd(bootstrap_p)

# Confidence interval
z <- qnorm(1 - (1 - ", input$conf, ")/2)

c(
    p_hat - z * se,
    p_hat + z * se
)"
                )

            } else {

                code <- paste0(
                    "## Regression investigation

# Select seasons
data <- subset(
    pws::PL_points,
    season <= '", input$end_season, "'
)

# Fit regression model

model <- lm(
    points_half2 ~ points_half1,
    data = data
)

# Prediction at selected point

predict(
    model,
    newdata = data.frame(
        points_half1 = ", input$x_split, "
    ),
    interval = 'confidence',
    level = ", input$conf_reg, "
)"
                )

            }

            code
        })

# =====================================================
# Dice plot
# =====================================================

output$dice_plot <- renderPlot({

    req(rv$dice)

    df <- data.frame(
        face = factor(rv$dice, levels = 1:6)
    )

    ggplot(df, aes(face)) +

        geom_bar(
            aes(fill = face == "6"),
            colour = "white",
            linewidth = 0.4
        ) +

        scale_fill_manual(
            values = c(
                "FALSE" = "#A9BFE3",  # soft blue (non-6)
                "TRUE"  = "#D9534F"   # highlight red (6)
            ),
            guide = "none"
        ) +

        theme_minimal(base_size = 14) +

        labs(
            x = "Face",
            y = "Frequency"
        )
})

# =====================================================
# Bootstrap plot (CI only after activation)
# =====================================================

output$bootstrap_plot <- renderPlot({

    req(rv$bootstrap_p)

    df <- data.frame(p = rv$bootstrap_p)

    p <- ggplot(df, aes(p)) +
        geom_histogram(
            bins = 30,
            fill = pal_lav,
            colour = "white"
        ) +
        theme_minimal(base_size = 14) +
        labs(x = expression(hat(p)), y = "Frequency")

    if (isTRUE(rv$ci_active)) {

        ci <- ci_inference()

        p <- p +
            annotate(
                "rect",
                xmin = ci[1],
                xmax = ci[2],
                ymin = 0,
                ymax = Inf,
                alpha = 0.15,
                fill = pal_red
            ) +
            geom_vline(
                xintercept = ci,
                colour = pal_red,
                linewidth = 1.2
            )
    }

    p
})

# =====================================================
# Inference summary UI
# =====================================================

output$inference_results <- renderUI({

    req(input$topic == "Inference")


    if (is.null(rv$dice)) {

        return(
        )

    }

    if (is.null(rv$p_hat)) {

        return(
        )

    }

    card(
        card_header("Inference Summary"),

        p(
            strong("Estimated p: "),
            round(rv$p_hat,3)
        ),

        p(
            strong("Bootstrap SE: "),
            round(rv$se,4)
        ),

        if (rv$ci_active) {

            p(
                strong("Confidence Interval: "),
                paste0(
                    "[",
                    round(ci_inference()[1],3),
                    ", ",
                    round(ci_inference()[2],3),
                    "]"
                )
            )

        }
    )

})
# =====================================================
# Regression (unchanged)
# =====================================================

reg_data <- reactive({

    seasons <- unique(pws::PL_points$season)

    end_index <- match(input$end_season, seasons)

    pws::PL_points[
        pws::PL_points$season %in% seasons[1:end_index],
    ]
})

reg_fit <- reactive({
    lm(points_half2 ~ points_half1, data = reg_data())
})

prediction <- reactive({

    predict(
        reg_fit(),
        newdata = data.frame(points_half1 = input$x_split),
        interval = "confidence",
        level = input$conf_reg
    )
})

plot_predictions <- reactive({

    fit <- reg_fit()
    df <- reg_data()

    grid <- data.frame(
        points_half1 = seq(
            min(df$points_half1, na.rm = TRUE),
            max(df$points_half1, na.rm = TRUE),
            length.out = 100
        )
    )

    preds <- predict(
        fit,
        newdata = grid,
        interval = "confidence",
        level = input$conf_reg
    )

    cbind(grid, preds)
})

output$reg_plot <- renderPlot({

    df <- reg_data()

    plot_df <- plot_predictions()

    pr <- prediction()

    ggplot(

        df,

        aes(
            points_half1,
            points_half2
        )

    ) +

        # observations
        geom_point(
            colour = pal_blue
        ) +

        # confidence band
        geom_ribbon(

            data = plot_df,

            aes(
                x = points_half1,
                ymin = lwr,
                ymax = upr
            ),

            fill = pal_lav,

            alpha = 0.20,

            inherit.aes = FALSE
        ) +

        # regression line
        geom_line(

            data = plot_df,

            aes(
                x = points_half1,
                y = fit
            ),

            colour = pal_lav,

            linewidth = 1.2,

            inherit.aes = FALSE
        ) +

        # vertical prediction line
        geom_vline(

            xintercept = input$x_split,

            colour = pal_red,

            linetype = "dashed",

            linewidth = 0.8
        ) +

        # horizontal prediction line
        geom_hline(

            yintercept = as.numeric(pr[1, "fit"]),

            colour = pal_red,

            linetype = "dashed",

            linewidth = 0.8
        ) +

        # prediction point

        annotate(
            "point",
            x = input$x_split,
            y = as.numeric(pr[1, "fit"]),
            colour = pal_red,
            size = 4
        ) +

        theme_minimal(

            base_size = 14
        ) +

        labs(

            x = "Points (Half 1)",

            y = "Points (Half 2)"
        )
})

output$regression_results <- renderUI({

    fit <- reg_fit()
    pr <- prediction()

    card(

        card_header("Regression Summary"),

        p(strong("Observations: "), nrow(reg_data())),

        p(strong("Slope: "), round(coef(fit)[2], 3)),

        p(strong("Intercept: "), round(coef(fit)[1], 1)),

        p(strong("Prediction: "), round(pr[1, "fit"], 1)),

        p(
            strong("Confidence Interval: "),
            paste0(
                "[",
                round(pr[1, "lwr"], 1),
                ", ",
                round(pr[1, "upr"], 1),
                "]"
            )
        )
    )
})

    })
}
