# =========================================================
# Chapter 5 — Inference & Regression
# =========================================================


# =========================================================
# Colours
# =========================================================

pal_blue <- "#7B9ACC"
pal_lav  <- "#CDB4DB"
pal_red  <- "#D9534F"

# =========================================================
# UI
# =========================================================

chapter5_ui <- function(id){

    ns <- NS(id)

    # =====================================================
    # Sidebar
    # =====================================================

    sidebar_controls <- sidebar(

        numericInput(
            ns("seed"),
            "Random seed",
            value = sample(
                1:999,
                1
            ),
            min = 1,
            max = 999
        ),

        radioButtons(
            ns("topic"),
            "Choose topic",
            choices = c(
                "Inference",
                "Regression"
            )
        ),

        # -------------------------------------------------
        # Inference controls
        # -------------------------------------------------

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Inference'",
                ns("topic")
            ),

            h4("Dice experiment controls"),

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
                "Bootstrap simulations",
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
                "Bootstrap method",
                choices = c(
                    "Simulation (true p)" = "true_p",
                    "Simulation (estimated p)" = "est_p",
                    "Resampling from data" = "resample"
                )
            ),

            actionButton(
                ns("roll"),
                "Roll Dice",
                class = "btn-primary"
            ),

            actionButton(
                ns("bootstrap"),
                "Bootstrap",
                class = "btn-primary"
            ),

            actionButton(
                ns("ci"),
                "Confidence Interval",
                class = "btn-primary"
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

            h4("Regression controls"),

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

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Inference'",
                ns("topic")
            ),

            card(

                card_header("What this topic explores"),

                p("
Bootstrap methods allow us to estimate
uncertainty using repeated simulation.
"),

                tags$ul(
                    tags$li("Sampling variability"),
                    tags$li("Bootstrap distributions"),
                    tags$li("Standard errors"),
                    tags$li("Confidence intervals")
                )
            )
        ),

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Regression'",
                ns("topic")
            ),

            card(

                card_header("What this topic explores"),

                p("
Regression models relationships
between quantitative variables.
"),

                tags$ul(
                    tags$li("Linear trends"),
                    tags$li("Predictions"),
                    tags$li("Slope"),
                    tags$li("Confidence intervals")
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
                    4,

                    card(
                        h4("Estimated p"),
                        h2(textOutput(ns("p_hat_display")))
                    )
                ),

                column(
                    4,

                    card(
                        h4("Bootstrap SE"),
                        h2(textOutput(ns("se_display")))
                    )
                ),

                column(
                    4,

                    card(
                        h4("Confidence level"),
                        h2(textOutput(ns("conf_display")))
                    )
                )
            ),

            br(),

            card(
                card_header("Observed dice outcomes"),
                plotOutput(
                    ns("dice_plot"),
                    height = 300
                )
            ),

            br(),

            card(
                card_header("Bootstrap distribution"),
                plotOutput(
                    ns("bootstrap_plot"),
                    height = 350
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
    # Learn
    # =====================================================

    learn_panel <- div(

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Inference'",
                ns("topic")
            ),

            card(

                card_header("Bootstrap confidence intervals"),

                p("
The bootstrap estimates uncertainty
by repeatedly resampling data.
"),

                tags$ol(
                    tags$li("Collect data"),
                    tags$li("Resample"),
                    tags$li("Recalculate the statistic"),
                    tags$li("Study the distribution")
                )
            ),

            br(),

            card(

                card_header("Key formula"),

                tags$pre(
                    "CI = Estimate ± z × SE"
                )
            ),

            br(),

            card(

                card_header("Common mistake"),

                p("
A 95% confidence interval does not
mean there is a 95% probability that
the true parameter lies inside the
calculated interval.
")
            )
        ),

        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Regression'",
                ns("topic")
            ),

            card(

                card_header("Simple linear regression"),

                tags$pre(
                    "Y = β₀ + β₁X + ε"
                )
            ),

            br(),

            card(

                card_header("Interpreting the slope"),

                p("
The slope measures the average change
in Y associated with a one-unit
increase in X.
")
            ),

            br(),

            card(

                card_header("Model assumptions"),

                tags$ul(
                    tags$li("Linearity"),
                    tags$li("Independence"),
                    tags$li("Constant variance"),
                    tags$li("Approximately normal residuals")
                )
            )
        )
    )

    # =========================================================
    # ACTIVITY PANEL
    # =========================================================


    activity_panel <- div(

        card(

            card_header("Interactive Activity"),

            p(
                "Launch the companion activity for this chapter."
            ),

            actionButton(
                ns("launch_activity"),
                "Launch Activity",
                class = "btn-success"
            )
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

            ci = NULL
        )

        observeEvent(input$launch_activity, {

            url <- pws:::launch_activity_from_lab(5)

            shinyjs::runjs(
                sprintf(
                    "window.open('%s', '_blank');",
                    url
                )
            )
        })

        # =====================================================
        # Inference
        # =====================================================

        observeEvent(input$roll, {

            set.seed(input$seed)

            rv$dice <- sample(

                1:6,

                size = input$n,

                replace = TRUE,

                prob = c(
                    rep((1 - input$p_true)/5, 5),
                    input$p_true
                )
            )

            rv$bootstrap_p <- NULL
            rv$ci <- NULL
        })

        # -----------------------------------------------------
        # Bootstrap
        # -----------------------------------------------------

        observeEvent(input$bootstrap, {

            req(rv$dice)

            n <- length(rv$dice)

            rv$bootstrap_p <- replicate(input$B, {

                if (input$boot_method == "true_p") {

                    d <- sample(

                        1:6,

                        size = n,

                        replace = TRUE,

                        prob = c(
                            rep((1 - input$p_true)/5, 5),
                            input$p_true
                        )
                    )

                } else if (input$boot_method == "est_p") {

                    p_hat <- mean(rv$dice == 6)

                    d <- sample(

                        1:6,

                        size = n,

                        replace = TRUE,

                        prob = c(
                            rep((1 - p_hat)/5, 5),
                            p_hat
                        )
                    )

                } else {

                    d <- sample(
                        rv$dice,
                        size = n,
                        replace = TRUE
                    )
                }

                mean(d == 6)
            })

            rv$p_hat <- mean(rv$dice == 6)

            rv$se <- sd(rv$bootstrap_p)
        })

        # -----------------------------------------------------
        # Confidence interval
        # -----------------------------------------------------

        observeEvent(input$ci, {

            req(rv$bootstrap_p)

            z <- qnorm(
                1 - (1 - input$conf)/2
            )

            rv$ci <- c(

                rv$p_hat - z * rv$se,

                rv$p_hat + z * rv$se
            )
        })

        # =====================================================
        # Generated code
        # =====================================================

        output$generated_code <- renderText({

            if (input$topic == "Inference") {

                paste0(

                    "set.seed(",
                    input$seed,
                    ")

## Simulate dice rolls

dice <- sample(
  1:6,
  size = ", input$n, ",
  replace = TRUE
)

## Bootstrap

B <- ", input$B, "

boot <- replicate(
  B,
  mean(
    sample(
      dice,
      replace = TRUE
    ) == 6
  )
)

p_hat <- mean(dice == 6)

se <- sd(boot)

ci <- p_hat + c(-1,1) *
      qnorm(",
        round(
            1 - (1 - input$conf)/2,
            3
        ),
        ") * se"
                )

            } else {

                end_index <- match(
                    input$end_season,
                    unique(pws::PL_points$season)
                )

                paste0(

                    "seasons <- unique(
  PL_points$season
)

PL_subset <- subset(

  PL_points,

  season %in%
    seasons[1:",
                end_index,
                "]

)

fit <- lm(

  points_half2 ~ points_half1,

  data = PL_subset

)

predict(

  fit,

  newdata = data.frame(

    points_half1 = ",
input$x_split,

"

  ),

  interval = 'confidence',

  level = ",
input$conf_reg,

"

)"
                )
            }
        })
# =====================================================
# Summary cards
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

    paste0(
        round(
            100 * input$conf
        ),
        "%"
    )
})

# =====================================================
# Dice plot
# =====================================================

output$dice_plot <- renderPlot({

    req(rv$dice)

    ggplot(

        data.frame(
            face = factor(
                rv$dice,
                levels = 1:6
            )
        ),

        aes(face)

    ) +

        geom_bar(

            fill = pal_blue

        ) +

        theme_minimal(

            base_size = 14

        ) +

        labs(

            x = "Face",

            y = "Frequency"
        )
})

# =====================================================
# Bootstrap plot
# =====================================================

output$bootstrap_plot <- renderPlot({

    req(rv$bootstrap_p)

    df <- data.frame(

        p = rv$bootstrap_p
    )

    p <- ggplot(

        df,

        aes(p)

    ) +

        geom_histogram(

            bins = 30,

            fill = pal_lav

        ) +

        theme_minimal(

            base_size = 14

        ) +

        labs(

            x = expression(hat(p)),

            y = "Frequency"
        )

    if (!is.null(rv$ci)) {

        p <- p +

            annotate(

                "rect",

                xmin = rv$ci[1],

                xmax = rv$ci[2],

                ymin = 0,

                ymax = Inf,

                alpha = 0.15,

                fill = pal_red

            ) +

            geom_vline(

                xintercept = rv$ci,

                colour = pal_red,

                linewidth = 1.2
            )
    }

    p
})

# =====================================================
# Inference summary
# =====================================================

output$inference_results <- renderUI({

    req(rv$p_hat)

    card(

        card_header(
            "Inference Summary"
        ),

        p(
            strong("Estimated p: "),
            round(
                rv$p_hat,
                3
            )
        ),

        p(
            strong("Bootstrap SE: "),
            round(
                rv$se,
                4
            )
        ),

        if (!is.null(rv$ci)) {

            p(
                strong("Confidence Interval: "),
                paste0(
                    "[",
                    round(
                        rv$ci[1],
                        3
                    ),
                    ", ",
                    round(
                        rv$ci[2],
                        3
                    ),
                    "]"
                )
            )

        } else {

            p(
                em(
                    "Confidence interval not yet calculated."
                )
            )
        }
    )
})

# =====================================================
# Regression
# =====================================================

# =====================================================
# Regression data subset
# =====================================================

reg_data <- reactive({

    seasons <- unique(
        pws::PL_points$season
    )

    end_index <- match(
        input$end_season,
        seasons
    )

    pws::PL_points[
        pws::PL_points$season %in%
            seasons[1:end_index],
    ]
})

reg_fit <- reactive({

    lm(

        points_half2 ~

            points_half1,

        data = reg_data()
    )
})

prediction <- reactive({

    predict(

        reg_fit(),

        newdata = data.frame(

            points_half1 =
                input$x_split
        ),

        interval = "confidence",

        level = input$conf_reg
    )
})

# =====================================================
# Regression confidence band
# =====================================================

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

    cbind(
        grid,
        preds
    )
})


# =====================================================
# Regression plot
# =====================================================

# =====================================================
# Regression plot
# =====================================================

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

        geom_point(
            colour = pal_blue
        ) +

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

        geom_segment(

            x = input$x_split,
            xend = input$x_split,

            y = min(df$points_half2, na.rm = TRUE),
            yend = pr[1, "fit"],

            colour = pal_red,
            linetype = "dashed",
            linewidth = 0.8
        ) +

        geom_segment(

            x = min(df$points_half1, na.rm = TRUE),
            xend = input$x_split,

            y = pr[1, "fit"],
            yend = pr[1, "fit"],

            colour = pal_red,
            linetype = "dashed",
            linewidth = 0.8
        ) +

        annotate(

            "text",

            x = input$x_split,

            y = min(df$points_half2, na.rm = TRUE),

            label = paste0("x = ", round(input$x_split, 1)),

            colour = pal_red,

            hjust = 1.2
        ) +

        annotate(

            "text",

            x = min(df$points_half1, na.rm = TRUE),

            y = pr[1, "fit"],

            label = paste0("ŷ = ", round(pr[1, "fit"], 1)),

            colour = pal_red,

            vjust = -1
        ) +

        annotate(

            "point",

            x = input$x_split,

            y = pr[1, "fit"],

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

# =====================================================
# Regression summary
# =====================================================

output$regression_results <- renderUI({

    fit <- reg_fit()

    pr <- prediction()

    card(

        card_header(
            "Regression Summary"
        ),

        p(
            strong("Seasons included: "),
            paste(
                unique(
                    pws::PL_points$season
                )[1],
                "to",
                input$end_season
            )
        ),

        p(
            strong("Observations: "),
            nrow(reg_data())
        ),

        p(
            strong("Slope: "),
            round(
                coef(fit)[2],
                3
            )
        ),

        p(
            strong("Intercept: "),
            round(
                coef(fit)[1],
                3
            )
        ),

        p(
            strong("Prediction: "),
            round(
                pr[1, "fit"],
                3
            )
        ),

        p(
            strong(
                "Confidence Interval: "
            ),

            paste0(

                "[",

                round(
                    pr[1, "lwr"],
                    3
                ),

                ", ",

                round(
                    pr[1, "upr"],
                    3
                ),

                "]"
            )
        )
    )
})
    })
}

