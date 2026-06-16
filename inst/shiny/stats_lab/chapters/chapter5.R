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

    # =====================================================
    # Sidebar
    # =====================================================

    sidebar_controls <- sidebar(

        h4("Basics of Inference"),

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

            h5("Simulation settings"),

            numericInput(
                ns("seed"),
                "Random seed",
                value = sample(1:999, 1),
                min = 1,
                max = 999
            ),

            hr(),

            h5("The Two-Dice game"),

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
                "Number of bootstrap simulations",
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

            card_header("📊 Learning from Samples"),

            p(
                strong("Main idea: "),
                "We rarely observe an entire population. Instead, we collect a sample and use it to estimate unknown quantities."
            ),

            hr(),

            h5("The problem"),

            p(
                "Suppose we want to know the probability of rolling a six with a particular die. We cannot know the true probability directly, but we can estimate it from observed rolls."
            ),

            hr(),

            h5("What happens in this topic?"),

            p("① Roll a die repeatedly."),

            p("② Estimate the probability of rolling a six."),

            p("③ Generate many bootstrap samples."),

            p("④ Use the bootstrap distribution to measure uncertainty."),

            p("⑤ Construct a confidence interval."),

            hr(),

            h5("Your job"),

            tags$ul(
                tags$li("Explore how estimates vary from sample to sample"),
                tags$li("Investigate the effect of sample size"),
                tags$li("Compare different bootstrap methods"),
                tags$li("Study how confidence intervals change")
            ),

            hr(),

            div(
                style = "
            background-color:#f8f9fa;
            border-left:5px solid #7B9ACC;
            padding:12px;
            border-radius:8px;
        ",

                h5("Questions to investigate"),

                tags$ul(
                    tags$li("How close is the estimate to the true probability?"),
                    tags$li("Why do different samples produce different estimates?"),
                    tags$li("How does sample size affect uncertainty?"),
                    tags$li("What does a confidence interval tell us?")
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

        card(

            card_header("What should you have learned?"),

            h5("1. Samples vary"),

            p(
                "Different samples from the same population produce different estimates."
            ),

            hr(),

            h5("2. Estimation involves uncertainty"),

            p(
                "An estimate is not a fact. It is our best guess based on limited information."
            ),

            hr(),

            h5("3. Bootstrap methods mimic repeated sampling"),

            p(
                "By repeatedly resampling the observed data, we can study how much an estimate might vary."
            ),

            hr(),

            h5("4. Confidence intervals measure precision"),

            p(
                "Narrow intervals indicate more precise estimates, while wider intervals indicate greater uncertainty."
            ),

            hr(),

            h5("5. Statistical inference is about populations"),

            p(
                "The ultimate goal is not to describe the sample itself, but to learn about the larger population from which it came."
            ),

            hr(),

            div(
                style = "
            background-color:#f8f9fa;
            border-left:5px solid #28a745;
            padding:12px;
            border-radius:8px;
        ",

                h5("Key takeaway"),

                p(
                    strong("Statistics allows us to learn from incomplete information."),
                    br(),
                    "Inference uses samples to estimate unknown population quantities while acknowledging uncertainty."
                )
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
            ci_active = FALSE
        )

        # =====================================================
        # Inference: roll dice
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
            rv$ci_active <- FALSE   # reset CI when new data
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
        })

        # =====================================================
        # CI activation button
        # =====================================================

        observeEvent(input$ci, {
            req(rv$bootstrap_p)
            rv$ci_active <- TRUE
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

            req(rv$p_hat)

            if (!isTRUE(rv$ci_active)) {

                return(
                    card(
                        card_header("Inference Summary"),
                        p(em("Click 'Confidence Interval' to reveal the interval."))
                    )
                )
            }

            ci <- ci_inference()

            card(
                card_header("Inference Summary"),

                p(strong("Estimated p: "), round(rv$p_hat, 3)),
                p(strong("Bootstrap SE: "), round(rv$se, 4)),

                p(
                    strong("Confidence Interval: "),
                    paste0("[", round(ci[1], 3), ", ", round(ci[2], 3), "]")
                )
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
