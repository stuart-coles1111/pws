
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

pal_roll    <- "#5B8DB8"
pal_est     <- "#6FB286"
pal_sim     <- "#9B72B0"
pal_ci      <- "#6F777D"
pal_reveal  <- "#D99452"
pal_restart <- "#C96B68"


# =========================================================
# UI
# =========================================================

chapter5_ui <- function(id) {

    ns <- NS(id)

    useShinyjs()

    seasons <- unique(pws::PL_points$season)

    # -----------------------------------------------------
    # Sidebar
    # -----------------------------------------------------

    sidebar_controls <- sidebar(

        width = 270,

        numericInput(
            ns("seed"),
            "Random seed",
            value = sample(1:999, 1),
            min = 1,
            max = 999,
            step = 1
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

        # =================================================
        # DICE ACTIVITIES
        # =================================================

        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'Inference'",
                ns("topic")
            ),

            hr(),

            h5("The One-Dice game"),

            radioButtons(
                ns("dice_activity"),
                "Choose activity:",
                choices = c(
                    "Inference from observed data" = "inference",
                    "Sampling distribution of the estimator" = "sampling"
                ),
                selected = "inference"
            ),

            # -------------------------------------------------
            # Observed-data inference
            # -------------------------------------------------

            conditionalPanel(
                condition = sprintf(
                    "input['%s'] == 'inference'",
                    ns("dice_activity")
                ),

                numericInput(
                    ns("n"),
                    "Number of dice rolls",
                    value = 1000,
                    min = 10
                ),

                radioButtons(
                    ns("prob_mode"),
                    "Probability of rolling a six:",
                    choices = c(
                        "Chosen manually" = "fixed",
                        "Randomised" = "random"
                    ),
                    selected = "fixed"
                ),

                conditionalPanel(
                    condition = sprintf(
                        "input['%s'] == 'fixed'",
                        ns("prob_mode")
                    ),

                    sliderInput(
                        ns("p_true"),
                        "True probability, θ, of rolling a 6",
                        min = 0.05,
                        max = 0.50,
                        value = 0.167,
                        step = 0.01
                    )
                ),

                conditionalPanel(
                    condition = sprintf(
                        "input['%s'] == 'random'",
                        ns("prob_mode")
                    ),

                    p(
                        style = "font-size:0.9em;color:#777;",
                        "The true probability will be chosen randomly."
                    )
                ),

                actionButton(
                    ns("roll"),
                    "Roll Dice",
                    class = "btn-primary",
                    style = paste0(
                        "background-color:", pal_roll,
                        ";border-color:", pal_roll, ";"
                    )
                ),

                hr(),

                actionButton(
                    ns("estimate"),
                    "Estimate probability",
                    class = "btn-success",
                    style = paste0(
                        "background-color:", pal_est,
                        ";border-color:", pal_est, ";"
                    )
                ),

                hr(),

                numericInput(
                    ns("B"),
                    "Number of bootstrap samples",
                    value = 5000,
                    min = 100
                ),

                actionButton(
                    ns("bootstrap"),
                    "Bootstrap",
                    class = "btn-info",
                    style = paste0(
                        "background-color:", pal_sim,
                        ";border-color:", pal_sim, ";"
                    )
                ),

                hr(),

                sliderInput(
                    ns("conf"),
                    "Confidence level",
                    min = 0.80,
                    max = 0.999,
                    value = 0.95,
                    step = 0.001
                ),

                actionButton(
                    ns("ci"),
                    "Confidence Interval for θ",
                    class = "btn-secondary",
                    style = paste0(
                        "background-color:", pal_ci,
                        ";border-color:", pal_ci, ";"
                    )
                ),

                br(),
                br(),

                actionButton(
                    ns("reveal"),
                    "Reveal true probability, θ",
                    class = "btn-warning",
                    style = paste0(
                        "background-color:", pal_reveal,
                        ";border-color:", pal_reveal,
                        ";color:white;"
                    )
                ),

                hr(),

                actionButton(
                    ns("restart"),
                    "Restart Experiment",
                    class = "btn-danger",
                    style = paste0(
                        "background-color:", pal_restart,
                        ";border-color:", pal_restart, ";"
                    )
                )
            ),

            # -------------------------------------------------
            # Sampling distribution
            # -------------------------------------------------

            conditionalPanel(
                condition = sprintf(
                    "input['%s'] == 'sampling'",
                    ns("dice_activity")
                ),

                sliderInput(
                    ns("sampling_p"),
                    "True probability, θ, of rolling a 6",
                    min = 0.05,
                    max = 0.50,
                    value = 0.167,
                    step = 0.001
                ),

                sliderInput(
                    ns("sampling_n"),
                    "Sample size, n",
                    min = 10,
                    max = 1000,
                    value = 100,
                    step = 10,
                    sep = ","
                ),

                sliderInput(
                    ns("sampling_M"),
                    "Number of simulated samples",
                    min = 100,
                    max = 20000,
                    value = 5000,
                    step = 100,
                    sep = ","
                ),

                actionButton(
                    ns("sampling_simulate"),
                    "Simulate sampling distribution",
                    class = "btn-info",
                    style = paste0(
                        "background-color:", pal_sim,
                        ";border-color:", pal_sim, ";"
                    )
                )
            )
        ),

        # =================================================
        # REGRESSION CONTROLS
        # =================================================

        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'Regression'",
                ns("topic")
            ),

            uiOutput(ns("regression_controls_ui"))
        )
    )


    # =====================================================
    # OVERVIEW
    # =====================================================

    overview_panel <- div(

        card(

            style = "
                border-radius:16px;
                border:none;
                box-shadow:0 4px 12px rgba(0,0,0,0.08);
                padding:10px;
            ",

            # -------------------------------------------------
            # Inference overview
            # -------------------------------------------------

            conditionalPanel(
                condition = sprintf(
                    "input['%s'] == 'Inference'",
                    ns("topic")
                ),

                card_header(
                    div(
                        "Module 5: Statistical inference",
                        style = "
                            font-size:1.4rem;
                            font-weight:700;
                            color:#2c3e50;
                        "
                    )
                ),

                p(
                    strong(
                        "Use the one-dice game to explore how we learn about an unknown probability from observed data."
                    )
                ),

                p(
                    "The underlying probability of rolling a six is not necessarily known. We observe a sample of dice rolls and use those observations to estimate the probability."
                ),

                h5("The one-dice experiment"),

                p(
                    "Choose the number of rolls and decide whether the true probability is chosen manually or randomly. Press Roll Dice to generate an observed sample."
                ),

                h5("Estimation and bootstrap"),

                p(
                    "Estimate the probability by dividing the number of sixes by the total number of rolls. Bootstrap resampling then illustrates how the estimate might vary from sample to sample."
                ),

                h5("Sampling distributions"),

                p(
                    "Repeatedly simulate samples from a population with a known probability. Examine whether the distribution of estimates is centred on the true probability and how its shape changes with sample size."
                ),

                h5("Confidence intervals"),

                p(
                    "Use the bootstrap distribution to construct a confidence interval. Explore how its width changes with the confidence level."
                ),

                hr(),

                h5("Questions to investigate"),

                tags$ul(
                    tags$li("How much does the estimate vary between samples?"),
                    tags$li("What happens when the sample size increases?"),
                    tags$li("Is the mean of the simulated estimates close to the true probability?"),
                    tags$li("How does the sampling distribution change with sample size?"),
                    tags$li("What does the bootstrap distribution tell us about uncertainty?"),
                    tags$li("What happens to the confidence interval when the confidence level increases?")
                )
            ),

            # -------------------------------------------------
            # Regression overview
            # -------------------------------------------------

            conditionalPanel(
                condition = sprintf(
                    "input['%s'] == 'Regression'",
                    ns("topic")
                ),

                card_header(
                    div(
                        "Module 5: Regression",
                        style = "
                            font-size:1.4rem;
                            font-weight:700;
                            color:#2c3e50;
                        "
                    )
                ),

                p(
                    strong(
                        "Use Premier League points to explore how regression describes relationships and makes predictions."
                    )
                ),

                p(
                    "The analysis examines the relationship between the points a team accumulates in the first half of a season and its points in the second half."
                ),

                h5("1. Explore the data"),

                p(
                    "Choose the seasons to include and examine the scatterplot. The plot updates when you change the selected period."
                ),

                h5("2. Fit a regression model"),

                p(
                    "Press Add regression line when you are ready to fit a linear regression model. The fitted line and its confidence band will appear. The data-selection controls are then locked."
                ),

                h5("3. Make a prediction"),

                p(
                    "Press Predict to reveal the prediction controls. Choose a number of first-half points to see the predicted mean number of second-half points and its confidence interval."
                ),

                p(
                    "The interval describes uncertainty about the mean response, rather than the range in which an individual team's points must lie."
                ),

                h5("Start another investigation"),

                p(
                    "Press New regression to remove the fitted model and unlock the data-selection controls. You can then investigate a different period."
                ),

                hr(),

                h5("Questions to investigate"),

                tags$ul(
                    tags$li("How strong is the relationship between first-half and second-half points?"),
                    tags$li("How does the fitted line change when fewer seasons are included?"),
                    tags$li("How does the confidence band reflect uncertainty about the mean relationship?"),
                    tags$li("How does the predicted value change as the prediction point moves?"),
                    tags$li("What does the diagonal reference line y = x tell us?")
                )
            )
        )
    )


    # =====================================================
    # GENERATED CODE PANEL
    # =====================================================

    code_panel <- div(

        card(

            card_header("Generated R code"),

            tags$pre(
                style = "
                    background:#F8F9FA;
                    padding:15px;
                    border-radius:10px;
                    font-size:14px;
                    white-space:pre-wrap;
                ",

                textOutput(ns("generated_code"))
            )
        )
    )


    # =====================================================
    # RESULTS PANEL
    # =====================================================

    results_panel <- div(

        # -------------------------------------------------
        # Observed-data inference
        # -------------------------------------------------

        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'Inference' && input['%s'] == 'inference'",
                ns("topic"),
                ns("dice_activity")
            ),

            fluidRow(

                column(
                    6,

                    card(
                        card_header("Observed data"),
                        plotOutput(ns("dice_plot"), height = 350)
                    )
                ),

                column(
                    6,

                    card(
                        card_header("Bootstrap estimates of θ"),
                        plotOutput(ns("bootstrap_plot"), height = 350)
                    )
                )
            ),

            br(),

            uiOutput(ns("inference_results"))
        ),

        # -------------------------------------------------
        # Sampling distribution
        # -------------------------------------------------

        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'Inference' && input['%s'] == 'sampling'",
                ns("topic"),
                ns("dice_activity")
            ),

            card(
                card_header("Sampling distribution of the estimator"),
                plotOutput(ns("sampling_plot"), height = 500)
            ),

            br(),

            uiOutput(ns("sampling_results"))
        ),

        # -------------------------------------------------
        # Regression
        # -------------------------------------------------

        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'Regression'",
                ns("topic")
            ),

            card(
                card_header("Points scored per season half"),
                plotOutput(ns("reg_plot"), height = 450)
            ),

            br(),

            uiOutput(ns("regression_results"))
        )
    )


    # =====================================================
    # BUILD CHAPTER
    # =====================================================

    chapter_page_ui(
        id = id,
        title = "📊 Module 5: Statistics",
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

chapter5_server <- function(id) {

    moduleServer(id, function(input, output, session) {

        # =====================================================
        # Formatting helper
        # =====================================================

        fmt3 <- function(x) {
            format(
                x,
                digits = 3,
                trim = TRUE,
                scientific = FALSE
            )
        }


        # =====================================================
        # Regression season list
        # =====================================================

        seasons <- unique(pws::PL_points$season)


        # =====================================================
        # REACTIVE STATE
        # =====================================================

        rv <- reactiveValues(

            # Dice inference
            dice = NULL,
            p_true_used = NULL,
            p_hat = NULL,
            x = NULL,
            n = NULL,
            bootstrap_p = NULL,
            se = NULL,
            ci_active = FALSE,
            reveal_true = FALSE,

            # Sampling distribution
            sampling_p = NULL,
            sampling_n = NULL,
            sampling_M = NULL,
            sampling_estimates = NULL,
            sampling_seed_used = NULL
        )


        # =====================================================
        # REGRESSION STATE
        # =====================================================

        reg_state <- reactiveValues(

            # explore: scatterplot only
            # fitted: regression line and confidence band
            # predict: prediction controls and result visible

            stage = "explore",

            data = NULL,
            fit = NULL,

            start_season = NULL,
            end_season = NULL,

            fit_conf = NULL
        )


        # =====================================================
        # REGRESSION DATA
        # =====================================================

        reg_data <- reactive({

            req(
                input$start_season,
                input$end_season
            )

            start_index <- match(
                input$start_season,
                seasons
            )

            end_index <- match(
                input$end_season,
                seasons
            )

            req(
                !is.na(start_index),
                !is.na(end_index),
                start_index <= end_index
            )

            df <- pws::PL_points[
                pws::PL_points$season %in%
                    seasons[start_index:end_index],
                ,
                drop = FALSE
            ]

            df <- df[
                complete.cases(
                    df[, c("points_half1", "points_half2")]
                ),
                ,
                drop = FALSE
            ]

            validate(
                need(
                    nrow(df) >= 3,
                    "Select a period containing at least three complete observations."
                ),
                need(
                    length(unique(df$points_half1)) >= 2,
                    "The selected period must contain variation in first-half points."
                )
            )

            df
        })


        # =====================================================
        # REGRESSION SIDEBAR
        # =====================================================

        output$regression_controls_ui <- renderUI({

            ns <- session$ns

            if (identical(reg_state$stage, "explore")) {

                tagList(

                    h4("Regression model"),

                    selectInput(
                        ns("start_season"),
                        "First season included",
                        choices = seasons,
                        selected = if (
                            !is.null(reg_state$start_season)
                        ) {
                            reg_state$start_season
                        } else {
                            head(seasons, 1)
                        }
                    ),

                    selectInput(
                        ns("end_season"),
                        "Final season included",
                        choices = seasons,
                        selected = if (
                            !is.null(reg_state$end_season)
                        ) {
                            reg_state$end_season
                        } else {
                            tail(seasons, 1)
                        }
                    ),

                    sliderInput(
                        ns("conf_reg"),
                        "Confidence level",
                        min = 0.80,
                        max = 0.99,
                        value = if (
                            !is.null(reg_state$fit_conf)
                        ) {
                            reg_state$fit_conf
                        } else {
                            0.95
                        },
                        step = 0.01
                    ),

                    checkboxInput(
                        ns("show_diagonal"),
                        "Show diagonal reference line (y = x)",
                        value = FALSE
                    ),

                    actionButton(
                        ns("add_regression"),
                        "Add regression line",
                        class = "btn-primary",
                        style = paste0(
                            "background-color:", pal_roll,
                            ";border-color:", pal_roll, ";"
                        )
                    )
                )


            } else {

                tagList(

                    h4("Regression fitting"),

                    p(
                        strong("First season: "),
                        reg_state$start_season
                    ),

                    p(
                        strong("Final season: "),
                        reg_state$end_season
                    ),

                    p(
                        strong("Confidence level: "),
                        paste0(
                            reg_state$fit_conf * 100,
                            "%"
                        )
                    ),

                    hr(),

                    h4("Second-half prediction"),

                    if (identical(reg_state$stage, "fitted")) {

                        actionButton(
                            ns("predict"),
                            "Predict",
                            class = "btn-success",
                            style = paste0(
                                "background-color:", pal_est,
                                ";border-color:", pal_est, ";"
                            )
                        )

                    } else {

                        tagList(

                            sliderInput(
                                ns("x_split"),
                                "Points in first half of season",
                                min = floor(
                                    min(reg_state$data$points_half1)
                                ),
                                max = ceiling(
                                    max(reg_state$data$points_half1)
                                ),
                                value = round(
                                    median(reg_state$data$points_half1)
                                ),
                                step = 1
                            ),

                            p(
                                style = "font-size:0.9em;color:#777;",
                                "Move the slider to explore predictions from the fitted model."
                            )
                        )
                    },

                    # Always show this button once the model has been fitted.
                    # It appears in both the fitted and predict stages.
                    hr(),

                    actionButton(
                        ns("new_regression"),
                        "New regression",
                        class = "btn-danger",
                        style = paste0(
                            "background-color:", pal_restart,
                            ";border-color:", pal_restart, ";"
                        )
                    )
                )
            }
            })


        # =====================================================
        # KEEP FINAL SEASON >= FIRST SEASON
        # =====================================================

        observeEvent(
            input$start_season,
            {
                req(input$start_season)

                start_index <- match(
                    input$start_season,
                    seasons
                )

                req(!is.na(start_index))

                valid_end_seasons <- seasons[
                    start_index:length(seasons)
                ]

                current_end <- input$end_season

                if (
                    is.null(current_end) ||
                    !(current_end %in% valid_end_seasons)
                ) {
                    current_end <- tail(
                        valid_end_seasons,
                        1
                    )
                }

                updateSelectInput(
                    session,
                    "end_season",
                    choices = valid_end_seasons,
                    selected = current_end
                )
            },
            ignoreInit = FALSE
        )


        # =====================================================
        # REGRESSION BUTTON EVENTS
        # =====================================================

        observeEvent(
            input$add_regression,
            {
                req(
                    input$topic == "Regression",
                    identical(reg_state$stage, "explore")
                )

                df <- reg_data()

                reg_state$data <- df

                reg_state$start_season <- input$start_season
                reg_state$end_season <- input$end_season
                reg_state$fit_conf <- input$conf_reg

                reg_state$fit <- lm(
                    points_half2 ~ points_half1,
                    data = df
                )

                reg_state$stage <- "fitted"
            },
            ignoreInit = TRUE
        )


        observeEvent(
            input$predict,
            {
                req(
                    input$topic == "Regression",
                    identical(reg_state$stage, "fitted"),
                    reg_state$fit
                )

                reg_state$stage <- "predict"
            },
            ignoreInit = TRUE
        )


        observeEvent(
            input$new_regression,
            {
                req(input$topic == "Regression")

                reg_state$stage <- "explore"

                reg_state$data <- NULL
                reg_state$fit <- NULL
                reg_state$fit_conf <- NULL

                # Preserve the selected period as the starting
                # point for the next investigation.
                reg_state$start_season <- input$start_season
                reg_state$end_season <- input$end_season
            },
            ignoreInit = TRUE
        )


        # =====================================================
        # UPDATE PREDICTION POINT WHEN DATA PERIOD CHANGES
        # =====================================================

        observeEvent(
            list(input$start_season, input$end_season),
            {
                req(
                    identical(reg_state$stage, "explore"),
                    input$start_season,
                    input$end_season
                )

                df <- tryCatch(
                    reg_data(),
                    error = function(e) NULL
                )

                if (is.null(df) || nrow(df) == 0) {
                    return()
                }

                # The prediction slider is created when the
                # user enters the prediction stage. Save a
                # sensible default for that stage.
                invisible(df)
            },
            ignoreInit = TRUE
        )


        # =====================================================
        # PREDICTION
        # =====================================================

        prediction <- reactive({

            req(
                identical(reg_state$stage, "predict"),
                reg_state$fit,
                input$x_split
            )

            predict(
                reg_state$fit,
                newdata = data.frame(
                    points_half1 = input$x_split
                ),
                interval = "confidence",
                level = reg_state$fit_conf
            )
        })


        # =====================================================
        # REGRESSION CONFIDENCE BAND
        # =====================================================

        plot_predictions <- reactive({

            req(
                reg_state$fit,
                reg_state$data
            )

            df <- reg_state$data

            grid <- data.frame(
                points_half1 = seq(
                    min(df$points_half1),
                    max(df$points_half1),
                    length.out = 200
                )
            )

            preds <- predict(
                reg_state$fit,
                newdata = grid,
                interval = "confidence",
                level = reg_state$fit_conf
            )

            cbind(
                grid,
                as.data.frame(preds)
            )
        })


        # =====================================================
        # REGRESSION PLOT
        # =====================================================

        output$reg_plot <- renderPlot({

            # Before fitting, the plot uses the current
            # selection and contains only the data points.

            if (identical(reg_state$stage, "explore")) {

                df <- reg_data()

            } else {

                # After fitting, use the saved data snapshot.
                df <- reg_state$data
            }

            req(df)

            p <- ggplot(
                df,
                aes(
                    x = points_half1,
                    y = points_half2
                )
            ) +

                geom_point(
                    colour = pal_blue,
                    size = 2,
                    alpha = 0.7,
                    position = position_jitter(
                        width = 0.25,
                        height = 0.25,
                        seed = 123
                    )
                ) +

                theme_minimal(
                    base_size = 14
                ) +

                labs(
                    x = "Points (first half of season)",
                    y = "Points (second half of season)"
                )

            # -------------------------------------------------
            # Diagonal reference
            # -------------------------------------------------

            if (
                isTRUE(input$show_diagonal) &&
                identical(reg_state$stage, "explore")
            ) {

                p <- p +
                    geom_abline(
                        slope = 1,
                        intercept = 0,
                        colour = "#777777",
                        linetype = "dashed",
                        linewidth = 0.9
                    )
            }

            if (
                reg_state$stage %in% c("fitted", "predict") &&
                isTRUE(input$show_diagonal)
            ) {

                p <- p +
                    geom_abline(
                        slope = 1,
                        intercept = 0,
                        colour = "#777777",
                        linetype = "dashed",
                        linewidth = 0.9
                    )
            }

            # -------------------------------------------------
            # Regression line and confidence band
            # -------------------------------------------------

            if (
                reg_state$stage %in% c("fitted", "predict")
            ) {

                plot_df <- plot_predictions()

                p <- p +

                    geom_ribbon(
                        data = plot_df,
                        aes(
                            x = points_half1,
                            ymin = lwr,
                            ymax = upr
                        ),
                        fill = pal_lav,
                        alpha = 0.25,
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
                    )
            }

            # -------------------------------------------------
            # Prediction markers
            # -------------------------------------------------

            if (identical(reg_state$stage, "predict")) {

                pr <- prediction()

                predicted_value <- as.numeric(
                    pr[1, "fit"]
                )

                p <- p +

                    geom_vline(
                        xintercept = input$x_split,
                        colour = pal_red,
                        linetype = "dashed",
                        linewidth = 0.8
                    ) +

                    geom_hline(
                        yintercept = predicted_value,
                        colour = pal_red,
                        linetype = "dashed",
                        linewidth = 0.8
                    ) +

                    annotate(
                        "point",
                        x = input$x_split,
                        y = predicted_value,
                        colour = pal_red,
                        size = 4
                    )
            }

            p
        })


        # =====================================================
        # REGRESSION SUMMARY
        # =====================================================

        output$regression_results <- renderUI({

            if (identical(reg_state$stage, "explore")) {
                return(NULL)
            }

            req(
                reg_state$fit,
                reg_state$data
            )

            fit <- reg_state$fit
            df <- reg_state$data

            items <- list(

                p(
                    strong("Seasons: "),
                    reg_state$start_season,
                    " to ",
                    reg_state$end_season,
                    "  |  ",
                    strong("Observations: "),
                    nrow(df)
                ),

                p(
                    strong("Intercept: "),
                    fmt3(coef(fit)[1]),
                    "  |  ",
                    strong("Slope: "),
                    fmt3(coef(fit)[2])
                ),

                p(
                    strong("Confidence level: "),
                    paste0(
                        fmt3(reg_state$fit_conf * 100),
                        "%"
                    )
                )
            )

            if (identical(reg_state$stage, "predict")) {

                pr <- prediction()

                items <- append(
                    items,
                    list(

                        hr(),

                        h5("Prediction"),

                        p(
                            strong("First-half points: "),
                            input$x_split
                        ),

                        p(
                            strong("Predicted mean second-half points: "),
                            fmt3(pr[1, "fit"])
                        ),

                        p(
                            strong(
                                paste0(
                                    fmt3(reg_state$fit_conf * 100),
                                    "% confidence interval for the mean: "
                                )
                            ),
                            paste0(
                                "[",
                                fmt3(pr[1, "lwr"]),
                                ", ",
                                fmt3(pr[1, "upr"]),
                                "]"
                            )
                        )
                    )
                )
            }

            card(
                style = "
                    background-color:#F5F7FB;
                    border:none;
                    border-radius:12px;
                    box-shadow:0 2px 8px rgba(0,0,0,0.05);
                ",

                card_header("Regression summary"),

                items
            )
        })


        # =====================================================
        # DICE BUTTON STATES
        # =====================================================

        observe({

            req(
                input$topic == "Inference",
                input$dice_activity == "inference"
            )

            if (is.null(rv$dice)) {

                enable("roll")
                disable("estimate")
                disable("bootstrap")
                disable("ci")
                disable("reveal")
                disable("restart")

                return()
            }

            if (is.null(rv$p_hat)) {

                disable("roll")
                enable("estimate")
                disable("bootstrap")
                disable("ci")
                disable("reveal")
                enable("restart")

                return()
            }

            if (is.null(rv$bootstrap_p)) {

                disable("roll")
                disable("estimate")
                enable("bootstrap")
                disable("ci")
                enable("reveal")
                enable("restart")

                return()
            }

            disable("roll")
            disable("estimate")
            enable("bootstrap")
            enable("ci")
            enable("reveal")
            enable("restart")
        })


        # =====================================================
        # ROLL DICE
        # =====================================================

        observeEvent(input$roll, {

            req(input$seed)

            set.seed(input$seed)

            if (input$prob_mode == "fixed") {

                rv$p_true_used <- input$p_true

            } else {

                rv$p_true_used <- runif(
                    1,
                    min = 0.05,
                    max = 0.50
                )
            }

            rv$dice <- sample(
                1:6,
                size = input$n,
                replace = TRUE,
                prob = c(
                    rep(
                        (1 - rv$p_true_used) / 5,
                        5
                    ),
                    rv$p_true_used
                )
            )

            rv$n <- length(rv$dice)
            rv$x <- sum(rv$dice == 6)

            rv$p_hat <- NULL
            rv$bootstrap_p <- NULL
            rv$se <- NULL

            rv$ci_active <- FALSE
            rv$reveal_true <- FALSE
        })


        # =====================================================
        # ESTIMATE PROBABILITY
        # =====================================================

        observeEvent(input$estimate, {

            req(rv$dice)

            rv$x <- sum(rv$dice == 6)
            rv$n <- length(rv$dice)

            rv$p_hat <- rv$x / rv$n

            rv$bootstrap_p <- NULL
            rv$se <- NULL
            rv$ci_active <- FALSE
        })


        # =====================================================
        # RESTART DICE EXPERIMENT
        # =====================================================

        observeEvent(input$restart, {

            new_seed <- sample(1:999, 1)

            updateNumericInput(
                session,
                "seed",
                value = new_seed
            )

            rv$dice <- NULL
            rv$p_true_used <- NULL
            rv$p_hat <- NULL
            rv$x <- NULL
            rv$n <- NULL
            rv$bootstrap_p <- NULL
            rv$se <- NULL
            rv$ci_active <- FALSE
            rv$reveal_true <- FALSE
        })


        # =====================================================
        # REVEAL TRUE PROBABILITY
        # =====================================================

        observeEvent(input$reveal, {

            req(rv$p_true_used)

            rv$reveal_true <- TRUE
        })


        # =====================================================
        # BOOTSTRAP
        # =====================================================

        observeEvent(input$bootstrap, {

            req(rv$dice, rv$p_hat)

            n <- length(rv$dice)

            rv$bootstrap_p <- replicate(
                input$B,
                mean(
                    sample(
                        rv$dice,
                        size = n,
                        replace = TRUE
                    ) == 6
                )
            )

            rv$se <- sd(rv$bootstrap_p)
            rv$ci_active <- FALSE
        })


        # =====================================================
        # CHANGING NUMBER OF BOOTSTRAP SAMPLES
        # =====================================================

        observeEvent(input$B, {

            req(rv$dice, rv$p_hat)

            rv$bootstrap_p <- NULL
            rv$se <- NULL
            rv$ci_active <- FALSE
        })


        # =====================================================
        # CONFIDENCE INTERVAL BUTTON
        # =====================================================

        observeEvent(input$ci, {

            req(rv$bootstrap_p)

            rv$ci_active <- TRUE
        })


        # =====================================================
        # CONFIDENCE INTERVAL CALCULATION
        # =====================================================

        ci_inference <- reactive({

            req(
                rv$bootstrap_p,
                rv$p_hat,
                rv$se,
                rv$ci_active
            )

            z <- qnorm(
                1 - (1 - input$conf) / 2
            )

            c(
                rv$p_hat - z * rv$se,
                rv$p_hat + z * rv$se
            )
        })


        # =====================================================
        # SAMPLING DISTRIBUTION SIMULATION
        # =====================================================

        observeEvent(input$sampling_simulate, {

            req(
                input$sampling_p,
                input$sampling_n,
                input$sampling_M,
                input$seed
            )

            simulation_seed <- input$seed

            set.seed(simulation_seed)

            rv$sampling_p <- input$sampling_p
            rv$sampling_n <- as.numeric(input$sampling_n)
            rv$sampling_M <- as.numeric(input$sampling_M)
            rv$sampling_seed_used <- simulation_seed

            rv$sampling_estimates <- replicate(
                rv$sampling_M,
                mean(
                    rbinom(
                        rv$sampling_n,
                        size = 1,
                        prob = rv$sampling_p
                    )
                )
            )

            next_seed <- sample(
                setdiff(1:999, simulation_seed),
                1
            )

            updateNumericInput(
                session,
                "seed",
                value = next_seed
            )
        })


        # =====================================================
        # GENERATED R CODE
        # =====================================================

        output$generated_code <- renderText({

            # -------------------------------------------------
            # Sampling distribution
            # -------------------------------------------------

            if (
                input$topic == "Inference" &&
                input$dice_activity == "sampling"
            ) {

                seed_used <- if (
                    !is.null(rv$sampling_seed_used)
                ) {
                    rv$sampling_seed_used
                } else {
                    input$seed
                }

                return(
                    paste0(
                        "## Sampling distribution of the estimator\n\n",
                        "set.seed(", seed_used, ")\n\n",
                        "p_true <- ", input$sampling_p, "\n",
                        "n <- ", input$sampling_n, "\n",
                        "M <- ", input$sampling_M, "\n\n",
                        "sampling_estimates <- replicate(\n",
                        "    M,\n",
                        "    mean(rbinom(n, size = 1, prob = p_true))\n",
                        ")\n\n",
                        "mean(sampling_estimates)"
                    )
                )
            }

            # -------------------------------------------------
            # Dice inference
            # -------------------------------------------------

            if (input$topic == "Inference") {

                p_used <- if (!is.null(rv$p_true_used)) {
                    rv$p_true_used
                } else if (input$prob_mode == "fixed") {
                    input$p_true
                } else {
                    0.25
                }

                n_used <- if (!is.null(rv$n)) {
                    rv$n
                } else {
                    input$n
                }

                probability_code <- if (
                    input$prob_mode == "random"
                ) {
                    "p_true <- runif(1, 0.05, 0.50)\n"
                } else {
                    paste0("p_true <- ", p_used, "\n")
                }

                return(
                    paste0(
                        "## One-dice inference and bootstrap\n\n",
                        "set.seed(", input$seed, ")\n\n",
                        probability_code,
                        "\ndice <- sample(\n",
                        "    1:6,\n",
                        "    size = ", n_used, ",\n",
                        "    replace = TRUE,\n",
                        "    prob = c(rep((1-p_true)/5, 5), p_true)\n",
                        ")\n\n",
                        "x <- sum(dice == 6)\n",
                        "n <- length(dice)\n",
                        "p_hat <- x / n\n\n",
                        "bootstrap_p <- replicate(\n",
                        "    ", input$B, ",\n",
                        "    mean(sample(dice, n, replace = TRUE) == 6)\n",
                        ")\n\n",
                        "se <- sd(bootstrap_p)\n",
                        "z <- qnorm(1 - (1 - ", input$conf, ")/2)\n\n",
                        "c(p_hat - z * se, p_hat + z * se)"
                    )
                )
            }

            # -------------------------------------------------
            # Regression
            # -------------------------------------------------

            if (identical(reg_state$stage, "explore")) {

                return(
                    paste0(
                        "## Explore the data\n\n",
                        "data <- subset(\n",
                        "    pws::PL_points,\n",
                        "    season >= '", input$start_season, "' &\n",
                        "    season <= '", input$end_season, "'\n",
                        ")\n\n",
                        "plot(\n",
                        "    data$points_half1,\n",
                        "    data$points_half2,\n",
                        "    xlab = 'Points in first half',\n",
                        "    ylab = 'Points in second half'\n",
                        ")"
                    )
                )
            }

            code <- paste0(
                "## Regression investigation\n\n",
                "data <- subset(\n",
                "    pws::PL_points,\n",
                "    season >= '", reg_state$start_season, "' &\n",
                "    season <= '", reg_state$end_season, "'\n",
                ")\n\n",
                "model <- lm(\n",
                "    points_half2 ~ points_half1,\n",
                "    data = data\n",
                ")\n\n",
                "plot(data$points_half1, data$points_half2)\n",
                "abline(model)\n"
            )

            if (identical(reg_state$stage, "predict")) {

                code <- paste0(
                    code,
                    "\n# Prediction and confidence interval\n\n",
                    "predict(\n",
                    "    model,\n",
                    "    newdata = data.frame(points_half1 = ",
                    input$x_split, "),\n",
                    "    interval = 'confidence',\n",
                    "    level = ", reg_state$fit_conf, "\n",
                    ")"
                )
            }

            code
        })


        # =====================================================
        # DICE PLOT
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
                        "FALSE" = pal_blue_soft,
                        "TRUE" = pal_red
                    ),
                    guide = "none"
                ) +

                theme_minimal(base_size = 14) +

                labs(
                    x = "Score",
                    y = "Frequency"
                )
        })


        # =====================================================
        # BOOTSTRAP PLOT
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

                labs(
                    x = expression(hat(theta)),
                    y = "Frequency"
                )

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

            if (isTRUE(rv$reveal_true)) {

                p <- p +

                    geom_vline(
                        xintercept = rv$p_true_used,
                        colour = pal_reveal,
                        linewidth = 1.3,
                        linetype = "dashed"
                    ) +

                    annotate(
                        "text",
                        x = rv$p_true_used,
                        y = Inf,
                        label = paste0(
                            "True θ = ",
                            round(rv$p_true_used, 3)
                        ),
                        colour = pal_reveal,
                        vjust = 1.5,
                        hjust = -0.05,
                        fontface = "bold"
                    )
            }

            p
        })


        # =====================================================
        # INFERENCE SUMMARY
        # =====================================================

        output$inference_results <- renderUI({

            req(
                input$topic == "Inference",
                input$dice_activity == "inference"
            )

            if (is.null(rv$dice)) {
                return(NULL)
            }

            result_items <- list()

            if (!is.null(rv$p_hat)) {

                result_items <- append(
                    result_items,
                    list(
                        p(
                            strong("Estimate of θ: "),
                            rv$x, " / ", rv$n, " = ",
                            fmt3(rv$p_hat)
                        )
                    )
                )
            }

            if (!is.null(rv$se)) {

                result_items <- append(
                    result_items,
                    list(
                        p(
                            strong("Bootstrap SE: "),
                            fmt3(rv$se)
                        )
                    )
                )
            }

            if (isTRUE(rv$ci_active)) {

                ci <- ci_inference()

                result_items <- append(
                    result_items,
                    list(
                        p(
                            strong(
                                paste0(
                                    fmt3(input$conf * 100),
                                    "% confidence interval: "
                                )
                            ),
                            paste0(
                                "[",
                                fmt3(ci[1]),
                                ", ",
                                fmt3(ci[2]),
                                "]"
                            )
                        )
                    )
                )
            }

            if (isTRUE(rv$reveal_true)) {

                result_items <- append(
                    result_items,
                    list(
                        p(
                            strong("True probability: "),
                            fmt3(rv$p_true_used)
                        )
                    )
                )
            }

            card(
                card_header("Inference summary"),
                result_items
            )
        })


        # =====================================================
        # SAMPLING DISTRIBUTION PLOT
        # =====================================================

        output$sampling_plot <- renderPlot({

            req(
                rv$sampling_estimates,
                rv$sampling_p,
                rv$sampling_n
            )

            estimates <- rv$sampling_estimates
            p_true <- rv$sampling_p
            n <- rv$sampling_n
            M <- length(estimates)

            simulated_mean <- mean(estimates)

            theoretical_se <- sqrt(
                p_true * (1 - p_true) / n
            )

            plot_df <- as.data.frame(
                table(estimates),
                stringsAsFactors = FALSE
            )

            names(plot_df) <- c("p_hat", "frequency")

            plot_df$p_hat <- as.numeric(
                as.character(plot_df$p_hat)
            )

            plot_df$frequency <- as.numeric(
                plot_df$frequency
            )

            x_min <- max(
                0,
                min(estimates, p_true - 4 * theoretical_se)
            )

            x_max <- min(
                1,
                max(estimates, p_true + 4 * theoretical_se)
            )

            x_grid <- seq(
                x_min,
                x_max,
                length.out = 500
            )

            normal_df <- data.frame(
                x = x_grid,
                y = dnorm(
                    x_grid,
                    mean = p_true,
                    sd = theoretical_se
                ) * M / n
            )

            y_max <- max(plot_df$frequency, na.rm = TRUE)

            ggplot() +

                geom_col(
                    data = plot_df,
                    aes(x = p_hat, y = frequency),
                    width = 0.8 / n,
                    fill = pal_lav,
                    colour = "white",
                    linewidth = 0.3
                ) +

                geom_line(
                    data = normal_df,
                    aes(x = x, y = y),
                    colour = pal_blue,
                    linewidth = 1.3
                ) +

                geom_vline(
                    xintercept = p_true,
                    colour = pal_reveal,
                    linewidth = 1.2,
                    linetype = "dashed"
                ) +

                geom_vline(
                    xintercept = simulated_mean,
                    colour = pal_red,
                    linewidth = 1.2
                ) +

                annotate(
                    "text",
                    x = p_true,
                    y = y_max * 0.98,
                    label = paste0("True θ = ", round(p_true, 3)),
                    colour = pal_reveal,
                    fontface = "bold",
                    hjust = -0.05,
                    vjust = 0
                ) +

                annotate(
                    "text",
                    x = simulated_mean,
                    y = y_max * 0.90,
                    label = paste0(
                        "Mean = ",
                        round(simulated_mean, 3)
                    ),
                    colour = pal_red,
                    fontface = "bold",
                    hjust = -0.05,
                    vjust = 0
                ) +

                theme_minimal(base_size = 14) +

                labs(
                    title = expression(
                        "Sampling distribution of " ~ hat(theta)
                    ),
                    subtitle = paste0(
                        "n = ", n,
                        ", M = ", format(M, big.mark = ",")
                    ),
                    x = expression(hat(theta)),
                    y = "Frequency"
                )
        })


        # =====================================================
        # SAMPLING DISTRIBUTION SUMMARY
        # =====================================================

        output$sampling_results <- renderUI({

            req(
                rv$sampling_estimates,
                rv$sampling_p,
                rv$sampling_n
            )

            simulated_mean <- mean(rv$sampling_estimates)

            card(
                style = "
                    background-color:#F5F7FB;
                    border:none;
                    border-radius:12px;
                ",

                card_header("Simulation summary"),

                p(
                    strong("True probability: "),
                    fmt3(rv$sampling_p)
                ),

                p(
                    strong("Mean of simulated estimates: "),
                    fmt3(simulated_mean)
                ),

                p(
                    strong("Difference from true θ: "),
                    fmt3(simulated_mean - rv$sampling_p)
                )
            )
        })

    })
}
