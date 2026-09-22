# =========================================================

# CHAPTER 8: COMPLEXITY

# =========================================================

# =========================================================

# PLOTTING

# =========================================================

league_position_plot <- function(
        df,
        league_position,
        rows = 4,
        scales = "fixed"
) {


    df_out <- data.frame(
        position = NULL
    )

    team_id <- seq_len(nrow(df))

    for (i in team_id) {

        df_team <- data.frame(

            team = rep(
                df[i, 1],
                ncol(league_position)
            ),

            position =
                as.numeric(
                    league_position[i, ]
                )

        )

        df_out <- rbind(
            df_out,
            df_team
        )
    }

    ggplot(
        df_out,
        aes(x = position)
    ) +

        geom_bar(
            fill = "lightblue",
            aes(
                y = 20 *
                    after_stat(count) /
                    sum(after_stat(count))
            )
        ) +

        scale_x_continuous(
            breaks = c(1, 5, 10, 15, 20),
            limits = c(0.5, 20.5)
        ) +

        xlab("Final league position") +

        ylab("Probability") +

        facet_wrap(
            ~ team,
            nrow = rows,
            scales = scales
        ) +

        theme_minimal(
            base_size = 16
        ) +

        theme(

            axis.title =
                element_text(
                    size = 18,
                    face = "bold"
                ),

            axis.text =
                element_text(
                    size = 14
                ),

            strip.text =
                element_text(
                    size = 14,
                    face = "bold"
                )
        )


}

# =========================================================

# STATIC SIMULATION CORE

# =========================================================

season_sim <- function(
        df,
        team_h,
        team_a,
        tau
) {


    mu_h <- exp(

        tau +

            df[
                match(
                    team_h,
                    df$teams
                ),
                "alpha"
            ] -

            df[
                match(
                    team_a,
                    df$teams
                ),
                "beta"
            ]

    )

    mu_a <- exp(

        df[
            match(
                team_a,
                df$teams
            ),
            "alpha"
        ] -

            df[
                match(
                    team_h,
                    df$teams
                ),
                "beta"
            ]

    )

    g_h <- rpois(
        length(team_h),
        mu_h
    )

    g_a <- rpois(
        length(team_a),
        mu_a
    )

    p_h <- ifelse(
        g_h > g_a,
        3,
        ifelse(
            g_h == g_a,
            1,
            0
        )
    )

    p_a <- ifelse(
        g_h > g_a,
        0,
        ifelse(
            g_h == g_a,
            1,
            3
        )
    )

    list(
        p_h,
        p_a,
        g_h - g_a,
        g_h,
        g_a
    )


}

league_sim <- function(
        df,
        schedule,
        tau
) {


    results <- season_sim(

        df,

        schedule[, 2],

        schedule[, 3],

        tau

    )

    points <- schedule

    points$hp <- results[[1]]

    points$ap <- results[[2]]

    points$gd <- results[[3]]

    points$gh <- results[[4]]

    points$ga <- results[[5]]


    df_h <- points %>%

        group_by(Home.Team) %>%

        summarise(

            tot = sum(hp),

            gd = sum(gd),

            gf = sum(gh),

            .groups = "drop"

        )


    df_a <- points %>%

        group_by(Away.Team) %>%

        summarise(

            tot = sum(ap),

            gd = -sum(gd),

            gf = sum(ga),

            .groups = "drop"

        )


    df_out <- df_h

    df_out[, -1] <-
        df_h[, -1] +
        df_a[, -1]


    match(

        df$teams,

        arrange(
            df_out,
            desc(tot),
            desc(gd),
            desc(gf)
        )[[1]]

    )


}

# =========================================================

# DYNAMIC MODEL

# =========================================================

make_dynamic <- function(
        teams,
        ro = 0.9,
        sigma = 0.1
) {


    n_teams <- nrow(teams)

    n_rounds <- 38

    teams_dynamic <- teams


    # Create columns for the attacking
    # and defensive strength in each round

    for (r in seq_len(n_rounds)) {

        teams_dynamic[[paste0("a_round_", r)]] <-
            NA_real_

        teams_dynamic[[paste0("b_round_", r)]] <-
            NA_real_

    }


    # Generate a random walk in team strength

    for (i in seq_len(n_teams)) {

        td <- MASS::mvrnorm(

            n_rounds - 1,

            mu = c(0, 0),

            Sigma =
                matrix(
                    c(
                        1,
                        ro,
                        ro,
                        1
                    ),
                    nrow = 2
                ) *
                sigma^2

        )


        td <- rbind(
            c(0, 0),
            td
        )


        ad <-
            teams$alpha[i] +
            cumsum(td[, 1])


        bd <-
            teams$beta[i] +
            cumsum(td[, 2])


        teams_dynamic[
            i,
            paste0(
                "a_round_",
                1:n_rounds
            )
        ] <- ad


        teams_dynamic[
            i,
            paste0(
                "b_round_",
                1:n_rounds
            )
        ] <- bd

    }


    teams_dynamic


}

dynamic_season_sim <- function(
        df,
        round,
        team_h,
        team_a,
        tau,
        ro = 0.9,
        sigma = 0.1
) {


    teams_dynamic <- make_dynamic(

        df,

        ro = ro,

        sigma = sigma

    )


    mu_h <- numeric(
        length(round)
    )

    mu_a <- numeric(
        length(round)
    )


    for (i in seq_along(round)) {

        home_idx <- match(
            team_h[i],
            df$teams
        )

        away_idx <- match(
            team_a[i],
            df$teams
        )

        r <- round[i]


        mu_h[i] <- exp(

            tau +

                teams_dynamic[
                    home_idx,
                    paste0(
                        "a_round_",
                        r
                    )
                ] -

                teams_dynamic[
                    away_idx,
                    paste0(
                        "b_round_",
                        r
                    )
                ]

        )


        mu_a[i] <- exp(

            teams_dynamic[
                away_idx,
                paste0(
                    "a_round_",
                    r
                )
            ] -

                teams_dynamic[
                    home_idx,
                    paste0(
                        "b_round_",
                        r
                    )
                ]

        )

    }


    g_h <- rpois(
        length(team_h),
        mu_h
    )

    g_a <- rpois(
        length(team_a),
        mu_a
    )


    p_h <- ifelse(

        g_h > g_a,

        3,

        ifelse(
            g_h == g_a,
            1,
            0
        )

    )


    p_a <- ifelse(

        g_h > g_a,

        0,

        ifelse(
            g_h == g_a,
            1,
            3
        )

    )


    list(

        p_h,

        p_a,

        g_h - g_a,

        g_h,

        g_a

    )


}

dynamic_league_sim <- function(
        df,
        schedule,
        tau,
        sigma = 0.1
) {


    results <- dynamic_season_sim(

        df,

        schedule[, 1],
        schedule[, 2],
        schedule[, 3],

        tau,

        sigma = sigma

    )


    points <- schedule

    points$hp <- results[[1]]

    points$ap <- results[[2]]

    points$gd <- results[[3]]

    points$gh <- results[[4]]

    points$ga <- results[[5]]


    df_h <- points %>%

        group_by(Home.Team) %>%

        summarise(

            tot = sum(hp),

            gd = sum(gd),

            gf = sum(gh),

            .groups = "drop"

        )


    df_a <- points %>%

        group_by(Away.Team) %>%

        summarise(

            tot = sum(ap),

            gd = -sum(gd),

            gf = sum(ga),

            .groups = "drop"

        )


    df_out <- df_h

    df_out[, -1] <-
        df_h[, -1] +
        df_a[, -1]


    match(

        df$teams,

        arrange(

            df_out,

            desc(tot),
            desc(gd),
            desc(gf)

        )[[1]]

    )


}

# =========================================================

# UI

# =========================================================

chapter8_ui <- function(id) {


    ns <- NS(id)


    # =====================================================
    # SIDEBAR
    # =====================================================

    sidebar_controls <- sidebar(

        h4("Football League Simulation"),

        hr(),

        h5("Data source"),

        radioButtons(

            ns("data_source"),

            NULL,

            choices = c(

                "PL fits: 25/26; Predict: 26/27" =
                    "current",

                "PL fits: 24/25; Predict: 25/26" =
                    "previous",

                "Upload my own data" =
                    "upload"

            ),

            selected = "current"

        ),


        conditionalPanel(

            condition =
                "input.data_source == 'upload'",

            ns = ns,


            fileInput(

                ns("pars_file"),

                "Upload team parameters CSV",

                accept = ".csv"

            ),


            helpText(

                "Required columns: teams, alpha, beta"

            ),


            downloadButton(

                ns("download_pars_template"),

                "Download parameters template"

            ),


            fileInput(

                ns("schedule_file"),

                "Upload fixture CSV",

                accept = ".csv"

            ),


            helpText(

                "Required columns: Round, Home.Team, Away.Team"

            ),


            downloadButton(

                ns("download_schedule_template"),

                "Download fixture template"

            )

        ),


        sliderInput(

            ns("tau"),

            "Home advantage (τ)",

            min = -0.5,

            max = 1,

            value = 0.2,

            step = 0.01

        ),


        hr(),


        h5("Simulation settings"),


        numericInput(

            ns("seed"),

            "Random seed",

            value = 44

        ),


        numericInput(

            ns("n_sim"),

            "Number of seasons to simulate",

            value = 1000,

            min = 100,

            step = 100

        ),


        hr(),


        h5("Static model"),


        p(

            "Team strengths remain fixed throughout the season."

        ),


        actionButton(

            ns("run_static"),

            "Simulate static model",

            class = "btn-primary"

        ),


        hr(),


        h5("Dynamic model"),


        sliderInput(

            ns("sigma"),

            "Dynamic variation (σ)",

            min = 0,

            max = 0.2,

            value = 0.05,

            step = 0.01

        ),


        p(

            "Team strengths are allowed to change during the season."

        ),


        actionButton(

            ns("run_dynamic"),

            "Simulate dynamic model",

            class = "btn-success"

        ),


        hr(),



        # =====================================================
        # COMPARISON
        # =====================================================

        h5("Comparison"),

        p(
            "Compare probability distributions based on the static and dynamic models."
        ),

        selectInput(

            ns("comparison_team"),

            "Team for comparison",

            choices = NULL,

            selected = "Arsenal"

        ),

        hr(),


        actionButton(

            ns("reset"),

            "Start again",

            class = "btn-danger"

        )

    )


    # =====================================================
    # OVERVIEW
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

                    "🕸️ Studying Complexity in a Football Model",

                    style = "
                font-size: 1.4rem;
                font-weight: 700;
                color: #2c3e50;
                "

                )

            ),


            p(

                strong("Main idea: "),

                "This module explores how a statistical model of football matches can be used to simulate an entire league season. Rather than treating a single simulated season as a prediction of exactly what will happen, repeated simulations show the range of outcomes that could plausibly occur given the assumptions of the model."

            ),


            hr(),


            h5("Background"),


            p(

                "The model used in this module is a simplified version of the Dixon–Coles approach to modelling football scores. It assumes that the goals scored by the home and away teams are independent Poisson random variables, with their expected values determined by the attacking and defensive strengths of the teams and an additional home advantage."

            ),


            tags$ul(

                tags$li(

                    strong("Match model: "),

                    "expected goals depend on the attacking strength of one team, the defensive strength of the other team, and the home advantage."

                ),

                tags$li(

                    strong("Team strengths: "),

                    "each team has parameters describing its attacking and defensive ability."

                ),

                tags$li(

                    strong("League simulation: "),

                    "the model can be used repeatedly to simulate an entire season and examine the distribution of possible final league positions."

                ),

                tags$li(

                    strong("Static model: "),

                    "team strengths remain fixed throughout the season."

                ),

                tags$li(

                    strong("Dynamic model: "),

                    "team strengths are allowed to change over the course of a season, introducing an additional source of uncertainty."

                )

            ),


            p(

                "The module therefore provides a way to explore how assumptions about model complexity affect the range of possible outcomes."

            ),


            hr(),


            h5("Your options"),


            p(

                "You can choose between pre-specified Premier League data or provide your own team parameters and fixture list."

            ),


            tags$ul(

                tags$li(

                    strong("Use the current Premier League data: "),

                    "use fitted parameters from the 2025/26 season to explore possible outcomes for the 2026/27 season."

                ),

                tags$li(

                    strong("Use the previous Premier League data: "),

                    "use fitted parameters from the 2024/25 season to explore possible outcomes for the 2025/26 season."

                ),

                tags$li(

                    strong("Upload your own data: "),

                    "provide team attack and defence parameters together with a fixture list, allowing the model to be applied to another league or season."

                )

            ),


            p(

                "You can then simulate many complete seasons using either a static or dynamic model."

            ),


            hr(),


            h5("A note on model complexity"),


            p(

                "The dynamic model is more complicated than the static model because it allows team strengths to change during the season. However, greater complexity does not automatically make a model better."

            ),


            p(

                "A more complicated model introduces additional assumptions and sources of variation. The useful question is therefore whether that additional complexity provides a more informative representation of the process we are trying to understand."

            ),


            p(

                "As you explore the results, consider what assumptions are responsible for the differences between the static and dynamic simulations."

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

                        "How much of a football season's outcome is determined by team strength and how much by random variation?"

                    ),

                    tags$li(

                        "Why can a team with a high expected finishing position still have a substantial probability of finishing much lower?"

                    ),

                    tags$li(

                        "How does allowing team strength to change over time affect the distribution of possible league positions?"

                    ),

                    tags$li(

                        "Why might two teams with different strengths still have overlapping distributions of possible finishing positions?"

                    ),

                    tags$li(

                        "When does a more complicated model provide useful additional information?"

                    )

                )

            )

        )

    )


    # =====================================================
    # CODE PANEL
    # =====================================================

    code_panel <- div(

        card(

            card_header("Generated R Code"),

            tags$pre(

                textOutput(
                    ns("generated_code")
                )

            )

        )

    )



    # =====================================================
    # RESULTS PANEL
    # =====================================================

    results_panel <- div(

        uiOutput(
            ns("sim_banner")
        ),

        # -------------------------------------------------
        # MODEL RESULTS
        # -------------------------------------------------

        card(

            card_header(
                "Final League Positions"
            ),

            uiOutput(
                ns("model_tabs")
            )

        )

    )




    # =====================================================
    # LEARNING PANEL
    # =====================================================

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


            tags$div(

                h5(
                    "1. Complex models introduce additional assumptions"
                ),

                p(

                    "A model becomes more complex when we allow additional features of the data-generating process to vary. In the dynamic model, team strengths are allowed to change during the season."

                ),


                hr(),


                h5(
                    "2. Random variation can produce a wide range of outcomes"
                ),

                p(

                    "Even when team strengths are fixed, individual matches contain random variation. Repeated simulation allows us to see the range of league positions that can result from this uncertainty."

                ),


                hr(),


                h5(
                    "3. A single simulation is not a prediction of exactly what will happen"
                ),

                p(

                    "The purpose of simulation is to explore the distribution of possible outcomes. Repeating the simulation many times gives a clearer picture of uncertainty than looking at one simulated season."

                ),


                hr(),


                h5(
                    "4. More complexity changes the distribution of predictions"
                ),

                p(

                    "Allowing team strengths to vary introduces another source of uncertainty. This can change the spread and shape of the distribution of possible finishing positions."

                ),


                hr(),


                h5("Key takeaway"),


                div(

                    style = "
                background-color: #f8f9fa;
                border-left: 5px solid #28a745;
                padding: 12px;
                border-radius: 8px;
                ",

                    p(

                        strong(
                            "More complicated models make different assumptions about the process."
                        ),

                        br(),

                        "Simulation helps us understand how those assumptions affect the predictions produced by the model."

                    )

                )

            )

        )

    )


    # =====================================================
    # PAGE
    # =====================================================

    chapter_page_ui(

        id = id,

        title = "🕸️ Chapter 8: Complexity",

        sidebar = sidebar_controls,

        overview = overview_panel,

        code = code_panel,

        results = results_panel,

        learn = learn_panel

    )


}

# =========================================================

# SERVER

# =========================================================

chapter8_server <- function(id) {


    moduleServer(

        id,

        function(input, output, session) {


            # =================================================
            # REACTIVE STATE
            # =================================================

            static_sim <- reactiveVal(NULL)

            dynamic_sim <- reactiveVal(NULL)


            rv <- reactiveValues(

                sim_running = FALSE,

                data_error = NULL

            )


            # =================================================
            # VALIDATE TEAM DATA
            # =================================================

            validate_teams <- function(df) {

                required <- c(
                    "teams",
                    "alpha",
                    "beta"
                )


                missing_columns <-
                    setdiff(
                        required,
                        names(df)
                    )


                if (
                    length(missing_columns) > 0
                ) {

                    stop(

                        paste0(

                            "Parameters file must contain the following columns: ",

                            paste(
                                required,
                                collapse = ", "
                            ),

                            ". Missing: ",

                            paste(
                                missing_columns,
                                collapse = ", "
                            )

                        )

                    )

                }


                if (
                    nrow(df) == 0
                ) {

                    stop(
                        "The parameters file contains no team records."
                    )

                }


                if (
                    any(
                        is.na(df$teams) |
                        trimws(df$teams) == ""
                    )
                ) {

                    stop(
                        "The parameters file contains blank team names."
                    )

                }


                if (
                    anyDuplicated(df$teams)
                ) {

                    duplicates <-
                        unique(
                            df$teams[
                                duplicated(df$teams)
                            ]
                        )


                    stop(

                        paste0(

                            "The parameters file contains duplicate team names: ",

                            paste(
                                duplicates,
                                collapse = ", "
                            )

                        )

                    )

                }


                if (
                    any(
                        is.na(df$alpha) |
                        !is.numeric(df$alpha)
                    )
                ) {

                    stop(
                        "The alpha column must contain numeric values with no missing values."
                    )

                }


                if (
                    any(
                        is.na(df$beta) |
                        !is.numeric(df$beta)
                    )
                ) {

                    stop(
                        "The beta column must contain numeric values with no missing values."
                    )

                }


                df

            }


            # =================================================
            # VALIDATE SCHEDULE
            # =================================================

            validate_schedule <- function(schedule) {

                required <- c(
                    "Round",
                    "Home.Team",
                    "Away.Team"
                )


                missing_columns <-
                    setdiff(
                        required,
                        names(schedule)
                    )


                if (
                    length(missing_columns) > 0
                ) {

                    stop(

                        paste0(

                            "Schedule file must contain the following columns: ",

                            paste(
                                required,
                                collapse = ", "
                            ),

                            ". Missing: ",

                            paste(
                                missing_columns,
                                collapse = ", "
                            )

                        )

                    )

                }


                if (
                    nrow(schedule) == 0
                ) {

                    stop(
                        "The schedule file contains no fixtures."
                    )

                }


                if (
                    any(
                        is.na(schedule$Home.Team) |
                        trimws(schedule$Home.Team) == ""
                    )
                ) {

                    stop(
                        "The schedule contains blank home-team names."
                    )

                }


                if (
                    any(
                        is.na(schedule$Away.Team) |
                        trimws(schedule$Away.Team) == ""
                    )
                ) {

                    stop(
                        "The schedule contains blank away-team names."
                    )

                }


                schedule

            }


            # =================================================
            # VALIDATE TEAM MATCH BETWEEN FILES
            # =================================================

            validate_team_match <- function(
        teams,
        schedule
            ) {

                parameter_teams <-
                    unique(
                        trimws(
                            teams$teams
                        )
                    )


                schedule_teams <-
                    unique(
                        c(
                            trimws(
                                schedule$Home.Team
                            ),
                            trimws(
                                schedule$Away.Team
                            )
                        )
                    )


                missing_parameters <-
                    setdiff(
                        schedule_teams,
                        parameter_teams
                    )


                unused_parameters <-
                    setdiff(
                        parameter_teams,
                        schedule_teams
                    )


                if (

                    length(missing_parameters) == 0 &&

                    length(unused_parameters) == 0

                ) {

                    return(TRUE)

                }


                message_parts <- character(0)


                if (
                    length(missing_parameters) > 0
                ) {

                    message_parts <-
                        c(

                            message_parts,

                            paste0(

                                "The following teams appear in the schedule but have no parameter values: ",

                                paste(
                                    missing_parameters,
                                    collapse = ", "
                                ),

                                "."

                            )

                        )

                }


                if (
                    length(unused_parameters) > 0
                ) {

                    message_parts <-
                        c(

                            message_parts,

                            paste0(

                                "The following teams appear in the parameter file but not in the schedule: ",

                                paste(
                                    unused_parameters,
                                    collapse = ", "
                                ),

                                "."

                            )

                        )

                }


                stop(

                    paste(

                        c(
                            "The team names in the parameter file and schedule do not match.",
                            message_parts,
                            "Please make sure that every team in the schedule has a corresponding row in the parameter file and that the names are spelled identically."
                        ),

                        collapse = "\n\n"

                    )

                )

            }


            # =================================================
            # DATA SOURCE
            # =================================================

            teams_data <- reactive({

                source <- input$data_source


                if (source == "current") {

                    validate_teams(
                        PL25_pars$teams
                    )


                } else if (source == "previous") {

                    validate_teams(
                        PL24_pars$teams
                    )


                } else if (source == "upload") {

                    req(
                        input$pars_file
                    )


                    validate_teams(

                        read.csv(

                            input$pars_file$datapath,

                            stringsAsFactors = FALSE

                        )

                    )

                }

            })


            schedule_data <- reactive({

                source <- input$data_source


                if (source == "current") {

                    PL26_schedule


                } else if (source == "previous") {

                    PL25_schedule


                } else if (source == "upload") {

                    req(
                        input$schedule_file
                    )


                    validate_schedule(

                        read.csv(

                            input$schedule_file$datapath,

                            stringsAsFactors = FALSE

                        )

                    )

                }

            })


            # =================================================
            # VALIDATED INPUT DATA
            # =================================================

            validated_data <- reactive({

                teams <-
                    teams_data()


                schedule <-
                    schedule_data()


                validate_team_match(

                    teams,

                    schedule

                )


                list(

                    teams = teams,

                    schedule = schedule

                )

            })


            # =================================================
            # DISPLAY UPLOAD VALIDATION ERROR
            # =================================================

            output$data_validation <- renderUI({

                if (
                    input$data_source != "upload"
                ) {

                    return(NULL)

                }


                if (
                    is.null(input$pars_file) ||
                    is.null(input$schedule_file)
                ) {

                    return(NULL)

                }


                result <- tryCatch(

                    {

                        validated_data()

                        NULL

                    },

                    error = function(e) {

                        div(

                            style = "
                        margin-top: 10px;
                        padding: 12px;
                        background-color: #f8d7da;
                        border: 1px solid #f5c2c7;
                        color: #842029;
                        border-radius: 6px;
                        ",

                            strong(
                                "Data validation error"
                            ),

                            br(),

                            HTML(
                                gsub(
                                    "\n\n",
                                    "<br><br>",
                                    e$message
                                )
                            )

                        )

                    }

                )


                result

            })


            # =================================================
            # UPLOAD VALIDATION MESSAGE IN SIDEBAR
            # =================================================

            insertUI(

                selector = paste0(
                    "#",
                    session$ns("schedule_file")
                ),

                where = "afterEnd",

                ui = uiOutput(
                    session$ns(
                        "data_validation"
                    )
                ),

                immediate = TRUE

            )


            # =================================================
            # TAU
            # =================================================

            tau_data <- reactive({

                input$tau

            })


            observeEvent(

                input$data_source,

                {

                    tau <- switch(

                        input$data_source,

                        current =
                            PL25_pars$tau,

                        previous =
                            PL24_pars$tau,

                        upload =
                            NULL

                    )


                    if (

                        !is.null(tau) &&

                        length(tau) == 1 &&

                        !is.na(tau)

                    ) {

                        updateSliderInput(

                            session,

                            "tau",

                            value = tau

                        )

                    }

                }

            )


            # =================================================
            # UPDATE TEAM SELECTOR
            # =================================================

            observe({

                req(
                    input$data_source
                )


                result <- tryCatch(

                    {

                        teams_data()$teams

                    },

                    error = function(e) {

                        NULL

                    }

                )


                if (
                    !is.null(result) &&
                    length(result) > 0
                ) {

                    current_team <-
                        input$comparison_team


                    if (
                        is.null(current_team) ||
                        !current_team %in% result
                    ) {

                        current_team <-
                            result[1]

                    }


                    updateSelectInput(

                        session,

                        "comparison_team",

                        choices = result,

                        selected = current_team

                    )

                }

            })

            # =================================================
            # KEEP CURRENT MODEL TAB WHEN COMPARISON TEAM CHANGES
            # =================================================

            observeEvent(

                input$comparison_team,

                {

                    current_tab <-
                        input$model_tabs_nav


                    # If the user is currently looking at the
                    # comparison tab, restore it after the
                    # comparison plot is redrawn.

                    if (
                        identical(
                            current_tab,
                            "comparison"
                        )
                    ) {

                        session$onFlushed(

                            function() {

                                bslib::nav_select(

                                    "model_tabs_nav",

                                    selected = "comparison"

                                )

                            },

                            once = TRUE

                        )

                    }

                },

                ignoreInit = TRUE

            )

            # =================================================
            # DATA SOURCE DISPLAY
            # =================================================

            output$data_source <- renderUI({

                source <-
                    input$data_source


                if (
                    source == "current"
                ) {

                    div(

                        style = "
                    padding: 10px;
                    background-color: #e8f4ea;
                    border-radius: 6px;
                    font-weight: 600;
                    ",

                        "Using fitted 2025/26 model parameters and 2025/26 fixtures"

                    )


                } else if (
                    source == "previous"
                ) {

                    div(

                        style = "
                    padding: 10px;
                    background-color: #e8eef8;
                    border-radius: 6px;
                    font-weight: 600;
                    ",

                        "Using fitted 2024/25 model parameters and 2025/26 fixtures"

                    )


                } else {

                    req(
                        input$pars_file,
                        input$schedule_file
                    )


                    div(

                        style = "
                    padding: 10px;
                    background-color: #fff3cd;
                    border-radius: 6px;
                    font-weight: 600;
                    ",

                        "Using uploaded data",

                        br(),

                        paste(
                            "Parameters:",
                            input$pars_file$name
                        ),

                        br(),

                        paste(
                            "Fixtures:",
                            input$schedule_file$name
                        )

                    )

                }

            })


            # =================================================
            # RESET SIMULATIONS WHEN DATA SOURCE CHANGES
            # =================================================

            observeEvent(

                input$data_source,

                {

                    static_sim(NULL)

                    dynamic_sim(NULL)

                    rv$sim_running <-
                        FALSE

                }

            )


            # =================================================
            # RESET SIMULATIONS WHEN UPLOADED FILES CHANGE
            # =================================================

            observeEvent(

                list(
                    input$pars_file,
                    input$schedule_file
                ),

                {

                    if (
                        input$data_source == "upload"
                    ) {

                        static_sim(NULL)

                        dynamic_sim(NULL)

                        rv$sim_running <-
                            FALSE

                    }

                },

                ignoreInit = TRUE

            )


            # =================================================
            # DOWNLOAD PARAMETERS TEMPLATE
            # =================================================

            output$download_pars_template <-
                downloadHandler(

                    filename = function() {

                        "team_parameters_template.csv"

                    },

                    content = function(file) {

                        write.csv(

                            PL25_pars$teams,

                            file,

                            row.names = FALSE

                        )

                    }

                )


            # =================================================
            # DOWNLOAD SCHEDULE TEMPLATE
            # =================================================

            output$download_schedule_template <-
                downloadHandler(

                    filename = function() {

                        "fixture_template.csv"

                    },

                    content = function(file) {

                        write.csv(

                            PL26_schedule,

                            file,

                            row.names = FALSE

                        )

                    }

                )


            # =================================================
            # SIMULATION BANNER
            # =================================================

            output$sim_banner <- renderUI({

                if (
                    rv$sim_running
                ) {

                    div(

                        style = "
                    padding: 12px;
                    margin-bottom: 10px;
                    background-color: #fff3cd;
                    border: 1px solid #ffeeba;
                    border-radius: 6px;
                    font-weight: 600;
                    ",

                        "Simulations in Progress"

                    )

                }

            })


            # =================================================
            # GENERATED CODE
            # =================================================

            output$generated_code <- renderText({

                source_text <-
                    switch(

                        input$data_source,

                        current = paste0(

                            "teams <- PL25_pars$teams\n",

                            "schedule <- PL26_schedule\n"

                        ),

                        previous = paste0(

                            "teams <- PL24_pars$teams\n",

                            "schedule <- PL25_schedule\n"

                        ),

                        upload = paste0(

                            "teams <- read.csv(\"your_parameters.csv\")\n",

                            "schedule <- read.csv(\"your_schedule.csv\")\n"

                        )

                    )


                paste0(

                    "# Data source\n",

                    source_text,

                    "\n",

                    "# Static model\n",

                    "league_sim(\n",

                    "  df = teams,\n",

                    "  schedule = schedule,\n",

                    "  tau = ",

                    round(
                        tau_data(),
                        3
                    ),

                    "\n",

                    ")\n\n",

                    "# Dynamic model\n",

                    "dynamic_league_sim(\n",

                    "  df = teams,\n",

                    "  schedule = schedule,\n",

                    "  tau = ",

                    round(
                        tau_data(),
                        3
                    ),

                    ",\n",

                    "  sigma = ",

                    input$sigma,

                    "\n",

                    ")"

                )

            })


            # =================================================
            # STATIC SIMULATION
            # =================================================

            observeEvent(

                input$run_static,

                {

                    data <- tryCatch(

                        validated_data(),

                        error = function(e) {

                            showNotification(

                                e$message,

                                type = "error",

                                duration = 8

                            )

                            NULL

                        }

                    )


                    if (
                        is.null(data)
                    ) {

                        return()

                    }


                    rv$sim_running <- TRUE


                    seed <-
                        input$seed

                    n_sim <-
                        input$n_sim

                    teams <-
                        data$teams

                    tau <-
                        tau_data()

                    sched <-
                        data$schedule


                    later::later(

                        function() {

                            set.seed(
                                seed
                            )


                            sims <- sapply(

                                seq_len(n_sim),

                                function(x) {

                                    league_sim(

                                        teams,

                                        sched,

                                        tau

                                    )

                                }

                            )


                            rownames(sims) <-
                                teams$teams


                            static_sim(
                                sims
                            )

                            rv$sim_running <-
                                FALSE

                            # Disable static simulation button after completion
                            updateActionButton(
                                session,
                                "run_static",
                                disabled = TRUE
                            )

                            # Automatically switch to the Static model tab
                            bslib::nav_select(
                                "model_tabs_nav",
                                "Static model",
                                session = session
                            )

                        },

                        0.05

                    )

                }

            )


            # =================================================
            # DYNAMIC SIMULATION
            # =================================================

            observeEvent(

                input$run_dynamic,

                {

                    data <- tryCatch(

                        validated_data(),

                        error = function(e) {

                            showNotification(

                                e$message,

                                type = "error",

                                duration = 8

                            )

                            NULL

                        }

                    )


                    if (
                        is.null(data)
                    ) {

                        return()

                    }


                    rv$sim_running <- TRUE


                    seed <-
                        input$seed

                    n_sim <-
                        input$n_sim

                    sigma <-
                        input$sigma

                    teams <-
                        data$teams

                    tau <-
                        tau_data()

                    sched <-
                        data$schedule


                    later::later(

                        function() {

                            set.seed(
                                seed
                            )


                            sims <- sapply(

                                seq_len(n_sim),

                                function(x) {

                                    dynamic_league_sim(

                                        teams,

                                        sched,

                                        tau,

                                        sigma

                                    )

                                }

                            )


                            rownames(sims) <-
                                teams$teams


                            dynamic_sim(
                                sims
                            )

                            rv$sim_running <-
                                FALSE

                            # Disable dynamic simulation button after completion
                            updateActionButton(
                                session,
                                "run_dynamic",
                                disabled = TRUE
                            )

                            # Automatically switch to the Dynamic model tab
                            bslib::nav_select(
                                "model_tabs_nav",
                                "Dynamic model",
                                session = session
                            )

                        },

                        0.05

                    )

                }

            )


            # =================================================
            # MODEL RESULT TABS
            # =================================================

            output$model_tabs <- renderUI({

                static_available <-
                    !is.null(
                        static_sim()
                    )


                dynamic_available <-
                    !is.null(
                        dynamic_sim()
                    )


                comparison_available <-
                    static_available &&
                    dynamic_available


                # -------------------------------------------------
                # STATIC MODEL PANEL
                # -------------------------------------------------

                static_panel <- nav_panel(

                    "Static model",

                    if (static_available) {

                        plotOutput(

                            session$ns(
                                "static_plot"
                            ),

                            height = 650

                        )

                    } else {

                        div(

                            style = "
                height: 650px;
                display: flex;
                flex-direction: column;
                align-items: center;
                justify-content: center;
                text-align: center;
                color: #6c757d;
                ",

                            h5(
                                "No static simulation results yet"
                            ),

                            p(
                                "Run the static model using the control in the sidebar."
                            )

                        )

                    }

                )


                # -------------------------------------------------
                # DYNAMIC MODEL PANEL
                # -------------------------------------------------

                dynamic_panel <- nav_panel(

                    "Dynamic model",

                    if (dynamic_available) {

                        plotOutput(

                            session$ns(
                                "dynamic_plot"
                            ),

                            height = 650

                        )

                    } else {

                        div(

                            style = "
                height: 650px;
                display: flex;
                flex-direction: column;
                align-items: center;
                justify-content: center;
                text-align: center;
                color: #6c757d;
                ",

                            h5(
                                "No dynamic simulation results yet"
                            ),

                            p(
                                "Run the dynamic model using the control in the sidebar."
                            )

                        )

                    }

                )


                # -------------------------------------------------
                # COMPARISON PANEL
                # -------------------------------------------------

                comparison_panel <- nav_panel(

                    "Comparison",

                    if (comparison_available) {

                        plotOutput(

                            session$ns(
                                "comparison_plot"
                            ),

                            height = 400

                        )

                    } else {

                        div(

                            style = "
                height: 400px;
                display: flex;
                flex-direction: column;
                align-items: center;
                justify-content: center;
                text-align: center;
                color: #6c757d;
                ",

                            h5(
                                "Comparison not available yet"
                            ),

                            p(
                                "Run both the static and dynamic models to compare their probability distributions."
                            )

                        )

                    }

                )


                # -------------------------------------------------
                # THREE TABS
                # -------------------------------------------------

                do.call(

                    navset_tab,

                    list(

                        id =
                            session$ns(
                                "model_tabs_nav"
                            ),

                        static_panel,

                        dynamic_panel,

                        comparison_panel

                    )

                )

            })



            # =================================================
            # STATIC PLOT
            # =================================================

            output$static_plot <- renderPlot({

                req(
                    static_sim()
                )


                league_position_plot(

                    teams_data(),

                    static_sim(),

                    rows = 4

                )

            })


            # =================================================
            # DYNAMIC PLOT
            # =================================================

            output$dynamic_plot <- renderPlot({

                req(
                    dynamic_sim()
                )


                league_position_plot(

                    teams_data(),

                    dynamic_sim(),

                    rows = 4

                )

            })



            # =================================================
            # COMPARISON PLOT
            # =================================================

            output$comparison_plot <- renderPlot({

                req(

                    static_sim(),

                    dynamic_sim(),

                    input$comparison_team

                )


                team <-
                    input$comparison_team


                s <-
                    static_sim()[team, ]

                d <-
                    dynamic_sim()[team, ]


                df <- data.frame(

                    Position = c(
                        s,
                        d
                    ),

                    Model = rep(

                        c(
                            "Static",
                            "Dynamic"
                        ),

                        each =
                            length(s)

                    )

                )


                ggplot(

                    df,

                    aes(
                        Position,
                        fill = Model
                    )

                ) +

                    geom_bar(

                        aes(

                            y =
                                after_stat(
                                    count /
                                        sum(count)
                                )

                        ),

                        position = "dodge"

                    ) +

                    scale_x_continuous(

                        breaks =
                            c(
                                1,
                                5,
                                10,
                                15,
                                20
                            ),

                        limits =
                            c(
                                0.5,
                                20.5
                            )

                    ) +

                    labs(

                        title =
                            paste(
                                "Static vs Dynamic:",
                                team
                            ),

                        subtitle =
                            "Distribution of simulated final league positions",

                        x =
                            "League Position",

                        y =
                            "Probability",

                        fill =
                            ""

                    ) +

                    theme_minimal(
                        base_size = 16
                    ) +

                    theme(

                        plot.title =
                            element_text(
                                size = 20,
                                face = "bold"
                            ),

                        plot.subtitle =
                            element_text(
                                size = 15,
                                color = "#6c757d"
                            ),

                        axis.title =
                            element_text(
                                size = 18,
                                face = "bold"
                            ),

                        axis.text =
                            element_text(
                                size = 14
                            ),

                        legend.text =
                            element_text(
                                size = 14
                            )

                    )

            })



            # =================================================
            # START AGAIN
            # =================================================

            observeEvent(

                input$reset,

                {

                    static_sim(NULL)

                    dynamic_sim(NULL)

                    rv$sim_running <-
                        FALSE

                    # Re-enable both simulation buttons
                    updateActionButton(
                        session,
                        "run_static",
                        disabled = FALSE
                    )

                    updateActionButton(
                        session,
                        "run_dynamic",
                        disabled = FALSE
                    )


                    new_seed <-
                        sample(
                            1:999,
                            1
                        )


                    updateNumericInput(

                        session,

                        "seed",

                        value =
                            new_seed

                    )

                }

            )

        }

    )


}
