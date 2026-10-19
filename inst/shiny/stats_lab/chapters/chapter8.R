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

        p(
            "Explore possible league outcomes by simulating
        complete football seasons."
        ),

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

            hr(),

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

        hr(),

        h5("Match settings"),

        sliderInput(

            ns("tau"),

            "Home advantage (τ)",

            min = -0.5,

            max = 1,

            value = 0.2,

            step = 0.01

        ),

        helpText(
            "Controls the home advantage in the expected
        number of goals scored."
        ),

        hr(),

        h5("Simulation settings"),

        numericInput(

            ns("seed"),

            "Random seed",

            value = 44

        ),

        helpText(
            "The same seed and settings allow you to reproduce
        a simulation."
        ),

        numericInput(

            ns("n_sim"),

            "Number of seasons to simulate",

            value = 1000,

            min = 100,

            step = 100

        ),

        helpText(
            "More simulations generally provide more stable
        estimates of finishing-position probabilities."
        ),

        hr(),

        h5("1. Static model"),

        p(
            "Team attacking and defensive strengths remain
        fixed throughout the season."
        ),

        actionButton(

            ns("run_static"),

            "Simulate static model",

            class = "btn-primary"

        ),

        hr(),

        h5("2. Dynamic model"),

        p(
            "Team attacking and defensive strengths can change
        during the season, introducing additional
        uncertainty."
        ),

        sliderInput(

            ns("sigma"),

            "Dynamic variation (σ)",

            min = 0,

            max = 0.2,

            value = 0.05,

            step = 0.01

        ),

        helpText(
            "Larger values allow greater changes in team
        strengths from one round to the next."
        ),

        actionButton(

            ns("run_dynamic"),

            "Simulate dynamic model",

            class = "btn-success"

        ),

        hr(),

        h5("3. Compare the models"),

        p(
            "After running both models, compare their
        distributions of possible finishing positions
        for an individual team."
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

                    "🕸 Studying Simulation in a Football Model",

                    style = "
                    font-size: 1.4rem;
                    font-weight: 700;
                    color: #2c3e50;
                "

                )

            ),

            p(

                strong("Main idea: "),

                "How can we explore the possible outcomes of
            a football season when the results of
            individual matches are uncertain? This module
            uses a statistical model of football scores
            to simulate entire league seasons repeatedly.
            By examining the resulting distributions of
            final league positions, we can investigate how
            team strength and random variation combine
            to produce different outcomes."

            ),

            hr(),

            h5("Background"),

            p(

                "The model used in this module is a simplified
            version of the Dixon–Coles approach to
            modelling football scores. It assumes that
            the goals scored by the home and away teams
            are independent Poisson random variables,
            with their expected values determined by
            the attacking and defensive strengths of the
            teams and an additional home advantage."

            ),

            tags$ul(

                tags$li(

                    strong("Match model: "),

                    "expected goals depend on the attacking
                strength of one team, the defensive
                strength of the other team, and home
                advantage."

                ),

                tags$li(

                    strong("Team strengths: "),

                    "each team has parameters describing its
                attacking and defensive ability."

                ),

                tags$li(

                    strong("Season simulation: "),

                    "the model generates match results across
                an entire fixture list, allowing us to
                calculate a final league table."

                ),

                tags$li(

                    strong("Repeated simulation: "),

                    "simulating many seasons reveals the range
                and relative probabilities of possible
                final league positions."

                ),

                tags$li(

                    strong("Static and dynamic models: "),

                    "the static model keeps team strengths
                fixed, while the dynamic model allows
                them to change during the season."

                )

            ),

            p(

                "Simulation allows us to move beyond a single
            prediction and investigate uncertainty.
            Comparing the two models also helps us
            explore how different assumptions about team
            strengths affect the results."

            ),

            hr(),

            h5("Your options"),

            p(

                "You can use the supplied Premier League data
            or provide your own team parameters and
            fixture list."

            ),

            tags$ul(

                tags$li(

                    strong("Current Premier League data: "),

                    "use parameters fitted to the 2025/26
                season to explore possible outcomes for
                the 2026/27 season."

                ),

                tags$li(

                    strong("Previous Premier League data: "),

                    "use parameters fitted to the 2024/25
                season to explore possible outcomes for
                the 2025/26 season."

                ),

                tags$li(

                    strong("Upload your own data: "),

                    "provide team attack and defence
                parameters together with a fixture list,
                allowing the model to be applied to
                another league or season."

                )

            ),

            p(

                "Choose how many seasons to simulate, run
            either model, and examine the distributions
            of possible finishing positions. Once both
            models have been run, you can compare their
            results for an individual team."

            ),

            hr(),

            h5("Understanding simulation"),

            p(

                "A simulated season is one possible outcome
            generated under the assumptions of the model.
            It is not a forecast of exactly what will
            happen. Individual match results vary
            randomly, so repeating the simulation
            produces different league tables even when
            the underlying team strengths remain
            unchanged."

            ),

            p(

                "The static model isolates the variation
            arising from random match results when team
            strengths are fixed. The dynamic model
            introduces an additional source of uncertainty
            by allowing team strengths to change during
            the season."

            ),

            p(

                "Comparing the results helps us investigate
            how this additional uncertainty changes the
            distribution of possible league positions.
            A wider distribution, for example, would
            indicate a greater range of possible
            finishing positions under that model."

            ),

            p(

                "Simulated probabilities are conditional on
            the model and its assumptions. They describe
            what the model considers plausible rather
            than guaranteeing what will happen in the
            real league."

            ),

            hr(),

            h5("A suggested way to explore the module"),

            tags$ol(

                tags$li(

                    strong("Start with the static model. "),

                    "Simulate a large number of seasons and
                examine the distribution of finishing
                positions for each team."

                ),

                tags$li(

                    strong("Look beyond the most likely result. "),

                    "Identify teams that could finish in a
                wide range of positions, even if their
                most likely position is relatively high
                or low."

                ),

                tags$li(

                    strong("Run the dynamic model. "),

                    "Allow team strengths to change during
                the season and examine how the results
                differ."

                ),

                tags$li(

                    strong("Compare the models. "),

                    "Choose a team and compare its
                distributions under the two models."

                ),

                tags$li(

                    strong("Interpret the differences. "),

                    "Consider whether changes in the
                distributions are consistent with the
                additional variation introduced by the
                dynamic model."

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

                        "How much variation in final league
                    positions can arise from random match
                    results alone?"

                    ),

                    tags$li(

                        "Why can a team with a high expected
                    finishing position still have a
                    substantial probability of finishing
                    much lower?"

                    ),

                    tags$li(

                        "How does allowing team strength to
                    change over time affect the
                    distribution of possible league
                    positions?"

                    ),

                    tags$li(

                        "Why might two teams with different
                    strengths still have overlapping
                    distributions of possible finishing
                    positions?"

                    ),

                    tags$li(

                        "How many seasons should we simulate
                    to obtain a useful estimate of
                    possible outcomes?"

                    ),

                    tags$li(

                        "What can simulation tell us, and
                    what can it not tell us, about the
                    actual outcome of a season?"

                    )

                )

            )

        )

    )


    # =====================================================
    # GENERATED CODE PANEL
    # =====================================================

    code_panel <- div(

        card(

            card_header("Generated R Code"),

            p(

                "The code below illustrates how the selected
            data and model settings can be used in R."

            ),

            tags$pre(

                style = "
                background-color: #f8f9fa;
                padding: 14px;
                border-radius: 8px;
                overflow-x: auto;
            ",

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

        card(

            card_header("Simulated Final League Positions"),

            p(

                "Each distribution shows how frequently
            teams finish in different league positions
            across the simulated seasons. The results
            represent probabilities under the selected
            model, not certainties about the actual
            season."

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

                    "What have you learned?",

                    style = "
                    font-size: 1.3rem;
                    font-weight: 700;
                    color: #2c3e50;
                "

                )

            ),

            h5("1. Simulation explores possible outcomes"),

            p(

                "A statistical model can generate a possible
            season of match results. Repeating this
            process produces many possible league tables,
            helping us understand the range of outcomes
            consistent with the model."

            ),

            hr(),

            h5("2. Random variation creates uncertainty"),

            p(

                "Even when team strengths remain fixed,
            match results vary randomly. Consequently,
            a team's final league position is uncertain
            rather than predetermined."

            ),

            hr(),

            h5("3. Distributions provide more information than a single prediction"),

            p(

                "The distribution of simulated finishing
            positions shows which outcomes are more or
            less likely under the model. It also reveals
            the possibility of unexpectedly good or
            poor seasons."

            ),

            hr(),

            h5("4. Model assumptions affect simulated outcomes"),

            p(

                "The static and dynamic models make different
            assumptions about team strengths. Comparing
            their results helps us investigate how
            allowing strengths to change during a season
            affects the uncertainty in final league
            positions."

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

                        "Simulation helps us explore
                    uncertainty by generating many
                    possible outcomes."

                    ),

                    br(),

                    "The results depend on the assumptions
                of the model, so simulated probabilities
                should be interpreted as conditional
                estimates rather than guarantees about
                what will happen."

                )

            )

        )

    )


    # =====================================================
    # PAGE
    # =====================================================

    chapter_page_ui(

        id = id,

        title = "🕸 Module 8: Simulation",

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
