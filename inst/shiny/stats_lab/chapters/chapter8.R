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

        teams_dynamic[[paste0("a_round_", r)]] <- NA_real_

        teams_dynamic[[paste0("b_round_", r)]] <- NA_real_

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


        h5("Comparison"),


        selectInput(

            ns("comparison_team"),

            "Team for comparison",

            choices = NULL,

            selected = "Arsenal"

        ),


        actionButton(

            ns("run_compare"),

            "Compare static vs dynamic",

            class = "btn-warning",

            disabled = TRUE

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


        card(

            card_header(
                "Final League Positions (Static Model)"
            ),

            plotOutput(

                ns("static_plot"),

                height = 650

            )

        ),


        br(),


        card(

            card_header(
                "Final League Positions (Dynamic Model)"
            ),

            plotOutput(

                ns("dynamic_plot"),

                height = 650

            )

        ),


        br(),


        card(

            card_header(
                "Static vs Dynamic Comparison"
            ),

            plotOutput(

                ns("comparison_plot"),

                height = 400

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

                sim_running = FALSE

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


                if (
                    !all(
                        required %in%
                        names(df)
                    )
                ) {

                    stop(

                        "Parameters file must contain: teams, alpha, beta"

                    )

                }


                df

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


                    read.csv(

                        input$schedule_file$datapath,

                        stringsAsFactors = FALSE

                    )

                }

            })


            tau_data <- reactive({

                input$tau

            })


            # =================================================
            # UPDATE COMPARISON BUTTON
            # =================================================

            observe({

                if (

                    !is.null(static_sim()) &&

                    !is.null(dynamic_sim())

                ) {

                    updateActionButton(

                        session,

                        "run_compare",

                        disabled = FALSE

                    )

                } else {

                    updateActionButton(

                        session,

                        "run_compare",

                        disabled = TRUE

                    )

                }

            })


            # =================================================
            # UPDATE TEAM SELECTOR
            # =================================================

            observeEvent(

                teams_data(),

                {

                    teams <-
                        teams_data()$teams


                    updateSelectInput(

                        session,

                        "comparison_team",

                        choices = teams,

                        selected = teams[1]

                    )

                }

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

                }

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

                    rv$sim_running <- TRUE


                    seed <-
                        input$seed

                    n_sim <-
                        input$n_sim

                    teams <-
                        teams_data()

                    tau <-
                        tau_data()

                    sched <-
                        schedule_data()


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

                    rv$sim_running <- TRUE


                    seed <-
                        input$seed

                    n_sim <-
                        input$n_sim

                    sigma <-
                        input$sigma

                    teams <-
                        teams_data()

                    tau <-
                        tau_data()

                    sched <-
                        schedule_data()


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

                        },

                        0.05

                    )

                }

            )


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

                    4

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

                    4

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

                        axis.title =
                            element_text(
                                size = 18,
                                face = "bold"
                            ),

                        axis.text =
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

