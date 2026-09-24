# =========================================================

# CHAPTER 7

# MODELS

# =========================================================

# =========================================================

# TWO-DICE GAME

# =========================================================

double_dice_game_sim <- function(n, p) {


    dice_number <- sample(
        1:2,
        n,
        prob = c(p, 1 - p),
        replace = TRUE
    )

    score <- numeric(n)

    for(i in seq_len(n)) {

        if(dice_number[i] == 1) {
            score[i] <- sample(1:6, 1)
        } else {
            score[i] <- sample(4:9, 1)
        }
    }

    score


}

dd_ests <- function(game_score_data){


    data_tab <- table(
        factor(game_score_data, levels = 1:9)
    )

    data_cut <- cut(
        game_score_data,
        c(0,3,6,9)
    )

    data_cut_tab <- table(
        factor(
            data_cut,
            levels = c(
                "(0,3]",
                "(3,6]",
                "(6,9]"
            )
        )
    )

    c(
        data_tab / length(game_score_data),
        data_cut_tab / length(game_score_data),
        data_cut_tab[1] /
            (2 * (data_cut_tab[1] + data_cut_tab[3])),
        data_cut_tab[3] /
            (2 * (data_cut_tab[1] + data_cut_tab[3]))
    )


}

m0_lik <- function(data, ests){
    length(data) * log(9)
}

m1_lik <- function(data, ests){


    probs <- pmax(
        ests[1:9],
        1e-12
    )

    data_tab <- table(
        factor(
            data,
            levels = 1:9
        )
    )

    -sum(
        data_tab * log(probs)
    )


}

m2_lik <- function(data, ests){


    probs <- pmax(
        ests[10:12] / 3,
        1e-12
    )

    data_cut <- cut(
        data,
        c(0,3,6,9)
    )

    data_cut_tab <- table(
        factor(
            data_cut,
            levels = c(
                "(0,3]",
                "(3,6]",
                "(6,9]"
            )
        )
    )

    -sum(
        data_cut_tab * log(probs)
    )


}

m3_lik <- function(data, ests){


    p1 <- max(
        ests[13] / 3,
        1e-12
    )

    p3 <- max(
        ests[14] / 3,
        1e-12
    )

    data_cut <- cut(
        data,
        c(0,3,6,9)
    )

    data_cut_tab <- table(
        factor(
            data_cut,
            levels = c(
                "(0,3]",
                "(3,6]",
                "(6,9]"
            )
        )
    )

    -(
        data_cut_tab[1] * log(p1) +
            data_cut_tab[2] * log(1/6) +
            data_cut_tab[3] * log(p3)
    )


}

dd_all_lik <- function(data, ests){


    c(
        m0_lik(data, ests),
        m1_lik(data, ests),
        m2_lik(data, ests),
        m3_lik(data, ests)
    )


}

cv_lik <- function(data, K = 5){


    ll <- rep(
        Inf,
        4
    )

    while(any(is.infinite(ll))) {

        nt <- round(
            length(data) / K
        )

        ind <- sample(
            rep(
                1:K,
                length.out = length(data)
            )
        )

        l <- NULL

        for(i in 1:K){

            df <- data[ind != i]
            dt <- data[ind == i]

            ests <- dd_ests(df)

            l <- rbind(
                l,
                dd_all_lik(
                    dt,
                    ests
                )
            )
        }

        ll <- apply(
            l,
            2,
            sum
        )
    }

    ll


}

double_dice_game_model_check <- function(
        data,
        seed = NULL
){


    if(!is.null(seed))
        set.seed(seed)

    ests <- dd_ests(data)

    l1 <- dd_all_lik(
        data,
        ests
    )

    l2 <- cv_lik(data)

    l3 <- cv_lik(
        data,
        K = 100
    )

    df <- rbind(
        l1,
        l2,
        l3
    ) |>
        as.data.frame() |>
        round(2)

    rownames(df) <- c(
        "in-sample",
        "cross-validation",
        "leave-one-out"
    )

    colnames(df) <- c(
        "Model N",
        "Model S",
        "Model D",
        "Model P"
    )

    t(df)


}

mod_ests <- function(x){


    r1 <- (
        x[1] +
            x[2] +
            x[3]
    ) / (
        3 * sum(x)
    )

    r2 <- (
        x[4] +
            x[5] +
            x[6]
    ) / (
        3 * sum(x)
    )

    r3 <- (
        x[7] +
            x[8] +
            x[9]
    ) / (
        3 * sum(x)
    )

    q <- (
        x[1] +
            x[2] +
            x[3]
    ) / (
        sum(x) -
            x[4] -
            x[5] -
            x[6]
    )

    p_N <- rep(
        1/9,
        9
    )

    p_S <- x / sum(x)

    p_D <- rep(
        c(r1, r2, r3),
        each = 3
    )

    p_P <- rep(
        c(
            q/6,
            1/6,
            (1-q)/6
        ),
        each = 3
    )

    list(
        p_N = p_N,
        p_S = p_S,
        p_D = p_D,
        p_P = p_P
    )


}

# =========================================================

# FOOTBALL MODEL CALCULATOR

# =========================================================

football_match_means <- function(
        alpha_home,
        beta_home,
        alpha_away,
        beta_away,
        tau
){


    list(

        home =
            exp(
                tau +
                    alpha_home -
                    beta_away
            ),

        away =
            exp(
                alpha_away -
                    beta_home
            )

    )


}

football_score_matrix <- function(
        mu_home,
        mu_away
){


    hp <- c(
        dpois(
            0:5,
            mu_home
        ),
        1 -
            ppois(
                5,
                mu_home
            )
    )

    ap <- c(
        dpois(
            0:5,
            mu_away
        ),
        1 -
            ppois(
                5,
                mu_away
            )
    )

    df <- expand.grid(

        Home = factor(
            c(
                0:5,
                "6+"
            ),
            levels = c(
                0:5,
                "6+"
            )
        ),

        Away = factor(
            c(
                0:5,
                "6+"
            ),
            levels = c(
                0:5,
                "6+"
            )
        )

    )

    df$Prob <- as.vector(
        outer(
            hp,
            ap
        )
    )

    df


}

# =========================================================

# FOOTBALL 1/X/2 PROBABILITIES

# =========================================================

football_match_win_probs <- function(
        mu_home,
        mu_away,
        score_max = 20
){


    home_probs <- dpois(
        0:score_max,
        mu_home
    )

    away_probs <- dpois(
        0:score_max,
        mu_away
    )

    score_probs <- outer(
        home_probs,
        away_probs,
        "*"
    )

    home_win_prob <-
        sum(
            score_probs[
                row(score_probs) >
                    col(score_probs)
            ]
        )

    away_win_prob <-
        sum(
            score_probs[
                row(score_probs) <
                    col(score_probs)
            ]
        )

    draw_prob <-
        sum(
            diag(score_probs)
        )

    c(
        home_win = home_win_prob,
        draw = draw_prob,
        away_win = away_win_prob
    )


}

# =========================================================

# CHAPTER 7 UI

# =========================================================

chapter7_ui <- function(id){


    library(shinyjs)

    ns <- NS(id)


    # =====================================================
    # SIDEBAR
    # =====================================================

    sidebar_controls <- sidebar(

        h4("Chapter 7 investigations"),

        radioButtons(

            ns("activity"),

            "Investigate:",

            choices = c(
                "Two-Dice Models" = "dice",
                "Football Model Calculator" = "football"
            ),

            selected = "dice"
        ),

        hr(),


        # =================================================
        # TWO-DICE CONTROLS
        # =================================================

        conditionalPanel(

            condition = "input.activity == 'dice'",
            ns = ns,

            h4("The Two-Dice Game"),

            numericInput(
                ns("seed"),
                "Random seed",
                value = sample(
                    1:999,
                    1
                )
            ),

            numericInput(
                ns("n_sim"),
                "Number of plays",
                100
            ),

            sliderInput(
                ns("p"),
                "Probability of Selecting the Red Dice",
                min = 0,
                max = 1,
                value = 0.4
            ),

            actionButton(
                ns("run"),
                "Run simulation",
                class = "btn-primary"
            ),

            hr(),

            actionButton(
                ns("fit_models"),
                "Fit models",
                class = "btn-success"
            ),

            checkboxGroupInput(
                ns("models"),
                "Models to display",
                choices = c(
                    "Model N" = "N",
                    "Model S" = "S",
                    "Model D" = "D",
                    "Model P" = "P"
                )
            ),

            hr(),

            actionButton(
                ns("compare"),
                "Compare diagnostics",
                class = "btn-warning"
            ),

            hr(),

            actionButton(
                ns("reset"),
                "Start again",
                class = "btn-danger"
            )
        ),


        # =================================================
        # FOOTBALL CONTROLS
        # =================================================

        conditionalPanel(

            condition = "input.activity == 'football'",
            ns = ns,

            h4("A Football Calculator"),

            hr(),

            h5("Data source"),

            radioButtons(

                ns("football_data_source"),

                NULL,

                choices = c(
                    "PL fits: 25/26; Predict: 26/27" = "current",
                    "PL fits: 24/25; Predict: 25/26" = "previous",
                    "Upload my own data" = "upload"
                ),

                selected = "current"
            ),

            conditionalPanel(

                condition =
                    "input.football_data_source == 'upload'",

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

            selectInput(
                ns("team1"),
                "Home team",
                choices = NULL
            ),

            selectInput(
                ns("team2"),
                "Away team",
                choices = NULL
            ),

            hr(),

            h5("Model parameters"),

            sliderInput(
                ns("alpha_home"),
                "Home attack (α)",
                min = -1.5,
                max = 1.5,
                value = 0,
                step = 0.01
            ),

            sliderInput(
                ns("beta_home"),
                "Home defence (β)",
                min = -1.5,
                max = 1.5,
                value = 0,
                step = 0.01
            ),

            sliderInput(
                ns("alpha_away"),
                "Away attack (α)",
                min = -1.5,
                max = 1.5,
                value = 0,
                step = 0.01
            ),

            sliderInput(
                ns("beta_away"),
                "Away defence (β)",
                min = -1.5,
                max = 1.5,
                value = 0,
                step = 0.01
            ),

            hr(),

            uiOutput(
                ns("football_data_source_info")
            )
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
                    "🧩 Comparing Statistical Models",
                    style = "
                font-size: 1.4rem;
                font-weight: 700;
                color: #2c3e50;
                "
                )
            ),

            p(
                strong("Main idea: "),
                "This module allows you to explore statistical models in two different settings. ",
                "The Two-Dice Game illustrates how competing models can describe the same data, ",
                "while the Football Model Calculator shows how a statistical model can be used ",
                "to calculate probabilities for an individual football match."
            ),

            hr(),

            h5("Background"),

            p(
                "The Two-Dice Game provides a simple setting in which several statistical ",
                "models can be compared. Each model makes different assumptions about how ",
                "the observed scores were generated."
            ),

            tags$ul(

                tags$li(
                    strong("Model N (Null): "),
                    "all scores are assumed to be equally likely."
                ),

                tags$li(
                    strong("Model S (Saturated): "),
                    "each score has its own estimated probability."
                ),

                tags$li(
                    strong("Model D (Data-driven): "),
                    "probabilities are based on patterns observed in the data."
                ),

                tags$li(
                    strong("Model P (Process-driven): "),
                    "probabilities are derived from the known mechanism that generated the data."
                )

            ),

            hr(),

            h5("Football model"),

            p(
                "The Football Model Calculator uses a Poisson model for the number of ",
                "goals scored by each team. Expected goals depend on the attacking strength ",
                "of one team, the defensive strength of the other team, and home advantage."
            ),

            p(
                "You can use fitted Premier League parameters, or upload your own team ",
                "parameters. Select two teams to examine the probability of different scorelines."
            ),

            hr(),

            h5("Your options"),

            tags$ul(

                tags$li(
                    strong("Two-Dice Models: "),
                    "simulate data, fit competing models, and compare their diagnostics."
                ),

                tags$li(
                    strong("Football Model Calculator: "),
                    "select two teams and explore their expected goals and scoreline probabilities."
                )

            ),

            hr(),

            h5("A note on model comparison"),

            p(
                "When comparing models, the model that fits the data best is not always ",
                "the most useful model."
            ),

            p(
                "Some models achieve excellent fit simply because they contain many parameters. ",
                "Others fit slightly less well but provide a clearer explanation of the underlying process."
            ),

            p(
                "A central goal of statistical modelling is therefore to balance accuracy and simplicity."
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
                        "How do the different Two-Dice models describe the same data?"
                    ),

                    tags$li(
                        "How do model assumptions affect predicted probabilities?"
                    ),

                    tags$li(
                        "How does a football model turn team strengths into probabilities for a match?"
                    ),

                    tags$li(
                        "Why can several different scorelines all have substantial probability?"
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

            card_header(
                "Generated R code"
            ),

            tags$pre(
                textOutput(
                    ns("generated_code")
                )
            )
        )
    )


    # =====================================================
    # RESULTS
    # =====================================================

    results_panel <- div(


        # =================================================
        # TWO-DICE RESULTS
        # =================================================

        conditionalPanel(

            condition = "input.activity == 'dice'",
            ns = ns,

            accordion(

                accordion_panel(

                    title =
                        "🎲 Rules of the Double Dice Game",

                    tags$ul(

                        tags$li(
                            "You have two standard dice, one red and one blue."
                        ),

                        tags$li(
                            "The red dice has sides labelled from 1 to 6."
                        ),

                        tags$li(
                            "The blue dice has sides labelled from 4 to 9."
                        ),

                        tags$li(
                            "First select one of the dice."
                        ),

                        tags$li(
                            "Then roll that dice to obtain your score."
                        )

                    ),

                    p(
                        "Selection between the red and blue dice may be deterministic ",
                        "or random, depending on how the game is played."
                    )

                ),

                open = FALSE
            ),

            layout_columns(

                card(

                    card_header(
                        "Score distribution"
                    ),

                    plotOutput(
                        ns("hist"),
                        height = 350
                    )
                ),

                card(

                    card_header(
                        "Estimated probabilities"
                    ),

                    DT::DTOutput(
                        ns("prob_table")
                    )
                ),

                col_widths = c(
                    6,
                    6
                )
            ),

            br(),

            card(

                card_header(
                    "Model diagnostics"
                ),

                DT::DTOutput(
                    ns("model_table")
                )
            )
        ),


        # =================================================
        # FOOTBALL RESULTS
        # =================================================

        conditionalPanel(

            condition = "input.activity == 'football'",
            ns = ns,

            navset_tab(

                nav_panel(

                    "Score probabilities",

                    br(),

                    card(

                        card_header(
                            "Scoreline Probability Matrix"
                        ),

                        plotOutput(
                            ns("score_matrix"),
                            height = 500
                        )
                    )
                ),

                nav_panel(

                    "Match summary",

                    br(),

                    card(

                        card_header(
                            "Match Summary"
                        ),

                        uiOutput(
                            ns("match_summary")
                        )
                    )
                )
            )
        )
    )


    # =====================================================
    # LEARN PANEL
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

            h5(
                "1. The same data can support multiple models"
            ),

            p(
                "A single dataset does not determine a unique explanation. ",
                "Different models can describe the same patterns in different ways."
            ),

            hr(),

            h5(
                "2. Models differ in how they structure randomness"
            ),

            p(
                "Some models assume uniform randomness, while others introduce structure ",
                "such as grouping, weighting, or conditional probabilities."
            ),

            hr(),

            h5(
                "3. Models turn assumptions into predictions"
            ),

            p(
                "The football example illustrates the same general principle. ",
                "Once a model specifies team strengths and home advantage, those assumptions ",
                "can be converted into probabilities for different match outcomes."
            ),

            hr(),

            h5(
                "4. Cross-validation helps test generalisation"
            ),

            p(
                "A model that fits existing data well is not necessarily useful. ",
                "Cross-validation checks whether a model performs well on unseen data."
            ),

            hr(),

            h5(
                "Key takeaway"
            ),

            div(

                style = "
            background-color: #f8f9fa;
            border-left: 5px solid #28a745;
            padding: 12px;
            border-radius: 8px;
            ",

                p(

                    strong(
                        "Statistical modelling is a comparison process, not a search for certainty."
                    ),

                    br(),

                    "We use models to represent competing explanations of data, ",
                    "and we use their assumptions and predictive performance to understand ",
                    "what the models imply."
                )
            )
        )
    )


    tagList(

        shinyjs::useShinyjs(),

        chapter_page_ui(

            id = id,

            title = "🧩 Module 7: Models",

            sidebar = sidebar_controls,

            overview = overview_panel,

            code = code_panel,

            results = results_panel,

            learn = learn_panel
        )
    )


}

# =========================================================

# CHAPTER 7 SERVER

# =========================================================

chapter7_server <- function(id){


    moduleServer(
        id,
        function(input, output, session){


            # =================================================
            # TWO-DICE STATE
            # =================================================

            sim_data <- reactiveVal(NULL)

            fitted_models <- reactiveVal(NULL)

            diagnostics <- reactiveVal(NULL)

            show_probs <- reactiveVal(FALSE)

            show_diag <- reactiveVal(FALSE)

            workflow_stage <- reactiveVal("start")


            output$generated_code <- renderText({

                if(input$activity == "football"){

                    source_text <- switch(

                        input$football_data_source,

                        current =
                            paste0(
                                "teams <- PL25_pars$teams\n",
                                "tau <- PL25_pars$tau\n"
                            ),

                        previous =
                            paste0(
                                "teams <- PL24_pars$teams\n",
                                "tau <- PL24_pars$tau\n"
                            ),

                        upload =
                            paste0(
                                "teams <- read.csv(\"your_parameters.csv\")\n",
                                "tau <- ", input$tau, "\n"
                            )
                    )

                    paste0(

                        "# Football model\n",

                        source_text,

                        "\n",

                        "home <- \"",
                        input$team1,
                        "\"\n",

                        "away <- \"",
                        input$team2,
                        "\"\n\n",

                        "mu_home <- exp(\n",
                        "  tau + alpha_home - beta_away\n",
                        ")\n\n",

                        "mu_away <- exp(\n",
                        "  alpha_away - beta_home\n",
                        ")\n\n",

                        "dpois(0:5, mu_home)\n",
                        "dpois(0:5, mu_away)\n\n",

                        "# 1/X/2 probabilities\n",

                        "match_win_probs(\n",
                        "  c(alpha_home, beta_home),\n",
                        "  c(alpha_away, beta_away),\n",
                        "  tau\n",
                        ")"

                    )

                } else {

                    stage <- workflow_stage()

                    code <- character(0)

                    if(stage == "start"){

                        code <- c(

                            "## Workflow",
                            "",
                            "# No code has been run yet.",
                            "# Press 'Run simulation' to begin."

                        )

                    }

                    if(
                        stage %in%
                        c(
                            "simulated",
                            "fitted",
                            "complete"
                        )
                    ){

                        code <- c(

                            code,

                            "## Step 1: Simulate outcomes from the Double Dice Game",
                            "",

                            paste0(
                                "set.seed(",
                                input$seed,
                                ")"
                            ),

                            "",

                            "game_scores <- double_dice_game_sim(",

                            paste0(
                                "    n = ",
                                input$n_sim,
                                ","
                            ),

                            paste0(
                                "    p = ",
                                input$p
                            ),

                            ")",

                            "",

                            "head(game_scores)"

                        )
                    }

                    if(
                        stage %in%
                        c(
                            "fitted",
                            "complete"
                        )
                    ){

                        code <- c(

                            code,

                            "",

                            "## Step 2: Estimate model probabilities",
                            "",

                            "counts <- table(",
                            "    factor(game_scores, levels = 1:9)",
                            ")",
                            "",

                            "estimates <- mod_ests(",
                            "    as.numeric(counts)",
                            ")"

                        )

                        if(
                            length(input$models) > 0
                        ){

                            code <- c(

                                code,

                                "",

                                "# Models displayed",

                                paste0(

                                    "models_to_display <- c(\"",

                                    paste(
                                        input$models,
                                        collapse = "\", \""
                                    ),

                                    "\")"

                                )
                            )
                        }
                    }

                    if(stage == "complete"){

                        code <- c(

                            code,

                            "",

                            "## Step 3: Compare model diagnostics",
                            "",

                            "double_dice_game_model_check(",
                            "    game_scores,",
                            "    seed = 3",
                            ")"

                        )
                    }

                    paste(
                        code,
                        collapse = "\n"
                    )
                }
            })


            # =================================================
            # TWO-DICE BUTTON STATES
            # =================================================

            observe({

                if(input$activity != "dice"){

                    shinyjs::disable("run")
                    shinyjs::disable("fit_models")
                    shinyjs::disable("compare")

                } else {

                    stage <- workflow_stage()

                    shinyjs::disable("run")
                    shinyjs::disable("fit_models")
                    shinyjs::disable("compare")

                    if(stage == "start"){

                        shinyjs::enable("run")

                    } else if(stage == "simulated"){

                        shinyjs::enable("fit_models")

                    } else if(stage == "fitted"){

                        shinyjs::enable("compare")
                    }
                }
            })


            observeEvent(
                input$run,
                {

                    set.seed(
                        input$seed
                    )

                    sim_data(

                        double_dice_game_sim(

                            n = input$n_sim,

                            p = input$p
                        )
                    )

                    fitted_models(NULL)

                    diagnostics(NULL)

                    show_probs(FALSE)

                    show_diag(FALSE)

                    workflow_stage(
                        "simulated"
                    )
                }
            )


            observeEvent(
                input$fit_models,
                {

                    req(
                        sim_data()
                    )

                    counts <- table(

                        factor(
                            sim_data(),
                            levels = 1:9
                        )
                    )

                    fitted_models(

                        mod_ests(
                            as.numeric(counts)
                        )
                    )

                    show_probs(TRUE)

                    workflow_stage(
                        "fitted"
                    )
                }
            )


            observeEvent(
                input$compare,
                {

                    req(
                        sim_data()
                    )

                    diagnostics(

                        double_dice_game_model_check(

                            sim_data(),

                            seed = 3
                        )
                    )

                    show_diag(TRUE)

                    workflow_stage(
                        "complete"
                    )
                }
            )


            observeEvent(
                input$reset,
                {

                    sim_data(NULL)

                    fitted_models(NULL)

                    diagnostics(NULL)

                    show_probs(FALSE)

                    show_diag(FALSE)

                    updateCheckboxGroupInput(

                        session,

                        "models",

                        selected =
                            character(0)
                    )

                    updateNumericInput(

                        session,

                        "seed",

                        value =
                            sample(
                                1:999,
                                1
                            )
                    )

                    workflow_stage(
                        "start"
                    )
                }
            )


            # =================================================
            # TWO-DICE PLOT
            # =================================================

            output$hist <- renderPlot({

                req(
                    sim_data()
                )

                observed <- data.frame(

                    Score = 1:9,

                    Frequency =
                        as.numeric(
                            table(
                                factor(
                                    sim_data(),
                                    levels = 1:9
                                )
                            )
                        ),

                    Type = "Observed"
                )

                plot_data <- observed

                title_text <-
                    "Observed frequencies"

                if(!is.null(fitted_models())){

                    ests <- fitted_models()

                    model_probs <- list(

                        N = ests$p_N,

                        S = ests$p_S,

                        D = ests$p_D,

                        P = ests$p_P
                    )

                    fitted <- purrr::map_dfr(

                        input$models,

                        function(m){

                            data.frame(

                                Score = 1:9,

                                Frequency =
                                    model_probs[[m]] *
                                    length(sim_data()),

                                Type =
                                    paste(
                                        "Model",
                                        m
                                    )
                            )
                        }
                    )

                    plot_data <- rbind(
                        observed,
                        fitted
                    )

                    title_text <-
                        "Observed vs fitted models"
                }

                ggplot(

                    plot_data,

                    aes(
                        x = factor(Score),
                        y = Frequency,
                        fill = Type
                    )

                ) +

                    geom_col(
                        position = "dodge"
                    ) +

                    scale_fill_manual(

                        values = c(

                            "Observed" = "#4C78A8",

                            "Model N" = "#F58518",

                            "Model S" = "#54A24B",

                            "Model D" = "#E45756",

                            "Model P" = "#B279A2"

                        ),

                        drop = FALSE
                    ) +

                    theme_minimal() +

                    labs(

                        title = title_text,

                        x = "Score",

                        y = "Frequency",

                        fill = ""
                    )
            })


            # =================================================
            # TWO-DICE PROBABILITY TABLE
            # =================================================

            output$prob_table <- DT::renderDT({

                req(
                    fitted_models()
                )

                ests <- fitted_models()

                df <- data.frame(

                    Score = 1:9,

                    "N" =
                        round(
                            ests$p_N,
                            3
                        ),

                    "S" =
                        round(
                            ests$p_S,
                            3
                        ),

                    "D" =
                        round(
                            ests$p_D,
                            3
                        ),

                    "P" =
                        round(
                            ests$p_P,
                            3
                        )
                )

                DT::datatable(

                    df,

                    rownames = FALSE,

                    options = list(
                        dom = "t"
                    )
                ) |>

                    DT::formatStyle(

                        columns = names(df)[2:5],

                        `text-align` = "center"
                    )
            })


            # =================================================
            # TWO-DICE DIAGNOSTICS
            # =================================================

            output$model_table <- DT::renderDT({

                req(
                    diagnostics()
                )

                df <- as.data.frame(
                    diagnostics()
                )

                dt <- DT::datatable(

                    df,

                    options = list(
                        dom = "t",
                        paging = FALSE,
                        ordering = FALSE
                    )
                )

                for(col in names(df)){

                    best_value <- min(
                        df[[col]],
                        na.rm = TRUE
                    )

                    dt <- dt |>

                        DT::formatStyle(

                            columns = col,

                            valueColumns = col,

                            backgroundColor =
                                DT::styleEqual(

                                    best_value,

                                    "#c6efce"
                                )
                        )
                }

                dt
            })


            # =================================================
            # FOOTBALL DATA
            # =================================================

            validate_football_teams <- function(df){

                required <- c(
                    "teams",
                    "alpha",
                    "beta"
                )

                if(
                    !all(
                        required %in%
                        names(df)
                    )
                ){

                    stop(
                        "Parameters file must contain: teams, alpha, beta"
                    )
                }

                df
            }


            football_teams_data <- reactive({

                source <-
                    input$football_data_source

                if(source == "current"){

                    validate_football_teams(
                        PL25_pars$teams
                    )

                } else if(
                    source == "previous"
                ){

                    validate_football_teams(
                        PL24_pars$teams
                    )

                } else {

                    req(
                        input$pars_file
                    )

                    validate_football_teams(

                        read.csv(

                            input$pars_file$datapath,

                            stringsAsFactors = FALSE
                        )
                    )
                }
            })


            observeEvent(

                input$football_data_source,

                {

                    tau <- switch(

                        input$football_data_source,

                        current =
                            PL25_pars$tau,

                        previous =
                            PL24_pars$tau,

                        upload =
                            NULL
                    )

                    if(
                        !is.null(tau) &&
                        length(tau) == 1 &&
                        !is.na(tau)
                    ){

                        updateSliderInput(

                            session,

                            "tau",

                            value = tau
                        )
                    }
                }
            )


            observeEvent(

                football_teams_data(),

                {

                    teams <-
                        football_teams_data()$teams

                    updateSelectInput(

                        session,

                        "team1",

                        choices = teams,

                        selected = teams[1]
                    )

                    updateSelectInput(

                        session,

                        "team2",

                        choices = teams,

                        selected = teams[2]
                    )
                }
            )


            observeEvent(

                input$team1,

                {

                    req(
                        football_teams_data()
                    )

                    home <-
                        football_teams_data() |>
                        dplyr::filter(
                            teams == input$team1
                        )

                    if(
                        nrow(home) != 1
                    )
                        return()

                    updateSliderInput(

                        session,

                        "alpha_home",

                        value =
                            home$alpha[[1]]
                    )

                    updateSliderInput(

                        session,

                        "beta_home",

                        value =
                            home$beta[[1]]
                    )
                },

                ignoreInit = FALSE
            )


            observeEvent(

                input$team2,

                {

                    req(
                        football_teams_data()
                    )

                    away <-
                        football_teams_data() |>
                        dplyr::filter(
                            teams == input$team2
                        )

                    if(
                        nrow(away) != 1
                    )
                        return()

                    updateSliderInput(

                        session,

                        "alpha_away",

                        value =
                            away$alpha[[1]]
                    )

                    updateSliderInput(

                        session,

                        "beta_away",

                        value =
                            away$beta[[1]]
                    )
                },

                ignoreInit = FALSE
            )


            output$football_data_source_info <-
                renderUI({

                    source <-
                        input$football_data_source

                    if(
                        source == "current"
                    ){

                        div(

                            style = "
                        padding: 10px;
                        background-color: #e8f4ea;
                        border-radius: 6px;
                        font-weight: 600;
                        ",

                            "Using fitted 2025/26 model parameters and 2025/26 fixtures"
                        )

                    } else if(
                        source == "previous"
                    ){

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
            # FOOTBALL MATCH CALCULATIONS
            # =================================================

            football_match_means_reactive <- reactive({

                req(
                    input$activity == "football"
                )

                football_match_means(

                    alpha_home =
                        input$alpha_home,

                    beta_home =
                        input$beta_home,

                    alpha_away =
                        input$alpha_away,

                    beta_away =
                        input$beta_away,

                    tau =
                        input$tau
                )
            })


            football_score_matrix_reactive <- reactive({

                mu <-
                    football_match_means_reactive()

                football_score_matrix(

                    mu_home =
                        mu$home,

                    mu_away =
                        mu$away
                )
            })


            football_win_probs_reactive <- reactive({

                mu <-
                    football_match_means_reactive()

                football_match_win_probs(

                    mu_home =
                        mu$home,

                    mu_away =
                        mu$away
                )
            })


            # =================================================
            # FOOTBALL SCORE MATRIX
            # =================================================

            output$score_matrix <- renderPlot({

                df <-
                    football_score_matrix_reactive()

                ggplot(

                    df,

                    aes(
                        x = Away,
                        y = Home,
                        fill = Prob
                    )
                ) +

                    geom_tile() +

                    geom_text(

                        aes(

                            label =
                                ifelse(

                                    Prob < 0.005,

                                    "",

                                    scales::percent(
                                        Prob,
                                        accuracy = 0.1
                                    )
                                )
                        ),

                        size = 5
                    ) +

                    scale_fill_gradient(

                        low = "#f7f7f7",

                        high = "#4C78A8",

                        labels =
                            scales::percent
                    ) +

                    labs(

                        title =
                            paste(
                                input$team1,
                                "vs",
                                input$team2
                            ),

                        x = "Away goals",

                        y = "Home goals",

                        fill = "Probability"
                    ) +

                    theme_minimal(
                        base_size = 16
                    ) +

                    theme(

                        plot.title =
                            element_text(
                                size = 20,
                                face = "bold",
                                hjust = 0.5
                            ),

                        axis.title =
                            element_text(
                                size = 18,
                                face = "bold"
                            ),

                        axis.text =
                            element_text(
                                size = 15
                            )
                    )
            })


            # =================================================
            # FOOTBALL MATCH SUMMARY
            # =================================================

            output$match_summary <- renderUI({

                mu <-
                    football_match_means_reactive()

                df <-
                    football_score_matrix_reactive()

                probs <-
                    football_win_probs_reactive()

                best <-
                    df[
                        which.max(df$Prob),
                    ]

                tags$div(

                    style = "
                padding: 20px;
                ",

                    h4(
                        "Expected goals"
                    ),

                    p(

                        strong(
                            input$team1
                        ),

                        paste0(
                            ": ",
                            round(
                                mu$home,
                                2
                            )
                        ),

                        br(),

                        strong(
                            input$team2
                        ),

                        paste0(
                            ": ",
                            round(
                                mu$away,
                                2
                            )
                        )
                    ),

                    hr(),

                    h4(
                        "1 / X / 2 probabilities"
                    ),

                    layout_columns(

                        card(

                            style = "
                        text-align: center;
                        background-color: #e8f4ea;
                        ",

                            h5(
                                paste(
                                    "1 —",
                                    input$team1
                                )
                            ),

                            h3(

                                scales::percent(
                                    probs["home_win"],
                                    accuracy = 0.1
                                )
                            )
                        ),

                        card(

                            style = "
                        text-align: center;
                        background-color: #f8f9fa;
                        ",

                            h5(
                                "X — Draw"
                            ),

                            h3(

                                scales::percent(
                                    probs["draw"],
                                    accuracy = 0.1
                                )
                            )
                        ),

                        card(

                            style = "
                        text-align: center;
                        background-color: #e8eef8;
                        ",

                            h5(
                                paste(
                                    "2 —",
                                    input$team2
                                )
                            ),

                            h3(

                                scales::percent(
                                    probs["away_win"],
                                    accuracy = 0.1
                                )
                            )
                        ),

                        col_widths = c(
                            4,
                            4,
                            4
                        )
                    ),

                    hr(),

                    h4(
                        "Most likely score"
                    ),

                    p(

                        style = "
                    font-size: 1.2rem;
                    ",

                        strong(

                            paste0(
                                best$Home,
                                " – ",
                                best$Away
                            )
                        ),

                        br(),

                        scales::percent(
                            best$Prob,
                            accuracy = 0.1
                        )
                    ),

                    h4(
                        "Total expected goals"
                    ),

                    p(

                        style = "
                    font-size: 1.2rem;
                    ",

                        round(
                            mu$home +
                                mu$away,
                            2
                        )
                    )
                )
            })


            # =================================================
            # FOOTBALL DOWNLOADS
            # =================================================

            output$download_pars_template <-
                downloadHandler(

                    filename = function(){
                        "team_parameters_template.csv"
                    },

                    content = function(file){

                        write.csv(

                            PL25_pars$teams,

                            file,

                            row.names = FALSE
                        )
                    }
                )


            output$download_schedule_template <-
                downloadHandler(

                    filename = function(){
                        "fixture_template.csv"
                    },

                    content = function(file){

                        write.csv(

                            PL26_schedule,

                            file,

                            row.names = FALSE
                        )
                    }
                )
        }
    )


}
