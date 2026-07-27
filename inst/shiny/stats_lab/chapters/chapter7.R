# =========================================================
# Helper functions
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

    data_cut <- cut(game_score_data, c(0,3,6,9))

    data_cut_tab <- table(
        factor(data_cut, levels = c("(0,3]", "(3,6]", "(6,9]"))
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

    probs <- pmax(ests[1:9], 1e-12)

    data_tab <- table(
        factor(data, levels = 1:9)
    )

    -sum(data_tab * log(probs))
}

m2_lik <- function(data, ests){

    probs <- pmax(ests[10:12] / 3, 1e-12)

    data_cut <- cut(data, c(0,3,6,9))

    data_cut_tab <- table(
        factor(data_cut, levels = c("(0,3]", "(3,6]", "(6,9]"))
    )

    -sum(data_cut_tab * log(probs))
}

m3_lik <- function(data, ests){

    p1 <- max(ests[13] / 3, 1e-12)
    p3 <- max(ests[14] / 3, 1e-12)

    data_cut <- cut(data, c(0,3,6,9))

    data_cut_tab <- table(
        factor(data_cut, levels = c("(0,3]", "(3,6]", "(6,9]"))
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

    ll <- rep(Inf, 4)

    while(any(is.infinite(ll))) {

        nt <- round(length(data) / K)

        ind <- sample(
            rep(1:K, length.out = length(data))
        )

        l <- NULL

        for(i in 1:K){

            df <- data[ind != i]
            dt <- data[ind == i]

            ests <- dd_ests(df)

            l <- rbind(l, dd_all_lik(dt, ests))
        }

        ll <- apply(l, 2, sum)
    }

    ll
}

double_dice_game_model_check <- function(data, seed = NULL){

    if(!is.null(seed))
        set.seed(seed)

    ests <- dd_ests(data)

    l1 <- dd_all_lik(data, ests)
    l2 <- cv_lik(data)
    l3 <- cv_lik(data, K = 100)

    df <- rbind(l1, l2, l3) |>
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

    r1 <- (x[1] + x[2] + x[3]) / (3 * sum(x))
    r2 <- (x[4] + x[5] + x[6]) / (3 * sum(x))
    r3 <- (x[7] + x[8] + x[9]) / (3 * sum(x))

    q <- (x[1] + x[2] + x[3]) /
        (sum(x) - x[4] - x[5] - x[6])

    p_N <- rep(1/9, 9)
    p_S <- x / sum(x)
    p_D <- rep(c(r1, r2, r3), each = 3)
    p_P <- rep(c(q/6, 1/6, (1 - q)/6), each = 3)

    list(
        p_N = p_N,
        p_S = p_S,
        p_D = p_D,
        p_P = p_P
    )
}

# =========================================================
# UI
# =========================================================

chapter7_ui <- function(id){

    library(shinyjs)

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("The Two-Dice Game"),

        numericInput(
            ns("seed"),
            "Random seed",
            value = sample(1:999,1)
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
    )

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
                "This module allows you to simulate data from the Two-Dice game, fit competing statistical models, and compare their performance using graphical and numerical diagnostics."
            ),

            hr(),

            h5("Background"),

            p(
                "Four statistical models are considered for the Two-Dice game. Each model provides a different explanation of how scores are generated."
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

            p(
                "The purpose of this module is to estimate these models, compare their predictions, and evaluate their performance using diagnostic measures."
            ),

            hr(),

            h5("Your options"),

            p(
                "You can adjust the probability of selecting the red die and the number of times the game is played. Then you can:"
            ),

            tags$ul(
                tags$li("Simulate data from the Two-Dice game."),
                tags$li("Fit the competing statistical models."),
                tags$li("Display estimated model probabilities graphically and numerically."),
                tags$li("Compare models using diagnostic measures.")
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
                        "How do the different models rank when describing data from the Two-Dice game?"
                    ),
                    tags$li(
                        "Does the ranking of the models depend on the sample size or the probability of selecting the red die?"
                    ),
                    tags$li(
                        "Do graphical comparisons and diagnostic measures always lead to the same conclusion?"
                    ),
                    tags$li(
                        "Can two models produce similar predictions while receiving different diagnostic scores?"
                    )
                )
            )
        )

    )

    code_panel <- div(
        card(
            card_header("Generated R code"),
            tags$pre(textOutput(ns("generated_code")))
        )
    )

    results_panel <- div(

        accordion(

            accordion_panel(

                title = "🎲 Rules of the Double Dice Game",

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
                    "Selection between the red and blue dice may be deterministic or random, depending on how the game is played."
                ),

                p(
                    "For example, we could toss a coin to choose between the dice, or we could decide that adults use the red dice while children use the blue dice."
                )

            ),

            open = FALSE
        ),

        layout_columns(

            card(
                card_header("Score distribution"),

                plotOutput(
                    ns("hist"),
                    height = 350
                )
            ),

            card(
                card_header("Estimated probabilities"),

                DT::DTOutput(
                    ns("prob_table")
                )
            ),

            col_widths = c(6, 6)
        ),

        br(),

        card(
            card_header("Model diagnostics"),

            DT::DTOutput(
                ns("model_table")
            )
        )
    )

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

                h5("1. The same data can support multiple models"),

                p(
                    "A single dataset does not determine a unique explanation.
                Different models can describe the same patterns in different ways,
                with varying assumptions about how the data were generated."
                ),

                hr(),

                h5("2. Models differ in how they structure randomness"),

                p(
                    "Some models assume uniform randomness, while others introduce structure
                such as grouping, weighting, or conditional probabilities.
                These assumptions strongly influence predicted outcomes."
                ),

                hr(),

                h5("3. Model comparison is about relative performance"),

                p(
                    "We are not asking which model is ‘true’, but which model best explains
                the observed data according to measures like likelihood and predictive fit."
                ),

                hr(),

                h5("4. Cross-validation helps test generalisation"),

                p(
                    "A model that fits existing data well is not necessarily useful.
                Cross-validation checks whether a model performs well on unseen data,
                which is a stronger test of usefulness."
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
                        strong("Statistical modelling is a comparison process, not a search for certainty."),
                        br(),
                        "We use models to represent competing explanations of data,
                    and we choose between them based on how well they describe and predict what we observe."
                    )
                )
            )
        )
    )



    tagList(
        shinyjs::useShinyjs(),

        chapter_page_ui(
            id = id,
            title = "🧩 Chapter 7: Models",
            sidebar = sidebar_controls,
            overview = overview_panel,
            code = code_panel,
            results = results_panel,
            learn = learn_panel
        )
    )

}


# =========================================================
# SERVER
# =========================================================

chapter7_server <- function(id){

    moduleServer(id, function(input, output, session){


        # -----------------------------
        # Reactive state
        # -----------------------------

        sim_data <- reactiveVal(NULL)

        fitted_models <- reactiveVal(NULL)

        diagnostics <- reactiveVal(NULL)


        show_probs <- reactiveVal(FALSE)

        show_diag <- reactiveVal(FALSE)

        workflow_stage <- reactiveVal("start")

        # These are used by conditionalPanel()
        output$show_probs <- reactive({
            isTRUE(show_probs())
        })

        output$show_diag <- reactive({
            isTRUE(show_diag())
        })

        # -----------------------------
        # Generated R code panel
        # -----------------------------

        output$generated_code <- renderText({

            stage <- workflow_stage()

            code <- character(0)

            if (stage == "start") {

                code <- c(
                    "## Workflow",
                    "",
                    "# No code has been run yet.",
                    "# Press 'Run simulation' to begin."
                )

            }

            if (stage %in% c("simulated", "fitted", "complete")) {

                code <- c(
                    code,
                    "## Step 1: Simulate outcomes from the Double Dice Game",
                    "",
                    paste0("set.seed(", input$seed, ")"),
                    "",
                    "game_scores <- double_dice_game_sim(",
                    paste0("    n = ", input$n_sim, ","),
                    paste0("    p = ", input$p),
                    ")",
                    "",
                    "head(game_scores)"
                )

            }

            if (stage %in% c("fitted", "complete")) {

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

                if (length(input$models) > 0) {

                    code <- c(
                        code,
                        "",
                        "# Models displayed",
                        paste0(
                            "models_to_display <- c(\"",
                            paste(input$models, collapse = "\", \""),
                            "\")"
                        )
                    )

                }

            }

            if (stage == "complete") {

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

            paste(code, collapse = "\n")

        })

observe({

    stage <- workflow_stage()

    shinyjs::disable("run")
    shinyjs::disable("fit_models")
    shinyjs::disable("compare")

    if (stage == "start") {

        shinyjs::enable("run")

    } else if (stage == "simulated") {

        shinyjs::enable("fit_models")

    } else if (stage == "fitted") {

        shinyjs::enable("compare")

    }

})

# -----------------------------
# Run simulation
# -----------------------------

observeEvent(input$run, {


    set.seed(input$seed)


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

    workflow_stage("simulated")

})



# -----------------------------
# Fit models button
# -----------------------------

observeEvent(input$fit_models, {


    req(sim_data())


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

    workflow_stage("fitted")

})



# -----------------------------
# Compare diagnostics button
# -----------------------------

observeEvent(input$compare, {


    req(sim_data())


    diagnostics(

        double_dice_game_model_check(
            sim_data(),
            seed = 3
        )

    )


    show_diag(TRUE)

    workflow_stage("complete")

})



# -----------------------------
# Start again
# -----------------------------

observeEvent(input$reset, {

    sim_data(NULL)

    fitted_models(NULL)

    diagnostics(NULL)

    show_probs(FALSE)

    show_diag(FALSE)

    updateCheckboxGroupInput(
        session,
        "models",
        selected = character(0)
    )

    new_seed <- sample(1:999, 1)

    updateNumericInput(
        session,
        "seed",
        value = new_seed
    )

    workflow_stage("start")

})


# -----------------------------
# Plot
# -----------------------------

# -----------------------------
# Plot
# -----------------------------

output$hist <- renderPlot({

    req(sim_data())

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

    title_text <- "Observed frequencies"

    if (!is.null(fitted_models())) {

        ests <- fitted_models()

        model_probs <- list(

            N = ests$p_N,

            S = ests$p_S,

            D = ests$p_D,

            P = ests$p_P

        )

        fitted <- purrr::map_dfr(

            input$models,

            function(m) {

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

        title_text <- "Observed vs fitted models"
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



# -----------------------------
# Probability table
# -----------------------------

output$prob_table <- DT::renderDT({


    req(fitted_models())


    ests <- fitted_models()


    df <- data.frame(

        Score = 1:9,

        "Model N" =
            round(
                ests$p_N,
                3
            ),

        "Model S" =
            round(
                ests$p_S,
                3
            ),

        "Model D" =
            round(
                ests$p_D,
                3
            ),

        "Model P" =
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
    )


})



# -----------------------------
# Diagnostics table
# -----------------------------

output$model_table <- DT::renderDT({


    req(diagnostics())


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


    # highlight best (lowest) diagnostic value
    # in each column

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


    })

}
