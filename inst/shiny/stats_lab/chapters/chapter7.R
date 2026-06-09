library(shiny)
library(ggplot2)
library(DT)
library(dplyr)

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

    ns <- NS(id)

    sidebar_controls <- sidebar(

        numericInput(ns("seed"), "Random seed", value = sample(1:999, 1)),

        numericInput(ns("n_sim"), "Number of plays", 100),

        sliderInput(
            ns("p"),
            "Probability of Dice 1",
            min = 0,
            max = 1,
            value = 0.4
        ),

        selectInput(
            ns("model_choice"),
            "Compare observed data to model",
            choices = c("None", "N", "S", "D", "P"),
            selected = "None"
        ),

        actionButton(ns("run"), "Run simulation", class = "btn-primary")
    )

    overview_panel <- div(
        card(
            card_header("What this chapter explores"),
            p("We compare competing statistical models for the same data."),
            tags$ul(
                tags$li("Simulation"),
                tags$li("Competing explanations"),
                tags$li("Likelihood"),
                tags$li("Model checking")
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

        h3("Summary diagnostics for data generated in the Two-Dice game."),
        br(),

        fluidRow(
            column(4, card(h4("Mean"), h2(textOutput(ns("mean"))))),
            column(4, card(h4("SD"), h2(textOutput(ns("sd"))))),
            column(4, card(h4("N"), h2(textOutput(ns("n_display")))))
        ),

        br(),

        card(
            card_header("Score distribution"),
            plotOutput(ns("hist"), height = 350)
        ),

        br(),

        card(
            card_header("Estimated probability of each score"),
            DTOutput(ns("prob_table"))
        ),

        br(),

        card(
            card_header("Model diagnostic values"),
            DTOutput(ns("model_table"))
        )
    )

    learn_panel <- div(
        card(
            card_header("Big idea"),
            tags$blockquote(
                style = "font-size:20px;font-weight:700;color:#7B9ACC;",
                "Good models explain patterns without overcomplicating them."
            )
        )
    )

    # =======================================================
    # Activity Panel
    # =======================================================

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

    chapter_page_ui(
        id = id,
        title = "🧩 Chapter 7: Models",
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

chapter7_server <- function(id){

    moduleServer(id, function(input, output, session){

        observeEvent(input$launch_activity, {

            url <- pws:::launch_activity_from_lab(7)

            shinyjs::runjs(
                sprintf(
                    "window.open('%s', '_blank');",
                    url
                )
            )
        })

        seed_val <- reactiveVal(sample(1:999, 1))

        sim_data <- eventReactive(input$run, {

            set.seed(seed_val())

            x <- double_dice_game_sim(
                input$n_sim,
                input$p
            )

            seed_val(sample(1:999, 1))

            x
        }, ignoreInit = TRUE)

        output$generated_code <- renderText({

            paste0(
                "game_scores <- double_dice_game_sim(\n",
                "  n = ", input$n_sim, ",\n",
                "  p = ", input$p, "\n",
                ")\n\n",
                "double_dice_game_model_check(game_scores)"
            )
        })

        output$mean <- renderText({
            round(mean(sim_data()), 3)
        })

        output$sd <- renderText({
            round(sd(sim_data()), 3)
        })

        output$n_display <- renderText({
            input$n_sim
        })

        output$hist <- renderPlot({

            df <- model_compare()

            # reshape into long format
            df_long <- tidyr::pivot_longer(
                df,
                cols = c("Observed", "Expected"),
                names_to = "Type",
                values_to = "Frequency"
            )

            # drop Expected when no model selected
            if (input$model_choice == "None") {
                df_long <- df_long[df_long$Type == "Observed", ]
            }

            ggplot(df_long, aes(x = factor(Score), y = Frequency, fill = Type)) +
                geom_col(
                    position = position_dodge(width = 0.7),
                    width = 0.6,
                    alpha = 0.7
                ) +
                theme_minimal() +
                labs(
                    x = "Score",
                    y = "Frequency",
                    title = paste("Observed vs Model:", input$model_choice),
                    fill = ""
                ) +
                scale_fill_manual(
                    values = c(
                        "Observed" = "#4C78A8",
                        "Expected" = "#F58518"
                    )
                )
        })

        output$prob_table <- renderDT({

            x <- as.numeric(table(factor(sim_data(), levels = 1:9)))
            ests <- mod_ests(x)

            df <- data.frame(
                Score = 1:9,
                "Model N" = round(ests$p_N, 3),
                "Model S" = round(ests$p_S, 3),
                "Model D" = round(ests$p_D, 3),
                "Model P" = round(ests$p_P, 3)
            )
            datatable(df, rownames = FALSE, options = list(dom = "t"))
        })

    output$model_table <- renderDT({

        df <- as.data.frame(
            double_dice_game_model_check(sim_data(), seed = 3)
        )

        dt <- datatable(
            df,
            options = list(
                dom = "t",
                paging = FALSE,
                ordering = FALSE
            )
        )

        for (col in names(df)) {

            min_val <- min(df[[col]], na.rm = TRUE)

            dt <- dt |>
                formatStyle(
                    columns = col,
                    valueColumns = col,
                    backgroundColor = styleEqual(
                        min_val,
                        "#c6efce"
                    )
                )
        }

        dt
    })

model_compare <- reactive({

    x <- sim_data()
    tab <- table(factor(x, levels = 1:9))
    n <- sum(tab)

    ests <- mod_ests(as.numeric(tab))

    model_probs <- switch(
        input$model_choice,
        "N" = ests$p_N,
        "S" = ests$p_S,
        "D" = ests$p_D,
        "P" = ests$p_P,
        NULL
    )

    data.frame(
        Score = 1:9,
        Observed = as.numeric(tab),
        Expected = if (is.null(model_probs)) rep(NA, 9) else model_probs * n
    )
})
    })
}
