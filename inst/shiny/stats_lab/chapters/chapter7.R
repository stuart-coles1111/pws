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

    data_cut <- cut(
        game_score_data,
        c(0,3,6,9)
    )

    data_cut_tab <- table(
        factor(
            data_cut,
            levels = c("(0,3]", "(3,6]", "(6,9]")
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
        factor(
            data_cut,
            levels = c("(0,3]", "(3,6]", "(6,9]")
        )
    )

    -sum(data_cut_tab * log(probs))
}

m3_lik <- function(data, ests){

    p1 <- max(ests[13] / 3, 1e-12)
    p3 <- max(ests[14] / 3, 1e-12)

    data_cut <- cut(data, c(0,3,6,9))

    data_cut_tab <- table(
        factor(
            data_cut,
            levels = c("(0,3]", "(3,6]", "(6,9]")
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

            l <- rbind(
                l,
                dd_all_lik(dt, ests)
            )
        }

        ll <- apply(l, 2, sum)
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

    l1 <- dd_all_lik(data, ests)
    l2 <- cv_lik(data)
    l3 <- cv_lik(data, K = 100)

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
        "Model 0",
        "Model 1",
        "Model 2",
        "Model 3"
    )

    df
}

# =========================================================
# UI
# =========================================================

chapter7_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("Model controls"),

        numericInput(
            ns("n_sim"),
            "Number of plays",
            value = 100,
            min = 20,
            step = 10
        ),

        sliderInput(
            ns("p"),
            "Probability of Dice 1",
            min = 0,
            max = 1,
            value = 0.4,
            step = 0.01
        ),

        numericInput(
            ns("seed"),
            "Random seed",
            value = 33
        ),

        actionButton(
            ns("run"),
            "Run simulation",
            class = "btn-primary"
        )
    )

    overview_panel <- div(

        card(

            card_header("What this chapter explores"),

            p(
                "Models are simplified explanations for how data are generated."
            ),

            tags$ul(
                tags$li("Simulation"),
                tags$li("Competing models"),
                tags$li("Likelihood"),
                tags$li("Cross-validation"),
                tags$li("Model comparison")
            )
        )
    )

    code_panel <- div(

        card(

            card_header("Generated R code"),

            tags$pre(
                textOutput(ns("generated_code"))
            )
        )
    )

    results_panel <- div(

        fluidRow(

            column(
                4,
                card(
                    h4("Mean"),
                    h2(textOutput(ns("mean")))
                )
            ),

            column(
                4,
                card(
                    h4("SD"),
                    h2(textOutput(ns("sd")))
                )
            ),

            column(
                4,
                card(
                    h4("Plays"),
                    h2(textOutput(ns("n_display")))
                )
            )
        ),

        br(),

        card(
            card_header("Score distribution"),
            plotOutput(ns("hist"), height = 350)
        ),

        br(),



        card(
            card_header("Model comparison"),
            DTOutput(ns("model_table"))
        )
    )

    learn_panel <- div(

        card(

            card_header("Big idea"),

            tags$blockquote(
                style = "
          font-size:22px;
          font-weight:700;
          color:#7B9ACC;
          border-left:5px solid #CDB4DB;
          padding-left:18px;
        ",
                "A good model explains data without being unnecessarily complicated."
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

        learn = learn_panel
    )
}

# =========================================================
# SERVER
# =========================================================

chapter7_server <- function(id){

    moduleServer(id, function(input, output, session){

        sim_data <- eventReactive(

            input$run,

            {

                set.seed(input$seed)

                double_dice_game_sim(
                    n = input$n_sim,
                    p = input$p
                )
            },

            ignoreNULL = FALSE
        )

        output$generated_code <- renderText({

            paste0(
                "game_scores <- double_dice_game_sim(
  n = ", input$n_sim, ",
  p = ", input$p, "
)

table(game_scores)

double_dice_game_model_check(game_scores)"
            )
        })

    output$mean <- renderText({

        req(sim_data())

        round(mean(sim_data()), 3)
    })

    output$sd <- renderText({

        req(sim_data())

        round(sd(sim_data()), 3)
    })

    output$n_display <- renderText({

        input$n_sim
    })

    output$hist <- renderPlot({

        req(sim_data())

        df <- as.data.frame(
            table(sim_data())
        )

        names(df) <- c(
            "Score",
            "Frequency"
        )

        ggplot(
            df,
            aes(Score, Frequency)
        ) +
            geom_col(
                fill = "#7B9ACC"
            ) +
            theme_minimal(base_size = 14)
    })



    output$model_table <- renderDT({

        req(sim_data())

        df <- double_dice_game_model_check(
            sim_data(),
            seed = 3
        )

        dt <- datatable(

            df,

            rownames = TRUE,

            options = list(
                dom = "t",
                ordering = FALSE,
                paging = FALSE,
                searching = FALSE,
                info = FALSE
            )
        )

        # Highlight minimum value in each row

        for(i in seq_len(nrow(df))){

            min_col <- which.min(as.numeric(df[i, ]))

            dt <- formatStyle(
                dt,
                columns = min_col,
                target = "cell",
                backgroundColor = styleEqual(
                    df[i, min_col],
                    "#D4EDDA"
                )
            )
        }

        dt
    })
    })
}

