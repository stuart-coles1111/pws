# =========================================================
# ACTIVITY 4:
# Forecasting, Uncertainty and Calibration
# =========================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(gt)
library(patchwork)

# =========================================================
# DATA
# =========================================================

questions <- tibble::tibble(

    question = 1:10,

    sport = c(
        "🎾 Tennis",
        "🏀 Basketball",
        "⚾ Baseball",
        "🏃 Marathon",
        "🚴 Cycling",
        "🏈 American football",
        "🏒 Ice hockey",
        "⚽ Football",
        "🏃 Athletics",
        "🏏 Cricket"
    ),

    text = c(
        "Fastest serve (mph) at the 2022 US Open?",
        "Lowest points scored by an NBA team in 2022-23?",
        "Average MLB attendance in 2022 (thousands)?",
        "NY Marathon finishers in 2022 (thousands)?",
        "Highest speed in 2022 Giro d'Italia (kph)?",
        "Highest NFL salary in 2022 (millions USD)?",
        "Highest NHL team goals in 2022-23?",
        "Highest goalkeeper saves in EPL 2022-23?",
        "Longest long jump in 2022 (metres)?",
        "Highest batting average in 2022 T20 World Cup?"
    ),

    answer = c(
        141.000,
        80.000,
        26.808,
        47.839,
        74.700,
        50.300,
        325.000,
        154.000,
        8.520,
        98.660
    )
)

# =========================================================
# FUNCTIONS
# =========================================================

response_score <- function(G, S, Theta,
                           alpha = 0.95,
                           dp = 2){

    sigma <- S / qnorm((1 + alpha)/2)

    score <- dnorm(
        G,
        Theta,
        sigma,
        log = TRUE
    )

    round(score, dp)
}

distribution_plot <- function(G, S, Theta = NULL,
                              reveal = FALSE,
                              alpha = 0.95){

    sigma <- S / qnorm((1 + alpha)/2)

    x <- seq(
        G - 4*sigma,
        G + 4*sigma,
        length = 1000
    )

    d <- tibble(
        x = x,
        y = dnorm(x, G, sigma)
    )

    p <- ggplot(d, aes(x,y)) +

        geom_area(
            data = subset(
                d,
                x >= G-S & x <= G+S
            ),
            fill = "#CDB4DB",
            alpha = 0.55
        ) +

        geom_line(
            linewidth = 1.2,
            colour = "#7B9ACC"
        ) +

        geom_vline(
            xintercept = G,
            colour = "#7B9ACC",
            linewidth = 1
        ) +

        labs(
            x = NULL,
            y = NULL
        ) +

        theme_minimal(base_size = 12) +

        theme(
            panel.grid.minor = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
        )

    if(reveal){

        p <- p +

            geom_vline(
                xintercept = Theta,
                colour = "#E76F51",
                linewidth = 1.3
            )
    }

    p
}

# =========================================================
# UI
# =========================================================

ui <- page_fluid(

    theme = bs_theme(
        version = 5,
        bootswatch = "minty",
        primary = "#7B9ACC",
        bg = "#F7F7FB",
        fg = "#2E3440",
        base_font = font_google("Inter")
    ),

    tags$head(

        tags$style(HTML("

            .main-title{
                background:linear-gradient(
                    90deg,
                    #A8DADC,
                    #CDB4DB
                );
                padding:20px;
                border-radius:14px;
                margin-bottom:20px;
                text-align:center;
            }

            .card-style{
                background:white;
                border-radius:14px;
                padding:20px;
                margin-bottom:20px;
                box-shadow:0 3px 10px rgba(0,0,0,0.08);
            }

            .question-card{
                border-left:6px solid #A8DADC;
            }

            .score-big{
                font-size:34px;
                font-weight:700;
                text-align:center;
                color:#7B9ACC;
            }

            .small-note{
                color:#666;
                font-size:13px;
            }

        "))
    ),

    div(
        class = "main-title",

        h1("🎯 Activity 4: Forecasting and Calibration"),

        p(
            "Estimate the true value and quantify your uncertainty."
        )
    ),

    layout_sidebar(

        sidebar = div(

            class = "card-style",

            h4("Participant"),

            textInput(
                "team_name",
                "Team",
                value = "Team 1"
            ),

            hr(),

            checkboxInput(
                "reveal",
                "Reveal true answers",
                FALSE
            ),

            hr(),

            downloadButton(
                "download_template",
                "Download Template"
            ),

            fileInput(
                "upload_csv",
                "Upload Responses"
            )
        ),

        navset_tab(

            # =================================================
            # TAB 1
            # =================================================

            nav_panel(

                "Forecast Entry",

                uiOutput("question_ui")
            ),

            # =================================================
            # TAB 2
            # =================================================

            nav_panel(

                "Score Explorer",

                div(
                    class = "card-style",

                    fluidRow(

                        column(
                            4,

                            selectInput(
                                "explore_question",
                                "Question",
                                choices = questions$question
                            )
                        ),

                        column(
                            4,

                            numericInput(
                                "explore_G",
                                "Best estimate (G)",
                                value = 100
                            )
                        ),

                        column(
                            4,

                            numericInput(
                                "explore_S",
                                "Uncertainty width (S)",
                                value = 20,
                                min = 0.01
                            )
                        )
                    )
                ),

                div(
                    class = "card-style",

                    plotOutput(
                        "explore_plot",
                        height = "320px"
                    )
                ),

                div(
                    class = "card-style",

                    div(
                        class = "score-big",
                        textOutput("explore_score")
                    ),

                    p(
                        class = "small-note",

                        "Scores are log densities evaluated at the true answer."
                    )
                )
            ),

            # =================================================
            # TAB 3
            # =================================================

            nav_panel(

                "Team Results",

                div(
                    class = "card-style",

                    h4("Scores by Question"),

                    DTOutput("score_table")
                ),

                div(
                    class = "card-style",

                    h4("Question Heatmap"),

                    plotOutput(
                        "heatmap",
                        height = "450px"
                    )
                )
            )
        )
    )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session){

    # -----------------------------------------------------
    # Dynamic question cards
    # -----------------------------------------------------

    output$question_ui <- renderUI({

        tagList(

            lapply(1:nrow(questions), function(i){

                div(

                    class = "card-style question-card",

                    h4(
                        paste0(
                            questions$sport[i],
                            " — Question ",
                            i
                        )
                    ),

                    p(
                        questions$text[i]
                    ),

                    fluidRow(

                        column(
                            4,

                            numericInput(
                                paste0("G_",i),
                                "Best estimate (G)",
                                value = NA
                            )
                        ),

                        column(
                            4,

                            numericInput(
                                paste0("S_",i),
                                "Uncertainty width (S)",
                                value = NA,
                                min = 0.01
                            )
                        )
                    ),

                    plotOutput(
                        paste0("plot_",i),
                        height = "180px"
                    ),

                    conditionalPanel(

                        condition = "input.reveal == true",

                        div(
                            class = "small-note",

                            strong("True answer: "),
                            round(questions$answer[i],2)
                        )
                    )
                )
            })
        )
    })

    # -----------------------------------------------------
    # Question plots
    # -----------------------------------------------------

    lapply(1:nrow(questions), function(i){

        output[[paste0("plot_",i)]] <- renderPlot({

            G <- input[[paste0("G_",i)]]
            S <- input[[paste0("S_",i)]]

            req(G,S)

            distribution_plot(
                G = G,
                S = S,
                Theta = questions$answer[i],
                reveal = input$reveal
            )
        })
    })

    # -----------------------------------------------------
    # Score explorer
    # -----------------------------------------------------

    output$explore_plot <- renderPlot({

        q <- input$explore_question

        Theta <- questions$answer[
            questions$question == q
        ]

        distribution_plot(
            G = input$explore_G,
            S = input$explore_S,
            Theta = Theta,
            reveal = TRUE
        )
    })

    output$explore_score <- renderText({

        q <- input$explore_question

        Theta <- questions$answer[
            questions$question == q
        ]

        s <- response_score(
            G = input$explore_G,
            S = input$explore_S,
            Theta = Theta
        )

        paste0(
            "Log score = ",
            round(s,2)
        )
    })

    # -----------------------------------------------------
    # Reactive response table
    # -----------------------------------------------------

    responses <- reactive({

        tibble(

            question = 1:10,

            G = sapply(
                1:10,
                function(i)
                    input[[paste0("G_",i)]]
            ),

            S = sapply(
                1:10,
                function(i)
                    input[[paste0("S_",i)]]
            )
        )
    })

    # -----------------------------------------------------
    # Scores
    # -----------------------------------------------------

    score_data <- reactive({

        d <- responses()

        d %>%

            mutate(

                answer = questions$answer,

                score = mapply(
                    response_score,
                    G,
                    S,
                    answer
                )
            )
    })

    # -----------------------------------------------------
    # Table
    # -----------------------------------------------------

    output$score_table <- renderDT({

        req(input$reveal)

        d <- score_data()

        datatable(

            d %>%

                select(
                    question,
                    G,
                    S,
                    answer,
                    score
                ),

            rownames = FALSE,

            options = list(
                pageLength = 10,
                dom = "tip"
            )
        )
    })

    # -----------------------------------------------------
    # Heatmap
    # -----------------------------------------------------

    output$heatmap <- renderPlot({

        req(input$reveal)

        d <- score_data()

        ggplot(
            d,
            aes(
                factor(question),
                "Team 1",
                fill = score
            )
        ) +

            geom_tile(
                colour = "white",
                linewidth = 1
            ) +

            geom_text(
                aes(
                    label = round(score,1)
                ),
                size = 5
            ) +

            scale_fill_gradient2(
                low = "#E76F51",
                mid = "#F7F7FB",
                high = "#7B9ACC",
                midpoint = median(d$score, na.rm=TRUE)
            ) +

            labs(
                x = "Question",
                y = NULL
            ) +

            theme_minimal(base_size = 13) +

            theme(
                panel.grid = element_blank()
            )
    })

    # -----------------------------------------------------
    # Template download
    # -----------------------------------------------------

    output$download_template <- downloadHandler(

        filename = function(){
            "activity4_template.csv"
        },

        content = function(file){

            tibble(

                question = 1:10,
                G = NA,
                S = NA

            ) %>%

                write.csv(
                    file,
                    row.names = FALSE
                )
        }
    )

    # -----------------------------------------------------
    # Upload CSV
    # -----------------------------------------------------

    observeEvent(input$upload_csv, {

        req(input$upload_csv)

        d <- read.csv(
            input$upload_csv$datapath
        )

        for(i in 1:10){

            updateNumericInput(
                session,
                paste0("G_",i),
                value = d$G[i]
            )

            updateNumericInput(
                session,
                paste0("S_",i),
                value = d$S[i]
            )
        }
    })
}

# =========================================================
# RUN APP
# =========================================================

shinyApp(ui, server)

