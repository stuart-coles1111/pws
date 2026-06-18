library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(patchwork)
library(readr)

# =========================================================
# DEFAULT QUESTIONS
# =========================================================

default_questions <- tibble::tibble(
    question = 1:10,

    category = c(
        "Sports","Sports","Sports","Sports","Sports",
        "Sports","Sports","Sports","Sports","Sports"
    ),

    label = c(
        "Tennis","Basketball","Baseball","Marathon","Cycling",
        "American football","Ice hockey","Football","Athletics","Cricket"
    ),

    icon = c(
        "🎾","🏀","⚾","🏃","🚴",
        "🏈","🏒","⚽","🏃","🏏"
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
        141, 80, 26.808, 47.839, 74.7,
        50.3, 325, 154, 8.52, 98.66
    ),

    units = c(
        "mph","points","thousands","thousands","kph",
        "millions USD","goals","saves","metres","average"
    )
)

# =========================================================
# FUNCTIONS
# =========================================================

activity4_response_score <- function(G, S, Theta,
                                     alpha = 0.95,
                                     dp = 2) {

    if (any(is.null(G), is.null(S), is.na(G), is.na(S), S <= 0)) {
        return(NA_real_)
    }

    sigma <- S / qnorm((1 + alpha) / 2)

    score <- dnorm(
        G,
        Theta,
        sigma,
        log = TRUE
    ) %>%
        round(dp)

    score
}

distribution_plot <- function(G,
                              S,
                              Theta = NULL,
                              alpha = 0.95) {

    if (any(is.null(G), is.null(S), is.na(G), is.na(S), S <= 0)) {
        return(ggplot() + theme_void())
    }

    G <- as.numeric(G)
    S <- as.numeric(S)

    sigma <- S / qnorm((1 + alpha) / 2)

    centre <- ifelse(is.null(Theta), G, Theta)

    x <- seq(
        centre - 4 * sigma,
        centre + 4 * sigma,
        length.out = 1000
    )

    d <- tibble(
        x = x,
        density = dnorm(x, Theta, sigma),
        log_density = dnorm(x, Theta, sigma, log = TRUE)
    )

    # =====================================================
    # DENSITY PLOT
    # =====================================================

    p1 <- ggplot(d, aes(x, density)) +

        geom_line(
            linewidth = 1.2,
            colour = "#7B9ACC"
        ) +

        geom_vline(
            xintercept = G,
            colour = "#7B9ACC",
            linewidth = 1
        ) +

        geom_vline(
            xintercept = Theta,
            colour = "#E76F51",
            linewidth = 1.3
        ) +

        theme_minimal(base_size = 12) +

        labs(
            title = "Forecast distribution",
            x = NULL,
            y = "Density"
        ) +

        theme(
            panel.grid.minor = element_blank()
        )

    # =====================================================
    # LOG SCORE PLOT
    # =====================================================

    theta_score <- dnorm(
        G,
        Theta,
        sigma,
        log = TRUE
    )

    p2 <- ggplot(d, aes(x, log_density)) +

        geom_line(
            linewidth = 1.2,
            colour = "#6D597A"
        ) +

        geom_vline(
            xintercept = G,
            colour = "#7B9ACC",
            linewidth = 1
        ) +

        geom_vline(
            xintercept = Theta,
            colour = "#E76F51",
            linewidth = 1.3
        ) +

        annotate(
            "point",
            x = G,
            y = theta_score,
            colour = "#E76F51",
            size = 3
        ) +

        theme_minimal(base_size = 12) +

        labs(
            title = "Log-density scoring scale",
            x = NULL,
            y = "Log density"
        ) +

        theme(
            panel.grid.minor = element_blank()
        )

    p1 / p2
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
                background:linear-gradient(90deg,#A8DADC,#CDB4DB);
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

            .answer-box{
                background:#F1F5FB;
                border-left:5px solid #7B9ACC;
                padding:12px;
                border-radius:10px;
                margin-top:15px;
                font-size:18px;
                font-weight:600;
            }

            .small-note{
                color:#666;
                font-size:13px;
            }

            .info-box{
                background:#F8F9FB;
                padding:18px;
                border-radius:14px;
                line-height:1.7;
            }

        "))
    ),

    div(
        class = "main-title",
        h1("🎯 Activity 4: Quiz Time"),
        p("Estimate the true value and quantify your uncertainty.")
    ),

    layout_sidebar(

        sidebar = div(

            class = "card-style",

            fileInput(
                "upload_questions",
                "Upload Question Set (optional)"
            ),

            hr(),

            textInput(
                "team_name",
                "Team name",
                value = "Team 1"
            ),

            actionButton(
                "save_team",
                "Save Team",
                class = "btn-primary"
            ),

            hr(),

            downloadButton(
                "download_template",
                "Download CSV Template"
            ),

            br(),
            br(),

            fileInput(
                "upload_csv",
                "Upload Team CSV"
            )
        ),

        navset_tab(

            # =====================================================
            # FORECAST ENTRY
            # =====================================================

            nav_panel(
                "Forecast Entry",
                uiOutput("question_ui")
            ),

            # =====================================================
            # SCORE EXPLORER
            # =====================================================

            nav_panel(

                "Score Explorer",

                div(
                    class = "card-style",

                    selectInput(
                        "explore_team",
                        "Select team",
                        choices = NULL
                    ),

                    uiOutput("question_selector"),

                    numericInput(
                        "explore_G",
                        "Best estimate (G)",
                        value = NA
                    ),

                    numericInput(
                        "explore_S",
                        "Uncertainty width (S)",
                        value = NA,
                        min = 0.01
                    ),

                    div(
                        class = "answer-box",
                        textOutput("true_answer")
                    )
                ),

                div(
                    class = "card-style",

                    h4(textOutput("explore_title")),

                    plotOutput(
                        "explore_plot",
                        height = "650px"
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
                        "The red point shows the log score assigned to the team's forecast."
                    )
                )
            ),

            # =====================================================
            # TEAM RESULTS
            # =====================================================

            nav_panel(

                "Team Results",

                div(
                    class = "card-style",

                    h4("Scores by Team and Question"),

                    DTOutput("score_table")
                ),

                div(
                    class = "card-style",

                    h4("Overall Team Comparison"),

                    plotOutput(
                        "team_compare_plot",
                        height = "500px"
                    )
                )
            ),

            # =====================================================
            # SUMMARY PAGE
            # =====================================================

            nav_panel(

                "📘 Summary",

                div(
                    class = "main-title",

                    h1("📘 Forecasting, Uncertainty, and Calibration"),

                    p(
                        "Understanding prediction accuracy, uncertainty, and probabilistic thinking."
                    )
                ),

                fluidRow(

                    column(
                        6,

                        div(
                            class = "card-style",

                            h3("🎯 Purpose of the Activity"),

                            div(
                                class = "info-box",

                                tags$p(
                                    "The activity asks participants to make numerical forecasts about real-world quantities."
                                ),

                                tags$p(
                                    "For each question, teams provide:"
                                ),

                                tags$ul(

                                    tags$li(
                                        "A best estimate (G)"
                                    ),

                                    tags$li(
                                        "An uncertainty width (S)"
                                    )
                                ),

                                tags$p(
                                    "The aim is not only to predict accurately, but also to express uncertainty realistically."
                                ),

                                tags$p(
                                    "Good forecasts balance precision with honest uncertainty."
                                )
                            )
                        )
                    ),

                    column(
                        6,

                        div(
                            class = "card-style",

                            h3("📊 Forecast Distributions"),

                            div(
                                class = "info-box",

                                tags$p(
                                    "Each team's prediction is interpreted as a probability distribution."
                                ),

                                tags$p(
                                    "The uncertainty width determines the spread of the distribution:"
                                ),

                                tags$ul(

                                    tags$li(
                                        "Small uncertainty implies high confidence"
                                    ),

                                    tags$li(
                                        "Large uncertainty implies lower confidence"
                                    )
                                ),

                                tags$p(
                                    "The app visualises these distributions and evaluates how much probability was assigned to the true outcome."
                                )
                            )
                        )
                    )
                ),

                fluidRow(

                    column(
                        6,

                        div(
                            class = "card-style",

                            h3("📐 Statistical Ideas"),

                            div(
                                class = "info-box",

                                tags$ul(

                                    tags$li(
                                        "Probability distributions"
                                    ),

                                    tags$li(
                                        "Forecast uncertainty"
                                    ),

                                    tags$li(
                                        "Calibration"
                                    ),

                                    tags$li(
                                        "Likelihood and density"
                                    ),

                                    tags$li(
                                        "Logarithmic scoring rules"
                                    ),

                                    tags$li(
                                        "Overconfidence versus underconfidence"
                                    ),

                                    tags$li(
                                        "Model evaluation"
                                    ),

                                    tags$li(
                                        "Decision-making under uncertainty"
                                    )
                                )
                            )
                        )
                    ),

                    column(
                        6,

                        div(
                            class = "card-style",

                            h3("🔍 Questions to Explore"),

                            div(
                                class = "info-box",

                                tags$ul(

                                    tags$li(
                                        "What happens when forecasts are too confident?"
                                    ),

                                    tags$li(
                                        "Why can inaccurate certainty be penalised heavily?"
                                    ),

                                    tags$li(
                                        "How does uncertainty affect scoring?"
                                    ),

                                    tags$li(
                                        "Can wider uncertainty sometimes improve performance?"
                                    ),

                                    tags$li(
                                        "Which teams appear best calibrated?"
                                    ),

                                    tags$li(
                                        "How should uncertainty be communicated?"
                                    )
                                )
                            )
                        )
                    )
                ),

                fluidRow(

                    column(
                        12,

                        div(
                            class = "card-style",

                            h3("🧠 Interpretation"),

                            div(
                                class = "info-box",

                                tags$p(
                                    "Forecasting is not only about producing accurate point estimates."
                                ),

                                tags$p(
                                    "It is also about expressing uncertainty appropriately."
                                ),

                                tags$blockquote(
                                    style = "
                                        font-size:22px;
                                        font-weight:700;
                                        color:#7B9ACC;
                                        border-left:5px solid #CDB4DB;
                                        padding-left:18px;
                                        margin-top:20px;
                                    ",

                                    "A good forecast assigns high probability to what actually happens."
                                ),

                                tags$p(
                                    "The logarithmic scoring rule rewards forecasts that place substantial probability near the true value."
                                ),

                                tags$p(
                                    "Overconfident forecasts can perform very poorly if reality differs from expectations, while excessively vague forecasts may fail to provide useful information."
                                ),

                                tags$p(
                                    "These ideas are central to statistics, economics, forecasting, machine learning, meteorology, and risk analysis."
                                )
                            )
                        )
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

    # =======================================================
    # QUESTIONS DATABASE
    # =======================================================

    questions <- reactiveVal(default_questions)

    # =======================================================
    # OPTIONAL QUESTION UPLOAD
    # =======================================================

    observeEvent(input$upload_questions, {

        req(input$upload_questions)

        d <- read_csv(
            input$upload_questions$datapath,
            show_col_types = FALSE
        )

        validate(
            need(
                all(c(
                    "question_id",
                    "category",
                    "label",
                    "icon",
                    "text",
                    "answer",
                    "units"
                ) %in% names(d)),
                "Question CSV must contain:
                question_id, category, label, icon, text, answer, units"
            )
        )

        d <- d %>%
            rename(question = question_id)

        questions(d)

        showNotification(
            "Alternative question set loaded.",
            type = "message"
        )
    })

    # =======================================================
    # TEAM DATABASE
    # =======================================================

    team_data <- reactiveVal(

        tibble(
            team = character(),
            question = integer(),
            G = numeric(),
            S = numeric()
        )
    )

    # =======================================================
    # QUESTION UI
    # =======================================================

    output$question_ui <- renderUI({

        q <- questions()

        tagList(

            lapply(seq_len(nrow(q)), function(i){

                div(

                    class = "card-style question-card",

                    h4(paste0(
                        q$icon[i],
                        " ",
                        q$label[i],
                        " — Question ",
                        q$question[i]
                    )),

                    p(q$text[i]),

                    fluidRow(

                        column(
                            4,

                            numericInput(
                                paste0("G_", i),
                                "Best estimate (G)",
                                value = NA
                            )
                        ),

                        column(
                            4,

                            numericInput(
                                paste0("S_", i),
                                "Uncertainty width (S)",
                                value = NA,
                                min = 0.01
                            )
                        )
                    )
                )
            })
        )
    })

    # =======================================================
    # QUESTION SELECTOR
    # =======================================================

    output$question_selector <- renderUI({

        q <- questions()

        selectInput(
            "explore_question",
            "Select question",
            choices = q$question,
            selected = q$question[1]
        )
    })

    # =======================================================
    # SAVE TEAM
    # =======================================================

    observeEvent(input$save_team, {

        req(input$team_name)

        q <- questions()

        new_team <- tibble(

            team = input$team_name,

            question = q$question,

            G = vapply(
                seq_len(nrow(q)),
                function(i) input[[paste0("G_", i)]],
                numeric(1)
            ),

            S = vapply(
                seq_len(nrow(q)),
                function(i) input[[paste0("S_", i)]],
                numeric(1)
            )
        )

        current <- team_data()

        current <- current %>%
            filter(team != input$team_name)

        team_data(
            bind_rows(current, new_team)
        )

        showNotification(
            paste("Saved", input$team_name),
            type = "message"
        )
    })

    # =======================================================
    # TEAM CSV UPLOAD (WIDE FORMAT)
    # =======================================================

    observeEvent(input$upload_csv, {

        req(input$upload_csv)


        d <- read_csv(
            input$upload_csv$datapath,
            show_col_types = FALSE
        )


        expected <- c(
            "team",
            paste0(rep(c("G","S"),10), rep(1:10, each=2))
        )


        validate(
            need(
                all(expected %in% names(d)),
                paste(
                    "CSV must contain columns:",
                    paste(expected, collapse=", ")
                )
            )
        )


        # Convert wide format:
        #
        # team G1 S1 G2 S2 ...
        #
        # into:
        #
        # team question G S


        long_data <- d %>%

            pivot_longer(

                cols = matches("^[GS][0-9]+$"),

                names_to = c(".value","question"),

                names_pattern = "([GS])(\\d+)"

            ) %>%

            mutate(

                team = as.character(team),

                question = as.integer(question)

            )


        current <- team_data()


        current <- current %>%

            filter(
                !team %in% unique(long_data$team)
            )


        team_data(

            bind_rows(
                current,
                long_data
            )

        )


        showNotification(
            "Teams uploaded successfully.",
            type = "message"
        )

    })

    # =======================================================
    # UPDATE TEAM SELECTOR
    # =======================================================

    observe({

        teams <- unique(team_data()$team)

        updateSelectInput(
            session,
            "explore_team",
            choices = teams,
            selected = teams[1]
        )
    })

    # =======================================================
    # LOAD TEAM DATA INTO EXPLORER
    # =======================================================

    observeEvent(
        list(input$explore_team, input$explore_question),
        {

            req(
                input$explore_team,
                input$explore_question
            )

            d <- team_data() %>%
                filter(
                    team == input$explore_team,
                    question == as.integer(input$explore_question)
                )

            req(nrow(d) == 1)

            updateNumericInput(
                session,
                "explore_G",
                value = d$G
            )

            updateNumericInput(
                session,
                "explore_S",
                value = d$S
            )
        }
    )

    # =======================================================
    # TRUE ANSWER DISPLAY
    # =======================================================

    output$true_answer <- renderText({

        q <- questions()

        i <- which(
            q$question == as.integer(input$explore_question)
        )

        paste0(
            "True answer: ",
            q$answer[i],
            " ",
            q$units[i]
        )
    })

    # =======================================================
    # ALL SCORES
    # =======================================================

    all_scores <- reactive({

        req(nrow(team_data()) > 0)

        team_data() %>%

            left_join(
                questions(),
                by = "question"
            ) %>%

            mutate(

                score = mapply(
                    activity4_response_score,
                    G,
                    S,
                    answer
                )
            )
    })

    # =======================================================
    # PAGE 2
    # =======================================================

    output$explore_title <- renderText({

        q <- questions()

        i <- which(
            q$question == as.integer(input$explore_question)
        )

        paste0(
            q$icon[i],
            " ",
            q$label[i],
            " — Question ",
            q$question[i]
        )
    })

    output$explore_plot <- renderPlot({

        validate(
            need(!is.na(input$explore_G), ""),
            need(!is.na(input$explore_S), "")
        )

        q <- questions()

        i <- which(
            q$question == as.integer(input$explore_question)
        )

        distribution_plot(
            G = input$explore_G,
            S = input$explore_S,
            Theta = q$answer[i]
        )
    })

    output$explore_score <- renderText({

        validate(
            need(!is.na(input$explore_G), ""),
            need(!is.na(input$explore_S), "")
        )

        q <- questions()

        i <- which(
            q$question == as.integer(input$explore_question)
        )

        s <- activity4_response_score(
            G = input$explore_G,
            S = input$explore_S,
            Theta = q$answer[i]
        )

        paste0("Log score = ", s)
    })

    # =======================================================
    # SCORE TABLE
    # =======================================================

    output$score_table <- renderDT({

        d <- all_scores() %>%

            dplyr::select(
                team,
                question,
                label,
                G,
                S,
                answer,
                score
            )

        datatable(
            d,
            rownames = FALSE,
            options = list(
                pageLength = 10,
                scrollX = TRUE
            )
        )
    })

    # =======================================================
    # TEAM COMPARISON
    # =======================================================

    output$team_compare_plot <- renderPlot({

        d <- all_scores() %>%

            group_by(team) %>%

            summarise(
                total_score = sum(score, na.rm = TRUE),
                .groups = "drop"
            )

        ggplot(
            d,
            aes(
                x = reorder(team, total_score),
                y = total_score,
                fill = team
            )
        ) +

            geom_col(width = 0.7) +

            coord_flip() +

            theme_minimal(base_size = 14) +

            labs(
                title = "Overall Team Scores",
                x = NULL,
                y = "Total log score"
            ) +

            theme(
                legend.position = "none"
            )
    })

    # =======================================================
    # DOWNLOAD TEMPLATE
    # =======================================================

    output$download_template <- downloadHandler(

        filename = function() {
            "activity4_template.csv"
        },


        content = function(file) {


            template <- tibble::tibble(

                team = 1:10

            )


            for(i in 1:10){

                template[[paste0("G",i)]] <- NA_real_

                template[[paste0("S",i)]] <- NA_real_

            }


            write.csv(

                template,

                file,

                row.names = FALSE

            )

        }

    )
}

# =========================================================
# RUN APP
# =========================================================

shinyApp(ui, server)
