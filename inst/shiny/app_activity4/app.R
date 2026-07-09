suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(ggplot2)
    library(dplyr)
    library(tidyr)
    library(DT)
    library(patchwork)
    library(readr)
})



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
            colour = "#7B9ACC",
            linewidth = 1.2
        ) +

        geom_vline(
            xintercept = G,
            colour = "#555555",
            linewidth = 1
        ) +

        geom_vline(
            xintercept = Theta,
            colour = "#E76F51",
            linewidth = 1.3
        )+

        theme_minimal(base_size = 12) +

        labs(
            title = "Normal Scoring Rule",
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
            colour = "#7B9ACC",
            linewidth = 1.2
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

        geom_hline(
            yintercept = theta_score,
            linetype = "dashed",
            colour = "#555555",
            linewidth = 0.9
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
            title = "Final Score",
            x = NULL,
            y = "Log density"
        ) +

        theme(
            panel.grid.minor = element_blank()
        )

    p1 / p2
}

load_question_set <- function(name) {

    readr::read_csv(
        system.file(
            "extdata",
            paste0(name, ".csv"),
            package = "pws"
        ),
        show_col_types = FALSE
    ) |>
        dplyr::rename(question = question_id)
}

# =========================================================
# UI
# =========================================================


ui <- page_navbar(

    title = "🎯 Activity 4: Quiz Time",

    theme = bs_theme(
        version = 5,
        bootswatch = "minty",
        primary = "#7B9ACC",
        bg = "#F7F7FB",
        fg = "#2E3440",
        base_font = font_google("Inter")
    ),

    header = tagList(

        tags$head(
            tags$style(HTML("

            .container-fluid {
                max-width: 100% !important;
                padding-left: 20px !important;
                padding-right: 20px !important;
            }

            .bslib-page-sidebar-main {
                width: 100% !important;
            }

            .card {
                width: 100%;
            }

            .main-title{
                background:linear-gradient(90deg,#A8DADC,#CDB4DB);
                padding:16px;
                border-radius:12px;
                margin-bottom:15px;
                text-align:center;
            }

            .btn{
                width:100%;
            }

            .sidebar-section{
    background:#FAFBFC;
    border:1px solid #E3E8EF;
    border-radius:10px;
    padding:12px;
    margin-bottom:28px;
}

.sidebar-title{
    font-size:16px;
    font-weight:700;
    color:#7B9ACC;
}

.sidebar-subtitle{
    font-size:0.85rem;
    color:#6C757D;
    margin-bottom:12px;
}

        "))
        ),

        div(
            class = "main-title",
            h1("🎯 Activity 4: Quiz Time")
        )

    ),

    # =========================
    # OVERVIEW
    # =========================

    overview_page(

        title = "Overview",

        explanation = tagList(

            p("This activity explores the importance of both accuracy and the quantification of uncertainty in estimation."),

            p("The activity comprises a 10-question quiz. When answering each question, participants must give a best guess (G) for the answer, as well as
              a measure of accuracy (S) for their best guess. Participants also specify an uncertainty measure (S) for each estimate. In this activity, S is defined so that if T denotes the true answer,
              then P(G−S<T<G+S)=0.5. In other words, participants choose S so that they believe there is a 50% probability that the true answer lies within S units of their best estimate."),

            p("By default, the questions in the quiz are sports-based. They can, however, be replaced by country-related questions, or with a user-supplied set of questions
              and answers. See the discussion in Section ?.? of Playing With Statistics for details."),

            p("The score for each question will depend both on how precise the estimate was, and how accurate the participant believed their estimate to be. Again, full details
            are given in Section ?.? of Playing With Statistics. A partcipant's overall score is the sum of the scores per question.")
        ),

        individual = tagList(
            p("The individual activity uses the default sports quiz and is designed to allow participants to compare their results with those obtained from Smartodds employees."),

            tags$ol(
                tags$li("Complete the quiz by entering your values of G and S for each question directly into the app interface."),
                tags$li("Examine your score for each question in the Score Explorer."),
                tags$li("Compare your results with the answers provided by Smartodds employees.")
            )
        ),

        group = tagList(

            p("The activity is ideally suited to a group meeting, with participants answering questions individually or working in teams."),

            p("By default, the app uses a sports-themed quiz. This can be replaced by a countries-themed quiz or by a user-supplied set of questions and answers."),

            p("Participant responses can be entered manually or uploaded from a CSV file. Templates for entering question sets and participant responses can be created directly from the app."),

            p("Once participant responses have been entered, the app can be used to:"),

            tags$ol(
                tags$li("Examine scores for each question."),
                tags$li("Compare total scores across teams.")
            ),

            p("The results provide a natural opportunity to discuss the importance of making the best possible estimate (G) while also giving an honest assessment of the associated uncertainty (S).")
        ),

        question = tagList(

            tags$ul(
                tags$li("What happens when forecasts are too confident?"),
                tags$li("Why are some forecasts heavily penalised?"),
                tags$li("How should uncertainty be communicated?"),
                tags$li("Can wider uncertainty sometimes help?")
            )
        )
    ),

    # =========================
    # ACTIVITY
    # =========================

    nav_panel(

        "Activity",

        layout_sidebar(

            sidebar = div(

                class = "card-style",

                # =====================================================
                # QUESTION SET
                # =====================================================

                div(

                    class = "sidebar-section",

                    div(class = "sidebar-title", "📋 Question set"),

                    div(
                        class = "sidebar-subtitle",
                        "Choose a built-in quiz or upload your own."
                    ),

                    selectInput(
                        "question_set",
                        NULL,
                        choices = c(
                            "Sports" = "sports",
                            "Countries" = "countries",
                            "Upload my own..." = "upload"
                        ),
                        selected = "sports"
                    ),

                    conditionalPanel(

                        condition = "input.question_set == 'upload'",

                        downloadButton(
                            "download_question_template",
                            "Create question template",
                            class = "btn-outline-primary"
                        ),

                        br(), br(),

                        fileInput(
                            "upload_questions",
                            "Upload question CSV"
                        )
                    )
                ),

                # =====================================================
                # PARTICIPANT DATA
                # =====================================================

                div(

                    class = "sidebar-section",

                    div(class = "sidebar-title", "👥 Participant data"),

                    div(
                        class = "sidebar-subtitle",
                        "Load saved responses or enter them manually."
                    ),

                    selectInput(
                        "participant_data",
                        NULL,
                        choices = c(
                            "None" = "none",
                            "Smartodds sample" = "smartodds",
                            "Upload my own..." = "upload"
                        ),
                        selected = "none"
                    ),

                    conditionalPanel(

                        condition = "input.participant_data == 'upload'",

                        numericInput(
                            "num_teams",
                            "Number of teams",
                            10,
                            1,
                            100
                        ),

                        downloadButton(
                            "download_template",
                            "Create participant template",
                            class = "btn-outline-primary"
                        ),

                        br(), br(),

                        fileInput(
                            "upload_csv",
                            "Upload participant CSV"
                        )
                    )
                ),

                # =====================================================
                # MANUAL ENTRY
                # =====================================================

                div(

                    class = "sidebar-section",

                    div(class = "sidebar-title", "✍ Manual entry"),

                    div(
                        class = "sidebar-subtitle",
                        "Enter one team's responses directly."
                    ),

                    textInput(
                        "team_name",
                        "Team name",
                        "Team 1"
                    ),

                    actionButton(
                        "save_team",
                        "Save team answers",
                        class = "btn-primary"
                    )
                )
            ),
            navset_tab(

                nav_panel("Q and A", uiOutput("question_ui")),

                nav_panel(

                    "Score Explorer (⚠️ reveals answers)",

                    div(
                        class = "card-style",

                        fluidRow(

                            column(
                                6,
                                selectInput(
                                    "explore_team",
                                    "Team",
                                    choices = NULL
                                )
                            ),

                            column(
                                6,
                                uiOutput("question_selector")
                            )

                        ),

                        h4(textOutput("explore_title")),

                        fluidRow(

                            column(
                                6,
                                numericInput(
                                    "explore_G",
                                    "Best estimate (G)",
                                    value = NA
                                )
                            ),

                            column(
                                6,
                                numericInput(
                                    "explore_S",
                                    "Uncertainty width (S)",
                                    value = NA,
                                    min = 0.01
                                )
                            )

                        ),

                        div(
                            class = "answer-box",
                            textOutput("true_answer")
                        )
                    ),

                    div(
                        class = "card-style",

                        plotOutput("explore_plot", height = "650px")
                    ),

                    div(
                        class = "card-style",

                        div(class = "score-big", textOutput("explore_score")),

                        p(
                            class = "small-note",
                            "The dashed horizontal line shows the log score assigned to the team's answer."
                        )
                    )
                ),

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
                        plotOutput("team_compare_plot", height = "500px")
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

    questions <- reactiveVal(
        load_question_set("sports")
    )

    observeEvent(input$question_set, {

        req(input$question_set)

        if (input$question_set == "upload")
            return()

        questions(
            load_question_set(input$question_set)
        )

    }, ignoreInit = TRUE)


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
            "Question",
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

    load_team_data <- function(path) {

        d <- read_csv(
            path,
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

    }

    observeEvent(input$upload_csv, {

        req(input$upload_csv)

        load_team_data(
            input$upload_csv$datapath
        )

    })

    observeEvent(input$participant_data, {

        req(input$participant_data == "smartodds")

        load_team_data(
            system.file(
                "extdata",
                "smartodds_sports_quiz.csv",
                package = "pws"
            )
        )

    })

    observe({

        if (input$question_set == "sports") {

            choices <- c(
                "None" = "none",
                "Smartodds sample" = "smartodds",
                "Upload team answers..." = "upload"
            )

        } else {

            choices <- c(
                "None" = "none",
                "Upload team answers..." = "upload"
            )

        }

        updateSelectInput(
            session,
            "participant_data",
            choices = choices,
            selected = "none"
        )

    })

    # =======================================================
    # QUESTION CSV TEMPLATE
    # =======================================================


    output$download_question_template <- downloadHandler(

        filename = function() {
            "question_template.csv"
        },

        content = function(file) {

            template <- tibble::tibble(

                question_id = 1:10,
                category = rep("", 10),
                label = rep("", 10),
                icon = rep("", 10),
                text = rep("", 10),
                answer = rep("", 10),
                units = rep("", 10)

            )

            write.csv(
                template,
                file,
                row.names = FALSE
            )

        }

    )

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

        DT::datatable(
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
                x = "Team",
                y = "Total score"
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

            n <- input$num_teams

            template <- tibble::tibble(
                team = as.character(1:n)
            )


            for(i in 1:10){

                template[[paste0("G", i)]] <- NA_real_

                template[[paste0("S", i)]] <- NA_real_

            }


            write.csv(
                template,
                file,
                row.names = FALSE,
                na = ""
            )

        }

    )
}

# =========================================================
# RUN APP
# =========================================================

shinyApp(ui, server)
