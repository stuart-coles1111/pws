suppressPackageStartupMessages({
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(gt)
})

# =========================================================
# Helpers
# =========================================================

generate_seq <- function(n = 50){
    sample(c("H","T"), n, replace = TRUE)
}

parse_seq <- function(x, n = 50){

    if(is.null(x) || x == "") return(NULL)

    x <- toupper(x)
    x <- unlist(strsplit(x, ""))
    x <- x[x %in% c("H", "T")]

    if(length(x) != n) return(NULL)

    x
}

activity1_stats <- function(x){
    list(
        heads = sum(x == "H"),
        longest_run = max(rle(x)$lengths)
    )
}

seq_df <- function(x){
    data.frame(
        pos = seq_along(x),
        toss = x
    )
}

# =========================================================
# Theoretical distributions
# =========================================================

theoretical_heads_df <- function(n = 50){
    data.frame(
        x = 0:n,
        prob = dbinom(0:n, n, 0.5)
    )
}

theoretical_runs_df <- function(n = 50,
                                x_low = 1,
                                x_up = 20){

    probs <-
        diff(
            sapply(
                (x_low - 1):x_up,
                pws:::ht_max_run_cdf,
                n = n
            )
        )

    data.frame(
        x = x_low:x_up,
        prob = probs
    )
}

# =========================================================
# Smartodds benchmark data
# =========================================================

human_heads <- apply(
    pws::activity1_data_sm,
    1,
    function(x) sum(x == "H")
)

human_runs <- apply(
    pws::activity1_data_sm,
    1,
    function(x) max(rle(x)$lengths)
)

# =========================================================
# Uploaded data validator
# =========================================================

validate_uploaded_data <- function(df){

    if(ncol(df) != 50) return(FALSE)

    mat <- as.matrix(df)
    mat <- trimws(toupper(mat))

    all(mat %in% c("H", "T"))
}

# =========================================================
# UI (UPDATED ONLY HERE)
# =========================================================

ui <- page_navbar(

    title = "🪙 Activity 1: Picturing randomness",

    theme = bs_theme(
        version = 5,
        bootswatch = "minty",
        primary = "#7B9ACC",
        bg = "#F7F7FB",
        fg = "#2E3440"
    ),

    header = tags$head(
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
    "))
    ),

    # =====================================================
    # TAB 1 — MAIN ACTIVITY
    # =====================================================

    nav_panel(

        "🪙 Activity",

        div(
            style = "
            text-align:center;
            padding:16px;
            border-radius:12px;
            background:linear-gradient(90deg,#A8DADC,#CDB4DB);
            margin-bottom:15px;",
            h1("🪙 Activity 1: Picturing Randomness")
        ),

        layout_sidebar(

            fillable = TRUE,

            sidebar = card(

                h4("Data Entry"),

                textAreaInput(
                    "user_seq",
                    "Enter 50 imaginary coin tosses (H/T)",
                    placeholder = "e.g. HTHTHTHT...",
                    height = "120px"
                ),

                actionButton(
                    "random_seq",
                    "Generate random sequence"
                ),

                actionButton(
                    "submit_seq",
                    "Analyse data",
                    class = "btn-primary"
                ),

                hr(),

                h4("Upload group data"),

                fileInput(
                    "upload_data",
                    "Upload CSV file",
                    accept = c(".csv")
                ),

                helpText(
                    "CSV should contain exactly 50 columns.",
                    "Each row should be one participant sequence.",
                    "Entries must be H or T."
                ),

                hr(),

                h4("Comparisons"),

                checkboxInput(
                    "compare_humans",
                    "Compare to Smartodds participants",
                    TRUE
                ),

                checkboxInput(
                    "compare_uploaded",
                    "Compare to uploaded data",
                    FALSE
                ),

                checkboxInput(
                    "compare_theoretical",
                    "Compare to theoretical population",
                    TRUE
                ),

                actionButton(
                    "compare_groups",
                    "Compare with groups",
                    class = "btn-success"
                ),

                actionButton(
                    "groups_analysis",
                    "Groups analysis",
                    class = "btn-info"
                )
            ),


            # ==========================
            # MAIN CONTENT
            # ==========================

            div(
                style = "width:100%;",

                card(
                    h4("Your sequence"),

                    div(
                        style = "font-family:monospace; font-size:18px;",
                        textOutput("seq_text")
                    )
                ),


                card(
                    h4("Visual structure"),

                    plotOutput(
                        "seq_plot",
                        height = 120
                    )
                ),


                card(
                    h4("Sequence statistics"),

                    fluidRow(

                        column(
                            6,

                            div(
                                style = "
                        background:#EEF2FF;
                        padding:18px;
                        border-radius:10px;
                        text-align:center;",

                                h5("Heads"),

                                h2(
                                    textOutput("user_heads")
                                )
                            )
                        ),


                        column(
                            6,

                            div(
                                style = "
                        background:#F3E8FF;
                        padding:18px;
                        border-radius:10px;
                        text-align:center;",

                                h5("Longest run"),

                                h2(
                                    textOutput("user_run")
                                )
                            )
                        )
                    )
                ),


                # ======================
                # GRAPHS SIDE BY SIDE
                # ======================

                fluidRow(

                    column(
                        6,

                        card(
                            h4("Number of Heads"),

                            plotOutput(
                                "heads_plot",
                                height = 320
                            )
                        )
                    ),


                    column(
                        6,

                        card(
                            h4("Longest Run"),

                            plotOutput(
                                "runs_plot",
                                height = 320
                            )
                        )
                    )
                ),


                # ======================
                # RESULTS BELOW GRAPHS
                # ======================

                uiOutput("stats_card")

            )
        )
    ),

    # =====================================================
    # TAB 2 — SUMMARY (NEW)
    # =====================================================

    nav_panel(

        "📘 Summary",

        div(
            style = "
            text-align:center;
            padding:18px;
            border-radius:14px;
            background:linear-gradient(90deg,#A8DADC,#CDB4DB);
            margin-bottom:20px;",
            h1("📘 Understanding Randomness in Coin Tosses"),
            p("What structure appears inside random sequences?")
        ),

        fluidRow(

            column(
                6,

                div(
                    class = "card-style",

                    h3("🪙 What is happening?"),

                    div(
                        class = "info-box",

                        tags$p("Each sequence is generated by repeated random coin flips."),
                        tags$p("Even though outcomes are random, patterns still emerge."),
                        tags$p("We study how these patterns behave across people and theory.")
                    )
                )
            ),

            column(
                6,

                div(
                    class = "card-style",

                    h3("📊 Why compare groups?"),

                    div(
                        class = "info-box",

                        tags$p("Human-generated sequences are often not truly random."),
                        tags$p("People tend to underestimate long runs of the same outcome."),
                        tags$p("Comparing datasets reveals these systematic biases.")
                    )
                )
            )
        ),

        fluidRow(

            column(
                6,

                div(
                    class = "card-style",

                    h3("📐 Key ideas"),

                    div(
                        class = "info-box",

                        tags$ul(
                            tags$li("Bernoulli trials"),
                            tags$li("Binomial distribution"),
                            tags$li("Runs and clustering"),
                            tags$li("Sampling variability"),
                            tags$li("Human perception of randomness")
                        )
                    )
                )
            ),

            column(
                6,

                div(
                    class = "card-style",

                    h3("🔍 Questions"),

                    div(
                        class = "info-box",

                        tags$ul(
                            tags$li("Why do humans avoid long runs?"),
                            tags$li("How random is human data compared to theory?"),
                            tags$li("What changes with longer sequences?"),
                            tags$li("How reliable are these statistics?")
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

                    h3("🧠 Big idea"),

                    div(
                        class = "info-box",

                        tags$p("Randomness still produces structure."),
                        tags$blockquote(
                            style = "
                                font-size:22px;
                                font-weight:700;
                                color:#7B9ACC;
                                border-left:5px solid #CDB4DB;
                                padding-left:18px;",
                            "Apparent patterns are inevitable in random processes."
                        )
                    )
                )
            )
        )
    )
)

# =========================================================
# SERVER (UNCHANGED)
# =========================================================

server <- function(input, output, session){

    rv <- reactiveValues(
        user_seq = NULL,
        uploaded_data = NULL,
        show_comparison = FALSE,
        show_group_analysis = FALSE
    )

    observeEvent(input$upload_data, {

        req(input$upload_data)

        df <- tryCatch({
            read.csv(
                input$upload_data$datapath,
                header = FALSE,
                stringsAsFactors = FALSE
            )
        }, error = function(e) NULL)

        if(is.null(df)){
            showNotification("Could not read CSV file.", type = "error")
            return()
        }

        if(!validate_uploaded_data(df)){
            showNotification(
                "Uploaded data must contain exactly 50 columns of H/T values.",
                type = "error"
            )
            return()
        }

        rv$uploaded_data <- trimws(toupper(as.matrix(df)))

        showNotification(
            paste("Uploaded", nrow(rv$uploaded_data), "participant sequences."),
            type = "message"
        )
    })

    observeEvent(input$submit_seq, {

        rv$show_comparison <- FALSE

        seq <- parse_seq(input$user_seq)

        if(is.null(seq)){
            showNotification(
                "Please enter exactly 50 H/T values.",
                type = "error"
            )
            return()
        }

        rv$user_seq <- seq
    })

    observeEvent(input$random_seq, {

        seq <- generate_seq(50)

        updateTextAreaInput(
            session,
            "user_seq",
            value = paste(seq, collapse = "")
        )
    })

    observeEvent(input$compare_groups, {

        req(rv$user_seq)

        rv$show_comparison <- TRUE

    })

    observeEvent(input$groups_analysis, {

        req(rv$user_seq)

        rv$show_group_analysis <- TRUE

    })

    current_seq <- reactive({
        req(rv$user_seq)
        rv$user_seq
    })

    uploaded_heads <- reactive({
        req(rv$uploaded_data)
        apply(rv$uploaded_data, 1, function(x) sum(x == "H"))
    })

    uploaded_runs <- reactive({
        req(rv$uploaded_data)
        apply(rv$uploaded_data, 1, function(x) max(rle(x)$lengths))
    })

    output$user_heads <- renderText({
        req(rv$user_seq)
        activity1_stats(current_seq())$heads
    })

    output$user_run <- renderText({
        req(rv$user_seq)
        activity1_stats(current_seq())$longest_run
    })

    output$seq_text <- renderText({
        req(rv$user_seq)
        paste(current_seq(), collapse = " ")
    })

    output$stats_card <- renderUI({

        if (!rv$show_group_analysis) {
            return(NULL)
        }

        card(
            h4("Group results analysis"),
            gt_output("stats_table")
        )
    })

    output$seq_plot <- renderPlot({

        df <- seq_df(current_seq())

        ggplot(df, aes(pos, 1, fill = toss)) +
            geom_tile(height = 1) +
            scale_fill_manual(values = c(H = "#7B9ACC", T = "#CDB4DB")) +
            theme_void() +
            theme(legend.position = "none")
    })

    output$stats_table <- render_gt({

        uploaded_available <- !is.null(rv$uploaded_data)

        summary_df <- data.frame(
            Statistic = c("Number of Heads", "Longest Run"),
            Smartodds_Mean = c(
                round(mean(human_heads), 2),
                round(mean(human_runs), 2)
            ),
            Smartodds_SD = c(
                round(sd(human_heads), 2),
                round(sd(human_runs), 2)
            ),
            Theoretical_Mean = c(
                25,
                round(pws:::mean_max_run_length(50), 2)
            ),
            Theoretical_SD = c(
                round(sqrt(50 * 0.5 * 0.5), 2),
                round(sqrt(pws:::var_max_run_length(50)), 2)
            )
        )

        if (uploaded_available) {
            summary_df$Uploaded_Mean <- c(
                round(mean(uploaded_heads()), 2),
                round(mean(uploaded_runs()), 2)
            )
            summary_df$Uploaded_SD <- c(
                round(sd(uploaded_heads()), 2),
                round(sd(uploaded_runs()), 2)
            )
        }

        # transpose
        summary_df_t <- as.data.frame(t(summary_df[-1]))

        # use the statistics as column names
        names(summary_df_t) <- summary_df$Statistic

        # create source column from row names
        summary_df_t$Source <- rownames(summary_df_t)
        rownames(summary_df_t) <- NULL

        # move Source to first column
        summary_df_t <- summary_df_t[, c("Source", "Number of Heads", "Longest Run")]

        # prettier labels
        summary_df_t$Source <- gsub("_", " ", summary_df_t$Source)

        gt(summary_df_t) %>%
            cols_label(
                Source = "",
                `Number of Heads` = "Number of Heads",
                `Longest Run` = "Longest Run"
            )
    })

    output$heads_plot <- renderPlot({

        req(rv$user_seq)

        if(!rv$show_comparison){
            return(NULL)
        }

        user_heads <- sum(current_seq() == "H")

        p <- ggplot()

        if(input$compare_humans){

            p <- p + geom_histogram(
                data = data.frame(
                    value = human_heads,
                    source = "Smartodds"
                ),
                aes(
                    x = value,
                    y = after_stat(density),
                    fill = source
                ),
                binwidth = 1,
                alpha = 0.5,
                boundary = -0.5,
                position = "identity"
            )
        }

        if(input$compare_uploaded && !is.null(rv$uploaded_data)){

            p <- p + geom_histogram(
                data = data.frame(
                    value = uploaded_heads(),
                    source = "Uploaded"
                ),
                aes(
                    x = value,
                    y = after_stat(density),
                    fill = source
                ),
                binwidth = 1,
                alpha = 0.5,
                boundary = -0.5,
                position = "identity"
            )
        }

        if(input$compare_theoretical){

            p <- p + geom_col(
                data = theoretical_heads_df() %>%
                    mutate(source = "Theoretical"),
                aes(
                    x = x,
                    y = prob,
                    fill = source
                ),
                alpha = 0.7,
                width = 0.9
            )
        }

        p <- p +
            geom_vline(
                aes(
                    xintercept = user_heads,
                    colour = "Your sequence"
                ),
                linewidth = 1.4
            ) +
            scale_fill_manual(
                name = NULL,
                values = c(
                    Smartodds = "#7B9ACC",
                    Uploaded = "#F4A261",
                    Theoretical = "#CDB4DB"
                ),
                drop = FALSE
            ) +
            scale_colour_manual(
                name = NULL,
                values = c(
                    "Your sequence" = "red"
                )
            ) +
            labs(
                x = "Number of Heads",
                y = "Frequency density"
            ) +
            theme_minimal(base_size = 14)

        p
    })

    output$runs_plot <- renderPlot({

        req(rv$user_seq)

        if(!rv$show_comparison){
            return(NULL)
        }

        user_run <- max(rle(current_seq())$lengths)

        p <- ggplot()

        if(input$compare_humans){

            p <- p + geom_histogram(
                data = data.frame(
                    value = human_runs,
                    source = "Smartodds"
                ),
                aes(
                    x = value,
                    y = after_stat(density),
                    fill = source
                ),
                binwidth = 1,
                alpha = 0.5,
                boundary = -0.5,
                position = "identity"
            )
        }

        if(input$compare_uploaded && !is.null(rv$uploaded_data)){

            p <- p + geom_histogram(
                data = data.frame(
                    value = uploaded_runs(),
                    source = "Uploaded"
                ),
                aes(
                    x = value,
                    y = after_stat(density),
                    fill = source
                ),
                binwidth = 1,
                alpha = 0.5,
                boundary = -0.5,
                position = "identity"
            )
        }

        if(input$compare_theoretical){

            p <- p + geom_col(
                data = theoretical_runs_df() %>%
                    mutate(source = "Theoretical"),
                aes(
                    x = x,
                    y = prob,
                    fill = source
                ),
                alpha = 0.7,
                width = 0.9
            )
        }

        p <- p +
            geom_vline(
                aes(
                    xintercept = user_run,
                    colour = "Your sequence"
                ),
                linewidth = 1.4
            ) +
            scale_fill_manual(
                name = NULL,
                values = c(
                    Smartodds = "#7B9ACC",
                    Uploaded = "#F4A261",
                    Theoretical = "#CDB4DB"
                ),
                drop = FALSE
            ) +
            scale_colour_manual(
                name = NULL,
                values = c(
                    "Your sequence" = "red"
                )
            ) +
            labs(
                x = "Longest Run",
                y = "Frequency density"
            ) +
            theme_minimal(base_size = 14)

        p
    })
}

# =========================================================
# RUN APP
# =========================================================

shinyApp(ui, server)
