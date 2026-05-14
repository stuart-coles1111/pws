library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# =========================================================
# Helpers
# =========================================================

generate_seq <- function(n = 50){
    sample(c("H","T"), n, replace = TRUE)
}

parse_seq <- function(x, n = 50){

    if(is.null(x) || x == "") return(NULL)

    x <- toupper(x)

    # extract only H/T characters
    x <- unlist(strsplit(x, ""))
    x <- x[x %in% c("H", "T")]

    if(length(x) != n) return(NULL)

    x
}

activity1_stats <- function(x) {
    list(
        heads = sum(x == "H"),
        longest_run = rle(x)$lengths %>% max(),
        switch_rate = sum(x[-1] != x[-length(x)]) / (length(x) - 1)
    )
}

seq_df <- function(x){
    data.frame(pos = seq_along(x), toss = x)
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
        fg = "#2E3440"
    ),

    div(
        style = "
            text-align:center;
            padding:16px;
            border-radius:12px;
            background:linear-gradient(90deg,#A8DADC,#CDB4DB);
            margin-bottom:15px;",
        h1("🪙 Activity 1: What does randomness look like?")
    ),

    layout_sidebar(

        sidebar = card(

            h4("Your sequence"),

            textAreaInput(
                "user_seq",
                "Enter 50 coin tosses (H and T)",
                placeholder = "e.g. H T H T T H H T ... or HTHTTHTT...",
                height = "120px"
            ),

            actionButton("submit_seq", "Analyse my sequence", class = "btn-primary"),
            actionButton("random_seq", "Generate random sequence"),

            hr(),

            checkboxInput("compare_humans", "Compare to Smartodds data", TRUE),
            checkboxInput("compare_random", "Compare to random sequences", TRUE),

            helpText("All sequences are equally likely. We study structure, not correctness.")
        ),

        mainPanel(

            # =================================================
            # 1. DISPLAY SEQUENCE
            # =================================================
            card(
                h4("Sequence"),
                div(style = "font-family:monospace; font-size:18px;",
                    textOutput("seq_text")
                )
            ),

            # =================================================
            # 2. VISUAL STRUCTURE
            # =================================================
            card(
                h4("Visual structure"),
                plotOutput("seq_plot", height = 120)
            ),

            # =================================================
            # 3. RANDOMNESS FINGERPRINT
            # =================================================
            card(
                h4("Randomness summary"),
                tableOutput("stats_table")
            ),

            # =================================================
            # 4. HEADS COMPARISON
            # =================================================
            card(
                h4("Number of Heads: comparison"),
                plotOutput("heads_plot", height = 260)
            ),

            # =================================================
            # 5. RUNS COMPARISON
            # =================================================
            card(
                h4("Longest run: comparison"),
                plotOutput("runs_plot", height = 260)
            )
        )
    )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session){

    rv <- reactiveValues(
        seq = generate_seq(50),
        user_seq = NULL
    )

    # -----------------------------------------------------
    # Submit user sequence
    # -----------------------------------------------------

    observeEvent(input$submit_seq, {

        seq <- parse_seq(input$user_seq)

        if(is.null(seq)){
            showNotification(
                "Please enter exactly 50 values of H or T",
                type = "error"
            )
            return()
        }

        rv$user_seq <- seq
    })

    # -----------------------------------------------------
    # Generate random sequence
    # -----------------------------------------------------

    observeEvent(input$random_seq, {
        rv$user_seq <- generate_seq(50)
    })

    # -----------------------------------------------------
    # current sequence (priority: user > default)
    # -----------------------------------------------------

    current_seq <- reactive({
        if(!is.null(rv$user_seq)) rv$user_seq else rv$seq
    })

    # -----------------------------------------------------
    # text output
    # -----------------------------------------------------

    output$seq_text <- renderText({
        paste(current_seq(), collapse = " ")
    })

    # -----------------------------------------------------
    # visual structure
    # -----------------------------------------------------

    output$seq_plot <- renderPlot({

        df <- seq_df(current_seq())

        ggplot(df, aes(pos, 1, fill = toss)) +
            geom_tile(height = 1) +
            scale_fill_manual(values = c(H = "#7B9ACC", T = "#CDB4DB")) +
            theme_void() +
            theme(legend.position = "none")
    })

    # -----------------------------------------------------
    # stats table
    # -----------------------------------------------------

    output$stats_table <- renderTable({

        s <- activity1_stats(current_seq())

        data.frame(
            Statistic = c("Heads", "Longest run", "Switch rate"),
            Value = c(s$heads, s$longest_run, round(s$switch_rate, 2))
        )
    })

    # -----------------------------------------------------
    # HEADS comparison
    # -----------------------------------------------------

    output$heads_plot <- renderPlot({

        req(input$compare_humans || input$compare_random)

        user <- sum(current_seq() == "H")

        df <- data.frame(value = user)

        if(input$compare_humans){
            humans <- apply(pws::activity1_data_sm, 1, function(x) sum(x == "H"))
            df <- rbind(df, data.frame(value = humans))
        }

        if(input$compare_random){
            sim <- replicate(200, sum(generate_seq(50) == "H"))
            df <- rbind(df, data.frame(value = sim))
        }

        ggplot(df, aes(value)) +
            geom_histogram(binwidth = 1, fill = "#7B9ACC", alpha = 0.6) +
            geom_vline(xintercept = user, colour = "red", linewidth = 1.2) +
            labs(x = "Number of Heads (out of 50)", y = "Frequency") +
            theme_minimal()
    })

    # -----------------------------------------------------
    # RUNS comparison
    # -----------------------------------------------------

    output$runs_plot <- renderPlot({

        req(input$compare_humans || input$compare_random)

        user <- rle(current_seq())$lengths %>% max()

        df <- data.frame(value = user)

        if(input$compare_humans){
            humans <- apply(pws::activity1_data_sm, 1, function(x)
                rle(x)$lengths %>% max())

            df <- rbind(df, data.frame(value = humans))
        }

        if(input$compare_random){
            sim <- replicate(200, {
                x <- generate_seq(50)
                rle(x)$lengths %>% max()
            })

            df <- rbind(df, data.frame(value = sim))
        }

        ggplot(df, aes(value)) +
            geom_histogram(binwidth = 1, fill = "#CDB4DB", alpha = 0.6) +
            geom_vline(xintercept = user, colour = "red", linewidth = 1.2) +
            labs(x = "Longest run", y = "Frequency") +
            theme_minimal()
    })

}

shinyApp(ui, server)
