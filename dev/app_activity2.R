library(shiny)
library(bslib)
library(ggplot2)

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

            .big{
                font-size:30px;
                font-weight:700;
                text-align:center;
            }

            .option-box{
                background:#F8F9FB;
                padding:10px;
                border-radius:10px;
                margin-bottom:6px;
            }

            .nav-btn{
                margin-top:10px;
            }
        "))
    ),

    div(class = "main-title",
        h1("🧠 Activity 2: Who wants to be a Danish millionaire?")
    ),

    uiOutput("page_ui")
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session){

    rv <- reactiveValues(page = 1)

    # ---------------------------------------------------------
    # navigation
    # ---------------------------------------------------------

    observeEvent(input$next1, rv$page <- 2)
    observeEvent(input$next2, rv$page <- 3)
    observeEvent(input$next3, rv$page <- 4)

    observeEvent(input$back1, rv$page <- 1)
    observeEvent(input$back2, rv$page <- 2)
    observeEvent(input$back3, rv$page <- 3)

    # reset full app
    observeEvent(input$reset, {
        rv$page <- 1
        updateSliderInput(session, "strength", value = 0.5)
    })

    # =========================================================
    # PAGE RENDER
    # =========================================================

    output$page_ui <- renderUI({

        # =====================================================
        # PAGE 1 — CONTEXT
        # =====================================================

        if(rv$page == 1){

            fluidRow(
                column(12,
                       div(class = "card-style",

                           h3("Final question"),

                           p("Which of these Danish comedy movies premiered first?"),

                           div(class="option-box", strong("A) "), "Sover Dolly på Ryggen"),
                           div(class="option-box", strong("B) "), "Klassefesten"),
                           div(class="option-box", strong("C) "), "Blå Mænd"),
                           div(class="option-box", strong("D) "), "Superclasico"),

                           hr(),

                           h4("Balder’s information"),
                           p("* He knows Movie C is older than Movies A and B."),
                           p("* He does not know whether C or D is older."),

                           hr(),

                           h4("Question"),
                           p("Was Balder correct to quit and keep 500,000 kroner? Or should he have tried to win a million by answering correctly"),

                           actionButton("next1", "Explore reasoning →")
                       )
                )
            )
        }

        # =====================================================
        # PAGE 2 — COMBINATORIAL ARGUMENT (3/4)
        # =====================================================

        else if(rv$page == 2){

            fluidRow(
                column(12,
                       div(class = "card-style",

                           h3("Argument 1: Counting the possible options"),

                           p("List movies in chronological order."),

                           p("Assume all 24 possible orderings of A, B, C, D are equally likely."),

                           p("Then condition on the fact that C is older than A and B."),

                           p("This leaves the following possibilities:"),


                           tags$div(
                               style="font-family:monospace; background:#F8F9FB; padding:10px; border-radius:10px;",
                               HTML("
                            A B D C<br>
                            A D B C<br>
                            B A D C<br>
                            B D A C<br>
                            D A B C<br>
                            D B A C<br>
                            B A C D<br>
                            A B C D
                            ")
                           ),

                           hr(),

                           p("In 6 of these 8 cases, C is older than D."),

                           p("Each is equally likely, so..."),


                           div(class="big",
                               "P(C older than D) = 3/4"
                           ),

                           hr(),

                           actionButton("back1", "← Back"),
                           actionButton("next2", "Next →")
                       )
                )
            )
        }

        # =====================================================
        # PAGE 3 — MODEL DEPENDENCE (SLIDER)
        # =====================================================

        else if(rv$page == 3){

            fluidRow(

                column(4,
                       div(class="card-style",

                           h4("Model assumption"),

                           sliderInput(
                               "strength",
                               "How informative is Balder’s knowledge?",
                               min = 0, max = 1, value = 0.5, step = 0.01
                           ),

                           p("0 → pure guess (50–50)"),
                           p("1 → structural assumption (3/4)"),

                           actionButton("back2", "← Back"),
                           actionButton("next3", "Next →")
                       )
                ),

                column(8,
                       div(class="card-style",

                           div(class="big",
                               textOutput("p_display")
                           ),

                           hr(),

                           plotOutput("p_plot", height = "250px"),

                           hr(),

                           uiOutput("interpretation")
                       )
                )
            )
        }

        # =====================================================
        # PAGE 4 — DECISION (EXPECTED VALUE)
        # =====================================================

        else if(rv$page == 4){

            fluidRow(

                column(12,
                       div(class="card-style",

                           h3("Argument 3: Was quitting rational?"),

                           p("Now consider the monetary consequences of the decision."),

                           hr(),

                           uiOutput("decision_summary"),

                           hr(),

                           actionButton("back3", "← Back"),
                           actionButton("reset", "Restart")
                       )
                )
            )
        }
    })

    # =========================================================
    # PAGE 3 MODEL
    # =========================================================

    p_val <- reactive({
        0.5 + input$strength * (0.75 - 0.5)
    })

    output$p_display <- renderText({
        paste0("P(C older than D) = ", round(p_val(), 3))
    })

    output$p_plot <- renderPlot({

        p <- p_val()

        df <- data.frame(
            outcome = c("C older than D", "D older than C"),
            prob = c(p, 1 - p)
        )

        ggplot(df, aes(outcome, prob, fill = outcome)) +
            geom_col(width = 0.6) +
            ylim(0, 1) +
            theme_minimal(base_size = 12) +
            theme(legend.position = "none") +
            scale_fill_manual(values = c("#7B9ACC", "#CDB4DB")) +
            labs(x = NULL, y = "Probability")
    })

    output$interpretation <- renderUI({

        if(input$strength < 0.2){
            HTML("<b>Naive view:</b> This behaves like a 50–50 guess.")
        } else if(input$strength < 0.7){
            HTML("<b>Intermediate view:</b> The constraint gives partial information.")
        } else {
            HTML("<b>Structured view:</b> Under uniform ordering assumptions, the probability becomes 3/4.")
        }
    })

    # =========================================================
    # PAGE 4 DECISION
    # =========================================================

    output$decision_summary <- renderUI({

        p <- p_val()

        ev_play <- p * 1000000 + (1 - p) * 32000
        quit_val <- 500000

        decision <- if(ev_play > quit_val){
            "Playing looks better in expectation."
        } else {
            "Quitting looks better in expectation."
        }

        HTML(paste0(
            "<b>If he plays:</b><br>",
            "Expected value = ", round(ev_play,0), " kroner<br><br>",

            "<b>If he quits:</b><br>",
            "Certain outcome = 500,000 kroner<br><br>",

            "<hr>",

            "<b>Conclusion:</b><br>",
            decision
        ))
    })
}

# =========================================================
# RUN APP
# =========================================================

shinyApp(ui, server)
