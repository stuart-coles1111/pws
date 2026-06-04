library(shiny)
library(ggplot2)
library(DT)

# =========================================================
# Chapter 1 UI
# =========================================================

chapter1_ui <- function(id){

    ns <- NS(id)

    # =======================================================
    # Sidebar controls
    # =======================================================

    sidebar_controls <- sidebar(

        h4("Simulation controls"),

        p(
            "Adjust the simulation parameters and explore how the
      distribution of goals changes."
        ),

        numericInput(
            ns("n_sim"),
            "Number of simulations",
            value = 10000,
            min = 100,
            step = 100
        ),

        sliderInput(
            ns("pois_mean"),
            "Average opportunities per match",
            min = 1,
            max = 50,
            value = 25
        ),

        sliderInput(
            ns("beta_1"),
            "Beta alpha 1",
            min = 0.001,
            max = 1,
            value = 0.02,
            step = 0.001
        ),

        sliderInput(
            ns("beta_2"),
            "Beta alpha 2",
            min = 0.01,
            max = 2,
            value = 0.2,
            step = 0.01
        ),

        numericInput(
            ns("seed"),
            "Random seed",
            value = 111
        ),

        hr(),

        actionButton(
            ns("run"),
            "Run simulation",
            class = "btn-primary"
        )
    )

    # =======================================================
    # Overview tab
    # =======================================================

    overview_panel <- div(

        card(

            card_header("What this chapter explores"),

            p("
        This chapter introduces statistical simulation using
        a simple football goals model.
      "),

            p("
        We repeatedly simulate matches and study the resulting
        distribution of goals scored.
      "),

            tags$ul(
                tags$li("Random variation"),
                tags$li("Repeated simulation"),
                tags$li("Distributions"),
                tags$li("Summary statistics"),
                tags$li("Sampling variability")
            )
        )
    )

    # =======================================================
    # Code tab
    # =======================================================

    code_panel <- div(

        card(

            card_header("Generated R code"),

            p("
        This is the equivalent R code for the current settings.
      "),

            tags$pre(
                style = "
          background:#F8F9FA;
          padding:15px;
          border-radius:10px;
          font-size:15px;
        ",

                textOutput(ns("generated_code"))
            )
        )
    )

    # =======================================================
    # Results tab
    # =======================================================

    results_panel <- div(

        fluidRow(

            column(
                4,

                card(

                    style = "
            background:#EEF2FF;
          ",

                    h4("Mean"),

                    h2(textOutput(ns("mean")))
                )
            ),

            column(
                4,

                card(

                    style = "
            background:#F3E8FF;
          ",

                    h4("Standard deviation"),

                    h2(textOutput(ns("sd")))
                )
            ),

            column(
                4,

                card(

                    style = "
            background:#EAF7F2;
          ",

                    h4("Simulations"),

                    h2(textOutput(ns("n_display")))
                )
            )
        ),

        br(),

        card(

            card_header("Distribution of goals"),

            plotOutput(
                ns("hist"),
                height = 350
            )
        ),

        br(),

        fluidRow(

            column(
                6,

                card(

                    card_header("First few simulated values"),

                    DTOutput(ns("head_table"))
                )
            ),

            column(
                6,

                card(

                    card_header("Frequency table"),

                    DTOutput(ns("freq_table"))
                )
            )
        ),

        br(),

        card(

            card_header("Understanding the output"),

            tags$pre(
                "goals_per_match
├── $data   → vector of simulated goals
└── $table  → frequency table"
            )
        )
    )

    # =======================================================
    # Learn tab
    # =======================================================

    learn_panel <- div(

        card(

            card_header("Key ideas"),

            tags$ul(

                tags$li("
          Simulation creates artificial data using probability models.
        "),

                tags$li("
          Randomness still produces structure and patterns.
        "),

                tags$li("
          Larger simulations produce more stable distributions.
        "),

                tags$li("
          Summary statistics help describe distributions.
        ")
            )
        ),

        br(),

        card(

            card_header("Try changing..."),

            tags$ul(

                tags$li("Reduce the number of simulations."),
                tags$li("Increase the Poisson mean."),
                tags$li("Change the beta parameters."),
                tags$li("Change the random seed.")
            )
        ),

        br(),

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
                "Simulation helps us understand uncertainty."
            )
        )
    )

    # =======================================================
    # Build chapter page
    # =======================================================

    chapter_page_ui(

        id = id,

        title = "🎲 Chapter 1: Randomness",

        sidebar = sidebar_controls,

        overview = overview_panel,

        code = code_panel,

        results = results_panel,

        learn = learn_panel
    )
}

# =========================================================
# Chapter 1 Server
# =========================================================

chapter1_server <- function(id){

    moduleServer(id, function(input, output, session){

        sim <- eventReactive(input$run, {

            pws::goals_sim(

                n_sim = input$n_sim,

                pois_mean = input$pois_mean,

                beta_1 = input$beta_1,

                beta_2 = input$beta_2,

                seed = input$seed
            )
        },
        ignoreNULL = FALSE)

        # =====================================================
        # Generated code
        # =====================================================

        output$generated_code <- renderText({

            paste0(
                "goals_per_match <- goals_sim(
  n_sim = ", input$n_sim, ",
  pois_mean = ", input$pois_mean, ",
  beta_1 = ", input$beta_1, ",
  beta_2 = ", input$beta_2, ",
  seed = ", input$seed, "
)

head(goals_per_match$data)

goals_per_match$table

mean(goals_per_match$data)
sd(goals_per_match$data)"
            )
        })

    # =====================================================
    # Summary stats
    # =====================================================

    output$mean <- renderText({

        round(mean(sim()$data), 3)
    })

    output$sd <- renderText({

        round(sd(sim()$data), 3)
    })

    output$n_display <- renderText({

        input$n_sim
    })

    # =====================================================
    # Histogram
    # =====================================================

    output$hist <- renderPlot({

        ggplot(
            sim()$table,
            aes(Goals, Frequency)
        ) +

            geom_col(
                fill = "#7B9ACC",
                alpha = 0.9
            ) +

            theme_minimal(base_size = 14) +

            labs(
                x = "Goals scored",
                y = "Frequency"
            )
    })

    # =====================================================
    # Head table
    # =====================================================

    output$head_table <- renderDT({

        datatable(

            data.frame(
                goals = head(sim()$data)
            ),

            options = list(
                dom = "t",
                pageLength = 6
            )
        )
    })

    # =====================================================
    # Frequency table
    # =====================================================

    output$freq_table <- renderDT({

        datatable(

            sim()$table,

            options = list(
                pageLength = 12
            )
        )
    })
    })
}
