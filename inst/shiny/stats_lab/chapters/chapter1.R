# =========================================================
# UI
# =========================================================

chapter1_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        radioButtons(
            ns("mode"),
            "Investigation",
            choices = c(
                "Goal scoring simulation" = "simulation",
                "Simple statistics toolkit" = "toolkit"
            )
        ),


        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'simulation'",
                ns("mode")
            ),

            h4("Comparing Theoretical and Observed Frequencies"),

            sliderInput(
                ns("pois_mean"),
                "Average number of goal scoring opportunities",
                min = 1,
                max = 50,
                value = 25
            ),

            sliderInput(
                ns("mu"),
                "Average probability of goal conversion",
                min = 0,
                max = 1,
                value = 0.1,
                step = 0.01
            ),

            sliderInput(
                ns("phi"),
                "Shape",
                min = 0,
                max = 200,
                value = 10,
                step = 1
            ),

            numericInput(
                ns("seed"),
                "Random seed",
                value = sample(1:999,1)
            ),

            numericInput(
                ns("n_sim"),
                "Number of simulated games",
                value = 1000,
                min = 100
            ),

            actionButton(
                ns("run"),
                "Run simulation",
                class = "btn-primary"
            )
        ),


        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'toolkit'",
                ns("mode")
            ),

            h4("Simple Statistics Toolkit"),

            radioButtons(
                ns("data_source"),
                "Data source",
                choices = c(
                    "Enter vector" = "vector",
                    "Upload CSV" = "csv"
                )
            ),

            conditionalPanel(

                condition = sprintf(
                    "input['%s'] == 'vector'",
                    ns("data_source")
                ),

                textAreaInput(
                    ns("vector_input"),
                    "Data values",
                    value = "1,2,3,4,5",
                    rows = 4
                )

            ),

            conditionalPanel(

                condition = sprintf(
                    "input['%s'] == 'csv'",
                    ns("data_source")
                ),

                numericInput(
                    ns("template_rows"),
                    "Template rows",
                    value = 20,
                    min = 1
                ),

                numericInput(
                    ns("template_cols"),
                    "Template columns",
                    value = 3,
                    min = 1
                ),

                downloadButton(
                    ns("download_template"),
                    "Download CSV template"
                ),

                hr(),

                fileInput(
                    ns("csv_file"),
                    "Upload completed CSV"
                )

            ),

            selectInput(
                ns("toolkit_action"),
                "Analysis",
                choices = c(
                    "Summary statistics",
                    "Histogram",
                    "Scatterplot"
                )
            ),

            selectInput(
                ns("hist_col"),
                "Histogram variable",
                choices = NULL
            ),

            selectInput(
                ns("x_col"),
                "X column",
                choices = NULL
            ),

            selectInput(
                ns("y_col"),
                "Y column",
                choices = NULL
            )
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
                    "🎲 Understanding Random Goal Scoring",
                    style = "
                    font-size: 1.4rem;
                    font-weight: 700;
                    color: #2c3e50;
                "
                )
            ),

            p(
                strong("Main idea: "),
                "Although individual football matches are unpredictable,
             the pattern of goals across many matches follows
             predictable statistical rules."
            ),

            hr(),

            h5("What happens in this model?"),

            tags$div(
                style = "margin-left: 10px;",

                p("① A game contains a random number of scoring opportunities."),

                p("② Each opportunity has some chance of becoming a goal."),

                p("③ That conversion probability varies from game to game."),

                p("④ The total number of goals scored is recorded.")
            ),

            hr(),

            h5("Your job"),

            p(
                "Use the controls to create your own goal-scoring model by choosing:"
            ),

            tags$ul(
                tags$li("The average number of opportunities per game"),
                tags$li("The average probability of converting an opportunity"),
                tags$li("How much the conversion probability varies"),
                tags$li("How many games to simulate")
            ),

            hr(),

            h5("What will you see?"),

            tags$ul(
                tags$li(
                    "The distributions used to generate opportunities and scoring probabilities"
                ),
                tags$li(
                    "The theoretical distribution of goals implied by your choices"
                ),
                tags$li(
                    "The observed results from simulated matches"
                ),
                tags$li(
                    "A comparison between theory and simulation"
                )
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
                        "How do the parameter values affect the shape of the goal distribution?"
                    ),
                    tags$li(
                        "How closely do simulated results match theoretical expectations?"
                    ),
                    tags$li(
                        "What changes when the number of simulated games increases?"
                    ),
                    tags$li(
                        "Which inputs have the strongest influence on scoring outcomes?"
                    )
                )
            )
        )
    )

    code_panel <- div(
        card(
            card_header("Generated R Code"),
            tags$pre(textOutput(ns("generated_code")))
        )
    )


    results_panel <- uiOutput(ns("results_panel"))

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

                h5("1. Random outcomes can still be predictable"),

                p(
                    "The number of goals in any single match is uncertain,
                 but the pattern across many matches follows a
                 predictable probability distribution."
                ),

                hr(),

                h5("2. Randomness occurs at more than one level"),

                p(
                    "Some games generate many opportunities and some generate few.
                 Even when an opportunity occurs, whether it becomes a goal
                 is still uncertain."
                ),

                hr(),

                h5("3. Assumptions shape outcomes"),

                p(
                    "Changing the average number of opportunities or the average
                 conversion probability changes the distribution of goals.
                 Different assumptions lead to different predictions."
                ),

                hr(),

                h5("4. Simulation reveals theory"),

                p(
                    "As the number of simulated games increases, the observed
                 frequencies move closer to the theoretical frequencies.
                 Random variation becomes less noticeable."
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
                        strong("Statistical models do not predict individual games."),
                        br(),
                        "They describe the long-run patterns produced by many
                     games and help us understand how randomness generates
                     observable outcomes."
                    )
                )
            )
        )
    )


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
# SERVER
# =========================================================

chapter1_server <- function(id){

    moduleServer(id, function(input, output, session){

        output$download_template <- downloadHandler(

            filename = function(){

                paste0(
                    "statistics_template_",
                    Sys.Date(),
                    ".csv"
                )

            },

            content = function(file){


                template <- as.data.frame(
                    matrix(
                        "",
                        nrow = input$template_rows,
                        ncol = input$template_cols
                    )
                )


                names(template) <- paste0(
                    "Variable_",
                    seq_len(input$template_cols)
                )


                write.csv(
                    template,
                    file,
                    row.names = FALSE
                )

            }
        )

        # -------------------------------------------------
        # Store snapshot of last simulation inputs
        # -------------------------------------------------

        last_run_inputs <- reactiveVal(NULL)

        # NEW: store simulation output explicitly
        sim_data <- reactiveVal(NULL)

        toolkit_data <- reactive({

            req(input$mode == "toolkit")


            if(input$data_source == "vector"){


                x <- as.numeric(
                    trimws(
                        unlist(
                            strsplit(
                                input$vector_input,
                                ","
                            )
                        )
                    )
                )


                validate(
                    need(
                        all(!is.na(x)),
                        "Vector must contain only numbers."
                    )
                )


                return(
                    list(
                        type="vector",
                        data=x
                    )
                )

            }


            req(input$csv_file)


            df <- readr::read_csv(
                input$csv_file$datapath,
                col_types = readr::cols(.default = readr::col_double()),
                show_col_types = FALSE
            )

            list(
                type="csv",
                data=df
            )

        })

        observeEvent(toolkit_data(), {

            req(input$data_source == "csv")

            dat <- toolkit_data()$data

            updateSelectInput(
                session,
                "x_col",
                choices = names(dat)
            )

            updateSelectInput(
                session,
                "y_col",
                choices = names(dat)
            )

        })

        observeEvent(toolkit_data(), {

            dat <- toolkit_data()

            if(dat$type == "csv"){

                updateSelectInput(
                    session,
                    "hist_col",
                    choices = names(dat$data)
                )

            }

        })

        output$summary_table <- renderTable({

            dat <- toolkit_data()

            if(dat$type == "vector"){

                x <- dat$data

                return(data.frame(
                    Variable = "Vector",
                    Count = length(x),
                    Mean = mean(x),
                    Median = median(x),
                    SD = sd(x),
                    Min = min(x),
                    Max = max(x)
                ))
            }

            df <- dat$data

            numeric_cols <- sapply(df, is.numeric)

            df <- df[, numeric_cols, drop = FALSE]

            validate(
                need(ncol(df) > 0, "No numeric columns found in CSV.")
            )

            stats <- lapply(names(df), function(col){

                x <- df[[col]]

                data.frame(
                    Variable = col,
                    Count = length(x),
                    Mean = mean(x, na.rm = TRUE),
                    Median = median(x, na.rm = TRUE),
                    SD = sd(x, na.rm = TRUE),
                    Min = min(x, na.rm = TRUE),
                    Max = max(x, na.rm = TRUE)
                )
            })

            do.call(rbind, stats)
        })

        output$tool_hist <- renderPlot({

            dat <- toolkit_data()


            if(dat$type == "vector"){

                x <- dat$data

            } else {

                x <- dat$data[[input$hist_col]]

            }


            hist(
                x,
                col = "#7B9ACC",
                border = "white",
                main = "Histogram",
                xlab = "Value"
            )

        })

        output$scatter <- renderPlot({

            dat <- toolkit_data()

            validate(
                need(
                    dat$type == "csv",
                    "Scatterplots require uploaded CSV data with multiple variables."
                )
            )

            req(input$x_col, input$y_col)

            df <- dat$data

            x <- df[[input$x_col]]
            y <- df[[input$y_col]]

            plot(
                x,
                y,
                pch = 19,
                col = "#CDB4DB",
                xlab = input$x_col,
                ylab = input$y_col,
                main = "Scatterplot"
            )

        })

        # -------------------------------------------------
        # Run simulation (triggered only on button press)
        # -------------------------------------------------

        observeEvent(input$run, {


            last_run_inputs(list(
                n_sim = input$n_sim,
                pois_mean = input$pois_mean,
                mu = input$mu,
                phi = input$phi
            ))

            sim_data(
                pws::goals_sim(
                    n_sim = input$n_sim,
                    pois_mean = input$pois_mean,
                    mu = input$mu,
                    phi = input$phi,
                    seed = input$seed
                )
            )
        })


        output$results_panel <- renderUI({

            if (input$mode == "simulation") {

                div(

                    card(
                        card_header("Goal scoring specification"),
                        plotOutput(session$ns("components"), height = 300)
                    ),

                    card(
                        card_header("Distribution of goals"),
                        plotOutput(session$ns("hist"), height = 400)
                    )
                )

            }else {

                div(

                    card(
                        card_header("Toolkit output"),

                        conditionalPanel(
                            condition = sprintf(
                                "input['%s'] == 'Summary statistics'",
                                session$ns("toolkit_action")
                            ),
                            tableOutput(session$ns("summary_table"))
                        ),

                        conditionalPanel(
                            condition = sprintf(
                                "input['%s'] == 'Histogram'",
                                session$ns("toolkit_action")
                            ),
                            plotOutput(session$ns("tool_hist"))
                        ),

                        conditionalPanel(
                            condition = sprintf(
                                "input['%s'] == 'Scatterplot'",
                                session$ns("toolkit_action")
                            ),
                            plotOutput(session$ns("scatter"))
                        )
                    )
                )
            }
        })

        # -------------------------------------------------
        # Code display
        # -------------------------------------------------

        output$generated_code <- renderText({

            if(input$mode == "simulation"){

                paste0(
                    "goals_sim(\n",
                    "  n_sim = ", input$n_sim,",\n",
                    "  pois_mean = ", input$pois_mean,",\n",
                    "  mu = ", input$mu,",\n",
                    "  phi = ", input$phi,",\n",
                    "  seed = ", input$seed,"\n",
                    ")"
                )

            } else {

                if(input$toolkit_action == "Summary statistics"){

                    paste0(
                        "# Work with your dataset\n",
                        "data <- your_dataset\n\n",
                        "# Summary statistics for all variables\n",
                        "summary(data)"
                    )

                } else if(input$toolkit_action == "Histogram"){

                    paste0(
                        "# Work with your dataset\n",
                        "data <- your_dataset\n\n",
                        "# Histogram of a variable\n",
                        "hist(data$", input$hist_col,
                        ",\n     col = 'steelblue')"
                    )

                } else {

                    paste0(
                        "# Work with your dataset\n",
                        "data <- your_dataset\n\n",
                        "# Scatterplot of two variables\n",
                        "plot(data$", input$x_col,
                        ", data$", input$y_col,
                        ",\n     pch = 19,\n     col = '#CDB4DB')"
                    )
                }
            }
        })

        # -------------------------------------------------
        # Components plot
        # -------------------------------------------------

        output$components <- renderPlot({

            phi <- pmax(input$phi, 0.01)

            beta_1 <- input$mu * input$phi
            beta_2 <- (1 - input$mu) * input$phi

            max_n <- qpois(0.999, input$pois_mean)

            pois_df <- data.frame(
                Opportunities = 0:max_n,
                Probability = dpois(0:max_n, lambda = input$pois_mean)
            )

            p1 <- ggplot(pois_df,
                         aes(x = factor(Opportunities),
                             y = Probability)) +
                geom_col(fill = "#7B9ACC") +
                theme_minimal(base_size = 12) +
                labs(title = "Number of chances",
                     x = "Opportunities",
                     y = "Probability")

            p_grid <- seq(0, 1, length.out = 500)

            beta_df <- data.frame(
                p = p_grid,
                Density = dbeta(p_grid, beta_1, beta_2)
            )

            p2 <- ggplot(beta_df,
                         aes(x = p,
                             y = Density)) +
                geom_line(colour = "#CDB4DB", linewidth = 1.2) +
                geom_vline(xintercept = input$mu,
                           linetype = 2,
                           colour = "grey40") +
                theme_minimal(base_size = 12) +
                labs(title = "Conversion probability",
                     x = "p",
                     y = "Density")

            p1 + p2
        })

        # -------------------------------------------------
        # MAIN PLOT
        # -------------------------------------------------

        output$hist <- renderPlot({

            # -----------------------------
            # THEORETICAL (always shown)
            # -----------------------------

            lambda <- input$pois_mean * input$mu
            phi <- pmax(input$phi, 0.01)

            beta_1 <- input$mu * phi
            beta_2 <- (1 - input$mu) * phi

            goals <- 0:qpois(0.999, lambda)

            probs <- dpois(
                goals,
                lambda = input$pois_mean * beta_1 / (beta_1 + beta_2)
            )

            probs <- pmax(probs, 0)
            probs <- probs / sum(probs)

            theoretical_df <- data.frame(
                Goals = goals,
                Frequency = input$n_sim * probs,
                Type = "Theoretical"
            )

            plot_df <- theoretical_df

            # -----------------------------
            # OBSERVED (only after run)
            # -----------------------------

            sim_out <- sim_data()

            show_observed <- FALSE

            if (!is.null(sim_out) && !is.null(last_run_inputs())) {

                prev <- last_run_inputs()

                show_observed <-
                    identical(prev$n_sim, input$n_sim) &&
                    identical(prev$pois_mean, input$pois_mean) &&
                    identical(prev$mu, input$mu) &&
                    identical(prev$phi, input$phi)
            }

            if (show_observed) {

                observed <- sim_out$table

                observed_df <- data.frame(
                    Goals = observed$Goals,
                    Frequency = observed$Frequency,
                    Type = "Observed"
                )

                plot_df <- rbind(observed_df, theoretical_df)
            }

            plot_df$Goals <- factor(
                plot_df$Goals,
                levels = sort(unique(plot_df$Goals))
            )

            ggplot(plot_df,
                   aes(x = Goals,
                       y = Frequency,
                       fill = Type)) +
                geom_col(position = position_dodge(width = 0.9)) +
                scale_fill_manual(values = c(
                    Observed = "#7B9ACC",
                    Theoretical = "#CDB4DB"
                )) +
                theme_minimal(base_size = 14) +
                labs(x = "Goals scored",
                     y = "Frequency",
                     fill = NULL)
        })
    })
}
