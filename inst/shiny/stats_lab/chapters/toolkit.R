stats_toolkit_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("Simple Statistics Toolkit"),


        radioButtons(
            ns("data_source"),
            "Data source",
            choices = c(
                "Enter data (x)" = "vector",
                "Upload CSV" = "csv"
            )
        ),



        conditionalPanel(

            condition = sprintf(
                "input['%s']=='vector'",
                ns("data_source")
            ),


            radioButtons(
                ns("vector_mode"),
                "Vector source",
                choices = c(
                    "Manual entry" = "manual",
                    "Simulate Poisson data" = "simulate"
                )
            ),


            conditionalPanel(

                condition = sprintf(
                    "input['%s']=='manual'",
                    ns("vector_mode")
                ),


                textAreaInput(
                    ns("vector_input"),
                    "Data values (x)",
                    value = paste(
                        rpois(100,2.5),
                        collapse=","
                    ),
                    rows = 6
                )

            ),


            conditionalPanel(

                condition = sprintf(
                    "input['%s']=='simulate'",
                    ns("vector_mode")
                ),


                numericInput(
                    ns("seed"),
                    "Random seed",
                    sample(1:999,1),
                    min = 1,
                    max = 999
                ),


                numericInput(
                    ns("sim_n"),
                    "Number of observations",
                    100,
                    min = 1
                ),


                actionButton(
                    ns("simulate_data"),
                    "Generate data"
                )

            )

        ),




        conditionalPanel(

            condition = sprintf(
                "input['%s']=='csv'",
                ns("data_source")
            ),


            numericInput(
                ns("template_rows"),
                "Template rows",
                20,
                min = 1
            ),


            numericInput(
                ns("template_cols"),
                "Template columns",
                3,
                min = 1
            ),


            downloadButton(
                ns("download_template"),
                "Download CSV template"
            ),


            fileInput(
                ns("csv_file"),
                "Upload completed CSV",
                accept = ".csv"
            )

        ),




        checkboxGroupInput(
            ns("toolkit_action"),
            "Display",
            choices = NULL
        ),

        conditionalPanel(

            condition = sprintf(
                "input['%s'].indexOf('Summary statistics') > -1",
                ns("toolkit_action")
            ),

            checkboxGroupInput(
                ns("summary_stats"),
                "Statistics",
                choices = c(
                    "Mean",
                    "Median",
                    "SD",
                    "Variance",
                    "Min",
                    "Max"
                ),
                selected = c(
                    "Mean",
                    "Median",
                    "SD"
                )
            )

        ),


        conditionalPanel(

            condition = sprintf(
                "input['%s']=='csv'",
                ns("data_source")
            ),

            selectInput(
                ns("summary_col"),
                "Variable",
                choices = NULL
            )

        ),

        conditionalPanel(

            condition = sprintf(
                "input['%s'].indexOf('Histogram') > -1",
                ns("toolkit_action")
            ),

            sliderInput(
                ns("hist_bins"),
                "Histogram bins",
                min = 1,
                max = 50,
                value = 10
            )
        ),


        conditionalPanel(

            condition = sprintf(
                "input['%s']=='csv' && input['%s'].indexOf('Scatterplot') > -1",
                ns("data_source"),
                ns("toolkit_action")
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
            ),


            checkboxInput(
                ns("add_lm"),
                "Add linear regression line",
                value = FALSE
            )

        )

    )





    overview_panel <- card(

        card_header(
            "Simple Statistics Toolkit"
        ),


        p(
            "Explore basic statistical summaries and visualisations from either manually entered data or uploaded CSV files."
        ),


        tags$ul(

            tags$li(
                "Calculate descriptive statistics"
            ),

            tags$li(
                "Create histograms"
            ),

            tags$li(
                "Explore relationships using scatterplots"
            )

        )

    )





    results_panel <- card(

        card_header(
            "Data Exploration"
        ),

        uiOutput(
            ns("combined_results")
        )

    )





    code_panel <- card(

        card_header(
            "Generated R Code"
        ),


        tags$pre(

            textOutput(
                ns("generated_code")
            )

        )

    )





    learn_panel <- card(

        card_header(
            "What should you have learned?"
        ),


        p(
            "Summary statistics describe the centre and spread of data."
        ),


        p(
            "Graphs help reveal patterns that are not obvious from numbers alone."
        )

    )





    chapter_page_ui(

        id = id,

        title = "🧰 Statistics Toolkit",

        sidebar = sidebar_controls,

        overview = overview_panel,

        results = results_panel,

        code = code_panel,

        learn = learn_panel

    )

}

stats_toolkit_server<-function(id){



    moduleServer(id, function(input, output, session){

        ns <- session$ns

        session$onSessionEnded(function() {
            graphics.off()
        })

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


                names(template) <-
                    paste0(
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

        observeEvent(input$data_source, {

            base_choices <- c(
                "Summary statistics",
                "Frequency table",
                "Histogram",
                "Boxplot"
            )

            if(input$data_source == "csv") {
                base_choices <- c(
                    base_choices,
                    "Scatterplot"
                )
            }

            updateCheckboxGroupInput(
                session,
                "toolkit_action",
                choices = base_choices,
                selected = intersect(
                    isolate(input$toolkit_action),
                    base_choices
                )
            )

        })

        observeEvent(input$simulate_data, {

            current_seed <- input$seed

            set.seed(current_seed)

            lambda <- runif(1, 2, 20)

            x <- rpois(
                input$sim_n,
                lambda
            )

            updateTextAreaInput(
                session,
                "vector_input",
                value = paste(
                    x,
                    collapse = ","
                )
            )

            updateNumericInput(
                session,
                "seed",
                value = sample(1:999, 1)
            )

        })


        toolkit_data <- reactive({

            if(input$data_source == "vector"){

                x <- as.numeric(
                    trimws(
                        unlist(strsplit(input$vector_input, ","))
                    )
                )

                validate(
                    need(all(!is.na(x)), "Vector must contain only numbers.")
                )

                return(list(
                    type = "vector",
                    data = x
                ))
            }

            # CSV mode
            req(input$data_source == "csv")
            req(input$csv_file)

            if (is.null(input$csv_file$datapath) || input$csv_file$datapath == "") {
                return(NULL)
            }

            df <- readr::read_csv(
                input$csv_file$datapath,
                show_col_types = FALSE
            )

            list(
                type = "csv",
                data = df
            )
        })

        observeEvent(input$data_source, {

            if(input$data_source == "vector") {

                updateSelectInput(session, "x_col", choices = character(0))
                updateSelectInput(session, "y_col", choices = character(0))

            }

        })


        observe({

            req(input$data_source == "csv")

            dat <- toolkit_data()
            req(!is.null(dat))
            req(dat$type == "csv")

            df <- dat$data

            current_x <- isolate(input$x_col)
            current_y <- isolate(input$y_col)
            current_summary <- isolate(input$summary_col)

            updateSelectInput(
                session,
                "x_col",
                choices = names(df),
                selected = if(current_x %in% names(df)) current_x else names(df)[1]
            )

            updateSelectInput(
                session,
                "y_col",
                choices = names(df),
                selected = if(current_y %in% names(df)) current_y else names(df)[min(2, ncol(df))]
            )

            updateSelectInput(
                session,
                "summary_col",
                choices = names(df),
                selected = if(current_summary %in% names(df)) current_summary else names(df)[1]
            )

        })

        output$combined_results <- renderUI({

            req(input$toolkit_action)

            displays <- input$toolkit_action

            numeric_panels <- list()
            graphic_panels <- list()

            # ---------------------------
            # Summary statistics
            # ---------------------------
            if("Summary statistics" %in% displays){

                numeric_panels <- append(
                    numeric_panels,
                    list(
                        card(
                            card_header("Summary Statistics"),
                            uiOutput(ns("summary_table"))
                        )
                    )
                )
            }

            # ---------------------------
            # Frequency table
            # ---------------------------
            if("Frequency table" %in% displays){

                numeric_panels <- append(
                    numeric_panels,
                    list(
                        card(
                            card_header("Frequency Table"),
                            DT::dataTableOutput(
                                ns("summary_table_inner")
                            )
                        )
                    )
                )
            }

            # ---------------------------
            # Histogram
            # ---------------------------
            if("Histogram" %in% displays){

                graphic_panels <- append(
                    graphic_panels,
                    list(
                        card(
                            card_header("Histogram"),
                            plotOutput(
                                ns("tool_hist"),
                                height = "400px"
                            )
                        )
                    )
                )
            }

            # ---------------------------
            # Boxplot
            # ---------------------------
            if("Boxplot" %in% displays){

                graphic_panels <- append(
                    graphic_panels,
                    list(
                        card(
                            card_header("Boxplot"),
                            plotOutput(
                                ns("tool_boxplot"),
                                height = "400px"
                            )
                        )
                    )
                )
            }

            # ---------------------------
            # Scatterplot
            # ---------------------------
            if(
                input$data_source == "csv" &&
                "Scatterplot" %in% displays
            ){

                graphic_panels <- append(
                    graphic_panels,
                    list(
                        card(
                            card_header("Scatterplot"),
                            plotOutput(
                                ns("scatter"),
                                height = "400px"
                            )
                        )
                    )
                )

                numeric_panels <- append(
                    numeric_panels,
                    list(
                        card(
                            card_header("Regression Results"),
                            tableOutput(
                                ns("regression_results")
                            )
                        )
                    )
                )
            }

            # ---------------------------
            # Layout logic
            # ---------------------------

            if(
                length(numeric_panels) > 0 &&
                length(graphic_panels) > 0
            ){

                layout_columns(

                    col_widths = c(6, 6),

                    tagList(numeric_panels),

                    tagList(graphic_panels)

                )

            } else if(length(numeric_panels) > 0){

                tagList(numeric_panels)

            } else if(length(graphic_panels) > 0){

                tagList(graphic_panels)

            } else {

                p("Select an analysis to display.")

            }

        })
        output$summary_table_inner <- DT::renderDataTable({

            dat <- toolkit_data()
            action <- input$toolkit_action

            # ---------------------------
            # Frequency table (IMPROVED)
            # ---------------------------
            if("Frequency table" %in% input$toolkit_action){
                x <-
                    if(dat$type == "vector") {
                        dat$data
                    } else {
                        req(input$summary_col)
                        dat$data[[input$summary_col]]
                    }

                freq <- as.data.frame(table(x))
                names(freq) <- c("Value", "Count")

                return(
                    DT::datatable(
                        freq,
                        options = list(
                            pageLength = 10,
                            dom = "t",
                            ordering = TRUE
                        ),
                        rownames = FALSE
                    )
                )
            }

            # ---------------------------
            # fallback
            # ---------------------------
            data.frame(Message = "Select Frequency table")

        })
        output$summary_table <- renderUI({

            req(input$summary_stats)

            stats_selected <- input$summary_stats

            dat <- toolkit_data()

            if(dat$type == "vector"){

                x <- dat$data

            } else {

                req(input$summary_col)

                x <- dat$data[[input$summary_col]]

            }

            stat_values <- list(

                Mean = mean(
                    x,
                    na.rm = TRUE
                ),

                Median = median(
                    x,
                    na.rm = TRUE
                ),

                SD = sd(
                    x,
                    na.rm = TRUE
                ),

                Variance = var(
                    x,
                    na.rm = TRUE
                ),

                Min = min(
                    x,
                    na.rm = TRUE
                ),

                Max = max(
                    x,
                    na.rm = TRUE
                )

            )

            boxes <- lapply(

                stats_selected,

                function(stat){

                    div(
                        style = "font-size: 0.85em;",
                        value_box(

                            title = stat,

                            value = tags$div(
                                style = "font-size: 1.2em; font-weight: 600;",
                                formatC(
                                    signif(stat_values[[stat]], 3),
                                    format = "fg"
                                )
                            )

                        )

                    )

                }

            )

            layout_columns(
                col_widths = c(4, 4, 4),
                !!!boxes
            )

        })




        output$tool_hist <- renderPlot({

            dat <- toolkit_data()

            x <- if(dat$type == "vector") {
                dat$data
            } else {
                req(input$summary_col)
                dat$data[[input$summary_col]]
            }

            validate(need(length(x) > 0, "No data"))

            hist(
                x,
                breaks = input$hist_bins,
                col = "#7B9ACC",
                border = "white",
                main = "Histogram",
                xlab = "Value"
            )
        })

        output$tool_boxplot <- renderPlot({

            dat <- toolkit_data()

            x <- if(dat$type == "vector") {
                dat$data
            } else {
                req(input$summary_col)
                dat$data[[input$summary_col]]
            }

            validate(
                need(is.numeric(x), "Boxplot requires numeric data")
            )

            boxplot(
                x,
                col = "#7B9ACC",
                border = "#2c3e50",
                main = "Boxplot",
                ylab = "Value"
            )
        })

        output$scatter <- renderPlot({

            dat <- toolkit_data()
            req(input$data_source == "csv")
            req(!is.null(input$csv_file))
            req(input$csv_file$datapath != "")

            req(
                input$x_col,
                input$y_col
            )


            x <- dat$data[[input$x_col]]

            y <- dat$data[[input$y_col]]


            plot(

                x,

                y,

                pch=19,

                col="#CDB4DB",

                xlab=input$x_col,

                ylab=input$y_col,

                main="Scatterplot"

            )


            if(input$add_lm) {


                model <- lm(
                    y ~ x
                )


                abline(
                    model,
                    col="#7B9ACC",
                    lwd=3
                )



            }


        })



        output$regression_results <- renderTable({

            dat <- toolkit_data()
            req(input$data_source == "csv")
            req(!is.null(input$csv_file))
            req(input$csv_file$datapath != "")

            req(
                input$add_lm,
                "Scatterplot" %in% input$toolkit_action
            )

            x <- dat$data[[input$x_col]]
            y <- dat$data[[input$y_col]]

            model <- lm(y ~ x)

            coefs <- summary(model)$coefficients

            data.frame(
                Term = rownames(coefs),
                Estimate = round(coefs[,1], 4),
                "Standard Error" = round(coefs[,2], 4),
                row.names = NULL
            )

        })

        output$generated_code <- renderText({

            if (input$data_source == "vector") {

                vals <- toolkit_data()$data

                data_code <- paste0(
                    "x <- c(",
                    paste(head(vals, 20), collapse = ", "),
                    if(length(vals) > 20) ", ..." else "",
                    ")"
                )

            } else {

                data_code <- "data <- read.csv('my_data.csv')"

            }


            action <- input$toolkit_action


            analysis_code <- switch(

                action,


                "Mean" = {

                    if(input$data_source == "vector") {

                        "mean(x)"

                    } else {

                        "sapply(data, mean, na.rm = TRUE)"

                    }

                },


                "Median" = {

                    if(input$data_source == "vector") {

                        "median(x)"

                    } else {

                        "sapply(data, median, na.rm = TRUE)"

                    }

                },


                "SD" = {

                    if(input$data_source == "vector") {

                        "sd(x)"

                    } else {

                        "sapply(data, sd, na.rm = TRUE)"

                    }

                },


                "Variance" = {

                    if(input$data_source == "vector") {

                        "var(x)"

                    } else {

                        "sapply(data, var, na.rm = TRUE)"

                    }

                },


                "Min" = {

                    if(input$data_source == "vector") {

                        "min(x)"

                    } else {

                        "sapply(data, min, na.rm = TRUE)"

                    }

                },


                "Max" = {

                    if(input$data_source == "vector") {

                        "max(x)"

                    } else {

                        "sapply(data, max, na.rm = TRUE)"

                    }

                },


                "Frequency table" = {

                    if(input$data_source == "vector") {

                        "table(x)"

                    } else {

                        paste0(
                            "table(data$",
                            input$summary_col,
                            ")"
                        )

                    }

                },


                "Histogram" = {

                    if(input$data_source == "vector") {

                        paste0(
                            "hist(x, breaks = ",
                            input$hist_bins,
                            ")"
                        )

                    } else {

                        paste0(
                            "hist(data$",
                            input$summary_col,
                            ", breaks = ",
                            input$hist_bins,
                            ")"
                        )

                    }

                },


                "Boxplot" = {

                    if(input$data_source == "vector") {

                        "boxplot(x)"

                    } else {

                        paste0(
                            "boxplot(data$",
                            input$summary_col,
                            ")"
                        )

                    }

                },


                "Scatterplot" = {

                    code <- paste0(
                        "plot(data$",
                        input$x_col,
                        ", data$",
                        input$y_col,
                        ")"
                    )


                    if(input$add_lm) {

                        code <- paste0(
                            code,
                            "\n",
                            "model <- lm(data$",
                            input$y_col,
                            " ~ data$",
                            input$x_col,
                            ")",
                            "\n",
                            "abline(model)",
                            "\n",
                            "summary(model)$coefficients"
                        )

                    }


                    code

                }

            )


            paste(
                "# Data",
                data_code,
                "",
                "# Analysis",
                analysis_code,
                sep = "\n"
            )

        })

    })

}
