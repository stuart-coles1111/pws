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




        selectInput(
            ns("toolkit_action"),
            "Analysis",
            choices = NULL
        ),




        conditionalPanel(

            condition = sprintf(
                "input['%s']=='Histogram'",
                ns("toolkit_action")
            ),


            sliderInput(
                ns("hist_bins"),
                "Histogram bins",
                1,
                10,
                5
            )

        ),




        conditionalPanel(

            condition = sprintf(
                "input['%s']=='csv' &&
                (input['%s']=='Histogram' ||
                 input['%s']=='Boxplot' ||
                 input['%s']=='Frequency table')",
                ns("data_source"),
                ns("toolkit_action"),
                ns("toolkit_action"),
                ns("toolkit_action")
            ),


            selectInput(
                ns("hist_col"),
                "Variable",
                choices = NULL
            )

        ),




        conditionalPanel(

            condition = sprintf(
                "input['%s']=='csv' &&
                input['%s']=='Scatterplot'",
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


        conditionalPanel(
            condition = sprintf(
                "input['%s']=='Summary statistics' ||
     input['%s']=='Mean' ||
     input['%s']=='Median' ||
     input['%s']=='SD' ||
     input['%s']=='Variance' ||
     input['%s']=='Min' ||
     input['%s']=='Max' ||
     input['%s']=='Frequency table' ||
     input['%s']=='Correlation'",
                ns("toolkit_action"),
                ns("toolkit_action"),
                ns("toolkit_action"),
                ns("toolkit_action"),
                ns("toolkit_action"),
                ns("toolkit_action"),
                ns("toolkit_action"),
                ns("toolkit_action"),
                ns("toolkit_action")
            ),

            uiOutput(
                ns("summary_table")
            )
        ),



        conditionalPanel(

            condition = sprintf(
                "input['%s']=='Histogram'",
                ns("toolkit_action")
            ),


            plotOutput(
                ns("tool_hist"),
                height = "400px"
            )

        ),



        conditionalPanel(

            condition = sprintf(
                "input['%s']=='Boxplot'",
                ns("toolkit_action")
            ),


            plotOutput(
                ns("tool_boxplot"),
                height = "400px"
            )

        ),



        conditionalPanel(

            condition = sprintf(
                "input['%s']=='Scatterplot'",
                ns("toolkit_action")
            ),


            plotOutput(
                ns("scatter"),
                height = "400px"
            ),

            conditionalPanel(

                condition = sprintf(
                    "input['%s']=='Scatterplot' && input['%s']==true",
                    ns("toolkit_action"),
                    ns("add_lm")
                ),

                tableOutput(
                    ns("regression_results")
                )

            )

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




        observe({

            if(input$data_source == "vector"){

                updateSelectInput(
                    session,
                    "toolkit_action",
                    choices = c(
                        "Summary statistics",
                        "Mean",
                        "Median",
                        "SD",
                        "Variance",
                        "Min",
                        "Max",
                        "Frequency table",
                        "Histogram",
                        "Boxplot"
                    )
                )

            } else {


                updateSelectInput(
                    session,
                    "toolkit_action",
                    choices = c(
                        "Summary statistics",
                        "Mean",
                        "Median",
                        "SD",
                        "Variance",
                        "Min",
                        "Max",
                        "Frequency table",
                        "Histogram",
                        "Boxplot",
                        "Scatterplot",
                        "Correlation"
                    )
                )

            }

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



            req(input$csv_file$datapath)


            df <- readr::read_csv(

                input$csv_file$datapath,

                show_col_types = FALSE

            )


            list(
                type="csv",
                data=df
            )

        })





        observe({

            dat <- toolkit_data()

            req(dat$type == "csv")

            df <- dat$data


            updateSelectInput(
                session,
                "x_col",
                choices = names(df)
            )


            updateSelectInput(
                session,
                "y_col",
                choices = names(df)
            )


            updateSelectInput(
                session,
                "hist_col",
                choices = names(df)
            )

        })


        output$summary_table_inner <- renderTable({

            dat <- toolkit_data()
            action <- input$toolkit_action

            # --------------------------------------------------
            # Frequency table
            # --------------------------------------------------
            if(action == "Frequency table") {

                x <-
                    if(dat$type == "vector") {
                        dat$data
                    } else {
                        req(input$hist_col)
                        dat$data[[input$hist_col]]
                    }

                return(as.data.frame(table(x)))
            }

            # --------------------------------------------------
            # Correlation matrix
            # --------------------------------------------------
            if(action == "Correlation") {

                req(dat$type == "csv")

                df <- dat$data
                numeric_cols <- sapply(df, is.numeric)
                df <- df[, numeric_cols, drop = FALSE]

                return(round(cor(df, use = "complete.obs"), 3))
            }

            # --------------------------------------------------
            # Summary statistics (full table version)
            # --------------------------------------------------
            if(action == "Summary statistics") {

                if(dat$type == "vector") {

                    x <- dat$data

                    return(
                        data.frame(
                            Variable = "x",
                            Count = length(x),
                            Mean = mean(x),
                            Median = median(x),
                            SD = sd(x),
                            Min = min(x),
                            Max = max(x)
                        )
                    )
                }

                df <- dat$data
                numeric_cols <- sapply(df, is.numeric)
                df <- df[, numeric_cols, drop = FALSE]

                results <- lapply(names(df), function(col) {

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

                do.call(rbind, results)
            }

            # --------------------------------------------------
            # Fallback (should rarely hit)
            # --------------------------------------------------
            data.frame(Message = "No table output for this selection")
        })

        output$summary_table <- renderUI({

            dat <- toolkit_data()
            action <- input$toolkit_action


            if(action %in% c("Mean","Median","SD","Variance","Min","Max")) {


                stat_fun <- switch(
                    action,
                    "Mean" = mean,
                    "Median" = median,
                    "SD" = sd,
                    "Variance" = var,
                    "Min" = min,
                    "Max" = max
                )


                make_box <- function(title, value){

                    value_box(
                        title = title,

                        value = formatC(
                            value,
                            digits = 3,
                            format = "f"
                        ),

                        theme = switch(
                            action,
                            "Mean" = "primary",
                            "Median" = "success",
                            "SD" = "warning",
                            "Variance" = "danger",
                            "Min" = "info",
                            "Max" = "secondary"
                        )
                        )

                }


                if(dat$type == "vector"){

                    return(
                        layout_columns(
                            make_box(
                                action,
                                stat_fun(dat$data)
                            )
                        )
                    )

                }


                df <- dat$data

                numeric_cols <- sapply(df, is.numeric)

                df <- df[, numeric_cols, drop = FALSE]


                boxes <- lapply(
                    names(df),
                    function(col){

                        make_box(
                            col,
                            stat_fun(
                                df[[col]],
                                na.rm = TRUE
                            )
                        )

                    }
                )


                return(
                    layout_columns(
                        !!!boxes
                    )
                )

            }


            tableOutput(session$ns("summary_table_inner"))

        })




        output$tool_hist <- renderPlot({


            dat <- toolkit_data()


            x <-

                if(dat$type=="vector"){

                    dat$data

                } else {

                    dat$data[[input$hist_col]]

                }



            hist(

                x,

                breaks=input$hist_bins,

                col="#7B9ACC",

                border="white",

                main="Histogram",

                xlab="Value"

            )

        })


        output$tool_boxplot <- renderPlot({

            dat <- toolkit_data()

            x <-

                if(dat$type == "vector") {

                    dat$data

                } else {

                    req(input$hist_col)

                    dat$data[[input$hist_col]]

                }


            boxplot(

                x,

                col = "#7B9ACC",

                border = "#2c3e50",

                whisklty = 1,

                staplewex = 0.5,

                main = "Boxplot",

                ylab = "Value"

            )

        })



        output$scatter <- renderPlot({

            dat <- toolkit_data()


            req(
                dat$type=="csv",
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

            req(
                input$add_lm,
                input$toolkit_action == "Scatterplot"
            )


            dat <- toolkit_data()


            x <- dat$data[[input$x_col]]

            y <- dat$data[[input$y_col]]


            model <- lm(
                y ~ x
            )


            coefs <- summary(model)$coefficients


            data.frame(

                Term = rownames(coefs),

                Estimate = round(
                    coefs[,1],
                    4
                ),

                "Standard Error" = round(
                    coefs[,2],
                    4
                ),

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


                "Summary statistics" = {

                    if (input$data_source == "vector") {

                        "summary(x)"

                    } else {

                        "summary(data)"

                    }

                },


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
                            input$hist_col,
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
                            input$hist_col,
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
                            input$hist_col,
                            ")"
                        )

                    }

                },


                "Correlation" = {

                    "cor(data, use = 'complete.obs')"

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
