stats_toolkit_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("Data Settings"),


        radioButtons(
            ns("data_source"),
            "Data source",
            choices = c(
                "Enter data (x)" = "vector",
                "Use mtcars example data" = "mtcars",
                "Upload CSV" = "csv"
            )
        ),

        conditionalPanel(

            condition = sprintf(
                "input['%s']=='mtcars'",
                ns("data_source")
            ),

            p(
                "Use the built-in mtcars dataset as an example of a CSV data file."
            ),

            downloadButton(
                ns("download_mtcars"),
                "Download mtcars CSV"
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
                    "Simulate data" = "simulate"
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
                "Number of Individuals",
                20,
                min = 1
            ),

            numericInput(
                ns("template_cols"),
                "Number of Variables",
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

        hr(),

        h4("Analysis Settings"),


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
                "input['%s']=='csv' || input['%s']=='mtcars'",
                ns("data_source"),
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
                "(input['%s']=='csv' || input['%s']=='mtcars') && input['%s'].indexOf('Scatterplot') > -1",
                ns("data_source"),
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

            sliderInput(
                ns("point_size"),
                "Point size",
                min = 0.5,
                max = 5,
                value = 1.5,
                step = 0.5
            ),


            checkboxInput(
                ns("add_lm"),
                "Add linear regression line",
                value = FALSE
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
                    "🧰 Statistics Toolkit",
                    style = "
                    font-size: 1.4rem;
                    font-weight: 700;
                    color: #2c3e50;
                "
                )
            ),

            p(
                strong("Main idea: "),
                "Explore data using basic numerical and graphical summaries. ",
                "Use the toolkit to investigate distributions, compare variables, ",
                "look for relationships, check calculations, and experiment with ",
                "different ways of summarising data."
            ),

            hr(),

            h5("Choose a data source"),

            tags$ul(

                tags$li(
                    strong("Enter data (x): "),
                    "enter your own numerical values manually, or simulate a ",
                    "numerical dataset using the simulation controls."
                ),

                tags$li(
                    strong("Use mtcars example data: "),
                    "use the built-in mtcars dataset immediately, without uploading ",
                    "a file. This is a small dataset containing several variables ",
                    "describing different car models and is useful for practising ",
                    "analyses involving more than one variable."
                ),

                tags$li(
                    strong("Upload CSV: "),
                    "download a CSV template if required, enter or edit your data, ",
                    "and then upload the completed file."
                )

            ),

            p(
                "When using mtcars or an uploaded CSV file, the available variables ",
                "are shown in the variable selectors in the Analysis Settings."
            ),

            hr(),

            h5("Explore the data"),

            p(
                "Select one or more options under ",
                strong("Display"),
                " to choose the analyses you want to see."
            ),

            tags$ul(

                tags$li(
                    strong("Summary statistics: "),
                    "choose a variable and display its mean, median, standard ",
                    "deviation, variance, minimum, and/or maximum."
                ),

                tags$li(
                    strong("Frequency table: "),
                    "display the observed frequencies of the values in a selected variable."
                ),

                tags$li(
                    strong("Histogram: "),
                    "display the distribution of a selected numerical variable and ",
                    "adjust the number of bins."
                ),

                tags$li(
                    strong("Boxplot: "),
                    "display a boxplot for a selected numerical variable."
                ),

                tags$li(
                    strong("Scatterplot: "),
                    "when using mtcars or an uploaded CSV file, select an X variable ",
                    "and a Y variable to investigate their relationship."
                ),

                tags$li(
                    strong("Regression line: "),
                    "when using a scatterplot, optionally add a fitted linear ",
                    "regression line and view the associated regression results."
                )

            ),

            hr(),

            h5("Things to observe"),

            tags$ul(

                tags$li(
                    "How much information is retained or lost when data are summarised?"
                ),

                tags$li(
                    "How do numerical and graphical summaries complement one another?"
                ),

                tags$li(
                    "What does a histogram or boxplot reveal about the distribution of a variable?"
                ),

                tags$li(
                    "What patterns or relationships can be seen in a scatterplot?"
                ),

                tags$li(
                    "Does a regression line provide a useful description of the relationship?"
                )

            ),

            hr(),

            h5("Using the R Code tab"),

            p(
                "The ",
                strong("R Code"),
                " tab shows an approximate version of the R code used to produce ",
                "the analyses displayed in the Explore tab. ",
                "You can use this code to see how the analysis could be carried out ",
                "directly in R and to help connect the interactive controls with the ",
                "underlying statistical commands."
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
                        "Is much detail lost when summarising data numerically and/or graphically?"
                    ),

                    tags$li(
                        "When are graphical summaries superior to numerical summaries?"
                    ),

                    tags$li(
                        "When are histograms preferable to boxplots?"
                    ),

                    tags$li(
                        "What can a scatterplot reveal that separate summaries of two variables cannot?"
                    ),

                    tags$li(
                        "Is it always appropriate to add a regression line to a scatterplot?"
                    ),

                    tags$li(
                        "How might your conclusions change when you explore a different variable or a different pair of variables?"
                    )

                )
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
        sim_details <- reactiveVal(NULL)

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



        output$download_mtcars <- downloadHandler(

            filename = function() {
                "mtcars.csv"
            },

            content = function(file) {

                mtcars_file <- system.file(
                    "extdata",
                    "mtcars.csv",
                    package = "pws"
                )

                validate(
                    need(
                        mtcars_file != "",
                        "mtcars.csv could not be found in the package."
                    )
                )

                file.copy(
                    mtcars_file,
                    file,
                    overwrite = TRUE
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

            if(input$data_source %in% c("csv", "mtcars")) {
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

            sim_details(
                list(
                    seed = current_seed,
                    n = input$sim_n,
                    lambda = lambda
                )
            )

            updateTextAreaInput(
                session,
                "vector_input",
                value = paste(
                    x,
                    collapse = ","
                )
            )

        })


        toolkit_data <- reactive({

            # ---------------------------
            # Vector mode
            # ---------------------------

            if (input$data_source == "vector") {

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
                        type = "vector",
                        data = x
                    )
                )
            }


            # ---------------------------
            # mtcars mode
            # ---------------------------

            if (input$data_source == "mtcars") {

                mtcars_file <- system.file(
                    "extdata",
                    "mtcars.csv",
                    package = "pws"
                )

                validate(
                    need(
                        mtcars_file != "",
                        "mtcars.csv could not be found."
                    )
                )

                df <- readr::read_csv(
                    mtcars_file,
                    show_col_types = FALSE
                )

                return(
                    list(
                        type = "dataframe",
                        data = df
                    )
                )
            }


            # ---------------------------
            # Uploaded CSV
            # ---------------------------

            req(input$data_source == "csv")
            req(input$csv_file)

            validate(
                need(
                    input$csv_file$datapath != "",
                    "Please upload a CSV file."
                )
            )

            df <- readr::read_csv(
                input$csv_file$datapath,
                show_col_types = FALSE
            )

            list(
                type = "dataframe",
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

            req(
                input$data_source %in% c("csv", "mtcars")
            )

            dat <- toolkit_data()

            req(!is.null(dat))

            req(
                dat$type == "dataframe"
            )

            df <- dat$data

            current_x <- isolate(input$x_col)
            current_y <- isolate(input$y_col)
            current_summary <- isolate(input$summary_col)

            updateSelectInput(
                session,
                "x_col",
                choices = names(df),
                selected = if(
                    current_x %in% names(df)
                ) {
                    current_x
                } else {
                    names(df)[1]
                }
            )

            updateSelectInput(
                session,
                "y_col",
                choices = names(df),
                selected = if(
                    current_y %in% names(df)
                ) {
                    current_y
                } else {
                    names(df)[min(2, ncol(df))]
                }
            )

            updateSelectInput(
                session,
                "summary_col",
                choices = names(df),
                selected = if(
                    current_summary %in% names(df)
                ) {
                    current_summary
                } else {
                    names(df)[1]
                }
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
                input$data_source %in% c("csv", "mtcars") &&
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

            req(
                input$data_source %in% c("csv", "mtcars")
            )

            req(
                input$x_col,
                input$y_col
            )

            x <- dat$data[[input$x_col]]
            y <- dat$data[[input$y_col]]

            plot(
                x,
                y,
                pch = 19,
                cex = input$point_size,
                col = "#CDB4DB",
                xlab = input$x_col,
                ylab = input$y_col,
                main = "Scatterplot"
            )

            if(input$add_lm) {

                model <- lm(y ~ x)

                abline(
                    model,
                    col = "#7B9ACC",
                    lwd = 3
                )
            }

        })


        output$regression_results <- renderTable({

            dat <- toolkit_data()
            req(
                input$data_source %in% c("csv", "mtcars")
            )

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

            req(input$data_source)

            actions <- input$toolkit_action

            if (is.null(actions)) {
                actions <- character(0)
            }


            # =========================================================
            # Data
            # =========================================================

            if (input$data_source == "vector") {

                if (input$vector_mode == "simulate") {

                    data_code <- paste(
                        "# Generate simulated data",
                        paste0(
                            "set.seed(",
                            input$seed,
                            ")"
                        ),
                        "",
                        paste0(
                            "nsim <- ",
                            input$sim_n
                        ),
                        "",
                        "lambda <- runif(1, 2, 20)",
                        "",
                        "x <- rpois(",
                        "    nsim,",
                        "    lambda",
                        ")",
                        sep = "\n"
                    )

                } else {

                    vals <- toolkit_data()$data

                    data_code <- paste0(
                        "x <- c(",
                        paste(
                            head(vals, 20),
                            collapse = ", "
                        ),
                        if (length(vals) > 20) ", ..." else "",
                        ")"
                    )

                }

            } else if (input$data_source == "mtcars") {

                data_code <- paste(
                    "# Load the mtcars example data",
                    'data <- read.csv("mtcars.csv")',
                    sep = "\n"
                )

            } else {

                file_name <- if (!is.null(input$csv_file)) {
                    input$csv_file$name
                } else {
                    "my_data.csv"
                }

                data_code <- paste(
                    "# Load uploaded CSV data",
                    paste0(
                        "data <- read.csv(",
                        shQuote(file_name),
                        ")"
                    ),
                    sep = "\n"
                )

            }


            # =========================================================
            # Analysis
            # =========================================================

            analysis_code <- character(0)


            # ---------------------------------------------------------
            # Summary statistics
            # ---------------------------------------------------------

            if ("Summary statistics" %in% actions) {

                stats <- input$summary_stats

                if (!is.null(stats)) {

                    if (input$data_source == "vector") {

                        if ("Mean" %in% stats)
                            analysis_code <- c(
                                analysis_code,
                                "mean(x)"
                            )

                        if ("Median" %in% stats)
                            analysis_code <- c(
                                analysis_code,
                                "median(x)"
                            )

                        if ("SD" %in% stats)
                            analysis_code <- c(
                                analysis_code,
                                "sd(x)"
                            )

                        if ("Variance" %in% stats)
                            analysis_code <- c(
                                analysis_code,
                                "var(x)"
                            )

                        if ("Min" %in% stats)
                            analysis_code <- c(
                                analysis_code,
                                "min(x)"
                            )

                        if ("Max" %in% stats)
                            analysis_code <- c(
                                analysis_code,
                                "max(x)"
                            )

                    } else {

                        varname <- input$summary_col

                        if (!is.null(varname) && nzchar(varname)) {

                            if ("Mean" %in% stats)
                                analysis_code <- c(
                                    analysis_code,
                                    paste0(
                                        "mean(data$`",
                                        varname,
                                        "`, na.rm = TRUE)"
                                    )
                                )

                            if ("Median" %in% stats)
                                analysis_code <- c(
                                    analysis_code,
                                    paste0(
                                        "median(data$`",
                                        varname,
                                        "`, na.rm = TRUE)"
                                    )
                                )

                            if ("SD" %in% stats)
                                analysis_code <- c(
                                    analysis_code,
                                    paste0(
                                        "sd(data$`",
                                        varname,
                                        "`, na.rm = TRUE)"
                                    )
                                )

                            if ("Variance" %in% stats)
                                analysis_code <- c(
                                    analysis_code,
                                    paste0(
                                        "var(data$`",
                                        varname,
                                        "`, na.rm = TRUE)"
                                    )
                                )

                            if ("Min" %in% stats)
                                analysis_code <- c(
                                    analysis_code,
                                    paste0(
                                        "min(data$`",
                                        varname,
                                        "`, na.rm = TRUE)"
                                    )
                                )

                            if ("Max" %in% stats)
                                analysis_code <- c(
                                    analysis_code,
                                    paste0(
                                        "max(data$`",
                                        varname,
                                        "`, na.rm = TRUE)"
                                    )
                                )

                        }

                    }

                }

            }


            # ---------------------------------------------------------
            # Frequency table
            # ---------------------------------------------------------

            if ("Frequency table" %in% actions) {

                if (input$data_source == "vector") {

                    analysis_code <- c(
                        analysis_code,
                        "table(x)"
                    )

                } else {

                    if (!is.null(input$summary_col)) {

                        analysis_code <- c(
                            analysis_code,
                            paste0(
                                "table(data$`",
                                input$summary_col,
                                "`)"
                            )
                        )

                    }

                }

            }


            # ---------------------------------------------------------
            # Histogram
            # ---------------------------------------------------------

            if ("Histogram" %in% actions) {

                if (input$data_source == "vector") {

                    analysis_code <- c(
                        analysis_code,
                        paste0(
                            "hist(x, breaks = ",
                            input$hist_bins,
                            ")"
                        )
                    )

                } else {

                    if (!is.null(input$summary_col)) {

                        analysis_code <- c(
                            analysis_code,
                            paste0(
                                "hist(data$`",
                                input$summary_col,
                                "`, breaks = ",
                                input$hist_bins,
                                ")"
                            )
                        )

                    }

                }

            }


            # ---------------------------------------------------------
            # Boxplot
            # ---------------------------------------------------------

            if ("Boxplot" %in% actions) {

                if (input$data_source == "vector") {

                    analysis_code <- c(
                        analysis_code,
                        "boxplot(x)"
                    )

                } else {

                    if (!is.null(input$summary_col)) {

                        analysis_code <- c(
                            analysis_code,
                            paste0(
                                "boxplot(data$`",
                                input$summary_col,
                                "`)"
                            )
                        )

                    }

                }

            }


            # ---------------------------------------------------------
            # Scatterplot
            # ---------------------------------------------------------

            if (
                input$data_source %in% c("csv", "mtcars") &&
                "Scatterplot" %in% actions
            ) {

                if (
                    !is.null(input$x_col) &&
                    !is.null(input$y_col)
                ) {

                    analysis_code <- c(
                        analysis_code,
                        paste0(
                            "plot(",
                            "data$`",
                            input$x_col,
                            "`, ",
                            "data$`",
                            input$y_col,
                            "`, ",
                            "pch = 19, ",
                            "cex = ",
                            input$point_size,
                            ")"
                        )
                    )


                    # Regression line

                    if (isTRUE(input$add_lm)) {

                        analysis_code <- c(
                            analysis_code,
                            paste0(
                                "model <- lm(",
                                "data$`",
                                input$y_col,
                                "` ~ data$`",
                                input$x_col,
                                "`",
                                ")"
                            ),
                            "abline(model)",
                            "summary(model)$coefficients"
                        )

                    }

                }

            }


            # =========================================================
            # Final output
            # =========================================================

            if (length(analysis_code) == 0) {

                paste(
                    "# Data",
                    data_code,
                    "",
                    "# Select an analysis to generate R code.",
                    sep = "\n"
                )

            } else {

                paste(
                    "# Data",
                    data_code,
                    "",
                    "# Analysis",
                    paste(
                        analysis_code,
                        collapse = "\n"
                    ),
                    sep = "\n"
                )

            }

        })

    })

}
