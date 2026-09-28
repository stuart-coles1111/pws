# =========================================================
# STATISTICS TOOLKIT
# =========================================================


stats_toolkit_ui <- function(id) {

    ns <- NS(id)


    # =========================================================
    # SIDEBAR
    # =========================================================

    sidebar_controls <- sidebar(

        h4("Data Settings"),


        # ---------------------------------------------------------
        # Data source
        # ---------------------------------------------------------

        radioButtons(
            ns("data_source"),
            "Data source",
            choices = c(
                "Enter data (x)" = "vector",
                "Use sports example data" = "sports",
                "Use mtcars example data" = "mtcars",
                "Upload CSV" = "csv"
            ),
            selected = "vector"
        ),


        # ---------------------------------------------------------
        # mtcars download
        # ---------------------------------------------------------

        conditionalPanel(

            condition = sprintf(
                "input['%s']=='mtcars'",
                ns("data_source")
            ),

            p(
                "Use the built-in mtcars dataset as an example ",
                "of a CSV data file."
            ),

            downloadButton(
                ns("download_mtcars"),
                "Download mtcars CSV"
            )

        ),


        # ---------------------------------------------------------
        # Sports download
        # ---------------------------------------------------------

        conditionalPanel(

            condition = sprintf(
                "input['%s']=='sports'",
                ns("data_source")
            ),

            p(
                "Use the built-in sports dataset as an example ",
                "of a CSV data file."
            ),

            downloadButton(
                ns("download_sports"),
                "Download sports CSV"
            )

        ),


        # ---------------------------------------------------------
        # Vector input
        # ---------------------------------------------------------

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
                ),
                selected = "manual"
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
                        rpois(100, 2.5),
                        collapse = ","
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
                    sample(1:999, 1),
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


        # ---------------------------------------------------------
        # CSV input
        # ---------------------------------------------------------

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


        # ---------------------------------------------------------
        # Subsetting
        # ---------------------------------------------------------

        conditionalPanel(

            condition = sprintf(
                "input['%s'] != 'vector'",
                ns("data_source")
            ),

            h5("Subset data"),

            selectInput(
                ns("subset_col"),
                "Categorical variable",
                choices = c("None" = ""),
                selected = ""
            ),

            selectInput(
                ns("subset_value"),
                "Subset category",
                choices = c("All" = ""),
                selected = ""
            )

        ),


        hr(),


        h4("Analysis Settings"),


        # ---------------------------------------------------------
        # Numerical variable
        # ---------------------------------------------------------

        conditionalPanel(

            condition = sprintf(
                "input['%s'] != 'vector'",
                ns("data_source")
            ),

            selectInput(
                ns("summary_col"),
                "Numerical variable",
                choices = NULL
            )

        ),


        # ---------------------------------------------------------
        # Descriptive analyses
        # ---------------------------------------------------------

        h5("Descriptive analyses"),

        checkboxGroupInput(
            ns("toolkit_action"),
            "Display",
            choices = c(
                "Summary statistics",
                "Graphical display"
            ),
            selected = "Summary statistics"
        ),


        # ---------------------------------------------------------
        # Graph type
        #
        # Appears immediately below the Display checkboxes
        # when Graphical display is selected.
        # ---------------------------------------------------------

        conditionalPanel(

            condition = sprintf(
                "input['%s'].indexOf('Graphical display') > -1",
                ns("toolkit_action")
            ),

            selectInput(
                ns("graph_type"),
                "Graph type",
                choices = c(
                    "Histogram" = "histogram",
                    "Boxplot" = "boxplot"
                ),
                selected = "histogram"
            ),


            # ---------------------------------------------------------
            # Histogram controls
            # ---------------------------------------------------------

            conditionalPanel(

                condition = sprintf(
                    "input['%s'] == 'histogram'",
                    ns("graph_type")
                ),

                sliderInput(
                    ns("hist_bins"),
                    "Histogram bins",
                    min = 1,
                    max = 50,
                    value = 10
                )

            )

        ),


        # ---------------------------------------------------------
        # Summary statistics selection
        # ---------------------------------------------------------

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


        # ---------------------------------------------------------
        # Relationship analysis
        # ---------------------------------------------------------

        # ---------------------------------------------------------
        # Relationship analysis
        # ---------------------------------------------------------

        conditionalPanel(

            condition = sprintf(
                "input['%s'] != 'vector'",
                ns("data_source")
            ),

            hr(),

            h5("Relationship analysis"),

            checkboxGroupInput(
                ns("scatter_action"),
                "Display",
                choices = c(
                    "Scatterplot"
                ),
                selected = character(0)
            )

        ),

        # ---------------------------------------------------------
        # Scatterplot controls
        # ---------------------------------------------------------

        conditionalPanel(

            condition = sprintf(
                "input['%s'] != 'vector' && input['%s'].indexOf('Scatterplot') > -1",
                ns("data_source"),
                ns("scatter_action")
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


    # =========================================================
    # OVERVIEW PANEL
    # =========================================================

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
                    strong("Use sports example data: "),
                    "use the built-in sports dataset as an example of a ",
                    "multi-variable CSV dataset."
                ),

                tags$li(
                    strong("Use mtcars example data: "),
                    "use the built-in mtcars dataset immediately, without ",
                    "uploading a file. This is a small dataset containing ",
                    "several variables describing different car models."
                ),

                tags$li(
                    strong("Upload CSV: "),
                    "download a CSV template if required, enter or edit your ",
                    "data, and then upload the completed file."
                )

            ),


            p(
                "When using sports, mtcars or an uploaded CSV file, the ",
                "available variables are shown in the variable selectors ",
                "in the Analysis Settings."
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
                    "choose a numerical variable and display its mean, ",
                    "median, standard deviation, variance, minimum, ",
                    "and/or maximum."
                ),

                tags$li(
                    strong("Histogram: "),
                    "display the distribution of a selected numerical ",
                    "variable and adjust the number of bins."
                ),

                tags$li(
                    strong("Boxplot: "),
                    "display a boxplot for a selected numerical variable."
                ),

                tags$li(
                    strong("Scatterplot: "),
                    "when using sports, mtcars or an uploaded CSV file, ",
                    "select an X variable and a Y variable to investigate ",
                    "their relationship."
                ),

                tags$li(
                    strong("Regression line: "),
                    "when using a scatterplot, optionally add a fitted ",
                    "linear regression line. The regression results are then ",
                    "displayed alongside the scatterplot."
                )

            ),


            hr(),


            h5("Things to observe"),

            h5("Things to observe"),


            tags$ul(

                tags$li(
                    "How much information is retained or lost when data ",
                    "are summarised?"
                ),

                tags$li(
                    "How do numerical and graphical summaries complement ",
                    "one another?"
                ),

                tags$li(
                    "What does a histogram or boxplot reveal about the ",
                    "distribution of a variable?"
                ),

                tags$li(
                    "What patterns or relationships can be seen in a ",
                    "scatterplot?"
                ),

                tags$li(
                    "Does a regression line provide a useful description ",
                    "of the relationship?"
                )

            ),


            hr(),


            h5("Using the R Code tab"),


            p(
                "The ",
                strong("R Code"),
                " tab shows an approximate version of the R code used ",
                "to produce the analyses displayed in the Explore tab. ",
                "You can use this code to see how the analysis could be ",
                "carried out directly in R."
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
                        "Is much detail lost when summarising data ",
                        "numerically and/or graphically?"
                    ),

                    tags$li(
                        "When are graphical summaries useful alongside ",
                        "numerical summaries?"
                    ),

                    tags$li(
                        "When are histograms preferable to boxplots?"
                    ),

                    tags$li(
                        "What can a scatterplot reveal that separate ",
                        "summaries of two variables cannot?"
                    ),

                    tags$li(
                        "Is it always appropriate to add a regression ",
                        "line to a scatterplot?"
                    ),

                    tags$li(
                        "How might your conclusions change when you ",
                        "explore a different variable or a different ",
                        "pair of variables?"
                    )

                )

            )

        )

    )


    # =========================================================
    # MTCARS INFORMATION
    # =========================================================

    mtcars_info <- conditionalPanel(

        condition = sprintf(
            "input['%s']=='mtcars'",
            ns("data_source")
        ),

        accordion(

            id = ns("mtcars_info"),

            open = FALSE,

            accordion_panel(

                "📖 About the mtcars dataset",


                p(
                    "The ",
                    strong("mtcars"),
                    " dataset contains data on fuel consumption and ",
                    "10 aspects of automobile design and performance ",
                    "for 32 automobiles."
                ),


                p(
                    "The data were extracted from the 1974 US magazine ",
                    em("Motor Trend"),
                    ". The dataset is commonly used in R examples ",
                    "and statistics teaching."
                ),


                h5("The variables"),


                tags$ul(

                    tags$li(
                        strong("mpg"),
                        " — Miles per US gallon."
                    ),

                    tags$li(
                        strong("cyl"),
                        " — Number of cylinders."
                    ),

                    tags$li(
                        strong("disp"),
                        " — Displacement, in cubic inches."
                    ),

                    tags$li(
                        strong("hp"),
                        " — Gross horsepower."
                    ),

                    tags$li(
                        strong("drat"),
                        " — Rear axle ratio."
                    ),

                    tags$li(
                        strong("wt"),
                        " — Weight, in 1,000 lbs."
                    ),

                    tags$li(
                        strong("qsec"),
                        " — 1/4 mile time."
                    ),

                    tags$li(
                        strong("vs"),
                        " — Engine shape: 0 = V-shaped, ",
                        "1 = straight."
                    ),

                    tags$li(
                        strong("am"),
                        " — Transmission: 0 = automatic, ",
                        "1 = manual."
                    ),

                    tags$li(
                        strong("gear"),
                        " — Number of forward gears."
                    ),

                    tags$li(
                        strong("carb"),
                        " — Number of carburetors."
                    )

                ),


                h5("A note about the data"),


                p(
                    "In the original R version of ",
                    strong("mtcars"),
                    ", the names of the cars are stored as row names ",
                    "rather than as a separate variable. The CSV file ",
                    "used in this app does not include those row names, ",
                    "so it contains the 11 variables listed above."
                ),


                p(
                    "For variables such as ",
                    strong("vs"),
                    " and ",
                    strong("am"),
                    ", the numerical values represent categories ",
                    "rather than quantities. For example, ",
                    strong("am"),
                    " uses 0 for automatic transmission and 1 for ",
                    "manual transmission."
                )

            )

        )

    )



    # =========================================================
    # SPORTS INFORMATION
    # =========================================================

    sports_info <- conditionalPanel(

        condition = sprintf(
            "input['%s']=='sports'",
            ns("data_source")
        ),

        accordion(

            id = ns("sports_info"),

            open = FALSE,

            accordion_panel(

                "📖 About the sports dataset",

                p(
                    "This dataset contains individual-level measurements ",
                    "from 40-m sprint testing of athletes from a range of ",
                    "sports. It provides an example of a real-world ",
                    "multivariable dataset that can be used to explore ",
                    "distributions, group differences, and relationships ",
                    "between numerical variables."
                ),

                p(
                    "The dataset contains 666 40-m sprint tests on male ",
                    "and female athletes from multiple sports. The data ",
                    "were collected from more than 600 Norwegian athletes ",
                    "performing 40-m sprint tests under highly controlled ",
                    "conditions as part of training monitoring."
                ),

                h5("The variables"),

                tags$ul(

                    tags$li(
                        strong("ID"),
                        " — Individual identifier."
                    ),

                    tags$li(
                        strong("Sport"),
                        " — The sport in which the individual competes."
                    ),

                    tags$li(
                        strong("Sex"),
                        " — Sex of the individual, recorded as M or F."
                    ),

                    tags$li(
                        strong("Age"),
                        " — Age in years at the time of testing."
                    ),

                    tags$li(
                        strong("Bodymass"),
                        " — Body mass in kilograms."
                    ),

                    tags$li(
                        strong("Time10m"),
                        " — Time to reach 10 metres from the start ",
                        "of the sprint, in seconds."
                    ),

                    tags$li(
                        strong("Time20m"),
                        " — Time to reach 20 metres from the start ",
                        "of the sprint, in seconds."
                    ),

                    tags$li(
                        strong("Time30m"),
                        " — Time to reach 30 metres from the start ",
                        "of the sprint, in seconds."
                    ),

                    tags$li(
                        strong("Time40m"),
                        " — Time to reach 40 metres from the start ",
                        "of the sprint, in seconds."
                    ),

                    tags$li(
                        strong("F0"),
                        " — Theoretical maximal horizontal force, ",
                        "expressed relative to body mass (N/kg)."
                    ),

                    tags$li(
                        strong("V0"),
                        " — Theoretical maximal velocity (m/s)."
                    ),

                    tags$li(
                        strong("Pmax"),
                        " — Maximal mechanical power relative to body ",
                        "mass (W/kg)."
                    ),

                    tags$li(
                        strong("FVSlope"),
                        " — Slope of the force–velocity relationship."
                    ),

                    tags$li(
                        strong("RFmax"),
                        " — Maximum ratio of horizontal force to total ",
                        "force."
                    ),

                    tags$li(
                        strong("DRF"),
                        " — Decrease in the ratio of horizontal force ",
                        "to total force as running velocity increases."
                    )

                ),

                h5("The sprint measurements"),

                p(
                    "The variables Time10m, Time20m, Time30m and Time40m ",
                    "are cumulative sprint times. They describe how long ",
                    "an individual took to reach each distance from the ",
                    "start of the sprint."
                ),

                p(
                    "For example, Time20m is the total time taken to reach ",
                    "20 metres, rather than the time taken to run only the ",
                    "10-metre section between 10 and 20 metres."
                ),

                p(
                    "These variables can be used to investigate questions ",
                    "such as whether athletes who reach 20 metres quickly ",
                    "also tend to reach 40 metres quickly, or how sprint ",
                    "performance varies between sports or between the ",
                    "recorded sex categories."
                ),

                h5("Force–velocity and power measures"),

                p(
                    "The variables F0, V0, Pmax, FVSlope, RFmax and DRF ",
                    "describe different aspects of force production, ",
                    "running velocity, mechanical power and the ",
                    "force–velocity relationship."
                ),

                p(
                    "These are numerical variables that can be explored ",
                    "using summary statistics, histograms, boxplots and ",
                    "scatterplots."
                ),

                h5("A note about the original data"),

                p(
                    "The original dataset is named ",
                    strong("Sprinttest_Olympiatoppen.tab"),
                    " and is provided as a tab-delimited data file."
                ),

                p(
                    "The original file contains 15 variables and 667 ",
                    "rows. The repository describes the file as containing ",
                    "data from 666 40-m sprint tests; the additional row is ",
                    "the units row included in the original file."
                ),

                p(
                    "When preparing the data for use in this toolkit, the ",
                    "units row is removed because it is not an individual ",
                    "observation. The required variables are then retained ",
                    "and the numerical measurement variables are converted ",
                    "to numeric values."
                ),

                h5("Source and reference"),

                p(
                    "The data were obtained from the DataverseNO repository ",
                    "at the University of Agder."
                ),

                p(
                    strong(
                        "Haugen, T., Seiler, S., & Breitschadel, F. (2020). "
                    ),
                    em(
                        "40m sprint mechanics dataset male and female athletes ",
                        "UiA/Olympiatoppen"
                    ),
                    ". DataverseNO. ",
                    "https://doi.org/10.18710/PJONBM"
                ),

                p(
                    "The dataset was produced by the Norwegian Olympic ",
                    "Federation and distributed through the University of ",
                    "Agder's research data repository."
                ),

                p(
                    "The dataset is available under a Creative Commons ",
                    "CC0 1.0 Universal Public Domain Dedication."
                ),

                p(
                    "A related publication using the dataset is:"
                ),

                p(
                    strong(
                        "Haugen, T. A., Breitschädel, F., & Seiler, S. (2019). "
                    ),
                    em(
                        "Sprint Mechanical Properties in Handball and ",
                        "Basketball Players"
                    ),
                    ". ",
                    em(
                        "International Journal of Sports Physiology and ",
                        "Performance, 14"
                    ),
                    "(10), 1388–1394. ",
                    "doi:10.1123/ijspp.2019-0180."
                ),

                h5("Using the dataset in this toolkit"),

                p(
                    "Try selecting ",
                    strong("Sport"),
                    " or ",
                    strong("Sex"),
                    " as the categorical variable and use the subset ",
                    "controls to explore particular groups of individuals."
                ),

                p(
                    "You can then use the numerical variable selector to ",
                    "investigate age, body mass, sprint times, or the ",
                    "force–velocity variables using summary statistics, ",
                    "histograms, boxplots and scatterplots."
                )

            )

        )

    )







    # =========================================================
    # RESULTS
    # =========================================================

    results_panel <- div(

        mtcars_info,

        sports_info,

        card(

            card_header(
                "Data Exploration"
            ),

            uiOutput(
                ns("combined_results")
            )

        )

    )


    # =========================================================
    # CODE
    # =========================================================

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


    # =========================================================
    # CHAPTER PAGE
    # =========================================================

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



# =========================================================
# SERVER
# =========================================================

stats_toolkit_server <- function(id) {


    moduleServer(
        id,

        function(input, output, session) {


            ns <- session$ns


            # =====================================================
            # Simulation details
            # =====================================================

            sim_details <- reactiveVal(NULL)


            # =====================================================
            # Raw dataframe
            #
            # This is deliberately independent of subsetting.
            # =====================================================

            raw_data <- reactive({

                req(input$data_source)


                # -------------------------------------------------
                # mtcars
                # -------------------------------------------------

                if (identical(input$data_source, "mtcars")) {

                    mtcars_file <- system.file(
                        "extdata",
                        "mtcars.csv",
                        package = "pws"
                    )

                    validate(
                        need(
                            nzchar(mtcars_file),
                            "mtcars.csv could not be found."
                        )
                    )

                    df <- readr::read_csv(
                        mtcars_file,
                        show_col_types = FALSE
                    )

                    validate(
                        need(
                            is.data.frame(df),
                            "The mtcars data could not be loaded."
                        )
                    )

                    return(df)

                }


                # -------------------------------------------------
                # Sports
                # -------------------------------------------------

                if (identical(input$data_source, "sports")) {

                    sports_file <- system.file(
                        "extdata",
                        "sports_data.csv",
                        package = "pws"
                    )

                    validate(
                        need(
                            nzchar(sports_file),
                            "sports_data.csv could not be found."
                        )
                    )

                    df <- readr::read_csv(
                        sports_file,
                        show_col_types = FALSE
                    )

                    validate(
                        need(
                            is.data.frame(df),
                            "The sports data could not be loaded."
                        )
                    )

                    return(df)

                }


                # -------------------------------------------------
                # CSV
                # -------------------------------------------------

                req(
                    identical(
                        input$data_source,
                        "csv"
                    )
                )

                req(input$csv_file)

                validate(
                    need(
                        !is.null(input$csv_file$datapath) &&
                            nzchar(input$csv_file$datapath),
                        "Please upload a CSV file."
                    )
                )

                df <- readr::read_csv(
                    input$csv_file$datapath,
                    show_col_types = FALSE
                )

                validate(
                    need(
                        is.data.frame(df),
                        "The uploaded file could not be read."
                    )
                )

                df

            })


            # =====================================================
            # Session graphics cleanup
            # =====================================================

            session$onSessionEnded(
                function() {
                    try(
                        graphics.off(),
                        silent = TRUE
                    )
                }
            )


            # =====================================================
            # Download CSV template
            # =====================================================

            output$download_template <- downloadHandler(

                filename = function() {

                    paste0(
                        "statistics_template_",
                        Sys.Date(),
                        ".csv"
                    )

                },

                content = function(file) {

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


            # =====================================================
            # Download mtcars
            # =====================================================

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
                            nzchar(mtcars_file),
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


            # =====================================================
            # Download sports
            # =====================================================

            output$download_sports <- downloadHandler(

                filename = function() {
                    "sports_data.csv"
                },

                content = function(file) {

                    sports_file <- system.file(
                        "extdata",
                        "sports_data.csv",
                        package = "pws"
                    )

                    validate(
                        need(
                            nzchar(sports_file),
                            "sports_data.csv could not be found in the package."
                        )
                    )

                    file.copy(
                        sports_file,
                        file,
                        overwrite = TRUE
                    )

                }

            )


            # =====================================================
            # Generate simulated vector data
            # =====================================================

            observeEvent(
                input$simulate_data,

                {

                    req(
                        input$seed,
                        input$sim_n
                    )

                    current_seed <- input$seed

                    set.seed(current_seed)

                    lambda <- runif(
                        1,
                        2,
                        20
                    )

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

                }

            )


            # =====================================================
            # Reset dependent inputs whenever the DATA SOURCE
            # changes.
            #
            # This is the key protection against the sports -> mtcars
            # stale-input problem.
            # =====================================================

            observeEvent(

                input$data_source,

                {

                    source <- input$data_source


                    # -------------------------------------------------
                    # Clear subset immediately.
                    #
                    # The old sports category may not exist in mtcars.
                    # -------------------------------------------------

                    updateSelectInput(
                        session,
                        "subset_col",
                        choices = c("None" = ""),
                        selected = ""
                    )

                    updateSelectInput(
                        session,
                        "subset_value",
                        choices = c("All" = ""),
                        selected = ""
                    )


                    # -------------------------------------------------
                    # Clear variable selectors while the new data
                    # are being loaded.
                    # -------------------------------------------------

                    updateSelectInput(
                        session,
                        "summary_col",
                        choices = character(0),
                        selected = character(0)
                    )

                    updateSelectInput(
                        session,
                        "x_col",
                        choices = character(0),
                        selected = character(0)
                    )

                    updateSelectInput(
                        session,
                        "y_col",
                        choices = character(0),
                        selected = character(0)
                    )



                    # -------------------------------------------------
                    # Reset scatterplot selection whenever the dataset
                    # changes.
                    #
                    # Scatterplot is available for dataframe datasets,
                    # but is always returned to its default unchecked
                    # state when switching datasets.
                    # -------------------------------------------------

                    if (
                        source %in%
                        c(
                            "csv",
                            "mtcars",
                            "sports"
                        )
                    ) {

                        updateCheckboxGroupInput(
                            session,
                            "scatter_action",
                            choices = c(
                                "Scatterplot"
                            ),
                            selected = character(0)
                        )

                    } else {

                        updateCheckboxGroupInput(
                            session,
                            "scatter_action",
                            choices = character(0),
                            selected = character(0)
                        )

                    }

                    updateCheckboxInput(
                        session,
                        "add_lm",
                        value = FALSE
                    )

                },

                ignoreInit = FALSE

            )


            # =====================================================
            # Data used for analysis
            #
            # IMPORTANT:
            # Subsetting is defensive. A stale input can never
            # produce a zero-length logical row index.
            # =====================================================

            toolkit_data <- reactive({

                req(input$data_source)


                # -------------------------------------------------
                # Vector
                # -------------------------------------------------

                if (
                    identical(
                        input$data_source,
                        "vector"
                    )
                ) {

                    req(input$vector_input)

                    pieces <- unlist(
                        strsplit(
                            input$vector_input,
                            ","
                        )
                    )

                    pieces <- trimws(pieces)

                    validate(
                        need(
                            length(pieces) > 0,
                            "Please enter some numerical data."
                        )
                    )

                    x <- suppressWarnings(
                        as.numeric(pieces)
                    )

                    validate(
                        need(
                            length(x) > 0,
                            "Please enter some numerical data."
                        ),

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


                # -------------------------------------------------
                # Dataframe
                # -------------------------------------------------

                df <- raw_data()

                validate(
                    need(
                        is.data.frame(df),
                        "No dataframe is available."
                    )
                )


                # -------------------------------------------------
                # DEFENSIVE SUBSETTING
                # -------------------------------------------------

                subset_col <- input$subset_col
                subset_value <- input$subset_value


                use_subset <- (

                    !is.null(subset_col) &&

                        length(subset_col) == 1 &&

                        !is.na(subset_col) &&

                        nzchar(subset_col) &&

                        subset_col %in% names(df) &&

                        !is.null(subset_value) &&

                        length(subset_value) == 1 &&

                        !is.na(subset_value) &&

                        nzchar(subset_value)

                )


                if (use_subset) {

                    column_values <- df[[subset_col]]

                    keep <- (

                        !is.na(column_values) &

                            as.character(column_values) ==
                            as.character(subset_value)

                    )


                    # -------------------------------------------------
                    # Additional protection:
                    # the row index must have exactly nrow(df) values.
                    # -------------------------------------------------

                    if (
                        length(keep) == nrow(df)
                    ) {

                        df <- df[
                            keep,
                            ,
                            drop = FALSE
                        ]

                    }

                }


                validate(
                    need(
                        nrow(df) > 0,
                        "No observations remain after subsetting."
                    )
                )


                list(
                    type = "dataframe",
                    data = df
                )

            })


            # =====================================================
            # Populate variable selectors from CURRENT raw dataset
            # =====================================================

            observe({

                req(
                    input$data_source %in%
                        c("csv", "mtcars", "sports")
                )

                df <- raw_data()

                req(
                    is.data.frame(df),
                    ncol(df) > 0
                )


                # -------------------------------------------------
                # Numerical columns
                # -------------------------------------------------

                numeric_cols <- names(df)[
                    vapply(
                        df,
                        is.numeric,
                        logical(1)
                    )
                ]


                # -------------------------------------------------
                # Categorical columns
                # -------------------------------------------------

                categorical_cols <- names(df)[
                    vapply(

                        df,

                        function(x) {

                            if (
                                is.character(x) ||
                                is.factor(x) ||
                                is.logical(x)
                            ) {
                                return(TRUE)
                            }

                            if (is.numeric(x)) {

                                n_unique <- length(
                                    unique(
                                        na.omit(x)
                                    )
                                )

                                return(
                                    n_unique <= 10
                                )
                            }

                            FALSE
                        },

                        logical(1)
                    )
                ]


                # -------------------------------------------------
                # Preserve valid existing selections
                # -------------------------------------------------

                current_subset_col <- isolate(
                    input$subset_col
                )

                current_summary <- isolate(
                    input$summary_col
                )

                current_x <- isolate(
                    input$x_col
                )

                current_y <- isolate(
                    input$y_col
                )


                # -------------------------------------------------
                # Categorical variable selector
                # -------------------------------------------------

                subset_choices <- c(
                    "None" = "",
                    setNames(
                        categorical_cols,
                        categorical_cols
                    )
                )

                selected_subset_col <- if (
                    !is.null(current_subset_col) &&
                    length(current_subset_col) == 1 &&
                    current_subset_col %in% categorical_cols
                ) {
                    current_subset_col
                } else {
                    ""
                }

                updateSelectInput(
                    session,
                    "subset_col",
                    choices = subset_choices,
                    selected = selected_subset_col
                )


                # -------------------------------------------------
                # Numerical variable selector
                # -------------------------------------------------

                selected_summary <- if (
                    length(numeric_cols) > 0 &&
                    !is.null(current_summary) &&
                    length(current_summary) == 1 &&
                    current_summary %in% numeric_cols
                ) {
                    current_summary
                } else if (
                    length(numeric_cols) > 0
                ) {
                    numeric_cols[1]
                } else {
                    character(0)
                }

                updateSelectInput(
                    session,
                    "summary_col",
                    choices = numeric_cols,
                    selected = selected_summary
                )


                # -------------------------------------------------
                # X variable
                # -------------------------------------------------

                selected_x <- if (
                    length(numeric_cols) > 0 &&
                    !is.null(current_x) &&
                    length(current_x) == 1 &&
                    current_x %in% numeric_cols
                ) {
                    current_x
                } else if (
                    length(numeric_cols) > 0
                ) {
                    numeric_cols[1]
                } else {
                    character(0)
                }

                updateSelectInput(
                    session,
                    "x_col",
                    choices = numeric_cols,
                    selected = selected_x
                )


                # -------------------------------------------------
                # Y variable
                # -------------------------------------------------

                selected_y <- if (
                    length(numeric_cols) >= 2 &&
                    !is.null(current_y) &&
                    length(current_y) == 1 &&
                    current_y %in% numeric_cols
                ) {
                    current_y
                } else if (
                    length(numeric_cols) >= 2
                ) {
                    numeric_cols[2]
                } else if (
                    length(numeric_cols) == 1
                ) {
                    numeric_cols[1]
                } else {
                    character(0)
                }

                updateSelectInput(
                    session,
                    "y_col",
                    choices = numeric_cols,
                    selected = selected_y
                )

            })

            # =====================================================
            # Populate subset values from selected categorical
            # variable
            # =====================================================

            observeEvent(

                input$subset_col,

                {

                    req(
                        input$data_source %in%
                            c(
                                "csv",
                                "mtcars",
                                "sports"
                            )
                    )

                    df <- raw_data()

                    req(
                        is.data.frame(df)
                    )


                    subset_col <- input$subset_col


                    # -------------------------------------------------
                    # No categorical variable selected
                    # -------------------------------------------------

                    if (
                        is.null(subset_col) ||
                        length(subset_col) != 1 ||
                        !nzchar(subset_col) ||
                        !subset_col %in% names(df)
                    ) {

                        updateSelectInput(
                            session,
                            "subset_value",
                            choices = c(
                                "All" = ""
                            ),
                            selected = ""
                        )

                        return()
                    }


                    # -------------------------------------------------
                    # Get values from selected column
                    # -------------------------------------------------

                    values <- unique(
                        as.character(
                            df[[subset_col]]
                        )
                    )

                    values <- values[
                        !is.na(values)
                    ]

                    values <- sort(
                        values
                    )


                    # -------------------------------------------------
                    # Build choices
                    # -------------------------------------------------

                    value_choices <- c(
                        "All" = "",
                        setNames(
                            values,
                            values
                        )
                    )


                    # -------------------------------------------------
                    # Preserve current value if it still exists
                    # -------------------------------------------------

                    current_value <- isolate(
                        input$subset_value
                    )

                    selected_value <- if (
                        !is.null(current_value) &&
                        length(current_value) == 1 &&
                        current_value %in% values
                    ) {
                        current_value
                    } else {
                        ""
                    }


                    updateSelectInput(
                        session,
                        "subset_value",
                        choices = value_choices,
                        selected = selected_value
                    )

                },

                ignoreInit = FALSE

            )



            # =====================================================
            # Combined results UI
            # =====================================================

            output$combined_results <- renderUI({

                displays <- input$toolkit_action

                if (is.null(displays)) {
                    displays <- character(0)
                }


                scatter_selected <- (
                    "Scatterplot" %in%
                        (input$scatter_action %||% character(0))
                )


                # =====================================================
                # SCATTERPLOT MODE
                #
                # Show scatterplot and regression results side by side.
                # =====================================================

                if (

                    input$data_source %in%
                    c(
                        "csv",
                        "mtcars",
                        "sports"
                    ) &&

                    scatter_selected

                ) {

                    scatter_panel <- card(

                        card_header(
                            "Scatterplot"
                        ),

                        plotOutput(
                            ns("scatter"),
                            height = "500px"
                        )

                    )


                    regression_panel <- card(

                        card_header(
                            "Regression results"
                        ),

                        uiOutput(
                            ns("regression_results")
                        )

                    )


                    return(

                        layout_columns(

                            col_widths = c(6, 6),

                            scatter_panel,

                            regression_panel

                        )

                    )

                }

                # =====================================================
                # Regression results UI
                # =====================================================

                output$regression_results <- renderUI({

                    if (
                        !isTRUE(input$add_lm)
                    ) {

                        return(

                            div(

                                style = "
                    color: #6c757d;
                    padding: 20px;
                    text-align: center;
                ",

                                p(
                                    "Regression results will appear here ",
                                    "when a regression line is selected."
                                )

                            )

                        )

                    }


                    tagList(

                        h5("Coefficients"),

                        tableOutput(
                            ns("regression_coefficients")
                        )

                    )

                })


                # =====================================================
                # NORMAL DESCRIPTIVE ANALYSES
                # =====================================================

                numeric_panels <- list()
                graphic_panels <- list()


                # -----------------------------------------------------
                # Summary statistics
                # -----------------------------------------------------

                if (
                    "Summary statistics" %in% displays
                ) {

                    numeric_panels <- append(

                        numeric_panels,

                        list(

                            card(

                                card_header(
                                    "Summary Statistics"
                                ),

                                uiOutput(
                                    ns("summary_table")
                                )

                            )

                        )

                    )

                }


                # -----------------------------------------------------
                # Graphical display
                # -----------------------------------------------------

                if (
                    "Graphical display" %in% displays
                ) {

                    if (
                        identical(
                            input$graph_type,
                            "histogram"
                        )
                    ) {

                        graphic_panels <- append(

                            graphic_panels,

                            list(

                                card(

                                    card_header(
                                        "Histogram"
                                    ),

                                    plotOutput(
                                        ns("tool_hist"),
                                        height = "500px"
                                    )

                                )

                            )

                        )

                    } else if (
                        identical(
                            input$graph_type,
                            "boxplot"
                        )
                    ) {

                        graphic_panels <- append(

                            graphic_panels,

                            list(

                                card(

                                    card_header(
                                        "Boxplot"
                                    ),

                                    plotOutput(
                                        ns("tool_boxplot"),
                                        height = "500px"
                                    )

                                )

                            )

                        )

                    }

                }


                # =====================================================
                # LAYOUT
                # =====================================================

                if (

                    length(numeric_panels) > 0 &&

                    length(graphic_panels) > 0

                ) {

                    layout_columns(

                        col_widths = c(6, 6),

                        tagList(
                            numeric_panels
                        ),

                        tagList(
                            graphic_panels
                        )

                    )

                } else if (

                    length(numeric_panels) > 0

                ) {

                    tagList(
                        numeric_panels
                    )

                } else if (

                    length(graphic_panels) > 0

                ) {

                    tagList(
                        graphic_panels
                    )

                } else {

                    p(
                        "Select an analysis to display."
                    )

                }

            })

            # =====================================================
            # Summary statistics
            # =====================================================

            output$summary_table <- renderUI({

                stats_selected <- input$summary_stats

                req(
                    stats_selected
                )


                dat <- toolkit_data()

                req(
                    !is.null(dat)
                )


                if (
                    identical(
                        dat$type,
                        "vector"
                    )
                ) {

                    x <- dat$data

                } else {

                    req(
                        input$summary_col
                    )

                    validate(
                        need(
                            input$summary_col %in%
                                names(dat$data),
                            "Please select a valid numerical variable."
                        )
                    )

                    x <- dat$data[[input$summary_col]]


                }


                validate(

                    need(
                        is.numeric(x),
                        "Summary statistics require numerical data."
                    ),

                    need(
                        length(x) > 0,
                        "No data are available."
                    )

                )


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

                    function(stat) {

                        div(

                            style = "font-size: 0.85em;",

                            value_box(

                                title = stat,

                                value = tags$div(

                                    style = "
                                        font-size: 1.2em;
                                        font-weight: 600;
                                    ",

                                    formatC(
                                        signif(
                                            stat_values[[stat]],
                                            3
                                        ),
                                        format = "fg"
                                    )

                                )

                            )

                        )

                    }

                )


                layout_columns(

                    col_widths = rep(
                        4,
                        length(boxes)
                    ),

                    !!!boxes

                )

            })


            # =====================================================
            # Histogram
            # =====================================================

            output$tool_hist <- renderPlot({

                dat <- toolkit_data()

                if (
                    identical(
                        dat$type,
                        "vector"
                    )
                ) {

                    x <- dat$data

                } else {

                    req(
                        input$summary_col
                    )

                    validate(
                        need(
                            input$summary_col %in%
                                names(dat$data),
                            "Please select a valid numerical variable."
                        )
                    )

                    x <- dat$data[[input$summary_col]]

                }


                validate(

                    need(
                        is.numeric(x),
                        "Histogram requires numerical data."
                    ),

                    need(
                        sum(!is.na(x)) > 0,
                        "No numerical observations are available."
                    )

                )


                hist(

                    x,

                    breaks = input$hist_bins,

                    col = "#7B9ACC",

                    border = "white",

                    main = "Histogram",

                    xlab = "Value"

                )

            })


            # =====================================================
            # Boxplot
            # =====================================================

            output$tool_boxplot <- renderPlot({

                dat <- toolkit_data()


                if (
                    identical(
                        dat$type,
                        "vector"
                    )
                ) {

                    x <- dat$data

                } else {

                    req(
                        input$summary_col
                    )

                    validate(
                        need(
                            input$summary_col %in%
                                names(dat$data),
                            "Please select a valid numerical variable."
                        )
                    )

                    x <- dat$data[[input$summary_col]]

                }


                validate(

                    need(
                        is.numeric(x),
                        "Boxplot requires numeric data."
                    ),

                    need(
                        sum(!is.na(x)) > 0,
                        "No numerical observations are available."
                    )

                )


                boxplot(

                    x,

                    col = "#7B9ACC",

                    border = "#2c3e50",

                    main = "Boxplot",

                    ylab = "Value"

                )

            })


            # =====================================================
            # Scatterplot
            # =====================================================

            output$scatter <- renderPlot({

                dat <- toolkit_data()


                req(
                    identical(
                        dat$type,
                        "dataframe"
                    )
                )


                req(
                    input$x_col,
                    input$y_col
                )


                validate(

                    need(
                        input$x_col %in%
                            names(dat$data),
                        "Please select a valid X variable."
                    ),

                    need(
                        input$y_col %in%
                            names(dat$data),
                        "Please select a valid Y variable."
                    )

                )


                x <- dat$data[[input$x_col]]

                y <- dat$data[[input$y_col]]


                validate(

                    need(
                        is.numeric(x),
                        "The X variable must be numeric."
                    ),

                    need(
                        is.numeric(y),
                        "The Y variable must be numeric."
                    )

                )


                keep <- complete.cases(
                    x,
                    y
                )


                x <- x[keep]
                y <- y[keep]


                validate(

                    need(
                        length(x) > 1,
                        paste(
                            "There are not enough complete observations",
                            "to make a scatterplot."
                        )
                    )

                )


                plot(

                    x,

                    y,

                    pch = 19,

                    cex = input$point_size,

                    col = "#CDB4DB",

                    xlab = input$x_col,

                    ylab = input$y_col,

                    main = paste(
                        input$y_col,
                        "against",
                        input$x_col
                    )

                )


                if (
                    isTRUE(input$add_lm)
                ) {

                    model <- lm(
                        y ~ x
                    )

                    abline(

                        model,

                        col = "#7B9ACC",

                        lwd = 3

                    )

                }

            })

            # =====================================================
            # Regression line coefficients
            # =====================================================



            output$regression_coefficients <- renderTable({

                req(
                    input$data_source %in%
                        c(
                            "csv",
                            "mtcars",
                            "sports"
                        )
                )

                req(
                    isTRUE(input$add_lm)
                )

                req(
                    "Scatterplot" %in%
                        (input$scatter_action %||% character(0))
                )

                dat <- toolkit_data()

                req(
                    identical(
                        dat$type,
                        "dataframe"
                    )
                )

                req(
                    input$x_col,
                    input$y_col
                )

                validate(

                    need(
                        input$x_col %in% names(dat$data),
                        "Please select a valid X variable."
                    ),

                    need(
                        input$y_col %in% names(dat$data),
                        "Please select a valid Y variable."
                    )

                )


                x <- dat$data[[input$x_col]]
                y <- dat$data[[input$y_col]]


                validate(

                    need(
                        is.numeric(x),
                        "The X variable must be numeric."
                    ),

                    need(
                        is.numeric(y),
                        "The Y variable must be numeric."
                    )

                )


                keep <- complete.cases(
                    x,
                    y
                )

                x <- x[keep]
                y <- y[keep]


                validate(

                    need(
                        length(x) > 1,
                        "There are not enough observations for regression."
                    )

                )


                model <- lm(
                    y ~ x
                )


                coefs <- summary(model)$coefficients


                data.frame(

                    Term = c(
                        "Intercept",
                        "Slope"
                    ),

                    Estimate = round(
                        coefs[, "Estimate"],
                        4
                    ),

                    `Std. Error` = round(
                        coefs[, "Std. Error"],
                        4
                    ),

                    `t value` = round(
                        coefs[, "t value"],
                        4
                    ),

                    `p value` = signif(
                        coefs[, "Pr(>|t|)"],
                        4
                    ),

                    row.names = NULL,

                    check.names = FALSE

                )

            })





            # =====================================================
            # Generated R code
            # =====================================================

            output$generated_code <- renderText({

                req(
                    input$data_source
                )

                actions <- input$toolkit_action

                if (is.null(actions)) {
                    actions <- character(0)
                }

                # -------------------------------------------------
                # DATA
                # -------------------------------------------------

                if (
                    identical(
                        input$data_source,
                        "vector"
                    )
                ) {

                    if (
                        identical(
                            input$vector_mode,
                            "simulate"
                        )
                    ) {

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

                            paste(
                                "x <- rpois(",
                                "    nsim,",
                                "    lambda",
                                ")",
                                sep = "\n"
                            ),

                            sep = "\n"
                        )

                    } else {

                        vals <- toolkit_data()$data

                        shown_vals <- head(
                            vals,
                            20
                        )

                        data_code <- paste0(
                            "x <- c(",
                            paste(
                                shown_vals,
                                collapse = ", "
                            ),
                            if (length(vals) > 20) ", ..." else "",
                            ")"
                        )

                    }

                } else if (
                    identical(
                        input$data_source,
                        "mtcars"
                    )
                ) {

                    data_code <- paste(
                        "# Load the mtcars example data",
                        'data <- read.csv("mtcars.csv")',
                        sep = "\n"
                    )

                } else if (
                    identical(
                        input$data_source,
                        "sports"
                    )
                ) {

                    data_code <- paste(
                        "# Load the sports example data",
                        'data <- read.csv("sports_data.csv")',
                        sep = "\n"
                    )

                } else {

                    file_name <- if (
                        !is.null(input$csv_file)
                    ) {
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

                # -------------------------------------------------
                # ANALYSIS
                # -------------------------------------------------

                analysis_code <- character(0)

                # -------------------------------------------------
                # Summary statistics
                # -------------------------------------------------

                if (
                    "Summary statistics" %in% actions
                ) {

                    stats <- input$summary_stats

                    if (!is.null(stats)) {

                        if (
                            identical(
                                input$data_source,
                                "vector"
                            )
                        ) {

                            if ("Mean" %in% stats) {
                                analysis_code <- c(
                                    analysis_code,
                                    "mean(x)"
                                )
                            }

                            if ("Median" %in% stats) {
                                analysis_code <- c(
                                    analysis_code,
                                    "median(x)"
                                )
                            }

                            if ("SD" %in% stats) {
                                analysis_code <- c(
                                    analysis_code,
                                    "sd(x)"
                                )
                            }

                            if ("Variance" %in% stats) {
                                analysis_code <- c(
                                    analysis_code,
                                    "var(x)"
                                )
                            }

                            if ("Min" %in% stats) {
                                analysis_code <- c(
                                    analysis_code,
                                    "min(x)"
                                )
                            }

                            if ("Max" %in% stats) {
                                analysis_code <- c(
                                    analysis_code,
                                    "max(x)"
                                )
                            }

                        } else {

                            varname <- input$summary_col

                            if (
                                !is.null(varname) &&
                                length(varname) == 1 &&
                                nzchar(varname)
                            ) {

                                if ("Mean" %in% stats) {
                                    analysis_code <- c(
                                        analysis_code,
                                        paste0(
                                            "mean(data$`",
                                            varname,
                                            "`, na.rm = TRUE)"
                                        )
                                    )
                                }

                                if ("Median" %in% stats) {
                                    analysis_code <- c(
                                        analysis_code,
                                        paste0(
                                            "median(data$`",
                                            varname,
                                            "`, na.rm = TRUE)"
                                        )
                                    )
                                }

                                if ("SD" %in% stats) {
                                    analysis_code <- c(
                                        analysis_code,
                                        paste0(
                                            "sd(data$`",
                                            varname,
                                            "`, na.rm = TRUE)"
                                        )
                                    )
                                }

                                if ("Variance" %in% stats) {
                                    analysis_code <- c(
                                        analysis_code,
                                        paste0(
                                            "var(data$`",
                                            varname,
                                            "`, na.rm = TRUE)"
                                        )
                                    )
                                }

                                if ("Min" %in% stats) {
                                    analysis_code <- c(
                                        analysis_code,
                                        paste0(
                                            "min(data$`",
                                            varname,
                                            "`, na.rm = TRUE)"
                                        )
                                    )
                                }

                                if ("Max" %in% stats) {
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

                }

                # -------------------------------------------------
                # Histogram
                # -------------------------------------------------

                if (
                    "Histogram" %in% actions
                ) {

                    if (
                        identical(
                            input$data_source,
                            "vector"
                        )
                    ) {

                        analysis_code <- c(
                            analysis_code,
                            paste0(
                                "hist(x, breaks = ",
                                input$hist_bins,
                                ")"
                            )
                        )

                    } else if (
                        !is.null(input$summary_col) &&
                        length(input$summary_col) == 1 &&
                        nzchar(input$summary_col)
                    ) {

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

                # -------------------------------------------------
                # Boxplot
                # -------------------------------------------------

                if (
                    "Boxplot" %in% actions
                ) {

                    if (
                        identical(
                            input$data_source,
                            "vector"
                        )
                    ) {

                        analysis_code <- c(
                            analysis_code,
                            "boxplot(x)"
                        )

                    } else if (
                        !is.null(input$summary_col) &&
                        length(input$summary_col) == 1 &&
                        nzchar(input$summary_col)
                    ) {

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

                # -------------------------------------------------
                # Scatterplot
                # -------------------------------------------------

                if (
                    input$data_source %in%
                    c(
                        "csv",
                        "mtcars",
                        "sports"
                    ) &&
                    "Scatterplot" %in%
                    (input$scatter_action %||% character(0))
                ) {

                    if (
                        !is.null(input$x_col) &&
                        length(input$x_col) == 1 &&
                        nzchar(input$x_col) &&
                        !is.null(input$y_col) &&
                        length(input$y_col) == 1 &&
                        nzchar(input$y_col)
                    ) {

                        analysis_code <- c(
                            analysis_code,

                            paste0(
                                "plot(data$`",
                                input$x_col,
                                "`, data$`",
                                input$y_col,
                                "`, pch = 19)"
                            )
                        )

                        if (
                            isTRUE(input$add_lm)
                        ) {

                            analysis_code <- c(
                                analysis_code,

                                paste0(
                                    "model <- lm(data$`",
                                    input$y_col,
                                    "` ~ data$`",
                                    input$x_col,
                                    "`)"
                                ),

                                "abline(model)",

                                "summary(model)"
                            )

                        }

                    }

                }

                # -------------------------------------------------
                # FINAL CODE
                # -------------------------------------------------

                if (
                    length(analysis_code) == 0
                ) {

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

        }
    )

}

