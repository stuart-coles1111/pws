# =========================================================
# UI
# =========================================================

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


            textAreaInput(
                ns("vector_input"),
                "Data values (x)",
                value = paste(
                    rpois(100,2.5),
                    collapse=","
                ),
                rows=6
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
                min=1
            ),


            numericInput(
                ns("template_cols"),
                "Template columns",
                3,
                min=1
            ),


            downloadButton(
                ns("download_template"),
                "Download CSV template"
            ),


            fileInput(
                ns("csv_file"),
                "Upload completed CSV"
            )

        ),



        selectInput(
            ns("toolkit_action"),
            "Analysis",
            choices=NULL
        ),



        conditionalPanel(

            condition=sprintf(
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

            condition=sprintf(
                "input['%s']=='csv' && input['%s']=='Histogram'",
                ns("data_source"),
                ns("toolkit_action")
            ),


            selectInput(
                ns("hist_col"),
                "Histogram variable",
                choices=NULL
            )

        ),



        conditionalPanel(

            condition=sprintf(
                "input['%s']=='csv' && input['%s']=='Scatterplot'",
                ns("data_source"),
                ns("toolkit_action")
            ),


            selectInput(
                ns("x_col"),
                "X column",
                choices=NULL
            ),


            selectInput(
                ns("y_col"),
                "Y column",
                choices=NULL
            )

        )

    )





    overview_panel <- card(

        card_header(
            "Simple Statistics Toolkit"
        ),


        p(
            "Explore basic statistical summaries and visualisations
            from either manually entered data or uploaded CSV files."
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
            "Investigate your data"
        ),


        conditionalPanel(

            condition=sprintf(
                "input['%s']=='Summary statistics'",
                ns("toolkit_action")
            ),

            tableOutput(
                ns("summary_table")
            )

        ),


        conditionalPanel(

            condition=sprintf(
                "input['%s']=='Histogram'",
                ns("toolkit_action")
            ),

            plotOutput(
                ns("tool_hist"),
                height="400px"
            )

        ),



        conditionalPanel(

            condition=sprintf(
                "input['%s']=='Scatterplot'",
                ns("toolkit_action")
            ),

            plotOutput(
                ns("scatter"),
                height="400px"
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
            "Summary statistics describe the centre and spread
            of data."
        ),


        p(
            "Graphs help reveal patterns that are not obvious
            from numbers alone."
        )

    )





    chapter_page_ui(

        id=id,

        title="🧰 Statistics Toolkit",

        sidebar=sidebar_controls,

        overview=overview_panel,

        results=results_panel,

        code=code_panel,

        learn=learn_panel

    )

}

stats_toolkit_server <-function(id){

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
                        "Histogram"
                    )
                )

            } else {


                updateSelectInput(
                    session,
                    "toolkit_action",
                    choices = c(
                        "Summary statistics",
                        "Histogram",
                        "Scatterplot"
                    )
                )

            }

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



            req(input$csv_file)



            df <- readr::read_csv(

                input$csv_file$datapath,

                col_types =
                    readr::cols(
                        .default =
                            readr::col_double()
                    ),

                show_col_types = FALSE

            )


            list(
                type="csv",
                data=df
            )

        })





        observeEvent(toolkit_data(),{


            req(
                input$data_source=="csv"
            )


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


            updateSelectInput(
                session,
                "hist_col",
                choices = names(dat)
            )


        })






        output$summary_table <- renderTable({


            dat <- toolkit_data()



            if(dat$type=="vector"){


                x <- dat$data


                return(

                    data.frame(

                        Variable="x",

                        Count=length(x),

                        Mean=mean(x),

                        Median=median(x),

                        SD=sd(x),

                        Min=min(x),

                        Max=max(x)

                    )

                )

            }



            df <- dat$data



            numeric_cols <-
                sapply(df,is.numeric)



            df <-
                df[,numeric_cols,drop=FALSE]



            results <- lapply(

                names(df),

                function(col){


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

                }

            )


            do.call(rbind, results)


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







        output$scatter <- renderPlot({


            dat <- toolkit_data()


            req(
                dat$type=="csv",
                input$x_col,
                input$y_col
            )


            plot(

                dat$data[[input$x_col]],

                dat$data[[input$y_col]],

                pch=19,

                col="#CDB4DB",

                xlab=input$x_col,

                ylab=input$y_col,

                main="Scatterplot"

            )

        })







        output$generated_code <- renderText({


            if(input$toolkit_action=="Summary statistics"){


                "summary(data)"


            } else if(input$toolkit_action=="Histogram"){

                if(input$data_source == "vector"){

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

            } else {


                paste0(
                    "plot(data$",
                    input$x_col,
                    ", data$",
                    input$y_col,
                    ")"
                )

            }

        })


    })

}
