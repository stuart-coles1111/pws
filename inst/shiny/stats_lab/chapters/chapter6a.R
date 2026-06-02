library(shiny)
library(ggplot2)
library(DT)

bp <- function(n) {
    p <- 1
    if (n > 1) {
        for (i in 1:n)
            p <- p * (365 - i + 1) / 365
    }
    p <- 1 - p
    p
}

# =========================================================
# Chapter 6 UI
# =========================================================

chapter6_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("Choose experiment"),

        selectInput(
            ns("demo"),
            "Experiment",
            choices = c(
                "Birthday Problem",
                "Difference in Proportions",
                "Data Dredging"
            )
        ),

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'Birthday Problem'",
                ns("demo")
            ),

            sliderInput(
                ns("nmax"),
                "Maximum group size",
                min = 10,
                max = 100,
                value = 60
            )
        ),

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'Difference in Proportions'",
                ns("demo")
            ),

            numericInput(ns("count1"), "Successes group 1", 21),
            numericInput(ns("trial1"), "Trials group 1", 31),

            numericInput(ns("count2"), "Successes group 2", 9),
            numericInput(ns("trial2"), "Trials group 2", 23),

            sliderInput(
                ns("alpha"),
                "Confidence level",
                min = 0.80,
                max = 0.99,
                value = 0.95,
                step = 0.01
            ),

            numericInput(
                ns("nsim"),
                "Simulations",
                value = 10000,
                min = 1000
            )
        ),

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'Data Dredging'",
                ns("demo")
            ),

            numericInput(
                ns("n_data"),
                "Observations",
                100
            ),

            numericInput(
                ns("n_var"),
                "Candidate predictors",
                50
            ),

            sliderInput(
                ns("sig_x"),
                "X variability",
                min = 1,
                max = 20,
                value = 10
            ),

            sliderInput(
                ns("sig_y"),
                "Y variability",
                min = 1,
                max = 20,
                value = 5
            )
        ),

        hr(),

        numericInput(
            ns("seed"),
            "Random seed",
            111
        ),

        actionButton(
            ns("run"),
            "Run analysis",
            class = "btn-primary"
        )
    )

    # =======================================================
    # Overview
    # =======================================================

    overview_panel <- div(

        card(

            card_header("What this chapter explores"),

            p("
        This chapter introduces probability,
        confidence intervals and the dangers
        of searching for patterns in random data.
      "),

            tags$ul(

                tags$li("The Birthday Problem"),

                tags$li("Comparing two proportions"),

                tags$li("Data dredging and false discoveries")
            )
        )
    )

    # =======================================================
    # Code
    # =======================================================

    code_panel <- div(

        card(

            card_header("Generated R code"),

            tags$pre(
                style="
          background:#F8F9FA;
          padding:15px;
          border-radius:10px;
        ",
                textOutput(ns("generated_code"))
            )
        )
    )

    # =======================================================
    # Results
    # =======================================================

    results_panel <- div(

        fluidRow(

            column(
                4,
                card(
                    h4("Statistic 1"),
                    h2(textOutput(ns("stat1")))
                )
            ),

            column(
                4,
                card(
                    h4("Statistic 2"),
                    h2(textOutput(ns("stat2")))
                )
            ),

            column(
                4,
                card(
                    h4("Experiment"),
                    h2(textOutput(ns("experiment")))
                )
            )
        ),

        br(),

        card(

            card_header("Visualisation"),

            plotOutput(
                ns("plot"),
                height = 450
            )
        ),

        br(),

        card(

            card_header("Output"),

            DTOutput(ns("table"))
        )
    )

    # =======================================================
    # Learn
    # =======================================================

    learn_panel <- div(

        card(

            card_header("Key ideas"),

            tags$ul(

                tags$li("Probability can be surprising."),

                tags$li("Confidence intervals quantify uncertainty."),

                tags$li("Large numbers of tests create false positives."),

                tags$li("Significant results are not always meaningful.")
            )
        ),

        br(),

        card(

            card_header("Big idea"),

            tags$blockquote(
                style="
          font-size:22px;
          font-weight:700;
          color:#7B9ACC;
          border-left:5px solid #CDB4DB;
          padding-left:18px;
        ",
                "The more we search random data, the easier it becomes to find apparently important patterns."
            )
        )
    )

    chapter_page_ui(

        id = id,

        title = "🔍 Chapter 6: Inference and False Discoveries",

        sidebar = sidebar_controls,

        overview = overview_panel,

        code = code_panel,

        results = results_panel,

        learn = learn_panel
    )
}

# =========================================================
# Chapter 6 Server
# =========================================================

chapter6_server <- function(id){

    moduleServer(id, function(input, output, session){

        analysis <- eventReactive(input$run, {

            set.seed(input$seed)

            if(input$demo == "Birthday Problem"){

                df <- data.frame(
                    N = 1:input$nmax,
                    P = sapply(1:input$nmax, bp)
                )

                list(
                    type = "birthday",
                    data = df
                )
            }

            else if(input$demo == "Difference in Proportions"){

                p1 <- input$count1 / input$trial1
                p2 <- input$count2 / input$trial2

                s1 <- rbinom(
                    input$nsim,
                    input$trial1,
                    p1
                ) / input$trial1

                s2 <- rbinom(
                    input$nsim,
                    input$trial2,
                    p2
                ) / input$trial2

                d <- s1 - s2

                se <- sd(d)

                m <- mean(d)

                qv <- qnorm((1 + input$alpha)/2)

                ci <- c(
                    m - qv*se,
                    m + qv*se
                )

                list(
                    type = "prop",
                    d = d,
                    se = se,
                    ci = ci
                )
            }

            else {

                y <- rnorm(
                    input$n_data,
                    0,
                    input$sig_y
                )

                x <- matrix(
                    rnorm(
                        input$n_var * input$n_data,
                        0,
                        input$sig_x
                    ),
                    nrow = input$n_var
                )

                pvals <- sapply(
                    1:input$n_var,
                    function(i){
                        summary(
                            lm(y ~ x[i,])
                        )$coeff[2,4]
                    }
                )

                best <- which.min(pvals)

                xx <- x[best,]

                fit <- lm(y ~ xx)

                list(
                    type = "dredge",
                    x = xx,
                    y = y,
                    coef = round(summary(fit)$coeff,4),
                    minp = min(pvals)
                )
            }

        }, ignoreNULL = FALSE)

        output$experiment <- renderText({
            input$demo
        })

        output$generated_code <- renderText({

            switch(

                input$demo,

                "Birthday Problem" =
                    paste0(
                        "plot_bp(nmax = ",
                        input$nmax,
                        ")"
                    ),

                "Difference in Proportions" =
                    paste0(
                        "prop.diff(counts = c(",
                        input$count1,",",
                        input$count2,
                        "), trials = c(",
                        input$trial1,",",
                        input$trial2,
                        "))"
                    ),

                "Data Dredging" =
                    paste0(
                        "data_dredge(n_data = ",
                        input$n_data,
                        ", n_var = ",
                        input$n_var,
                        ")"
                    )
            )
        })

        output$plot <- renderPlot({

            a <- analysis()

            if(a$type == "birthday"){

                ggplot(
                    a$data,
                    aes(N,P)
                ) +
                    geom_point(
                        colour="#7B9ACC"
                    ) +
                    geom_hline(
                        yintercept=.5,
                        colour="red"
                    ) +
                    theme_minimal(base_size=14)
            }

            else if(a$type == "prop"){

                ggplot(
                    data.frame(d=a$d),
                    aes(d)
                ) +
                    geom_histogram(
                        bins=20,
                        fill="#7B9ACC",
                        colour="white"
                    ) +
                    theme_minimal(base_size=14)
            }

            else {

                ggplot(
                    data.frame(
                        x=a$x,
                        y=a$y
                    ),
                    aes(x,y)
                ) +
                    geom_point(
                        colour="#7B9ACC"
                    ) +
                    geom_smooth(
                        method="lm",
                        colour="red"
                    ) +
                    theme_minimal(base_size=14)
            }
        })

        output$stat1 <- renderText({

            a <- analysis()

            if(a$type=="birthday"){

                round(max(a$data$P),3)

            } else if(a$type=="prop"){

                round(a$se,4)

            } else {

                signif(a$minp,4)
            }
        })

        output$stat2 <- renderText({

            a <- analysis()

            if(a$type=="birthday"){

                a$data$N[
                    which.min(abs(a$data$P-.5))
                ]

            } else if(a$type=="prop"){

                paste0(
                    round(a$ci[1],3),
                    " to ",
                    round(a$ci[2],3)
                )

            } else {

                input$n_var
            }
        })

        output$table <- renderDT({

            a <- analysis()

            if(a$type=="birthday"){

                datatable(a$data)

            } else if(a$type=="prop"){

                datatable(
                    data.frame(
                        Lower=a$ci[1],
                        Upper=a$ci[2],
                        SE=a$se
                    )
                )

            } else {

                datatable(
                    as.data.frame(a$coef)
                )
            }
        })

    })
}
