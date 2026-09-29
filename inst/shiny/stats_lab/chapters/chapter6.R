# =========================================================
# Chapter 6 - Birthday problem functions
# =========================================================


# ---------------------------------------------------------
# Classic birthday problem
# ---------------------------------------------------------
# Probability that at least two people in a group of n
# share a birthday, assuming 365 equally likely birthdays.
# ---------------------------------------------------------

bp <- function(n) {

    if (n <= 1) {
        return(0)
    }

    p_no_match <- prod(
        (365 - (0:(n - 1))) / 365
    )

    1 - p_no_match
}


# ---------------------------------------------------------
# Poisson approximation
# ---------------------------------------------------------
# Probability that at least m people share a birthday
# somewhere among the 365 possible birthdays.
#
# For any particular birthday:
#
#   X ~ Poisson(lambda)
#
# where
#
#   lambda = n / 365
#
# We approximate the probability that every birthday has
# fewer than m people by
#
#   P(X < m)^365
#
# and therefore
#
#   P(at least one birthday has m or more people)
#       = 1 - P(X < m)^365
# ---------------------------------------------------------

birthday_poisson <- function(n, m) {

    lambda <- n / 365

    p_less_m <- ppois(
        m - 1,
        lambda
    )

    1 - p_less_m^365
}


# ---------------------------------------------------------
# Exact calculation
# ---------------------------------------------------------
# Calculates the probability that at least m people share
# a birthday using dynamic programming.
#
# The calculation tracks the number of people allocated
# across birthdays while ensuring that no birthday contains
# m or more people.
#
# The calculation is performed using log-probabilities to
# reduce numerical problems for larger values of n.
# ---------------------------------------------------------

birthday_dp <- function(n, m) {

    # If m > n, it is impossible for m people to share
    # a birthday.
    if (m > n) {
        return(0)
    }

    # If m <= 1, at least one birthday must contain
    # at least one person.
    if (m <= 1) {
        return(1)
    }

    # We calculate the probability that no birthday contains
    # m or more people.
    #
    # A state represents the number of people allocated so far.
    #
    # We work with scaled weights rather than probabilities.
    # For a particular birthday, allocating k people contributes
    # 1 / k! to the coefficient.

    dp <- numeric(n + 1)

    dp[1] <- 1

    for (b in 1:365) {

        new_dp <- numeric(n + 1)

        for (j in 0:n) {

            current <- dp[j + 1]

            if (current == 0) {
                next
            }

            max_k <- min(
                m - 1,
                n - j
            )

            for (k in 0:max_k) {

                new_dp[j + k + 1] <-
                    new_dp[j + k + 1] +
                    current / factorial(k)
            }
        }

        dp <- new_dp

        # Once we have allocated 365 birthdays, there is no
        # need to continue.
        if (b == 365) {
            break
        }
    }

    coefficient <- dp[n + 1]

    if (coefficient <= 0 || !is.finite(coefficient)) {
        return(NA_real_)
    }

    # Probability of a particular allocation pattern is
    #
    # n! / 365^n
    #
    # multiplied by the coefficient calculated above.

    log_p_no_match <-
        lgamma(n + 1) -
        n * log(365) +
        log(coefficient)

    p_no_match <- exp(log_p_no_match)

    # Protect against tiny numerical errors.
    p_no_match <- min(
        max(p_no_match, 0),
        1
    )

    1 - p_no_match
}


# =========================================================
# Chapter 6 UI
# =========================================================

chapter6_ui <- function(id){

    ns <- NS(id)

    sidebar_controls <- sidebar(

        h4("Statistics in Context"),

        selectInput(
            ns("demo"),
            "Experiment",
            choices = c(
                "Birthday Problem",
                "Assessing the ITV jinx",
                "Data Dredging"
            )
        ),

        # ==========================================
        # Birthday Problem
        # ==========================================

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'Birthday Problem'",
                ns("demo")
            ),

            radioButtons(
                ns("birthday_type"),
                "Choose birthday investigation",

                choices = c(
                    "Classic birthday problem" = "classic",
                    "Non-Classic birthday problem" = "context"
                ),

                selected = "classic"
            ),


            conditionalPanel(

                condition = sprintf(
                    "input['%s']=='classic'",
                    ns("birthday_type")
                ),

                sliderInput(
                    ns("p_level"),
                    "Probability threshold (p)",
                    min = 0.01,
                    max = 0.99,
                    value = 0.50,
                    step = 0.01
                )

            ),


            conditionalPanel(

                condition = sprintf(
                    "input['%s']=='context'",
                    ns("birthday_type")
                ),

                sliderInput(
                    ns("match_size"),
                    "Number sharing birthday",
                    min = 2,
                    max = 10,
                    value = 4
                ),

                sliderInput(
                    ns("team_small"),
                    "Team size",
                    min = 5,
                    max = 100,
                    value = 20
                ),

                sliderInput(
                    ns("team_large"),
                    "Company size",
                    min = 10,
                    max = 500,
                    value = 100
                ),

                radioButtons(
                    ns("birthday_method"),
                    "Calculation method",

                    choices = c(
                        "Dynamic programming (exact)" = "dp",
                        "Poisson approximation (fast)" = "poisson"
                    ),

                    selected = "poisson"
                ),

                radioButtons(
                    ns("birthday_scale"),
                    "Display scale",

                    choices = c(
                        "Probability" = "prob",
                        "Log probability" = "log"
                    ),

                    selected = "log"
                )

            )

        ),

        # ==========================================
        # Difference in Proportions
        # ==========================================

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'Assessing the ITV jinx'",
                ns("demo")
            ),

            h5("BBC"),

            numericInput(
                ns("trial1"),
                "Number of Matches",
                31
            ),

            numericInput(
                ns("count1"),
                "England Wins",
                21
            ),

            hr(),

            h5("ITV"),

            numericInput(
                ns("trial2"),
                "Number of Matches",
                23
            ),

            numericInput(
                ns("count2"),
                "England Wins",
                9
            ),

            sliderInput(
                ns("alpha"),
                "Confidence level",
                min = 0.80,
                max = 0.99,
                value = 0.95,
                step = 0.01
            ),

            numericInput(
                ns("seed"),
                "Random seed",
                value = sample.int(999, 1),
                min = 1,
                max = 999
            )
        ),

        # ==========================================
        # Data Dredging
        # ==========================================

        conditionalPanel(

            condition = sprintf(
                "input['%s'] == 'Data Dredging'",
                ns("demo")
            ),

            numericInput(
                ns("seed_dredge"),
                "Random seed",
                value = sample.int(999, 1),
                min = 1,
                max = 999
            ),

            sliderInput(
                ns("n_data"),
                "Observations",
                min = 10,
                max = 500,
                value = 100,
                step = 10
            ),

            sliderInput(
                ns("n_var"),
                "Candidate predictors",
                min = 5,
                max = 200,
                value = 50,
                step = 5
            ),

            actionButton(
                ns("show_summary"),
                "Summary",
                icon = icon("book-open")
            )
        )

    )


    # =======================================================

    # Overview

    # =======================================================

    overview_panel <- div(

        card(

            style = "
    border-radius: 16px;
    border: none;
    box-shadow: 0 4px 12px rgba(0,0,0,0.08);
    padding: 10px;
    ",

            # =================================================
            # BIRTHDAY PROBLEM
            # =================================================

            conditionalPanel(

                condition = sprintf(
                    "input['%s'] == 'Birthday Problem'",
                    ns("demo")
                ),

                card_header(
                    div(
                        "Module 6: The birthday problem",
                        style = "
                font-size: 1.4rem;
                font-weight: 700;
                color: #2c3e50;
                "
                    )
                ),

                p(
                    strong(
                        "This example explores how the probability of a shared birthday depends on how the question is defined."
                    )
                ),

                p(
                    "The birthday problem is a useful illustration of how quickly probabilities can change ",
                    "when the event being considered is defined differently. ",
                    "It also shows why apparently surprising probabilities need to be interpreted carefully."
                ),

                hr(),

                h5("Classic birthday problem"),

                p(
                    "The classic problem asks how many people are needed before there is a specified probability ",
                    "that at least two people share the same birthday."
                ),

                p(
                    "The calculation assumes 365 equally likely birthdays. ",
                    "For a group of n people, we can first calculate the probability that everybody has a different ",
                    "birthday and then subtract this from 1."
                ),

                p(
                    "Use the probability threshold in the sidebar to investigate how the required group size changes ",
                    "when we change the probability we are interested in."
                ),

                hr(),

                h5("Non-classic birthday problem"),

                p(
                    "The second investigation changes the question. ",
                    "Instead of asking whether two particular people share a birthday, we can ask whether ",
                    "any group of people contains a specified number who share a birthday."
                ),

                p(
                    "For example, we might ask whether any four people in a group of 20 share the same birthday. ",
                    "This is a different event from asking whether four particular people share a particular date."
                ),

                p(
                    "The Explorer allows you to compare these differently defined events and see how much the ",
                    "probability changes."
                ),

                hr(),

                h5("Calculation methods"),

                p(
                    "For the non-classic problem, the probability can be calculated using either ",
                    strong("dynamic programming"),
                    " or a ",
                    strong("Poisson approximation"),
                    "."
                ),

                p(
                    "The dynamic programming method calculates the probability more directly by tracking how ",
                    "people can be distributed across the 365 possible birthdays without any birthday reaching ",
                    "the specified group size."
                ),

                p(
                    "The Poisson method provides a faster approximation. ",
                    "It treats the number of people associated with an individual birthday as approximately ",
                    "Poisson distributed."
                ),

                hr(),

                h5("How to use the Explorer"),

                tags$ol(

                    tags$li(
                        "Choose either the classic or non-classic birthday problem."
                    ),

                    tags$li(
                        "For the classic problem, choose the probability threshold."
                    ),

                    tags$li(
                        "For the non-classic problem, choose how many people must share a birthday."
                    ),

                    tags$li(
                        "Choose the group sizes and, for the non-classic problem, the calculation method."
                    ),

                    tags$li(
                        "Compare the resulting probabilities and consider how the definition of the event affects the answer."
                    )
                ),

                hr(),

                h5("Questions to investigate"),

                div(
                    style = "
            background-color: #f8f9fa;
            border-left: 5px solid #7B9ACC;
            padding: 12px;
            border-radius: 8px;
            ",

                    tags$ul(

                        tags$li(
                            "How many people are needed for a 50% chance of a shared birthday?"
                        ),

                        tags$li(
                            "How does the required group size change when the probability threshold changes?"
                        ),

                        tags$li(
                            "How different are the probabilities when the event is defined in different ways?"
                        ),

                        tags$li(
                            "How does the Poisson approximation compare with the exact calculation?"
                        ),

                        tags$li(
                            "Why can changing the wording of a probability question produce such a large change in the answer?"
                        )
                    )
                )
            ),


            # =================================================
            # ASSESSING THE ITV JINX
            # =================================================

            conditionalPanel(

                condition = sprintf(
                    "input['%s'] == 'Assessing the ITV jinx'",
                    ns("demo")
                ),

                card_header(
                    div(
                        "Module 6: Assessing the ITV jinx",
                        style = "
                font-size: 1.4rem;
                font-weight: 700;
                color: #2c3e50;
                "
                    )
                ),

                p(
                    strong(
                        "This example investigates an apparent difference in England's win rates under two broadcasters."
                    )
                ),

                p(
                    "The example compares the proportion of matches won by England when games were shown ",
                    "by the BBC and by ITV."
                ),

                p(
                    "An observed difference between two proportions does not necessarily mean that the two ",
                    "underlying probabilities are genuinely different. ",
                    "Samples vary from one set of observations to another, so some difference can arise simply ",
                    "through random variation."
                ),

                hr(),

                h5("Comparing two proportions"),

                p(
                    "The Explorer uses the observed numbers of England wins and matches for the two broadcasters ",
                    "to calculate the difference between the two observed proportions."
                ),

                p(
                    "It then simulates repeated samples under the observed proportions. ",
                    "This provides a way of visualising how much the difference between the two groups can vary ",
                    "from one sample to another."
                ),

                p(
                    "A confidence interval is used to represent uncertainty around the estimated difference."
                ),

                hr(),

                h5("Interpreting the result"),

                p(
                    "If the interval includes zero, a difference of zero remains compatible with the simulated ",
                    "sampling variation represented by the analysis."
                ),

                p(
                    "If the interval does not include zero, the observed difference is further from zero than ",
                    "would be expected under the uncertainty represented by this calculation."
                ),

                p(
                    "This does not by itself establish why the difference occurred. ",
                    "Other factors, such as which matches were shown, when they were played, or how the broadcaster ",
                    "was selected, may also be relevant to the interpretation."
                ),

                hr(),

                h5("How to use the Explorer"),

                tags$ol(

                    tags$li(
                        "Enter the number of matches shown by each broadcaster."
                    ),

                    tags$li(
                        "Enter the number of England wins for each broadcaster."
                    ),

                    tags$li(
                        "Choose the confidence level."
                    ),

                    tags$li(
                        "Choose a random seed if you want to reproduce a particular simulation."
                    ),

                    tags$li(
                        "Examine the distribution of simulated differences and the resulting confidence interval."
                    )
                ),

                hr(),

                h5("Questions to investigate"),

                div(
                    style = "
            background-color: #f8f9fa;
            border-left: 5px solid #7B9ACC;
            padding: 12px;
            border-radius: 8px;
            ",

                    tags$ul(

                        tags$li(
                            "How large is the observed difference between the two broadcasters?"
                        ),

                        tags$li(
                            "How much variation occurs in the simulated differences?"
                        ),

                        tags$li(
                            "What happens when the confidence level is changed?"
                        ),

                        tags$li(
                            "What does it mean if the confidence interval contains zero?"
                        ),

                        tags$li(
                            "Why should an observed association be interpreted in the context of how the data were obtained?"
                        )
                    )
                )
            ),


            # =================================================
            # DATA DREDGING
            # =================================================

            conditionalPanel(

                condition = sprintf(
                    "input['%s'] == 'Data Dredging'",
                    ns("demo")
                ),

                card_header(
                    div(
                        "Module 6: Data dredging",
                        style = "
                font-size: 1.4rem;
                font-weight: 700;
                color: #2c3e50;
                "
                    )
                ),

                p(
                    strong(
                        "This example demonstrates how searching through many possible relationships can produce apparently interesting results by chance."
                    )
                ),

                p(
                    "In this simulation, the response variable and all candidate predictor variables are ",
                    "generated independently. ",
                    "There is therefore no underlying relationship between them."
                ),

                p(
                    "However, the Explorer examines many candidate predictors and selects the one producing ",
                    "the smallest p-value."
                ),

                p(
                    "Because random data contain random fluctuations, some predictors will appear more strongly ",
                    "related to the response than others. ",
                    "When many predictors are examined, it becomes increasingly likely that at least one will ",
                    "produce an apparently impressive result."
                ),

                hr(),

                h5("Searching for patterns"),

                p(
                    "The important point is that the strongest-looking relationship is selected ",
                    "after many possible relationships have been examined."
                ),

                p(
                    "The resulting regression line can therefore look convincing even though the data were ",
                    "generated without any real association."
                ),

                p(
                    "This illustrates why a small p-value does not automatically mean that a genuine relationship ",
                    "has been discovered."
                ),

                hr(),

                h5("Multiple comparisons"),

                p(
                    "Every statistical test gives randomness another opportunity to produce an unusual result."
                ),

                p(
                    "If only one predictor is tested, an unusually small p-value is relatively unusual. ",
                    "If dozens or hundreds of predictors are tested, however, there are many opportunities for ",
                    "one of them to produce a small p-value simply by chance."
                ),

                p(
                    "Selecting the most interesting result from a large collection of analyses can therefore ",
                    "give a misleading impression of the strength of the evidence."
                ),

                hr(),

                h5("How to use the Explorer"),

                tags$ol(

                    tags$li(
                        "Choose the number of observations."
                    ),

                    tags$li(
                        "Choose the number of candidate predictors."
                    ),

                    tags$li(
                        "Choose a random seed if you want to reproduce a particular simulation."
                    ),

                    tags$li(
                        "Press the Summary button to display the regression statistics for the selected relationship."
                    ),

                    tags$li(
                        "Repeat the experiment with different numbers of candidate predictors and different seeds."
                    )
                ),

                hr(),

                h5("Questions to investigate"),

                div(
                    style = "
            background-color: #f8f9fa;
            border-left: 5px solid #7B9ACC;
            padding: 12px;
            border-radius: 8px;
            ",

                    tags$ul(

                        tags$li(
                            "What happens to the smallest p-value when the number of predictors is increased?"
                        ),

                        tags$li(
                            "Can a statistically significant relationship appear when the variables are completely independent?"
                        ),

                        tags$li(
                            "Why does searching through many predictors increase the chance of finding an apparently unusual result?"
                        ),

                        tags$li(
                            "What happens when the experiment is repeated with a different random seed?"
                        ),

                        tags$li(
                            "Why is it important to know how many analyses were performed before interpreting a statistical result?"
                        )
                    )
                )
            )
        )


    )




    # =======================================================
    # Code
    # =======================================================

    code_panel <- div(

        card(

            card_header("Generated R code"),

            p(
                style = "
            color:#666;
            margin-bottom:10px;
        ",
                "The following R code reproduces the calculation shown above."
            ),

            tags$pre(
                style="
                background:#F8F9FA;
                padding:15px;
                border-radius:10px;
                white-space:pre-wrap;
            ",
                textOutput(ns("generated_code"))
            )
        )
    )



    # =======================================================
    # Results
    # =======================================================

    results_panel <- div(

        card(
            card_header("Visualisation"),
            plotOutput(ns("plot"), height = 450)
        ),

        br(),

        uiOutput(ns("results_panel"))
    )


    # =======================================================
    # Learn
    # =======================================================

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

            h5("1. Rare events are not impossible"),

            p(
                "Even very unlikely events will occur if we repeat experiments enough times."
            ),

            hr(),

            h5("2. Statistical methods always produce answers"),

            p(
                "Even when there is no real signal, methods like confidence intervals and regression will still produce results that look meaningful."
            ),

            hr(),

            h5("3. ‘Significance’ does not guarantee truth"),

            p(
                "A statistically significant result can still arise from random variation rather than a real effect."
            ),

            hr(),

            h5("4. Searching creates false discoveries"),

            p(
                "The more hypotheses or patterns we test, the more likely we are to find something that looks important by chance alone."
            ),

            hr(),

            h5("5. Regression can be fooled by noise"),

            p(
                "With enough variables, regression will almost always find relationships—even in purely random data."
            ),

            hr(),

            div(
                style = "
                background-color:#f8f9fa;
                border-left:5px solid #dc3545;
                padding:12px;
                border-radius:8px;
            ",

                h5("Key takeaway"),

                p(
                    strong("Structure can be an illusion."),
                    br(),
                    "Statistical tools are powerful, but they do not distinguish between real patterns and patterns created by randomness. ",
                    "Interpretation matters as much as calculation."
                )
            )
        )
    )


    chapter_page_ui(
        id = id,
        title = "🔍 Module 6: Context",
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

        summary_visible <- reactiveVal(FALSE)


        # -------------------------------------------------
        # Auto-switch to Results tab on experiment change
        # -------------------------------------------------

        observeEvent(input$demo, {

            summary_visible(FALSE)

            updateTabsetPanel(
                session,
                "chapter_tab",
                selected = "Results"
            )


            if(input$demo %in% c(
                "Assessing the ITV jinx",
                "Data Dredging"
            )) {

                updateNumericInput(
                    session,
                    "seed",
                    value = sample.int(999, 1)
                )

            }

        }, ignoreInit = TRUE)


        # -------------------------------------------------
        # Hide data-dredging summary when inputs change
        # -------------------------------------------------

        observeEvent(
            list(
                input$p_level,
                input$count1,
                input$trial1,
                input$count2,
                input$trial2,
                input$alpha,
                input$seed,
                input$seed_dredge,
                input$n_data,
                input$n_var
            ),
            {
                summary_visible(FALSE)
            },
            ignoreInit = TRUE
        )


        # -------------------------------------------------
        # Keep company size above team size
        # -------------------------------------------------

        observeEvent(input$team_small, {

            required_min <- input$team_small + 1

            if(input$team_large < required_min){

                updateSliderInput(
                    session,
                    "team_large",
                    value = required_min,
                    min = required_min
                )

            } else {

                updateSliderInput(
                    session,
                    "team_large",
                    min = required_min
                )

            }

        })


        # -------------------------------------------------
        # Show data-dredging summary
        # -------------------------------------------------

        observeEvent(input$show_summary, {
            summary_visible(TRUE)
        })


        # -------------------------------------------------
        # Reactive analysis
        # -------------------------------------------------

        analysis <- reactive({


            # =================================================
            # Birthday Problem
            # =================================================

            if(input$demo == "Birthday Problem"){


                # -------------------------------------------------
                # Classic birthday problem
                # -------------------------------------------------

                if(input$birthday_type == "classic"){

                    nmax <- 60

                    df <- data.frame(
                        N = 1:nmax,
                        P = sapply(
                            1:nmax,
                            bp
                        )
                    )

                    required_n <- df$N[
                        which(
                            df$P >= input$p_level
                        )[1]
                    ]


                    list(
                        type = "birthday",
                        data = df,
                        required_n = required_n
                    )


                } else {


                    # -------------------------------------------------
                    # Non-classic birthday problem
                    # -------------------------------------------------

                    m <- input$match_size

                    n1 <- input$team_small

                    n2 <- max(
                        input$team_large,
                        n1 + 1
                    )


                    # -------------------------------------------------
                    # Simple probability scenarios
                    # -------------------------------------------------

                    k1 <- 365^m

                    k2 <- 365^(m - 1)


                    # -------------------------------------------------
                    # Calculate probabilities
                    # -------------------------------------------------

                    if(input$birthday_method == "dp"){

                        p1 <- birthday_dp(
                            n1,
                            m
                        )

                        p2 <- birthday_dp(
                            n2,
                            m
                        )

                    } else {

                        p1 <- birthday_poisson(
                            n1,
                            m
                        )

                        p2 <- birthday_poisson(
                            n2,
                            m
                        )

                    }


                    # -------------------------------------------------
                    # Results data frame
                    # -------------------------------------------------

                    data <- data.frame(

                        Scenario = factor(

                            c(
                                paste(
                                    m,
                                    "specific people,\nspecific date"
                                ),

                                paste(
                                    m,
                                    "specific people,\nany date"
                                ),

                                paste(
                                    "Any",
                                    m,
                                    "in group of",
                                    n1
                                ),

                                paste(
                                    "Any",
                                    m,
                                    "in group of",
                                    n2
                                )
                            ),

                            levels = c(

                                paste(
                                    m,
                                    "specific people,\nspecific date"
                                ),

                                paste(
                                    m,
                                    "specific people,\nany date"
                                ),

                                paste(
                                    "Any",
                                    m,
                                    "in group of",
                                    n1
                                ),

                                paste(
                                    "Any",
                                    m,
                                    "in group of",
                                    n2
                                )
                            )
                        ),

                        k = c(
                            k1,
                            k2,
                            1 / p1,
                            1 / p2
                        )
                    )


                    list(

                        type = "birthday_context",

                        data = data,

                        probabilities = c(
                            1 / k1,
                            1 / k2,
                            p1,
                            p2
                        )

                    )

                }

            }


            # =================================================
            # Difference in Proportions
            # =================================================

            else if(
                input$demo == "Assessing the ITV jinx"
            ){

                set.seed(input$seed)

                p1 <- input$count1 / input$trial1

                p2 <- input$count2 / input$trial2

                nsim <- 5000

                s1 <- rbinom(
                    nsim,
                    input$trial1,
                    p1
                ) / input$trial1

                s2 <- rbinom(
                    nsim,
                    input$trial2,
                    p2
                ) / input$trial2

                d <- s1 - s2

                se <- sd(d)

                m <- mean(d)

                qv <- qnorm(
                    (1 + input$alpha) / 2
                )

                ci <- c(
                    m - qv * se,
                    m + qv * se
                )


                list(
                    type = "prop",
                    d = d,
                    se = se,
                    ci = ci,
                    estimate = p1 - p2
                )
            }


            # =================================================
            # Data dredging
            # =================================================

            else {

                set.seed(input$seed_dredge)

                y <- rnorm(
                    input$n_data,
                    0,
                    5
                )

                x <- matrix(
                    rnorm(
                        input$n_var * input$n_data,
                        0,
                        10
                    ),
                    nrow = input$n_var
                )


                pvals <- sapply(
                    1:input$n_var,
                    function(i){

                        summary(
                            lm(
                                y ~ x[i, ]
                            )
                        )$coefficients[2, 4]

                    }
                )


                best <- which.min(pvals)

                xx <- x[best, ]

                fit <- lm(
                    y ~ xx
                )


                list(
                    type = "dredge",
                    x = xx,
                    y = y,
                    coef = summary(fit)$coefficients,
                    minp = min(pvals)
                )
            }

        })


        # =====================================================
        # Code display
        # =====================================================

        output$generated_code <- renderText({

            if(input$demo == "Birthday Problem") {

                if(input$birthday_type == "classic") {

                    paste0(
                        "birthday_classic(\n",
                        "    threshold = ",
                        input$p_level,
                        "\n",
                        ")"
                    )

                } else {

                    paste0(
                        "birthday_context(\n",
                        "    matches = ",
                        input$match_size,
                        ",\n",
                        "    groups = c(",
                        input$team_small,
                        ", ",
                        input$team_large,
                        "),\n",
                        "    method = \"",
                        input$birthday_method,
                        "\"\n",
                        ")"
                    )

                }

            } else if(
                input$demo == "Assessing the ITV jinx"
            ) {

                paste0(
                    "difference_in_proportions(\n",
                    "    wins = c(",
                    input$count1,
                    ", ",
                    input$count2,
                    "),\n",
                    "    matches = c(",
                    input$trial1,
                    ", ",
                    input$trial2,
                    ")\n",
                    ")"
                )

            } else {

                paste0(
                    "data_dredging(\n",
                    "    n_data = ",
                    input$n_data,
                    ",\n",
                    "    n_predictors = ",
                    input$n_var,
                    "\n",
                    ")"
                )

            }

        })


        # =====================================================
        # Plot
        # =====================================================

        output$plot <- renderPlot({

            a <- analysis()


            # =====================================================
            # Classic birthday problem
            # =====================================================

            if(a$type == "birthday"){

                ggplot(
                    a$data,
                    aes(N, P)
                ) +

                    geom_line(
                        colour = "#7B9ACC",
                        linewidth = 1
                    ) +

                    geom_point(
                        colour = "#7B9ACC"
                    ) +

                    geom_hline(
                        yintercept = input$p_level,
                        colour = "red",
                        linetype = "dashed"
                    ) +

                    geom_vline(
                        xintercept = a$required_n,
                        colour = "darkred",
                        linetype = "dotted"
                    ) +

                    annotate(
                        "point",
                        x = a$required_n,
                        y = bp(a$required_n),
                        colour = "red",
                        size = 4
                    ) +

                    theme_minimal(
                        base_size = 14
                    ) +

                    theme(

                        axis.title.x = element_text(
                            size = 16,
                            face = "bold"
                        ),

                        axis.title.y = element_text(
                            size = 16,
                            face = "bold"
                        ),

                        axis.text.x = element_text(
                            size = 14
                        ),

                        axis.text.y = element_text(
                            size = 14
                        )

                    )

            }


            # =====================================================
            # Context-dependent birthday problem
            # =====================================================

            else if(
                a$type == "birthday_context"
            ){

                plot_data <- a$data


                if(input$birthday_scale == "prob"){

                    plot_data$value <- a$probabilities

                    ylab <- "Probability"

                } else {

                    plot_data$value <- log10(
                        pmax(
                            a$probabilities,
                            .Machine$double.xmin
                        )
                    )

                    ylab <- "log(Probability)"

                }

                ggplot(
                    plot_data,
                    aes(
                        x = Scenario,
                        y = value
                    )
                ) +

                    geom_point(
                        size = 4,
                        colour = "#7B9ACC"
                    ) +

                    geom_line(
                        aes(group = 1),
                        colour = "#7B9ACC"
                    ) +

                    labs(
                        y = ylab,
                        x = NULL
                    ) +

                    theme_minimal(
                        base_size = 14
                    ) +

                    theme(

                        axis.title.x = element_text(
                            size = 16,
                            face = "bold"
                        ),

                        axis.title.y = element_text(
                            size = 16,
                            face = "bold"
                        ),

                        axis.text.x = element_text(
                            size = 14,
                            angle = 30,
                            hjust = 1
                        ),

                        axis.text.y = element_text(
                            size = 14
                        )

                    )

            }


            # =====================================================
            # Difference in proportions
            # =====================================================

            else if(a$type == "prop"){

                ggplot(
                    data.frame(d = a$d),
                    aes(d)
                ) +

                    geom_histogram(
                        bins = 20,
                        fill = "#7B9ACC",
                        colour = "white"
                    ) +

                    geom_vline(
                        xintercept = a$ci,
                        colour = "red",
                        linetype = "dashed"
                    ) +

                    theme_minimal(
                        base_size = 14
                    )

            }


            # =====================================================
            # Data dredging
            # =====================================================

            else {

                ggplot(
                    data.frame(
                        x = a$x,
                        y = a$y
                    ),
                    aes(x, y)
                ) +

                    geom_point(
                        colour = "#7B9ACC"
                    ) +

                    geom_smooth(
                        method = "lm",
                        formula = y ~ x,
                        colour = "red"
                    ) +

                    theme_minimal(
                        base_size = 14
                    )
            }

        })


        # =====================================================
        # Results panel
        # =====================================================

        output$results_panel <- renderUI({

            a <- analysis()


            # =================================================
            # Classic birthday problem
            # =================================================

            if(a$type == "birthday"){

                card(

                    card_header("Key result"),

                    h6(
                        sprintf(
                            paste0(
                                "Minimum number of invitees required for there ",
                                "to be a probability of at least %.0f%% that ",
                                "2 or more people share the same birthday is %d."
                            ),

                            100 * input$p_level,

                            a$required_n
                        )
                    )

                )

            }


            # =================================================
            # Context birthday problem
            # =================================================

            else if(
                a$type == "birthday_context"
            ){

                card(

                    card_header(
                        "How surprising is the event?"
                    ),

                    p(
                        "The probability depends strongly on how the question is framed."
                    ),

                    tags$table(

                        class = "table",

                        tags$tr(
                            tags$th("Scenario"),
                            tags$th("Probability"),
                            tags$th("Reciprocal probability")
                        ),

                        lapply(
                            1:nrow(a$data),

                            function(i){

                                tags$tr(

                                    tags$td(
                                        a$data$Scenario[i]
                                    ),

                                    tags$td(
                                        sprintf(
                                            "%.6f",
                                            a$probabilities[i]
                                        )
                                    ),

                                    tags$td(
                                        format(
                                            round(
                                                a$data$k[i]
                                            ),
                                            big.mark = ","
                                        )
                                    )

                                )

                            }
                        )

                    ),

                    br(),

                    accordion(

                        open = FALSE,

                        accordion_panel(

                            "How are these probabilities calculated?",

                            h5(
                                "Exact dynamic programming approach"
                            ),

                            p(
                                "The exact calculation treats birthdays as 365 possible boxes. ",
                                "It tracks the probability that people can be allocated to these ",
                                "boxes without any box reaching the chosen group size."
                            ),

                            p(
                                "The calculation keeps only states where no birthday has yet ",
                                "reached m people. At the end, the probability of interest is found ",
                                "by subtracting this probability from 1."
                            ),

                            withMathJax(),

                            p(
                                "$$P(\\text{at least }m)=1-P(\\text{maximum birthday count}<m)$$"
                            ),

                            hr(),

                            h5(
                                "Poisson approximation"
                            ),

                            p(
                                "The approximation treats the number of people sharing any one ",
                                "birthday as approximately Poisson distributed."
                            ),

                            p(
                                "The expected number of people on each birthday is:"
                            ),

                            p(
                                "$$\\lambda=\\frac{n}{365}$$"
                            ),

                            p(
                                "The probability that every birthday has fewer than m people is ",
                                "approximated by multiplying the probability for one birthday ",
                                "across all 365 birthdays."
                            ),

                            p(
                                "$$P(\\text{at least }m)",
                                "\\approx",
                                "1-[P(X<m)]^{365}$$"
                            )

                        )
                    )

                )

            }


            # =================================================
            # Difference in proportions
            # =================================================

            else if(a$type == "prop"){

                inside <-
                    0 >= a$ci[1] &&
                    0 <= a$ci[2]

                sig_level <-
                    100 * (1 - input$alpha)


                conclusion <- if(inside) {

                    sprintf(
                        "No evidence of an ITV jinx at the %.1f%% significance level.",
                        sig_level
                    )

                } else {

                    sprintf(
                        "Some evidence of an ITV jinx at the %.1f%% significance level.",
                        sig_level
                    )

                }


                card(

                    card_header(
                        "Difference in proportions"
                    ),

                    p(
                        sprintf(
                            "Estimated difference (p1 - p2): %.3f",
                            a$estimate
                        )
                    ),

                    p(
                        sprintf(
                            "SE: %.4f",
                            a$se
                        )
                    ),

                    p(
                        sprintf(
                            "%.0f%% Confidence interval: [%.3f, %.3f]",
                            100 * input$alpha,
                            a$ci[1],
                            a$ci[2]
                        )
                    ),

                    hr(),

                    strong(conclusion)

                )

            }


            # =================================================
            # Data dredging
            # =================================================

            else {

                if(!summary_visible()) {
                    return(NULL)
                }


                card(

                    card_header(
                        "Data dredging result"
                    ),

                    p(
                        "The graph shows the strongest apparent relationship between Y and the ",
                        "candidate predictor variables generated in this simulation."
                    ),

                    p(
                        "The regression statistics for this selected relationship are:"
                    ),

                    tags$ul(

                        tags$li(
                            sprintf(
                                "Gradient estimate: %.3f",
                                a$coef[2, 1]
                            )
                        ),

                        tags$li(
                            sprintf(
                                "Standard error: %.3f",
                                a$coef[2, 2]
                            )
                        ),

                        tags$li(
                            sprintf(
                                "Smallest p-value: %.5f",
                                a$minp
                            )
                        )

                    ),

                    hr(),

                    p(
                        "In the simulation, Y and all candidate predictors were generated independently, ",
                        "so the true regression gradient is zero."
                    ),

                    p(
                        "However, after examining many candidate predictors, one of them will often ",
                        "appear to have a statistically significant relationship with Y purely by chance."
                    ),

                    p(
                        strong("Key lesson: "),
                        "when many possible relationships are investigated, the most extreme result ",
                        "often looks meaningful even when every variable is completely unrelated. ",
                        "Data dredging turns random variation into apparently convincing evidence."
                    )

                )

            }

        })

    })
}

