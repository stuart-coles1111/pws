suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(ggplot2)
    library(dplyr)
    library(purrr)
    library(scales)
})

# =========================================================
# PERMUTATIONS
# =========================================================

permute <- function(x) {
    if (length(x) == 1)
        return(list(x))

    out <- list()

    for (i in seq_along(x)) {
        rest <- x[-i]
        perms <- permute(rest)

        for (p in perms) {
            out <- append(out, list(c(x[i], p)))
        }
    }

    out
}

movies <- c("A", "B", "C", "D")

all_orders <- permute(movies)

perm_df <- tibble(ordering = map_chr(all_orders, ~ paste(.x, collapse =
                                                             " ")))

valid_order <- function(ord) {
    x <- strsplit(ord, " ")[[1]]

    posA <- match("A", x)
    posB <- match("B", x)
    posC <- match("C", x)

    posC > posA & posC > posB
}

perm_df$valid <- map_lgl(perm_df$ordering, valid_order)

# =========================================================
# UTILITY FUNCTION
# =========================================================

reference_wealth <- 0.5

utility_function <- function(x, lambda = 1) {
    delta <- x - reference_wealth

    ifelse(delta >= 0, delta, lambda * delta)
}

# =========================================================
# DISTRIBUTION SETTINGS
# =========================================================

shape_param <- 2.2
scale_param <- 18

# =========================================================
# UI
# =========================================================
ui <- page_navbar(
    theme = bs_theme(
        version = 5,
        bootswatch = "minty",
        base_font = font_google("Inter")
    ),

    header = tagList(tags$head(tags$style(
        HTML(
            "

                .main-title{
                    background:linear-gradient(90deg,#A8DADC,#CDB4DB);
                    padding:22px;
                    border-radius:16px;
                    margin-bottom:22px;
                    text-align:center;
                }

                .card-style{
                    background:white;
                    border-radius:16px;
                    padding:22px;
                    margin-bottom:20px;
                    box-shadow:0 3px 12px rgba(0,0,0,0.08);
                }

                .big{
                    font-size:30px;
                    font-weight:700;
                    text-align:center;
                }

                .perm-grid{
                    display:grid;
                    grid-template-columns:repeat(4,1fr);
                    gap:8px;
                    margin-top:18px;
                }

                .perm-box{
                    font-family:monospace;
                    padding:10px;
                    border-radius:8px;
                    text-align:center;
                    font-size:16px;
                }

                .neutral{ background:#ECEFF4; }
                .valid{ background:#D8F3DC; }
                .invalid{
                    background:#F8D7DA;
                    opacity:0.2;
                    text-decoration:line-through;
                }

                .explain{
                    font-size:18px;
                    line-height:1.7;
                }

                .formula-box{
                    background:#F4F8FC;
                    border-radius:12px;
                    padding:18px;
                    margin-top:18px;
                    text-align:center;
                }

                .info-box{
                    background:#F8F9FB;
                    padding:18px;
                    border-radius:14px;
                    line-height:1.7;
                }

                .hero-card{
                    background:linear-gradient(135deg,#457B9D,#A8DADC);
                    color:#1D3557;
                    border-radius:22px;
                    padding:35px;
                    text-align:center;
                    box-shadow:0 8px 25px rgba(0,0,0,0.12);
                    margin-bottom:25px;
               }

                .money-display{
                    font-size:42px;
                    font-weight:800;
                    color:#FFD166;
                    margin:20px;
                }

                .movie-option{
                    background:white;
                    color:#1D3557;
                    padding:12px;
                    border-radius:12px;
                    margin:8px auto;
                    max-width:450px;
                    font-size:20px;
                    font-weight:600;
                }

                .progress-box{
                    background:#F4F8FC;
                    border-radius:20px;
                    padding:10px;
                    text-align:center;
                    margin-bottom:20px;
                    font-weight:600;
                }

                .dilemma-box{
                    background:#FFF3CD;
                    border-left:6px solid #E0A800;
                    padding:18px;
                    border-radius:10px;
                    font-size:20px;
                    margin-top:20px;
                }

                .card-style{
                    animation:fadeIn .5s ease-in;
                }

                @keyframes fadeIn{
                    from{
                        opacity:0;
                        transform:translateY(10px);
                    }
                    to{
                        opacity:1;
                        transform:translateY(0);
                    }
                }

                                .btn-primary{
                    background:#457B9D;
                    border:none;
                    border-radius:12px;
                    padding:12px 25px;
                    font-size:18px;
                }

                .btn-primary:hover{
                    background:#1D3557;
                }

                .millionaire-panel {
    background:
        radial-gradient(circle at top, #243b7a 0%, #090f2c 70%);
    border-radius:20px;
    padding:35px;
    color:white;
    text-align:center;
    box-shadow:
        0 10px 30px rgba(0,0,0,0.4),
        inset 0 0 30px rgba(255,215,0,0.15);
    margin-bottom:25px;
}

.question-number {
    color:#FFD700;
    font-size:22px;
    font-weight:700;
    letter-spacing:2px;
}

.prize {
    color:#FFD700;
    font-size:44px;
    font-weight:800;
    text-shadow:0 0 10px rgba(255,215,0,0.5);
    margin:15px;
}

.millionaire-question {
    background:
        linear-gradient(90deg,#10194a,#1d3275);

    border:3px solid #66D9FF;

    padding:25px 45px;

    margin:25px auto;

    max-width:850px;

    font-size:26px;
    font-weight:700;

    color:white;

    text-align:center;

    clip-path:polygon(
        4% 0%,
        96% 0%,
        100% 50%,
        96% 100%,
        4% 100%,
        0% 50%
    );

    box-shadow:
        0 0 15px rgba(102,217,255,0.5);
}


.answer-choice {

    background:
        linear-gradient(90deg,#10194a,#263c8f);

    border:3px solid #66D9FF;

    color:white;

    padding:15px 35px;

    height:70px;

    display:flex;

    align-items:center;

    font-size:20px;

    font-weight:600;


    clip-path:polygon(
        8% 0%,
        92% 0%,
        100% 50%,
        92% 100%,
        8% 100%,
        0% 50%
    );

    transition:0.2s;

}


.answer-choice:hover {

    background:#C9A227;

    color:#10194a;

}
.answer-choice:hover {
    background:#C9A227;
    color:#10194a;
}

.naive-choice {

    background:
        linear-gradient(135deg,#15245c,#273d8f);

    border:3px solid #66D9FF;

    color:white;

    padding:25px;

    margin:15px;

    text-align:center;

    border-radius:15px;

    font-size:24px;

    font-weight:700;

    box-shadow:
        0 0 15px rgba(102,217,255,0.35);

}


.naive-percent {

    color:#FFD700;

    font-size:40px;

    margin-top:10px;

}
            "
        )
    )), div(
        class = "main-title", h1("🧠 Activity 2: Who Wants to be a Danish Millionaire?")
    )),

    # =====================================================
    # OVERVIEW
    # =====================================================

    overview_page(

        explanation = tagList(

            p(
                "This activity introduces conditional probability and decision making under uncertainty."
            ),

            p(
                "Students explore how probabilities change when new information becomes available before extending these ideas to probability distributions, expected utility and behavioural decision making."
            )

        ),

        individual = tagList(

            tags$ol(
                tags$li("Work through each stage of the activity independently."),
                tags$li("Predict probabilities before revealing new information."),
                tags$li("Interpret how the sample space changes."),
                tags$li("Explore the decision-making scenarios.")
            )

        ),

        group = tagList(

            tags$ol(
                tags$li("Compare probability calculations with classmates."),
                tags$li("Discuss why conditioning changes probabilities."),
                tags$li("Compare different attitudes towards risk."),
                tags$li("Relate the examples to real-world decision making.")
            )

        ),

        question = tagList(

            tags$ul(
                tags$li("Naively it seems Balder has a 50-50 choice for the final answer. Why might that reasoning be too simplistic?"),
                tags$li("What is the correct way to interpret Balder's knowledge, and how does it affect the probability of the final answer outcomes?"),
                tags$li("Even if the final answer outcomes are 50-50, can an argument be made that Balder should have answered the question anyway?"),
                tags$li("What additional factors does this depend on?"),
                tags$li("Taking everything into account, do you think Balder made the right choice?"),

            )

        )

    ),

    # =====================================================
    # ACTIVITY
    # =====================================================

    nav_panel(
        "Activity",
        uiOutput("page_ui")
    )
)


# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session) {
    rv <- reactiveValues(page = 1, reveal = FALSE)

    observeEvent(input$next1, {
        rv$page <- 2
    })

    observeEvent(input$next2, {
        rv$page <- 3
        rv$reveal <- FALSE
    })

    observeEvent(input$reveal_btn, {
        rv$reveal <- TRUE
    })

    observeEvent(input$next3, {
        rv$page <- 4
    })

    observeEvent(input$why_formula, {
        rv$page <- 5
    })

    observeEvent(input$next4, {
        rv$page <- 6
    })

    observeEvent(input$back1, {
        rv$page <- 1
    })

    observeEvent(input$back2, {
        rv$page <- 2
    })

    observeEvent(input$back3, {
        rv$page <- 3
    })

    observeEvent(input$back4, {
        rv$page <- 4
    })

    observeEvent(input$back5, {
        rv$page <- 5
    })

    observeEvent(input$next5, {
        rv$page <- 6
    })

    observeEvent(input$reset, {
        rv$page <- 1
        rv$reveal <- FALSE

        updateSliderInput(session, "z", value = 18)
        updateSliderInput(session, "lambda", value = 1)
        updateSliderInput(session, "p", value = 0.5)
    })

    # =====================================================
    # PAGE RENDER
    # =====================================================

    output$page_ui <- renderUI({
        if (rv$page == 1) {
            fluidRow(column(
                12,

                div(
                    class = "millionaire-panel",

                    div(
                        class = "question-number",
                        "QUESTION 15"
                    ),

                    div(
                        class = "prize",
                        "1,000,000 kr"
                    ),

                    div(
                        class = "millionaire-question",
                        "Which Danish comedy movie premiered first?"
                    ),

                    fluidRow(

                        column(
                            6,
                            div(
                                class = "answer-choice",
                                "A) Sover Dolly på Ryggen"
                            )
                        ),

                        column(
                            6,
                            div(
                                class = "answer-choice",
                                "B) Klassefesten"
                            )
                        ),

                        column(
                            6,
                            div(
                                class = "answer-choice",
                                "C) Blå Mænd"
                            )
                        ),

                        column(
                            6,
                            div(
                                class = "answer-choice",
                                "D) Superclasico"
                            )
                        )

                    )
                ),

                div(
                    class = "card-style",

                    h3("🧩 What does Balder know?"),

                    div(
                        class = "info-box",

                        div(
                            style = "font-size:20px; line-height:1.8;",

                            HTML(
                                "<b>Fact 1.</b><br>
                Balder knows that Movie <b>C</b> is older than Movies <b>A</b> and <b>B</b>."
                            )
                        )
                    ),

                    br(),

                    div(
                        class = "info-box",

                        div(
                            style = "font-size:20px; line-height:1.8;",

                            HTML(
                                "<b>Fact 2.</b><br>
                Balder does not know whether Movie C or Movie D is older."
                            )
                        )
                    ),

                    br(),

                    div(
                        class = "dilemma-box",

                        HTML(
                            "
            <div style='font-size:24px;font-weight:700;margin-bottom:12px;'>
            The decision
            </div>

            <div style='font-size:20px;line-height:1.8;'>

            Balder has two choices:

            <ul>

                <li>
                <b>Walk away</b> and keep <b>500,000 kroner</b>.
                </li>

                <li>
                <b>Answer the question</b>, which has two possible outcomes:

                    <ul>

                        <li>Correct &rarr; <b>1,000,000 kroner</b>.</li>

                        <li>Incorrect &rarr; <b>50,000 kroner</b>.</li>

                    </ul>

                </li>

            </ul>

            Should Balder answer the question or decline and keep the 500,000 kroner?

            </div>
            "
                        )

                    ),

                    br(),

                    actionButton("next1", "Explore arguments →")

                )

            ))
        }

        else if (rv$page == 2) {

            fluidRow(

                column(
                    12,

                    div(
                        class = "card-style",

                        h2("A tempting argument"),

                        div(
                            class = "explain",

                            p("Balder knows:"),

                            tags$ul(
                                tags$li("Movie C is older than Movie A."),
                                tags$li("Movie C is older than Movie B.")
                            ),

                            p(
                                "Therefore, the oldest movie must be either C or D."
                            ),

                            p(
                                "A natural conclusion is that C and D are now equally likely to be the oldest movie."
                            )
                        ),

                        fluidRow(

                            column(
                                6,

                                div(
                                    class = "naive-choice",

                                    "Movie C",

                                    div(
                                        class = "naive-percent",
                                        "50%"
                                    )
                                )

                            ),

                            column(
                                6,

                                div(
                                    class = "naive-choice",

                                    "Movie D",

                                    div(
                                        class = "naive-percent",
                                        "50%"
                                    )
                                )

                            )

                        ),

                        div(
                            class = "dilemma-box",

                            HTML("
    <b>But there is a hidden assumption.</b>

    <br><br>

    We have treated Balder's information as if it only tells us:

    <br><br>

    <center>
    <b>The oldest movie is either C or D.</b>
    </center>

    <br>

    What if the details of the information matter?
    ")
                        ),

                        br(),

                        div(
                            class = "dilemma-box",

                            HTML("
                    <b>Reflection.</b><br><br>

                    This argument sounds very reasonable.

                    But have we really used all of Balder's information?
                    ")
                        ),

                        br(),


                        actionButton("back1","← Back"),
                        actionButton("next2","Look more carefully →")

                    )
                )
            )
        }

        else if (rv$page == 3) {
            fluidRow(column(
                12,

                div(
                    class = "card-style",

                    h3("Information changes probabilities"),

                    div(class = "explain", if (!rv$reveal) {
                        HTML(
                            "
                                    Without additional information, all 24 orderings are equally likely.
                                    In 6 of these, C is oldest.<br><br>

                                    <center style='font-size:28px;font-weight:700;'>
                                    P(C is oldest) = 6/24 = 1/4
                                    </center>
                                "
                        )

                    } else {
                        HTML(
                            "
                                    After applying Balder’s information, 8 orderings remain.
                                    In 6 of these, C is oldest.<br><br>

                                    <center style='font-size:28px;font-weight:700;'>
                                    P(C is oldest | info) = 6/8 = 3/4
                                    </center>
                                "
                        )
                    }),

                    br(),

                    if (!rv$reveal) {
                        actionButton("reveal_btn", "Apply information")
                    },

                    div(class = "perm-grid", lapply(1:nrow(
                        perm_df
                    ), function(i) {
                        row <- perm_df[i, ]
                        cls <- "neutral"

                        if (rv$reveal) {
                            cls <- if (row$valid)
                                "valid"
                            else
                                "invalid"
                        }

                        div(class = paste("perm-box", cls), row$ordering)
                    })),

                    if (rv$reveal) {
                        div(
                            style = "
                                    margin-top:25px;
                                    padding:18px;
                                    background:#FFF3CD;
                                    border-left:6px solid #E0A800;
                                    border-radius:10px;
                                    font-size:18px;
                                    line-height:1.7;
                                ",

                            HTML(
                                "
                                    <b>Reflection.</b><br>
                                    We assumed all remaining orderings are equally likely.<br><br>

                                    But is that reasonable?
                                    Could some still be more likely than others?
                                "
                            )
                        )
                    },

                    br(),

                    actionButton("back2", "← Back"),
                    actionButton("next3", "Next →")
                )
            ))
        }

        else if (rv$page == 4) {

            tagList(

                div(
                    class = "card-style",

                    h3("Formulating Balder's information as a probability model"),

                    div(
                        class = "info-box",

                        HTML(
                            "
                    <p>
                    Since Balder knows nothing about the relative ages of Movies
                    <b>C</b> and <b>D</b>, it is reasonable to model their ages as
                    independent random variables drawn from the same unknown
                    distribution with cumulative distribution function
                    <b>F(z)</b>.
                    </p>

                    <p>
                    Balder also knows that Movie <b>C</b> is older than Movies
                    <b>A</b> and <b>B</b>. We represent this information by
                    conditioning on the event
                    <b>C &gt; z</b>, where <b>z</b> denotes the age of the older
                    of Movies <b>A</b> and <b>B</b>.
                    </p>

                    <p style='text-align:center;font-size:28px;font-weight:700;'>
                    P(C &gt; D &nbsp;|&nbsp; C &gt; z)
                    </p>

                    <p>
                    This probability depends on the unknown distribution
                    <b>F</b>. To illustrate the effect of Balder's information,
                    we use the example probability density function below.
                    </p>
                    "
                        )

                    )

                ),

                fluidRow(

                    column(
                        4,

                        div(
                            class = "card-style",

                            h3("Probability model"),

                            sliderInput(
                                "z",
                                "Threshold z",
                                min = 0,
                                max = 50,
                                value = 18,
                                step = 0.5
                            ),

                            div(class = "formula-box", uiOutput("tail_probability")),

                            br(),

                            p(
                                "As z increases, fewer movies are older than z, so P(C > z) becomes smaller.",
                                class = "explain"
                            ),

                            actionButton("back3", "← Back"),
                            actionButton("next4", "Next →")
                        )
                    ), column(
                        8,

                        div(
                            class = "card-style",

                            h4("Probability density function of age of Movies C and D"),

                            plotOutput("dist_plot", height = "450px"),

                            div(
                                style = "
                                margin-top:20px;
                                padding:18px;
                                background:#F4F8FC;
                                border-left:6px solid #7B9ACC;
                                border-radius:10px;
                                font-size:20px;
                                line-height:1.8;
                                text-align:center;
                            ",

                                uiOutput("p_cd_panel")
                            ),

                            br(),

                            actionButton("why_formula", "Why does this formula work?")
                        )
                    ))
            )
        }

        else if (rv$page == 5) {
            fluidRow(column(
                12,

                div(
                    class = "card-style",

                    h3("Why does the formula work?"),

                    p("We know that Movie C is older than z.", class =
                          "explain"),

                    p("Now consider two possible cases for Movie D:", class =
                          "explain"),

                    fluidRow(column(
                        6,

                        div(
                            style = "
                                        background:#D8F3DC;
                                        padding:20px;
                                        border-radius:12px;
                                        min-height:260px;
                                    ",

                            h4("Case 1: Movie D is less than z years old"),

                            p("This happens with probability F(z).", class =
                                  "explain"),

                            p(
                                "If D is younger than z, then C must automatically be older than D.",
                                class = "explain"
                            ),

                            div(
                                style = 'font-size:28px;font-weight:700;text-align:center;',
                                HTML("P(C &gt; D) = 1")
                            )
                        )
                    ), column(
                        6,

                        div(
                            style = "
                                        background:#F4F8FC;
                                        padding:20px;
                                        border-radius:12px;
                                        min-height:260px;
                                    ",

                            h4("Case 2: Movie D is more than z years old"),

                            p("This happens with probability 1 − F(z).", class =
                                  "explain"),

                            p("Now both C and D are older than z.", class =
                                  "explain"),

                            p(
                                "By symmetry, each movie is equally likely to be older than the other.",
                                class = "explain"
                            ),

                            div(
                                style = 'font-size:28px;font-weight:700;text-align:center;',
                                HTML("P(C &gt; D) = 1/2")
                            )
                            )
                    )),

                    br(),

                    div(
                        style = "
                                background:#FFF3CD;
                                padding:22px;
                                border-radius:12px;
                                text-align:center;
                                font-size:22px;
                                line-height:1.8;
                            ",

                        HTML(
                            "
    Putting things together:

    <br><br>

    <span style='font-size:24px;'>

    P(C &gt; D &nbsp;|&nbsp; C &gt; z)

    <br>

    = F(z) &times; 1 + (1 - F(z)) &times; 1/2

    <br>

    = (1 + F(z))/2

    </span>
    "
                        )
                    ),

                    br(),

                    actionButton("back4", "← Back"),
                    actionButton("next5", "Next →")
                )
            ))
        }

        else if (rv$page == 6) {
            fluidRow(column(
                4,

                div(
                    class = "card-style",

                    h3("Loss aversion"),

                    sliderInput("lambda", "λ", 0, 3, 1, 0.01),
                    sliderInput("p", "Win Probability", 0, 1, 0.5, 0.01),

                    hr(),

                    actionButton("back4", "← Back"),
                    actionButton("reset", "Restart")
                )
            ), column(
                8,

                div(
                    class = "card-style",

                    div(
                        style = "
                                margin-bottom:22px;
                                padding:18px;
                                background:#F4F8FC;
                                border-left:6px solid #7B9ACC;
                                border-radius:10px;
                                font-size:18px;
                                line-height:1.8;
                            ",

                        HTML(
                            "
                                Whatever Balder’s calculation for the probabilities of the various movies being oldest,
                                he must also consider what he stands to gain or lose from answering the question,
                                and how much those outcomes matter to him.<br><br>

                                This can be analysed formally using <b>decision analysis</b>,
                                which combines probabilities with the happiness (or utility)
                                associated with different outcomes.<br><br>

                                The goal is to compare Balder’s <b>expected happiness</b>
                                under the different actions available to him.

                                Experiment by changing either or both of the win probability for the final question
                                and λ which detewrmines Balder's relative change in happiness when winning and losing money.

                                (The exact role of λ can be seen in the Happiness Function plot).
                            "
                        )
                    ),

                    br(),

                    div(
                        class = "info-box",

                        HTML(
                            "
        <p>
        The happiness function measures changes relative to Balder's current
        position: walking away with <b>500,000 kroner</b>.
        </p>

        <p>
        For a chosen happiness function, we can calculate how Balder's happiness
        changes if he answers the final question and is either correct or incorrect.
        These changes are shown in the happiness plot.
        </p>

        <p>
        By combining these possible changes with the probability of answering
        correctly, we can calculate Balder's expected change in happiness from
        choosing to answer.
        </p>

        <p>
        If this expected change is positive, answering the question increases
        Balder's expected happiness, so answering is the optimal choice.
        If it is negative, Balder should walk away.
        </p>
        "
                        )

                    ),

                    br(),

                    h4("Happiness function"),

                    plotOutput("utility_plot", height = "300px"),

                    hr(),

                    h4("Expected happiness"),

                    plotOutput("eu_plot", height = "300px"),

                    hr(),

                    uiOutput("decision_text")
                )
            ))
        }
    })

    # =====================================================
    # PAGE 3 REACTIVES
    # =====================================================

    current_values <- reactive({
        z <- input$z

        Fz <- pweibull(z, shape_param, scale_param)
        tail_prob <- 1 - Fz

        p_cd <- (1 + Fz) / 2

        list(
            z = z,
            Fz = Fz,
            tail_prob = tail_prob,
            p_cd = p_cd
        )
    })

    output$tail_probability <- renderUI({
        vals <- current_values()

        HTML(
            paste0(
                "<div style='font-size:28px;font-weight:700;'>
            P(C > z) = ",
                round(vals$tail_prob, 3),
                "</div>"
            )
        )
    })

    output$p_cd_panel <- renderUI({
        vals <- current_values()

        HTML(
            paste0(
                "
            <div style='font-size:20px; line-height:1.8;'>

            Under this model,

            <br><br>

            <span style='font-size:28px;font-weight:600;'>

            P(C &gt; D &nbsp;|&nbsp; C &gt; z)

            <br>

            = (1 + F(z))/2

            <br>

            = <span style='font-size:38px;font-weight:700;color:#4A6FA5;'>",
                round(vals$p_cd, 3),
                "</span>

            </span>

            </div>
            "
            )
        )
    })

    output$dist_plot <- renderPlot({
        vals <- current_values()

        x <- seq(0, 50, length.out = 1000)
        y <- dweibull(x, shape_param, scale_param)

        df <- data.frame(x, y)

        ymax <- max(y)

        ggplot(df, aes(x, y)) +

            geom_area(fill = "#DDE5F2") +

            geom_area(data = subset(df, x >= vals$z),
                      fill = "#7B9ACC") +

            annotate(
                "segment",
                x = vals$z,
                xend = vals$z,
                y = 0,
                yend = ymax,
                linetype = "dashed",
                linewidth = 1
            ) +

            annotate(
                "text",
                x = vals$z,
                y = -0.003,
                label = "z",
                size = 7,
                fontface = "bold"
            ) +

            coord_cartesian(clip = "off") +

            theme_minimal(base_size = 18) +

            labs(x = "Movie age", y = "Probability density")
    })

    # =====================================================
    # PAGE 5
    # =====================================================

    output$utility_plot <- renderPlot({
        lambda <- input$lambda

        x <- seq(0, 1, length.out = 500)
        y <- utility_function(x, lambda)

        ggplot(data.frame(x, y), aes(x, y)) +

            geom_line(color = "#7B9ACC", linewidth = 1.3) +

            geom_vline(xintercept = 0.5, linetype = "dashed") +

            theme_minimal(base_size = 20) +

            theme(axis.title = element_text(size = 18),
                  axis.text  = element_text(size = 16)) +

            labs(x = "Wealth (millions of kroner)", y = "Happiness")
    })

    output$eu_plot <- renderPlot({
        lambda <- input$lambda
        p <- input$p

        u <- function(x)
            utility_function(x, lambda)

        df <- data.frame(outcome = c("Win", "Lose"),
                         contrib = c(p * u(1), (1 - p) * u(0.064)))

        ggplot(df, aes(outcome, contrib, fill = outcome)) +

            geom_col(width = 0.6) +

            scale_fill_manual(values = c("#7B9ACC", "#F8D7DA")) +

            theme_minimal(base_size = 20) +

            theme(
                axis.title = element_text(size = 18),
                axis.text  = element_text(size = 16),
                legend.position = "none"
            ) +

            labs(x = "Outcome", y = "Expected happiness")
    })

    output$decision_text <- renderUI({
        lambda <- input$lambda
        p <- input$p

        u <- function(x)
            utility_function(x, lambda)

        EU_play <- p * u(1) + (1 - p) * u(0.064)
        EU_quit <- u(0.5)

        decision <- if (EU_play > EU_quit) {
            "Optimal strategy is Play"
        } else {
            "Optimal strategy is Quit"
        }

        HTML(
            paste0(
                "<b>If Play:</b> ",
                round(EU_play, 3),
                "<br>",
                "<b>If Quit:</b> ",
                round(EU_quit, 3),
                "<br><br>",
                "<center style='font-size:22px;font-weight:bold;'>",
                decision,
                "</center>"
            )
        )
    })
}

# =========================================================
# RUN APP
# =========================================================

shinyApp(ui, server)
