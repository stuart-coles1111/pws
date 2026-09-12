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
    theme = pws_theme(),

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
    margin-bottom:18px;
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
                "This interactive activity accompanies the discussion of probability and decision making in the book. It uses a simple game-show scenario to explore how we should reason about probability when we have partial information, and how probabilistic beliefs can ultimately inform decisions."
            ),
            p(
                "The activity begins with a seemingly straightforward question: given what Balder knows about the ages of four movies, how likely is it that Movie C is the oldest? We first examine the tempting 50-50 argument, before looking more carefully at what Balder's information actually tells us."
            ),
            p(
                "We then use the example to explore conditional probability and probability modelling. In particular, we consider how information about C's age can affect the probability that C is older than D, and how this depends on assumptions about the unknown distribution of movie ages."
            ),
            p(
                "The final part turns from probability to decision making. Even if we can calculate the probability that Balder will answer correctly, this does not by itself determine whether he should take the question. The decision also depends on the possible gains and losses, and on how Balder values those gains and losses. We capture this using a simple happiness function and explore how probability and preferences together determine an optimal strategy."
            ),
            div(
                class = "info-box",
                HTML(
                    "
                <b>How to use this activity</b><br><br>

                The activity is designed primarily for use in <b>group meetings</b>.
                The different stages provide opportunities for discussion,
                comparison of intuitions, and examination of the assumptions
                behind different probability arguments.

                <br><br>

                It can also be used <b>individually</b>. If working alone,
                pause at each stage and consider what you think the answer
                should be, and why, before moving on. The aim is not simply
                to obtain the final answer, but to explore the reasoning
                that leads to it.
                "
                )
            )
        ),

        individual = tagList(
            h4("If you are working individually"),
            tags$ol(
                tags$li(
                    "Work through the activity at your own pace."
                ),
                tags$li(
                    "Before revealing the next step, try to decide what you think the answer should be and formulate your own reasoning."
                ),
                tags$li(
                    "Pay attention to the assumptions being made about the information Balder has and about the things he does not know."
                ),
                tags$li(
                    "Experiment with the interactive elements and consider how changing the assumptions affects the conclusions."
                )
            )
        ),

        group = tagList(
            h4("If you are working in a group"),
            tags$ol(
                tags$li(
                    "Discuss the initial 50-50 argument before moving to the next stage. Why does it seem reasonable?"
                ),
                tags$li(
                    "Ask what Balder's information actually tells us, and what it leaves unknown."
                ),
                tags$li(
                    "Compare the different probability arguments and discuss the assumptions on which they depend."
                ),
                tags$li(
                    "In the final section, discuss whether the probability of winning is enough to determine what Balder should do."
                ),
                tags$li(
                    "Consider how different attitudes towards gains and losses might lead to different decisions."
                )
            )
        ),

        question = tagList(
            h4("Questions to consider"),
            tags$ul(
                tags$li(
                    "Why does the apparently natural 50-50 argument seem convincing?"
                ),
                tags$li(
                    "What exactly does Balder's information tell us, and what does it not tell us?"
                ),
                tags$li(
                    "How does conditioning on Balder's information change the probability that C is the oldest?"
                ),
                tags$li(
                    "What assumptions are needed to construct a probability model for the ages of C and D?"
                ),
                tags$li(
                    "How does the probability of answering correctly affect the decision to play?"
                ),
                tags$li(
                    "Why might the probability of winning not be sufficient to determine the best decision?"
                ),
                tags$li(
                    "How does Balder's attitude towards gains and losses affect the decision?"
                ),
                tags$li(
                    "Taking probabilities and consequences together, when should Balder answer the question?"
                )
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

                    h3("🧩  Balder's Information?"),

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
                Balder does not know which of Movie C or Movie D is oldest."
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

                        <li>Correct &rarr; <b> Win 1,000,000 kroner</b>.</li>

                        <li>Incorrect &rarr; <b>Win 50,000 kroner</b>.</li>

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

    We have treated Balder's knowledge as if it only tells us:

    <br><br>

    <center>
    <b>The oldest movie is either C or D.</b>
    </center>

    <br>

    Is this a correct interpretation?
    ")
                        ),

                        br(),

                        div(
                            class = "dilemma-box",

                            HTML("
                    <b>Reflection.</b><br><br>

                    This argument sounds very reasonable.

                    But could Balder's knowledge actually contain additional relevant information?
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

                    h3("A better argument"),

                    div(class = "explain", if (!rv$reveal) {
                        HTML(
                            "
                        Taking a step back, suppose Balder were completely
                        ignorant about the ages of all four movies.
                        Then all 24 orderings would be equally likely.

                        In 6 of these orderings, C is the oldest movie.
                        Therefore:

                        <br><br>

                        <center style='font-size:28px;font-weight:700;'>
                        P(C is oldest) = 6/24 = 1/4.
                        </center>

                        <br>

                        Now we can ask what happens when Balder's information
                        is taken into account.
                        "
                        )

                    } else {
                        HTML(
                            "
                        After applying Balder's information, 8 orderings remain.
                        In 6 of these, C is oldest.

                        <br><br>

                        <center style='font-size:28px;font-weight:700;'>
                        P(C is oldest | information) = 6/8 = 3/4
                        </center>

                        <br>

                        So, if the remaining orderings are equally likely,
                        Balder's probability of answering correctly is
                        <b>3/4</b>.
                        "
                        )
                    }),

                    div(
                        style = "
                        margin-top:20px;
                        padding:18px;
                        background:#F4F8FC;
                        border-left:6px solid #7B9ACC;
                        border-radius:10px;
                        font-size:18px;
                        line-height:1.7;
                    ",

                        HTML(
                            "
                        <b>But notice what we have assumed.</b>

                        <br><br>

                        Balder's information tells us which orderings are
                        possible. It does not, by itself, tell us that all
                        of those remaining orderings are equally likely.

                        <br><br>

                        So there are really two different questions:

                        <ul>
                            <li>Which orderings are consistent with what Balder knows?</li>
                            <li>How likely is each of those orderings?</li>
                        </ul>

                        The first question is answered by Balder's information.
                        The second requires us to make assumptions about how
                        movie ages are distributed.
                        "
                        )
                    ),

                    br(),

                    if (!rv$reveal) {
                        actionButton("reveal_btn", "Apply information")
                    },

                    br(),

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
                            <b>Key idea.</b><br><br>

                            Conditioning on information tells us which
                            possibilities remain. To assign probabilities
                            to those possibilities, we also need a model
                            for how the movie ages are distributed.
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
                        Balder knows that Movie <b>C</b> is older than both
                        Movies <b>A</b> and <b>B</b>. This gives him information
                        about how old C must be.
                        </p>

                        <p>
                        Let <b>z</b> denote the age of the older of Movies
                        <b>A</b> and <b>B</b>. Then Balder's information tells
                        him that:
                        </p>

                        <p style='text-align:center;font-size:26px;font-weight:700;'>
                        C &gt; z
                        </p>

                        <p>
                        We also need to make an assumption about Movies
                        <b>C</b> and <b>D</b>. If Balder has no information
                        that distinguishes them, it is natural to suppose
                        that, before taking his information into account,
                        they have the same age distribution.
                        </p>

                        <p>
                        We represent this by modelling their ages as
                        independent draws from the same distribution, with
                        cumulative distribution function <b>F(z)</b>.
                        </p>

                        <p>
                        We can now combine these two ideas: Balder knows
                        that <b>C &gt; z</b>, while C and D are otherwise
                        symmetric.
                        </p>

                        <p style='text-align:center;font-size:28px;font-weight:700;'>
                        P(C &gt; D &nbsp;|&nbsp; C &gt; z)
                        </p>

                        <p>
                        The question is: <b>how does knowing that C is older
                        than z change the probability that C is older than D?</b>
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

                            div(
                                class = "formula-box",
                                uiOutput("tail_probability")
                            ),

                            br(),

                            p(
                                "As z increases, fewer movies are older than z, so P(C > z) becomes smaller.",
                                class = "explain"
                            ),

                            actionButton("back3", "← Back"),
                            actionButton("next4", "Next →")
                        )
                    ),

                    column(
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

                            actionButton(
                                "why_formula",
                                "Why does this formula work?"
                            )
                        )
                    )
                )
            )
        }



        else if (rv$page == 5) {
            fluidRow(
                column(
                    12,

                    div(
                        class = "card-style",

                        h3("Why does the formula work?"),

                        p(
                            "We know that Movie C is older than z.",
                            class = "explain"
                        ),

                        p(
                            "Now consider two possible cases for Movie D:",
                            class = "explain"
                        ),

                        fluidRow(
                            column(
                                6,

                                div(
                                    style = "
                                    background:#D8F3DC;
                                    padding:20px;
                                    border-radius:12px;
                                    min-height:260px;
                                ",

                                    h4("Case 1: Movie D is less than z years old"),

                                    p(
                                        "This happens with probability F(z).",
                                        class = "explain"
                                    ),

                                    p(
                                        "If D is younger than z, then C must automatically be older than D.",
                                        class = "explain"
                                    ),

                                    div(
                                        style = "font-size:28px;font-weight:700;text-align:center;",
                                        HTML("P(C &gt; D) = 1")
                                    )
                                )
                            ),

                            column(
                                6,

                                div(
                                    style = "
                                    background:#F4F8FC;
                                    padding:20px;
                                    border-radius:12px;
                                    min-height:260px;
                                ",

                                    h4("Case 2: Movie D is more than z years old"),

                                    p(
                                        "This happens with probability 1 − F(z).",
                                        class = "explain"
                                    ),

                                    p(
                                        "Now both C and D are older than z.",
                                        class = "explain"
                                    ),

                                    p(
                                        "By symmetry, each movie is equally likely to be older than the other.",
                                        class = "explain"
                                    ),

                                    div(
                                        style = "font-size:28px;font-weight:700;text-align:center;",
                                        HTML("P(C &gt; D) = 1/2")
                                    )
                                )
                            )
                        ),

                        br(),

                        div(
                            style = "
                            background:#FFF3CD;
                            padding:22px;
                            border-radius:12px;
                            font-size:20px;
                            line-height:1.8;
                        ",

                            HTML(
                                "
                            <div style='text-align:center;'>
                            <b>Putting the two cases together</b>
                            </div>

                            <br>

                            If D is below z, C is certainly older than D.

                            <br>

                            If D is above z, C and D are symmetric,
                            so C is older with probability 1/2.

                            <br><br>

                            Therefore:

                            <br><br>

                            <div style='text-align:center;font-size:24px;'>

                            P(C &gt; D &nbsp;|&nbsp; C &gt; z)

                            <br>

                            = F(z) &times; 1 + (1 - F(z)) &times; 1/2

                            <br>

                            = (1 + F(z))/2

                            </div>
                            "
                            )
                        ),

                        br(),

                        actionButton("back4", "← Back"),
                        actionButton("next5", "Next →")
                    )
                )
            )
        }



        else if (rv$page == 6) {

            tagList(

                div(
                    class = "card-style",

                    h3("From probability to decision"),

                    div(
                        class = "explain",

                        p(
                            "However we choose to use Balder's information to calculate the probability that Movie C is the oldest, a decision still has to be made: should Balder answer the question or walk away?"
                        ),

                        p(
                            "This decision depends not only on the probability of answering correctly, but also on the potential gains and losses associated with each choice, and on how Balder values those gains and losses."
                        ),

                        p(
                            "We can summarise how Balder values these outcomes using a ",
                            tags$b("happiness function"),
                            ". The probability of winning, together with this happiness function, allows us to compare the two choices in terms of expected happiness."
                        ),

                        div(
                            style = "
                            margin-top:20px;
                            padding:18px;
                            background:#F4F8FC;
                            border-left:6px solid #7B9ACC;
                            border-radius:10px;
                            text-align:center;
                            font-size:21px;
                            line-height:1.8;
                        ",

                            HTML(
                                "
                            <b>Probability of winning</b>
                            &nbsp;&nbsp; + &nbsp;&nbsp;
                            <b>Value of each outcome</b>
                            <br>
                            &darr;
                            <br>
                            <b>Expected happiness</b>
                            <br>
                            &darr;
                            <br>
                            <b>Decision</b>
                            "
                            )
                        )
                    )
                ),

                fluidRow(

                    column(
                        4,

                        div(
                            class = "card-style",

                            h3("Maximising happiness"),

                            sliderInput(
                                "lambda",
                                "λ",
                                0,
                                3,
                                1,
                                0.01
                            ),

                            sliderInput(
                                "p",
                                "Probability of answering correctly",
                                0,
                                1,
                                0.5,
                                0.01
                            ),

                            hr(),

                            actionButton("back4", "← Back"),
                            actionButton("reset", "Restart")
                        )
                    ),

                    column(
                        8,

                        div(
                            class = "card-style",

                            fluidRow(

                                column(
                                    5,

                                    h4("Happiness function"),

                                    div(
                                        class = "info-box",

                                        HTML(
                                            "
                                        <p>
                                        The happiness function measures changes
                                        relative to Balder's current position:
                                        walking away with <b>500,000 kroner</b>.
                                        </p>

                                        <p>
                                        The parameter <b>λ</b> controls how strongly
                                        Balder dislikes losing money relative to
                                        how much he values an equivalent gain.
                                        </p>

                                        <p>
                                        For a chosen happiness function, we can
                                        calculate how Balder's happiness changes
                                        if he answers and is either correct or
                                        incorrect.
                                        </p>
                                        "
                                        )
                                    )
                                ),

                                column(
                                    7,

                                    plotOutput(
                                        "utility_plot",
                                        height = "350px"
                                    )

                                )

                            ),

                            hr(),

                            fluidRow(

                                column(
                                    5,

                                    h4("Expected happiness"),

                                    div(
                                        class = "info-box",

                                        HTML(
                                            "
                                        <p>
                                        We now combine the probability of answering
                                        correctly with the happiness associated
                                        with each possible outcome.
                                        </p>

                                        <p>
                                        This gives Balder's <b>expected happiness
                                        from answering</b>.
                                        </p>

                                        <p>
                                        We can compare this with the happiness
                                        from walking away with 500,000 kroner.
                                        "
                                        )
                                    )
                                ),

                                column(
                                    7,

                                    plotOutput(
                                        "eu_plot",
                                        height = "350px"
                                    )

                                )

                            ),

                            hr(),

                            uiOutput("decision_text")

                        )
                    )

                )

            )
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
            "Given these assumptions, answering maximises expected happiness."
        } else {
            "Given these assumptions, walking away maximises expected happiness."
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
