library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(purrr)
library(scales)

# =========================================================
# PERMUTATIONS
# =========================================================

permute <- function(x){

    if(length(x) == 1) return(list(x))

    out <- list()

    for(i in seq_along(x)){

        rest <- x[-i]
        perms <- permute(rest)

        for(p in perms){
            out <- append(out, list(c(x[i], p)))
        }
    }

    out
}

movies <- c("A","B","C","D")

all_orders <- permute(movies)

perm_df <- tibble(
    ordering = map_chr(all_orders, ~paste(.x, collapse=" "))
)

valid_order <- function(ord){

    x <- strsplit(ord, " ")[[1]]

    posA <- match("A", x)
    posB <- match("B", x)
    posC <- match("C", x)

    posC > posA & posC > posB
}

perm_df$valid <- map_lgl(perm_df$ordering, valid_order)

# =========================================================
# UTILITY FUNCTION (REFERENCE POINT)
# =========================================================

reference_wealth <- 0.5

utility_function <- function(x, lambda = 1){

    delta <- x - reference_wealth

    ifelse(delta >= 0, delta, lambda * delta)
}

# =========================================================
# UI
# =========================================================

ui <- page_fluid(

    theme = bs_theme(
        version = 5,
        bootswatch = "minty",
        base_font = font_google("Inter")
    ),

    tags$head(
        tags$style(HTML("

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
            .invalid{ background:#F8D7DA; opacity:0.2; text-decoration:line-through; }

            .explain{
                font-size:18px;
                line-height:1.7;
            }

        "))
    ),

    div(
        class="main-title",
        h1("🧠 Activity 2: Who Wants to be a Danish Millionaire?")
    ),

    uiOutput("page_ui")
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session){

    rv <- reactiveValues(
        page = 1,
        reveal = FALSE
    )

    observeEvent(input$next1, { rv$page <- 2; rv$reveal <- FALSE })
    observeEvent(input$next2, rv$page <- 3)
    observeEvent(input$next3, rv$page <- 4)

    observeEvent(input$back1, rv$page <- 1)
    observeEvent(input$back2, rv$page <- 2)
    observeEvent(input$back3, rv$page <- 3)

    observeEvent(input$reveal_btn, rv$reveal <- TRUE)

    observeEvent(input$reset, {
        rv$page <- 1
        rv$reveal <- FALSE
        updateSliderInput(session, "fz", value = 0.6)
        updateSliderInput(session, "lambda", value = 1)
        updateSliderInput(session, "p", value = 0.5)
    })

    # =====================================================
    # PAGE RENDER
    # =====================================================

    output$page_ui <- renderUI({

        if(rv$page == 1){

            fluidRow(
                column(12,
                       div(class="card-style",
                           h3("Final question"),
                           p("Which Danish comedy movie premiered first?"),

                           div("A) Sover Dolly på Ryggen"),
                           div("B) Klassefesten"),
                           div("C) Blå Mænd"),
                           div("D) Superclasico"),

                           hr(),

                           h4("Balder’s information"),

                           p("• He knows Movie C is older than A and B."),
                           p("• He does not know whether C or D is older."),

                           hr(),

                           p("The orderings below are written from youngest to oldest.",
                             style="font-size:18px;"),

                           actionButton("next1", "Explore reasoning →")
                       )
                )
            )
        }

        else if(rv$page == 2){

            fluidRow(
                column(12,
                       div(class="card-style",

                           h3("Information changes probabilities"),

                           div(class="explain",

                               if(!rv$reveal){

                                   HTML("
                                Without additional information, all 24 orderings are equally likely.
                                In 6 of these, C is oldest.<br><br>

                                <center style='font-size:28px;font-weight:700;'>
                                P(C is oldest) = 6/24 = 1/4
                                </center>
                                ")

                               } else {

                                   HTML("
                                After applying Balder’s information, 8 orderings remain.
                                In 6 of these, C is oldest.<br><br>

                                <center style='font-size:28px;font-weight:700;'>
                                P(C is oldest | info) = 6/8 = 3/4
                                </center>
                                ")
                               }
                           ),

                           br(),

                           if(!rv$reveal)
                               actionButton("reveal_btn", "Apply information"),

                           div(class="perm-grid",
                               lapply(1:nrow(perm_df), function(i){

                                   row <- perm_df[i,]
                                   cls <- "neutral"

                                   if(rv$reveal){
                                       cls <- if(row$valid) "valid" else "invalid"
                                   }

                                   div(class=paste("perm-box", cls), row$ordering)
                               })
                           ),

                           if(rv$reveal){

                               div(
                                   style="
                                margin-top:25px;
                                padding:18px;
                                background:#FFF3CD;
                                border-left:6px solid #E0A800;
                                border-radius:10px;
                                font-size:18px;
                                line-height:1.7;
                                ",
                                   HTML("<b>Reflection.</b><br>
                                We assumed all remaining orderings are equally likely.<br><br>
                                But is that reasonable? Could some still be more likely than others?")
                               )
                           },

                           br(),
                           actionButton("back1", "← Back"),
                           actionButton("next2", "Next →")
                       )
                )
            )
        }

        else if(rv$page == 3){

            fluidRow(

                column(4,
                       div(class="card-style",
                           h3("Probability model"),

                           sliderInput("fz","F(z)",0,0.99,0.6,0.01),

                           p("As C becomes older, probability it is oldest increases.", class="explain"),

                           hr(),

                           HTML("<div style='text-align:center;font-size:24px;'>
                        P(C > D) = (1 + F(z))/2
                        </div>"),

                           actionButton("back2","← Back"),
                           actionButton("next3","Next →")
                       )
                ),

                column(8,
                       div(class="card-style",

                           h4("Conditional reasoning"),

                           plotOutput("dist_plot", height="450px"),

                           br(),

                           div(class="big", textOutput("p_display")),

                           div(
                               style="
                                margin-top:20px;
                                padding:18px;
                                background:#E8F4FD;
                                border-left:6px solid #7B9ACC;
                                border-radius:10px;
                                font-size:18px;
                                line-height:1.7;
                            ",
                               HTML("<b>Final thought.</b><br>
                            Balder’s decision doesn’t depend only on the probability of winning.<br><br>
                            It also depends on what he stands to gain or lose, and how much he values those outcomes.")
                           )
                       )
                )
            )
        }

        else {

            fluidRow(
                column(4,
                       div(class="card-style",
                           h3("Loss aversion"),

                           sliderInput("lambda","λ",0,3,1,0.01),
                           sliderInput("p","Win Probability",0,1,0.5,0.01),

                           hr(),
                           actionButton("back3","← Back"),
                           actionButton("reset","Restart")
                       )
                ),

                column(8,
                       div(class="card-style",

                           h4("Happiness function"),
                           plotOutput("utility_plot", height="300px"),

                           hr(),

                           h4("Expected happiness"),
                           plotOutput("eu_plot", height="300px"),

                           hr(),

                           uiOutput("decision_text")
                       )
                )
            )
        }
    })

    # =====================================================
    # PAGE 3
    # =====================================================

    output$p_display <- renderText({
        fz <- input$fz
        paste0("P(C > D) = ", round((1+fz)/2, 3))
    })

    output$dist_plot <- renderPlot({

        fz <- input$fz
        shape <- 2.2
        scale <- 18

        z <- qweibull(fz, shape, scale)

        x <- seq(0,50,length.out=1000)
        y <- dweibull(x, shape, scale)

        df <- data.frame(x,y)
        ymax <- max(y)

        ggplot(df, aes(x,y)) +
            geom_area(fill="#DDE5F2") +
            geom_area(data=subset(df, x>=z), fill="#7B9ACC") +

            annotate("segment",
                     x=z,xend=z,y=0,yend=ymax,
                     linetype="dashed", linewidth=1
            ) +

            annotate("text",
                     x=z,y=-0.003,label="z",
                     size=7,fontface="bold"
            ) +

            coord_cartesian(clip="off") +

            theme_minimal(base_size=18) +
            labs(x="Age", y="Probability Density")
    })

    # =====================================================
    # PAGE 4
    # =====================================================

    reference_wealth <- 0.5

    utility_function <- function(x, lambda=1){
        d <- x - reference_wealth
        ifelse(d>=0,d,lambda*d)
    }

    output$utility_plot <- renderPlot({

        lambda <- input$lambda

        x <- seq(0,1,length.out=500)
        y <- utility_function(x, lambda)

        ggplot(data.frame(x,y), aes(x,y)) +
            geom_line(color="#7B9ACC", linewidth=1.3) +
            geom_vline(xintercept=0.5, linetype="dashed") +
            theme_minimal(base_size = 20) +
            theme(
                axis.title = element_text(size = 18),
                axis.text  = element_text(size = 16)
            ) +
            labs(x="Wealth (millions of kroner)", y="Happiness")
    })

    output$eu_plot <- renderPlot({

        lambda <- input$lambda
        p <- input$p

        u <- function(x) utility_function(x, lambda)

        df <- data.frame(
            outcome=c("Win","Lose"),
            contrib=c(p*u(1), (1-p)*u(0.064))
        )

        ggplot(df, aes(outcome, contrib, fill=outcome)) +
            geom_col(width=0.6) +
            scale_fill_manual(values=c("#7B9ACC","#F8D7DA")) +
            theme_minimal(base_size = 20) +
            theme(
                axis.title = element_text(size = 18),
                axis.text  = element_text(size = 16),
                legend.position = "none"
            ) +
            labs(x = "Outcome",
                y="Expected happiness")
    })

    output$decision_text <- renderUI({

        lambda <- input$lambda
        p <- input$p

        u <- function(x) utility_function(x, lambda)

        EU_play <- p*u(1) + (1-p)*u(0.064)
        EU_quit <- u(0.5)

        decision <- if(EU_play > EU_quit) "Optimal strategy is Play" else "Optimal strategy is Quit"

        HTML(paste0(
            "<b>If Play:</b> ", round(EU_play,3), "<br>",
            "<b>If Quit:</b> ", round(EU_quit,3), "<br><br>",
            "<center style='font-size:22px;font-weight:bold;'>", decision, "</center>"
        ))
    })
}

# =========================================================
# RUN APP
# =========================================================

shinyApp(ui, server)
