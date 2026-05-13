library(shiny)
library(bslib)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(reshape2)
library(DT)
library(gtools)

# =========================================================
# Helper Functions
# =========================================================

mski_sim <- function(n, weight, mu, sd){

    tech <- runif(n, 0, 10)
    mat  <- runif(n, 0, 10)
    fit  <- runif(n, 0, 10)

    jump <- mu +
        weight[1] * tech +
        weight[2] * mat +
        weight[3] * fit +
        rnorm(n, 0, sd)

    data.frame(
        Technique = round(tech, 2),
        Materials = round(mat, 2),
        Fitness = round(fit, 2),
        Jump_Length = round(jump, 2)
    )
}

ski_jump <- function(spend, weight, mu, sd){

    mu +
        weight[1] * spend[1] +
        weight[2] * spend[2] +
        weight[3] * spend[3] +
        rnorm(1, 0, sd)
}

# =========================================================
# UI
# =========================================================

ui <- page_navbar(

    title = "🎿 Ski Jump Challenge",

    header = tagList(

        useShinyjs(),

        tags$head(

            tags$script(
                src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.9.3/dist/confetti.browser.min.js"
            ),

            tags$script(HTML("
Shiny.addCustomMessageHandler('trigger_confetti', function(message) {

  confetti({
    particleCount: 120,
    spread: 70,
    origin: { x: 0.2, y: 0.6 }
  });

  confetti({
    particleCount: 120,
    spread: 70,
    origin: { x: 0.8, y: 0.6 }
  });

  confetti({
    particleCount: 220,
    spread: 100,
    origin: { y: 0.5 }
  });

});
")),

            tags$style(HTML("

body{
  background:#F6F8FC;
}

.main-title{
  background:linear-gradient(90deg,#A8DADC,#CDB4DB);
  padding:28px;
  border-radius:18px;
  margin-bottom:25px;
  text-align:center;
  box-shadow:0 4px 12px rgba(0,0,0,0.08);
}

.main-title h1{
  font-weight:700;
  color:#1D3557;
  margin-bottom:10px;
}

.main-title p{
  font-size:18px;
  color:#3D405B;
  margin:0;
}

.card-style{
  background:white;
  border-radius:18px;
  padding:24px;
  margin-bottom:20px;
  box-shadow:0 4px 14px rgba(0,0,0,0.08);
}

.btn-primary{
  background:#89C2D9!important;
  border-color:#89C2D9!important;
  font-weight:600!important;
  font-size:16px!important;
  border-radius:12px!important;
  padding:10px 18px!important;
  margin-top:10px;
  width:100%;
}

.message-panel{
  background:linear-gradient(135deg,#FFFFFF,#F8FBFF);
  border-left:6px solid #89C2D9;
  padding:20px 24px;
  border-radius:16px;
  margin-top:15px;
}

.message-text{
  font-size:20px;
  font-weight:500;
  color:#2E4057;
}

.metric-box{
  background:#F8FAFC;
  border-radius:14px;
  padding:16px;
  text-align:center;
  margin-bottom:12px;
}

.metric-value{
  font-size:28px;
  font-weight:700;
  color:#1D3557;
}

.metric-label{
  color:#6B7280;
}

.success-panel{
  background:#D8F3DC;
  color:#1B4332;
  padding:20px;
  border-radius:16px;
  font-size:24px;
  font-weight:700;
  text-align:center;
  margin-top:15px;
}

.fail-panel{
  background:#FDE2E4;
  color:#9D0208;
  padding:20px;
  border-radius:16px;
  font-size:24px;
  font-weight:700;
  text-align:center;
  margin-top:15px;
}

"))
        )
    ),

    theme = bs_theme(
        version = 5,
        bootswatch = "lux",
        primary = "#7B9ACC",
        bg = "#F6F8FC",
        fg = "#2E3440",
        base_font = font_google("Inter")
    ),

    # =====================================================
    # MAIN TAB
    # =====================================================

    nav_panel(

        "🎿 Interactive Competition",

        div(
            class = "main-title",

            h1("🎿 Ski Jump World Record Challenge"),

            p(
                "Use historical data to optimise your strategy and beat the world record."
            )
        ),

        layout_sidebar(

            sidebar = div(

                class = "card-style",

                h4("Competition Controls"),

                numericInput(
                    "seed",
                    "Random seed",
                    123
                ),

                checkboxInput(
                    "random_weights",
                    "Randomise hidden weights",
                    FALSE
                ),

                numericInput(
                    "wr",
                    "World record distance (m)",
                    254.5
                ),

                tags$hr(),

                h4("Training Phase"),

                numericInput(
                    "train_data_spend",
                    "Units spent on training data",
                    0,
                    0,
                    10
                ),

                actionButton(
                    "buy_train_data",
                    "1: Buy Training Data",
                    class = "btn-primary"
                ),

                tags$hr(),

                numericInput(
                    "train_tech",
                    "Technique spend",
                    0,
                    0,
                    10
                ),

                numericInput(
                    "train_mat",
                    "Materials spend",
                    0,
                    0,
                    10
                ),

                numericInput(
                    "train_fit",
                    "Fitness spend",
                    0,
                    0,
                    10
                ),

                actionButton(
                    "run_training",
                    "2: Run Training Jump",
                    class = "btn-primary"
                ),

                tags$hr(),

                h4("Competition Phase"),

                numericInput(
                    "comp_data_spend",
                    "Units spent on additional data",
                    0,
                    0,
                    10
                ),

                actionButton(
                    "buy_comp_data",
                    "3: Buy Competition Data",
                    class = "btn-primary"
                ),

                tags$hr(),

                numericInput(
                    "comp_tech",
                    "Additional Technique spend",
                    0,
                    0,
                    10
                ),

                numericInput(
                    "comp_mat",
                    "Additional Materials spend",
                    0,
                    0,
                    10
                ),

                numericInput(
                    "comp_fit",
                    "Additional Fitness spend",
                    0,
                    0,
                    10
                ),

                actionButton(
                    "run_competition",
                    "4: Competition Jump",
                    class = "btn-primary"
                )
            ),

            div(

                fluidRow(

                    column(
                        4,

                        div(
                            class = "metric-box",

                            div(
                                class = "metric-value",
                                textOutput("training_bank")
                            ),

                            div(
                                class = "metric-label",
                                "Training Bank"
                            )
                        )
                    ),

                    column(
                        4,

                        div(
                            class = "metric-box",

                            div(
                                class = "metric-value",
                                textOutput("competition_bank")
                            ),

                            div(
                                class = "metric-label",
                                "Competition Bank"
                            )
                        )
                    ),

                    column(
                        4,

                        div(
                            class = "metric-box",

                            div(
                                class = "metric-value",
                                textOutput("total_data")
                            ),

                            div(
                                class = "metric-label",
                                "Historical Jumps"
                            )
                        )
                    )
                ),

                div(
                    class = "card-style",

                    div(
                        class = "message-panel",
                        uiOutput("status_message")
                    )
                ),

                div(
                    class = "card-style",

                    h3("Historical Jump Data"),

                    DTOutput("data_table")
                ),

                div(
                    class = "card-style",

                    h3("Regression Analysis"),

                    plotOutput(
                        "regression_plot",
                        height = "550px"
                    )
                ),

                div(
                    class = "card-style",

                    h3("Regression Coefficients"),

                    tableOutput("coef_table")
                ),

                div(
                    class = "card-style",

                    h3("Jump Outcomes"),

                    uiOutput("jump_results")
                )
            )
        )
    )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session){

    rv <- reactiveValues(

        weight = NULL,
        sd = NULL,
        mu = NULL,

        d1 = NULL,
        d2 = NULL,
        all_data = NULL,

        training_bank = 10,
        competition_bank = 10,

        training_jump = NULL,
        competition_jump = NULL,

        train_complete = FALSE
    )

    # =====================================================
    # BUY TRAINING DATA
    # =====================================================

    observeEvent(input$buy_train_data, {

        set.seed(input$seed)

        true_mu <- 254.5

        rv$mu <- true_mu - 60.5

        if(input$random_weights){

            rv$weight <- as.numeric(
                rdirichlet(1, rep(10, 3)) * 9
            )

            rv$sd <- exp(rnorm(1, log(10), 0.25))

        } else {

            rv$weight <- c(4, 2, 3)
            rv$sd <- 10
        }

        spend <- round(input$train_data_spend)

        if(spend > 10){

            showNotification(
                "Cannot spend more than 10 units.",
                type = "error"
            )

            return()
        }

        rv$training_bank <- 10 - spend

        ndata <- spend * 10

        if(ndata > 0){

            rv$d1 <- mski_sim(
                ndata,
                rv$weight,
                rv$mu,
                rv$sd
            )

            rv$d1$Phase <- "Training"

            rv$all_data <- rv$d1
        }

        output$status_message <- renderUI({

            HTML(paste0(
                "<div class='message-text'>
                📊 Training data purchased:
                <b>", ndata,
                "</b> historical jumps.
                </div>"
            ))
        })
    })

    # =====================================================
    # TRAINING JUMP
    # =====================================================

    observeEvent(input$run_training, {

        spend <- c(
            input$train_tech,
            input$train_mat,
            input$train_fit
        )

        if(sum(spend) > rv$training_bank){

            showNotification(
                "Training spend exceeds remaining bank.",
                type = "error"
            )

            return()
        }

        rv$training_jump <- round(

            ski_jump(
                spend,
                rv$weight,
                rv$mu,
                rv$sd
            ),

            2
        )

        rv$train_complete <- TRUE

        output$status_message <- renderUI({

            HTML(paste0(
                "<div class='message-text'>
                🎿 Training jump completed:
                <b>", rv$training_jump,
                " metres</b>
                </div>"
            ))
        })
    })

    # =====================================================
    # BUY COMPETITION DATA
    # =====================================================

    observeEvent(input$buy_comp_data, {

        req(rv$train_complete)

        spend <- round(input$comp_data_spend)

        if(spend > 10){

            showNotification(
                "Cannot spend more than 10 units.",
                type = "error"
            )

            return()
        }

        rv$competition_bank <- 10 - spend

        ndata <- spend * 10

        if(ndata > 0){

            rv$d2 <- mski_sim(
                ndata,
                rv$weight,
                rv$mu,
                rv$sd
            )

            rv$d2$Phase <- "Competition"

            rv$all_data <- rbind(
                rv$all_data,
                rv$d2
            )
        }

        output$status_message <- renderUI({

            HTML(paste0(
                "<div class='message-text'>
                📈 Competition data added.
                Total observations:
                <b>", nrow(rv$all_data),
                "</b>
                </div>"
            ))
        })
    })

    # =====================================================
    # COMPETITION JUMP
    # =====================================================

    observeEvent(input$run_competition, {

        req(rv$train_complete)

        total_spend <- c(

            input$train_tech + input$comp_tech,
            input$train_mat + input$comp_mat,
            input$train_fit + input$comp_fit
        )

        additional <- c(
            input$comp_tech,
            input$comp_mat,
            input$comp_fit
        )

        if(sum(additional) > rv$competition_bank){

            showNotification(
                "Competition spend exceeds remaining bank.",
                type = "error"
            )

            return()
        }

        if(any(total_spend > 10)){

            showNotification(
                "Maximum total category spend is 10.",
                type = "error"
            )

            return()
        }

        rv$competition_jump <- round(

            ski_jump(
                total_spend,
                rv$weight,
                rv$mu,
                rv$sd
            ),

            2
        )

        # ============================================
        # CONFETTI
        # ============================================

        if(rv$competition_jump >= input$wr){

            session$sendCustomMessage(
                "trigger_confetti",
                list()
            )
        }

        output$status_message <- renderUI({

            HTML(paste0(
                "<div class='message-text'>
                🏁 Competition jump:
                <b>", rv$competition_jump,
                " metres</b>
                </div>"
            ))
        })
    })

    # =====================================================
    # BANK OUTPUTS
    # =====================================================

    output$training_bank <- renderText({
        paste0(rv$training_bank, " units")
    })

    output$competition_bank <- renderText({
        paste0(rv$competition_bank, " units")
    })

    output$total_data <- renderText({

        if(is.null(rv$all_data)) return(0)

        nrow(rv$all_data)
    })

    # =====================================================
    # DATA TABLE
    # =====================================================

    output$data_table <- renderDT({

        req(rv$all_data)

        datatable(
            rv$all_data,
            options = list(pageLength = 8)
        )
    })

    # =====================================================
    # REGRESSION PLOT
    # =====================================================

    output$regression_plot <- renderPlot({

        req(rv$all_data)

        d <- reshape2::melt(
            rv$all_data,
            id.vars = c("Jump_Length", "Phase")
        )

        colnames(d)[3] <- "Variable"
        colnames(d)[4] <- "Value"

        ggplot(d, aes(Value, Jump_Length)) +

            facet_wrap(~Variable, scales = "free_x") +

            geom_point(
                aes(colour = Phase),
                alpha = 0.75,
                size = 2.4
            ) +

            scale_colour_manual(
                values = c(
                    "Training" = "#5DADE2",
                    "Competition" = "#1B4F72"
                )
            ) +

            geom_smooth(
                method = "lm",
                colour = "#E63946",
                se = FALSE,
                linewidth = 1.2
            ) +

            theme_minimal(base_size = 16) +

            theme(
                strip.text = element_text(size = 16, face = "bold"),
                legend.position = "top"
            ) +

            labs(
                x = "Investment",
                y = "Jump Length (m)",
                colour = "Data Source"
            )
    })

    # =====================================================
    # REGRESSION TABLE
    # =====================================================

    output$coef_table <- renderTable({

        req(rv$all_data)

        l1 <- lm(
            Jump_Length ~ Technique,
            data = rv$all_data
        )$coefficients

        l2 <- lm(
            Jump_Length ~ Materials,
            data = rv$all_data
        )$coefficients

        l3 <- lm(
            Jump_Length ~ Fitness,
            data = rv$all_data
        )$coefficients

        tab <- rbind(l1, l2, l3)

        colnames(tab) <- c(
            "Intercept",
            "Gradient"
        )

        rownames(tab) <- c(
            "Technique",
            "Materials",
            "Fitness"
        )

        round(tab, 2)
    })

    # =====================================================
    # RESULTS PANEL
    # =====================================================

    output$jump_results <- renderUI({

        req(rv$training_jump)

        tagList(

            div(
                class = "message-panel",

                HTML(paste0(
                    "<div class='message-text'>
                    🎿 Training Jump:
                    <b>",
                    rv$training_jump,
                    " m</b>
                    </div>"
                ))
            ),

            if(!is.null(rv$competition_jump)){

                div(
                    class = "message-panel",

                    HTML(paste0(
                        "<div class='message-text'>
                        🏁 Competition Jump:
                        <b>",
                        rv$competition_jump,
                        " m</b>
                        </div>"
                    ))
                )
            },

            if(!is.null(rv$competition_jump)){

                if(rv$competition_jump >= input$wr){

                    div(
                        class = "success-panel",

                        paste0(
                            "🏆 WORLD RECORD BROKEN! ",
                            round(rv$competition_jump,2),
                            " m"
                        )
                    )

                } else {

                    div(
                        class = "fail-panel",

                        paste0(
                            "❌ Record not beaten. Final jump: ",
                            round(rv$competition_jump,2),
                            " m"
                        )
                    )
                }
            }
        )
    })
}

# =========================================================
# RUN APP
# =========================================================

shinyApp(ui, server)
