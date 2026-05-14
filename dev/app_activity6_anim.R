library(shiny)
library(bslib)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(reshape2)
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
        Technique   = round(tech, 2),
        Materials   = round(mat, 2),
        Fitness     = round(fit, 2),
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
# Shared Main Panel
# =========================================================

main_panel_ui <- function(show_comp_results = FALSE){

    div(

        fluidRow(

            column(
                4,
                div(
                    class = "metric-box",
                    div(class = "metric-value", textOutput("training_bank")),
                    div(class = "metric-label", "Training Bank")
                )
            ),

            column(
                4,
                div(
                    class = "metric-box",
                    div(class = "metric-value", textOutput("competition_bank")),
                    div(class = "metric-label", "Competition Bank")
                )
            ),

            column(
                4,
                div(
                    class = "metric-box",
                    div(class = "metric-value", textOutput("total_data")),
                    div(class = "metric-label", "Historical Jumps")
                )
            )
        ),

        div(
            class = "card-style",
            div(
                class = "message-panel",
                uiOutput(if(show_comp_results) "status_message_comp" else "status_message")
            )
        ),

        # =====================================================
        # REMOVED TABLE DISPLAY
        # =====================================================

        div(
            class = "card-style",
            h3("Regression Analysis"),
            plotOutput(
                if(show_comp_results) "regression_plot_comp" else "regression_plot",
                height = "450px"
            )
        ),

        div(
            class = "card-style",
            h3("Regression Coefficients"),
            tableOutput(if(show_comp_results) "coef_table_comp" else "coef_table")
        ),

        div(
            class = "card-style",
            h3("Jump Outcomes"),
            uiOutput(if(show_comp_results) "jump_results_comp" else "jump_results")
        ),

        # =====================================================
        # Ski Jump Animation
        # =====================================================

        div(
            class = "card-style",
            h3("Ski Jump Simulation"),

            div(
                class = "ski-world",

                div(class = "wr-marker-label", "WR"),
                div(class = "wr-marker"),

                div(
                    id = if(show_comp_results) "skier-comp" else "skier",
                    class = "skier",
                    "⛷️"
                ),

                div(
                    id = if(show_comp_results) "jump-marker-comp" else "jump-marker",
                    class = "jump-marker"
                ),

                div(
                    id = if(show_comp_results) "jump-label-comp" else "jump-label",
                    class = "jump-label"
                )
            )
        )
    )
}

# =========================================================
# UI
# =========================================================

ui <- page_navbar(

    title = "🎿 Ski Jump Challenge",

    header = tagList(

        useShinyjs(),

        # =====================================================
        # CSS
        # =====================================================

        tags$head(

            tags$style(HTML("

      body{
        background:#F6F8FC;
      }

      .card-style{
        background:white;
        border-radius:18px;
        padding:24px;
        margin-bottom:20px;
        box-shadow:0 4px 14px rgba(0,0,0,0.08);
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

      .message-panel{
        background:linear-gradient(135deg,#FFFFFF,#F8FBFF);
        border-left:6px solid #89C2D9;
        padding:20px 24px;
        border-radius:16px;
      }

      .ski-world{
        position:relative;
        width:100%;
        height:120px;
        background:linear-gradient(to bottom,#BFE7FF 0%, #EAF7FF 45%, #FFFFFF 100%);
        overflow:hidden;
        border-radius:12px;
      }

      .skier{
        position:absolute;
        top:40px;
        right:0px;
        font-size:42px;
        z-index:5;
        transition:right 2s linear;
      }

      .jump-marker{
        position:absolute;
        top:70px;
        width:4px;
        height:40px;
        background:#1D3557;
        display:none;
      }

      .wr-marker{
        position:absolute;
        top:70px;
        left:10%;
        width:4px;
        height:40px;
        background:#E63946;
      }

      .wr-marker-label{
        position:absolute;
        top:40px;
        left:10%;
        font-weight:700;
        color:#E63946;
      }

      .jump-label{
        position:absolute;
        top:10px;
        font-weight:700;
        color:#1D3557;
      }

      .spending-grid{
        display:grid;
        grid-template-columns:1fr 1fr 1fr;
        gap:12px;
      }

      "))

        ),

        # =====================================================
        # JS
        # =====================================================

        tags$script(HTML("

      Shiny.addCustomMessageHandler('start_jump', function(message){

        const skier = document.getElementById(message.id);

        if(!skier) return;

        const worldRecord = message.world_record;
        const jump = message.jump_distance;

        const container = skier.parentElement.offsetWidth;

        const runup = 50;
        const maxX = container - 50;

        let landingRight =
          runup + (maxX - runup) * (jump / worldRecord);

        landingRight = Math.min(maxX, landingRight);

        skier.style.transition = 'right 2s linear';
        skier.style.right = landingRight + 'px';

        setTimeout(() => {

          const marker = document.getElementById(message.marker);
          const label  = document.getElementById(message.label);

          if(marker){
            marker.style.right = landingRight + 'px';
            marker.style.display = 'block';
          }

          if(label){
            label.style.right = (landingRight - 25) + 'px';
            label.innerHTML = jump.toFixed(1) + ' m';
          }

        }, 2100);

      });

    "))
    ),

    theme = bs_theme(
        version = 5,
        bootswatch = "lux",
        primary = "#7B9ACC",
        base_font = font_google("Inter")
    ),

    # =========================================================
    # TRAINING PHASE
    # =========================================================

    nav_panel(

        "🎯 Training Phase",

        div(
            class = "main-title",
            h1("🎿 Ski Jump Training Camp"),
            p("Explore historical data and optimise your first jump.")
        ),

        layout_sidebar(

            sidebar = div(

                class = "card-style",

                h4("Game Setup"),

                numericInput("seed", "Random seed", 123),

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

                h4("Training Data"),

                numericInput(
                    "train_data_spend",
                    "Units spent on training data",
                    0,
                    min = 0,
                    max = 10
                ),

                actionButton(
                    "buy_train_data",
                    "1: Buy Training Data",
                    class = "btn-primary"
                ),

                tags$hr(),

                h4("Spending"),

                div(
                    class = "spending-grid",

                    numericInput(
                        "train_tech",
                        "Technique",
                        0,
                        min = 0,
                        max = 10
                    ),

                    numericInput(
                        "train_mat",
                        "Materials",
                        0,
                        min = 0,
                        max = 10
                    ),

                    numericInput(
                        "train_fit",
                        "Fitness",
                        0,
                        min = 0,
                        max = 10
                    )
                ),

                br(),

                actionButton(
                    "run_training",
                    "2: Run Training Jump",
                    class = "btn-primary"
                )
            ),

            main_panel_ui(FALSE)
        )
    ),

    # =========================================================
    # COMPETITION PHASE
    # =========================================================

    nav_panel(

        "🏁 Competition Phase",

        div(
            class = "main-title",
            h1("🏁 Final Competition"),
            p("Use your remaining budget wisely and chase the world record.")
        ),

        layout_sidebar(

            sidebar = div(

                class = "card-style",

                h4("Competition Data"),

                numericInput(
                    "comp_data_spend",
                    "Units spent on additional data",
                    0,
                    min = 0,
                    max = 10
                ),

                actionButton(
                    "buy_comp_data",
                    "3: Buy Competition Data",
                    class = "btn-primary"
                ),

                tags$hr(),

                h4("Spending"),

                div(
                    class = "spending-grid",

                    numericInput(
                        "comp_tech",
                        "Technique",
                        0,
                        min = 0,
                        max = 10
                    ),

                    numericInput(
                        "comp_mat",
                        "Materials",
                        0,
                        min = 0,
                        max = 10
                    ),

                    numericInput(
                        "comp_fit",
                        "Fitness",
                        0,
                        min = 0,
                        max = 10
                    )
                ),

                br(),

                actionButton(
                    "run_competition",
                    "4: Competition Jump",
                    class = "btn-primary"
                )
            ),

            main_panel_ui(TRUE)
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

    # =========================================================
    # TRAINING DATA
    # =========================================================

    observeEvent(input$buy_train_data, {

        set.seed(input$seed)

        rv$mu <- 254.5 - 60.5

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

        # =====================================================
        # HARD CONSTRAINT
        # =====================================================

        if(spend > 10){

            showNotification(
                "Training data spending cannot exceed 10 units.",
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

        msg <- HTML(
            paste0(
                "<div class='message-text'>📊 Training data purchased: <b>",
                ndata,
                "</b> historical jumps.</div>"
            )
        )

        output$status_message <- renderUI(msg)
        output$status_message_comp <- renderUI(msg)
    })

    # =========================================================
    # TRAINING JUMP
    # =========================================================

    observeEvent(input$run_training, {

        spend <- c(
            input$train_tech,
            input$train_mat,
            input$train_fit
        )

        # total within phase
        if(sum(spend) > rv$training_bank){

            showNotification(
                "Training spending exceeds remaining bank.",
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

        output$status_message <- renderUI(
            HTML(
                paste0(
                    "<div class='message-text'>🎿 Training jump completed: <b>",
                    rv$training_jump,
                    " metres</b></div>"
                )
            )
        )

        output$status_message_comp <- renderUI(
            HTML(
                paste0(
                    "<div class='message-text'>🎿 Training jump completed: <b>",
                    rv$training_jump,
                    " metres</b></div>"
                )
            )
        )

        session$sendCustomMessage(
            "start_jump",
            list(
                id = "skier",
                marker = "jump-marker",
                label = "jump-label",
                jump_distance = rv$training_jump,
                world_record = input$wr
            )
        )
    })

    # =========================================================
    # COMPETITION DATA
    # =========================================================

    observeEvent(input$buy_comp_data, {

        req(rv$train_complete)

        spend <- round(input$comp_data_spend)

        # =====================================================
        # HARD CONSTRAINT
        # =====================================================

        if(spend > 10){

            showNotification(
                "Competition data spending cannot exceed 10 units.",
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

        output$status_message_comp <- renderUI(

            HTML(
                paste0(
                    "<div class='message-text'>📈 Competition data added: <b>",
                    nrow(rv$all_data),
                    "</b> total jumps.</div>"
                )
            )
        )
    })

    # =========================================================
    # COMPETITION JUMP
    # =========================================================

    observeEvent(input$run_competition, {

        req(rv$train_complete)

        additional <- c(
            input$comp_tech,
            input$comp_mat,
            input$comp_fit
        )

        # =====================================================
        # PHASE CONSTRAINT
        # =====================================================

        if(sum(additional) > rv$competition_bank){

            showNotification(
                "Competition spending exceeds remaining bank.",
                type = "error"
            )

            return()
        }

        # =====================================================
        # CROSS-PHASE CATEGORY CONSTRAINT
        # =====================================================

        total_spend <- c(
            input$train_tech + input$comp_tech,
            input$train_mat  + input$comp_mat,
            input$train_fit  + input$comp_fit
        )

        if(any(total_spend > 10)){

            showNotification(
                "Total spending on any category across both phases cannot exceed 10.",
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

        output$status_message_comp <- renderUI(

            HTML(
                paste0(
                    "<div class='message-text'>🏁 Competition jump: <b>",
                    rv$competition_jump,
                    " metres</b></div>"
                )
            )
        )

        session$sendCustomMessage(
            "start_jump",
            list(
                id = "skier-comp",
                marker = "jump-marker-comp",
                label = "jump-label-comp",
                jump_distance = rv$competition_jump,
                world_record = input$wr
            )
        )
    })

    # =========================================================
    # METRICS
    # =========================================================

    output$training_bank <- renderText({
        paste0(rv$training_bank, " units")
    })

    output$competition_bank <- renderText({
        paste0(rv$competition_bank, " units")
    })

    output$total_data <- renderText({

        if(is.null(rv$all_data)){
            0
        } else {
            nrow(rv$all_data)
        }

    })

    # =========================================================
    # SHARED OUTPUTS
    # =========================================================

    render_shared_outputs <- function(prefix = ""){

        output[[paste0("regression_plot", prefix)]] <- renderPlot({

            req(rv$all_data)

            d <- melt(
                rv$all_data,
                id.vars = c("Jump_Length", "Phase")
            )

            colnames(d)[3:4] <- c("Variable", "Value")

            ggplot(
                d,
                aes(Value, Jump_Length)
            ) +

                facet_wrap(
                    ~Variable,
                    scales = "free_x"
                ) +

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
                    formula = y ~ x,
                    colour = "#E63946",
                    se = FALSE,
                    linewidth = 1.2
                ) +

                theme_minimal(base_size = 14) +

                theme(
                    strip.text = element_text(face = "bold"),
                    legend.position = "top"
                ) +

                labs(
                    x = "Investment",
                    y = "Jump Length (m)",
                    colour = "Data Source"
                )

        })

        output[[paste0("coef_table", prefix)]] <- renderTable({

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
    }

    render_shared_outputs("")
    render_shared_outputs("_comp")

    # =========================================================
    # JUMP RESULT PANELS
    # =========================================================

    output$jump_results <- renderUI({

        req(rv$training_jump)

        div(
            class = "message-panel",

            HTML(
                paste0(
                    "<div class='message-text'>Your training jump: <b>",
                    rv$training_jump,
                    " metres</b></div>"
                )
            )
        )
    })

    output$jump_results_comp <- renderUI({

        req(rv$competition_jump)

        div(
            class = "message-panel",

            HTML(
                paste0(
                    "<div class='message-text'>Competition jump: <b>",
                    rv$competition_jump,
                    " metres</b></div>"
                )
            )
        )
    })
}

# =========================================================
# APP
# =========================================================

shinyApp(ui, server)
