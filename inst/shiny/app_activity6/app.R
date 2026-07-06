suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(shinyjs)
    library(ggplot2)
    library(dplyr)
    library(reshape2)
    library(gtools)
    library(rhandsontable)
})


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

validate_spend <- function(vals, max_vals, phase_name){

    if(any(vals < 0, na.rm = TRUE)){

        return(paste0(
            phase_name,
            ": spending cannot be negative."
        ))
    }

    if(any(vals > max_vals, na.rm = TRUE)){

        return(paste0(
            phase_name,
            ": one or more categories exceed the allowable maximum."
        ))
    }

    if(sum(vals, na.rm = TRUE) > 10){

        return(paste0(
            phase_name,
            ": total spending cannot exceed 10."
        ))
    }

    return(NULL)
}

# =========================================================
# UI Helper
# =========================================================

main_panel_ui <- function(show_comp_results = FALSE){

    div(

        fluidRow(

            # ===================================================
            # LEFT COLUMN
            # ===================================================

            column(
                width = 6,

                # =========================
                # ANIMATION (TOP LEFT)
                # =========================
                div(
                    class = "card-style",

                    h3("Ski Jump Simulation"),

                    div(
                        class = "ski-world",

                        div(class = "wr-marker-label", "WR"),
                        div(class = "wr-marker"),

                        div(
                            id = if(show_comp_results)
                                "skier-comp"
                            else
                                "skier",
                            class = "skier",
                            "⛷️"
                        ),

                        div(
                            id = if(show_comp_results)
                                "jump-marker-comp"
                            else
                                "jump-marker",
                            class = "jump-marker"
                        ),

                        div(
                            id = if(show_comp_results)
                                "jump-label-comp"
                            else
                                "jump-label",
                            class = "jump-label"
                        )
                    )
                ),

                # =========================
                # SPENDING TABLE + BUTTONS
                # =========================
                div(
                    class = "card-style",

                    h3(
                        if(show_comp_results)
                            "Competition Spending"
                        else
                            "Training Spending"
                    ),

                    if(show_comp_results){

                        div(
                            class = "step-panel",
                            HTML("
                            <b>Step 1:</b> Choose competition data.<br>
                            <b>Step 2:</b> Buy Competition Data.<br>
                            <b>Step 3:</b> Choose resources.<br>
                            <b>Step 4:</b> Buy Competition Resources.<br>
                            <b>Step 5:</b> Competition Jump.
                            ")
                        )

                    } else {

                        div(
                            class = "step-panel",
                            HTML("
                            <b>Step 1:</b> Choose historical data.<br>
                            <b>Step 2:</b> Buy Historical Data.<br>
                            <b>Step 3:</b> Choose training resources.<br>
                            <b>Step 4:</b> Buy Training Resources.<br>
                            <b>Step 5:</b> Run Training Jump.
                            ")
                        )
                    },

                    br(),

                    rHandsontableOutput(
                        if(show_comp_results)
                            "spend_table_comp"
                        else
                            "spend_table_train"
                    ),

                    br(),

                    if(show_comp_results){

                        tagList(
                            actionButton("buy_comp_data","1: Buy Competition Data", class="btn-primary"),
                            actionButton("buy_comp_resources","2: Buy Competition Resources", class="btn-primary"),
                            actionButton("run_competition","3: Competition Jump", class="btn-primary")
                        )

                    } else {

                        tagList(
                            actionButton("buy_train_data","1: Buy Historical Data", class="btn-primary"),
                            actionButton("buy_train_resources","2: Buy Training Resources", class="btn-primary"),
                            actionButton("run_training","3: Run Training Jump", class="btn-primary")
                        )
                    }
                ),

                # =========================
                # STATUS MESSAGE
                # =========================
                div(
                    class = "card-style",
                    div(
                        class = "message-panel",
                        uiOutput(
                            if(show_comp_results)
                                "status_message_comp"
                            else
                                "status_message"
                        )
                    )
                )
            ),

            # ===================================================
            # RIGHT COLUMN
            # ===================================================

            column(
                width = 6,

                # =========================
                # REGRESSION PLOT (TOP RIGHT)
                # =========================
                div(
                    class = "card-style",

                    h3("Regression Analysis"),

                    plotOutput(
                        if(show_comp_results)
                            "regression_plot_comp"
                        else
                            "regression_plot",
                        height = "420px"
                    )
                ),

                # =========================
                # COEFFICIENTS (UNDER PLOT)
                # =========================
                div(
                    class = "card-style",

                    h3("Regression Coefficients"),

                    tableOutput(
                        if(show_comp_results)
                            "coef_table_comp"
                        else
                            "coef_table"
                    )
                ),

                # =========================
                # JUMP RESULTS (BOTTOM RIGHT)
                # =========================
                div(
                    class = "card-style",

                    h3("Jump Outcomes"),

                    uiOutput(
                        if(show_comp_results)
                            "jump_results_comp"
                        else
                            "jump_results"
                    )
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

    theme = bs_theme(
        version = 5,
        bootswatch = "lux",
        primary = "#7B9ACC",
        base_font = font_google("Inter")
    ),

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
}

.main-title p{
  font-size:18px;
  color:#3D405B;
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
}

.message-text{
  font-size:20px;
  font-weight:500;
  color:#2E4057;
}

.success-panel{
  background:#D8F3DC;
  color:#1B4332;
  padding:20px;
  border-radius:16px;
  font-size:24px;
  font-weight:700;
  text-align:center;
}

.fail-panel{
  background:#FDE2E4;
  color:#9D0208;
  padding:20px;
  border-radius:16px;
  font-size:24px;
  font-weight:700;
  text-align:center;
}

.step-panel{
  background:#EEF6FB;
  border-left:5px solid #89C2D9;
  padding:14px 18px;
  border-radius:12px;
  font-size:15px;
  color:#1D3557;
  line-height:1.7;
}

.info-box{
  background:#F8F9FB;
  padding:18px;
  border-radius:14px;
  line-height:1.7;
}

"))
        ),

        div(
            class = "main-title",

            h1("🎿 Ski Jump Challenge")
        )
    ),

    # =====================================================
    # OVERVIEW
    # =====================================================

    overview_page(

        explanation = tagList(

            p(
                "In this activity you investigate how information has value when making predictions."
            ),

            p(
                "With a limited budget, you decide how much to spend on historical data and training resources before using regression to predict ski jump distances."
            )
        ),

        individual = tagList(

            tags$ol(

                tags$li("Allocate your training budget."),

                tags$li("Purchase historical data."),

                tags$li("Purchase training resources."),

                tags$li("Fit a regression model."),

                tags$li("Predict and evaluate your jump.")
            )
        ),

        group = tagList(

            tags$ol(

                tags$li("Compare different spending strategies."),

                tags$li("Compare prediction accuracy across teams."),

                tags$li("Discuss the value of information."),

                tags$li("Investigate how budgets affect performance.")
            )
        ),

        question = tagList(

            tags$ul(

                tags$li("Should you spend more on data or resources?"),

                tags$li("How much does additional information improve prediction?"),

                tags$li("Can different strategies produce equally good results?"),

                tags$li("What is the opportunity cost of each spending decision?")
            )
        )
    ),

    # =====================================================
    # ACTIVITY
    # =====================================================

    nav_panel(

        "Activity",

        navset_tab(

            # =======================================================
            # TRAINING TAB
            # =======================================================

            nav_panel(

                "🎯 Training Phase",

                div(
                    class = "main-title",

                    h1("🎯 Training Phase"),

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
                        )
                    ),

                    main_panel_ui(FALSE)
                )
            ),

            # =======================================================
            # COMPETITION TAB
            # =======================================================

            nav_panel(

                "🏁 Competition Phase",

                div(
                    class = "main-title",

                    h1("🏁 Competition Phase"),

                    p("Use your remaining budget wisely and chase the world record.")
                ),

                layout_sidebar(

                    sidebar = div(

                        class = "card-style",

                        h4("Competition Rules"),

                        p("Maximum spend in any category across both phases is 10 units."),

                        p("Each phase has a total budget of 10 units.")
                    ),

                    main_panel_ui(TRUE)
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

        training_jump = NULL,
        competition_jump = NULL,

        train_complete = FALSE,
        resources_purchased = FALSE,
        comp_resources_purchased = FALSE
    )

    # =======================================================
    # TRAINING TABLE
    # =======================================================

    output$spend_table_train <- renderRHandsontable({

        vals <- c(0,0,0,0)

        if(!is.null(input$spend_table_train)){

            tmp <- hot_to_r(input$spend_table_train)

            vals <- as.numeric(tmp[2,1:4])

            vals[is.na(vals)] <- 0
        }

        df <- data.frame(

            Data      = c(10, vals[1]),
            Technique = c(10, vals[2]),
            Materials = c(10, vals[3]),
            Fitness   = c(10, vals[4]),
            Total     = c(10, sum(vals)),

            row.names = c(
                "Maximum spend available",
                "Actual spend"
            )
        )

        rhandsontable(
            df,
            rowHeaders = c(
                "Maximum available spend",
                "Actual spend"
            ),
            rowHeaderWidth = 220,
            stretchH = "all",
            width = 650,
            height = 140
        ) %>%

            hot_col(
                col = c("Data","Technique","Materials","Fitness"),
                type = "numeric",
                format = "0"
            ) %>%

            hot_row(1, readOnly = TRUE) %>%

            hot_col("Total", readOnly = TRUE)
    })

    # =======================================================
    # COMP TABLE
    # =======================================================

    output$spend_table_comp <- renderRHandsontable({

        train_vals <- c(0,0,0,0)

        if(!is.null(input$spend_table_train)){

            train_df <- hot_to_r(input$spend_table_train)

            train_vals <- as.numeric(train_df[2,1:4])

            train_vals[is.na(train_vals)] <- 0
        }

        max_vals <- c(
            10,
            10 - train_vals[2],
            10 - train_vals[3],
            10 - train_vals[4]
        )

        comp_vals <- c(0,0,0,0)

        if(!is.null(input$spend_table_comp)){

            comp_df <- hot_to_r(input$spend_table_comp)

            comp_vals <- as.numeric(comp_df[2,1:4])

            comp_vals[is.na(comp_vals)] <- 0
        }

        df <- data.frame(

            Data      = c(max_vals[1], comp_vals[1]),
            Technique = c(max_vals[2], comp_vals[2]),
            Materials = c(max_vals[3], comp_vals[3]),
            Fitness   = c(max_vals[4], comp_vals[4]),
            Total     = c(10, sum(comp_vals)),

            row.names = c(
                "Maximum spend available",
                "Actual spend"
            )
        )

        rhandsontable(
            df,
            rowHeaders = c(
                "Maximum available spend",
                "Actual spend"
            ),
            rowHeaderWidth = 220,
            stretchH = "all",
            width = 650,
            height = 140
        ) %>%

            hot_col(
                col = c("Data","Technique","Materials","Fitness"),
                type = "numeric",
                format = "0"
            ) %>%

            hot_row(1, readOnly = TRUE) %>%

            hot_col("Total", readOnly = TRUE)
    })

    # =======================================================
    # BUY TRAINING DATA
    # =======================================================

    observeEvent(input$buy_train_data, {

        set.seed(input$seed)

        rv$mu <- 254.5 - 60.5

        if(input$random_weights){

            rv$weight <- as.numeric(
                rdirichlet(1, rep(10, 3)) * 9
            )

            rv$sd <- exp(rnorm(1, log(10), 0.25))

        } else {

            rv$weight <- c(4,2,3)
            rv$sd <- 10
        }

        req(input$spend_table_train)

        train_df <- hot_to_r(input$spend_table_train)

        vals <- as.numeric(train_df[2,1:4])
        vals[is.na(vals)] <- 0

        msg <- validate_spend(
            vals,
            rep(10,4),
            "Training phase"
        )

        if(!is.null(msg)){

            showNotification(msg, type = "error")
            return()
        }

        spend <- vals[1]

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

        msg <- HTML(paste0(
            "<div class='message-text'>
            📊 Historical training data purchased:
            <b>", ndata,
            "</b> jumps.
            </div>"
        ))

        output$status_message <- renderUI(msg)
        output$status_message_comp <- renderUI(msg)
    })

    # =======================================================
    # BUY TRAINING RESOURCES
    # =======================================================

    observeEvent(input$buy_train_resources, {

        req(rv$weight)

        train_df <- hot_to_r(input$spend_table_train)

        vals <- as.numeric(train_df[2,1:4])
        vals[is.na(vals)] <- 0

        msg <- validate_spend(
            vals,
            rep(10,4),
            "Training phase"
        )

        if(!is.null(msg)){

            showNotification(msg, type = "error")
            return()
        }

        resource_spend <- vals[2:4]

        rv$resources_purchased <- TRUE

        output$status_message <- renderUI({

            HTML(paste0(
                "<div class='message-text'>
                🛠️ Training resources purchased:
                <b>",
                sum(resource_spend),
                "</b> total units allocated.
                </div>"
            ))
        })
    })

    # =======================================================
    # RUN TRAINING
    # =======================================================

    observeEvent(input$run_training, {

        req(rv$weight)
        req(rv$resources_purchased)

        train_df <- hot_to_r(input$spend_table_train)

        vals <- as.numeric(train_df[2,1:4])
        vals[is.na(vals)] <- 0

        msg <- validate_spend(
            vals,
            rep(10,4),
            "Training phase"
        )

        if(!is.null(msg)){

            showNotification(msg, type = "error")
            return()
        }

        spend <- vals[2:4]

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

        msg <- HTML(paste0(
            "<div class='message-text'>
            🎿 Training jump completed:
            <b>", rv$training_jump,
            " metres</b>
            </div>"
        ))

        output$status_message <- renderUI(msg)
        output$status_message_comp <- renderUI(msg)

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

    # =======================================================
    # BUY COMP DATA
    # =======================================================

    observeEvent(input$buy_comp_data, {

        req(rv$train_complete)

        comp_df <- hot_to_r(input$spend_table_comp)

        vals <- as.numeric(comp_df[2,1:4])
        vals[is.na(vals)] <- 0

        max_vals <- c(
            10,
            as.numeric(comp_df[1,2]),
            as.numeric(comp_df[1,3]),
            as.numeric(comp_df[1,4])
        )

        msg <- validate_spend(
            vals,
            max_vals,
            "Competition phase"
        )

        if(!is.null(msg)){

            showNotification(msg, type = "error")
            return()
        }

        spend <- vals[1]

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

        output$status_message_comp <- renderUI({

            HTML(paste0(
                "<div class='message-text'>
                📈 Competition data added:
                <b>", nrow(rv$all_data),
                "</b> total jumps.
                </div>"
            ))
        })
    })

    # =======================================================
    # BUY COMP RESOURCES
    # =======================================================

    observeEvent(input$buy_comp_resources, {

        req(rv$train_complete)

        comp_df <- hot_to_r(input$spend_table_comp)

        vals <- as.numeric(comp_df[2,1:4])

        vals[is.na(vals)] <- 0

        max_vals <- as.numeric(comp_df[1,1:4])

        msg <- validate_spend(
            vals,
            max_vals,
            "Competition phase"
        )

        if(!is.null(msg)){

            showNotification(msg, type = "error")
            return()
        }

        rv$comp_resources_purchased <- TRUE

        output$status_message_comp <- renderUI({

            HTML(paste0(
                "<div class='message-text'>
                🛠️ Competition resources purchased:
                <b>",
                sum(vals[2:4]),
                "</b> total units allocated.
                </div>"
            ))
        })
    })

    # =======================================================
    # RUN COMPETITION
    # =======================================================

    observeEvent(input$run_competition, {

        req(rv$train_complete)
        req(rv$comp_resources_purchased)

        train_df <- hot_to_r(input$spend_table_train)
        comp_df  <- hot_to_r(input$spend_table_comp)

        vals <- as.numeric(comp_df[2,1:4])
        vals[is.na(vals)] <- 0

        max_vals <- as.numeric(comp_df[1,1:4])

        msg <- validate_spend(
            vals,
            max_vals,
            "Competition phase"
        )

        if(!is.null(msg)){

            showNotification(msg, type = "error")
            return()
        }

        train_vals <- as.numeric(train_df[2,2:4])
        comp_vals  <- as.numeric(comp_df[2,2:4])

        train_vals[is.na(train_vals)] <- 0
        comp_vals[is.na(comp_vals)] <- 0

        total_spend <- train_vals + comp_vals

        rv$competition_jump <- round(
            ski_jump(
                total_spend,
                rv$weight,
                rv$mu,
                rv$sd
            ),
            2
        )

        if(rv$competition_jump >= input$wr){

            session$sendCustomMessage(
                "trigger_confetti",
                list()
            )
        }

        output$status_message_comp <- renderUI({

            HTML(paste0(
                "<div class='message-text'>
                🏁 Competition jump:
                <b>", rv$competition_jump,
                " metres</b>
                </div>"
            ))
        })

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

    # =======================================================
    # SHARED OUTPUTS
    # =======================================================

    render_shared_outputs <- function(prefix = ""){

        output[[paste0("regression_plot", prefix)]] <- renderPlot({

            req(rv$all_data)

            d <- melt(
                rv$all_data,
                id.vars = c("Jump_Length", "Phase")
            )

            colnames(d)[3:4] <- c("Variable", "Value")

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
                    formula = y ~ x,
                    colour = "#E63946",
                    se = FALSE,
                    linewidth = 1.2
                ) +

                theme_minimal(base_size = 16) +

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

            tab <- rbind(l1,l2,l3)

            colnames(tab) <- c(
                "Intercept",
                "Gradient"
            )

            rownames(tab) <- c(
                "Technique",
                "Materials",
                "Fitness"
            )

            round(tab,2)
        })
    }

    render_shared_outputs("")
    render_shared_outputs("_comp")

    # =======================================================
    # RESULTS
    # =======================================================

    output$jump_results <- renderUI({

        req(rv$training_jump)

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
        )
    })

    output$jump_results_comp <- renderUI({

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

                tagList(

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
                    ),

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
                )
            }
        )
    })
}

# =========================================================
# RUN APP
# =========================================================

shinyApp(ui, server)
