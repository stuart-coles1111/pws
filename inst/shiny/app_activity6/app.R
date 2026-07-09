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

                    br(),

                    div(
                        class = "card-style",

                        h4("📊 Historical Data"),

                        sliderInput(
                            if(show_comp_results)
                                "comp_data_rows"
                            else
                                "train_data_rows",

                            "Historical data purchased (jumps)",

                            min = 0,
                            max = 100,
                            value = 0,
                            step = 10
                        ),

                        uiOutput(
                            if(show_comp_results)
                                "comp_data_summary"
                            else
                                "train_data_summary"
                        ),

                        br(),

                        actionButton(
                            if(show_comp_results)
                                "buy_comp_data"
                            else
                                "buy_train_data",

                            if(show_comp_results)
                                "1: Buy Competition Data"
                            else
                                "1: Buy Historical Data",

                            class="btn-primary"
                        )
                    ),

                    br(),

                ),

                br(),

                div(
                    class = "card-style",

                    h4("🛠 Resources"),

                    sliderInput(
                        if(show_comp_results)
                            "comp_technique"
                        else
                            "train_technique",

                        "Technique budget",

                        min = 0,
                        max = 10000,
                        value = 0,
                        step = 1000
                    ),

                    sliderInput(
                        if(show_comp_results)
                            "comp_materials"
                        else
                            "train_materials",

                        "Materials budget",

                        min = 0,
                        max = 10000,
                        value = 0,
                        step = 1000
                    ),

                    sliderInput(
                        if(show_comp_results)
                            "comp_fitness"
                        else
                            "train_fitness",

                        "Fitness budget",

                        min = 0,
                        max = 10000,
                        value = 0,
                        step = 1000
                    ),

                    uiOutput(
                        if(show_comp_results)
                            "comp_resource_summary"
                        else
                            "train_resource_summary"
                    ),

                    br(),

                    tagList(

                        if(show_comp_results){

                            actionButton(
                                "buy_comp_resources",
                                "2: Buy Competition Resources",
                                class="btn-primary"
                            )

                        } else {

                            actionButton(
                                "buy_train_resources",
                                "2: Buy Training Resources",
                                class="btn-primary"
                            )
                        }
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
                    ),

                    br(),

                    if(show_comp_results){

                        actionButton(
                            "run_competition",
                            "3: Competition Jump",
                            class="btn-primary"
                        )

                    } else {

                        actionButton(
                            "run_training",
                            "3: Run Training Jump",
                            class="btn-primary"
                        )
                    }
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

rules_accordion <- function(id){

    bslib::accordion(

        id = id,

        open = FALSE,

        bslib::accordion_panel(

            title = "📖 Rules of play",

            tags$ol(

                tags$li(
                    "There are two phases: a training phase and a competition phase."
                ),

                tags$li(
                    "In each phase you have a budget of $10,000 to spend among three categories: Technique, Materials and Fitness."
                ),

                tags$li(
                    "Your total spending on each category across both phases cannot exceed $10,000."
                ),

                tags$li(
                    "Budget cannot be carried over from the training phase to the competition phase."
                ),

                tags$li(
                    "You make a jump after spending in each phase. The length of your jump depends partly on your spending choices."
                ),

                tags$li(
                    "You can buy historical ski jump data to help decide how to allocate your budget. Each $1,000 spent buys data from 10 historical jumps."
                ),

                tags$li(
                    "Historical data contain the spending allocation and jump outcome for the selected number of jumps. In these historical jumps, competitors could spend up to $10,000 on each category, but faced no other spending restrictions."
                ),

                tags$li(
                    "Data purchased during the training phase are retained and combined with any additional data purchased during the competition phase."
                ),

                tags$li(
                    "Your aim is to beat the world record."
                )

            )
        )
    )
}
# =========================================================
# UI
# =========================================================

ui <- page_navbar(

    title = "🎿 Activity 6: Breaking Records",

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

/* ================= SKI JUMP ANIMATION ================= */

.ski-world{
  position:relative;
  width:100%;
  height:140px;
  background:linear-gradient(
    to bottom,
    #BFE7FF 0%,
    #EAF7FF 45%,
    #FFFFFF 100%
  );
  overflow:hidden;
  border-radius:12px;
}

.skier{
  position:absolute;
  top:45px;
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

"))
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

                "Training Phase",

                rules_accordion("training_rules"),

                div(
                    class = "main-title",

                    h1("Training Phase")

                ),

                layout_sidebar(

                    sidebar = div(

                        class = "card-style",

                        h4("Game Setup"),

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

                        numericInput("seed", "Random seed", 123)

                    ),

                    main_panel_ui(FALSE)
                )
            ),

            # =======================================================
            # COMPETITION TAB
            # =======================================================

            nav_panel(

                "Competition Phase",

                rules_accordion("competition_rules"),

                div(
                    class = "main-title",

                    h1("Competition Phase")

                ),

                layout_sidebar(

                    sidebar = div(

                        class = "card-style",

                        h4("Competition Budget"),

                        uiOutput("competition_budget")

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

    get_train_resource_spend <- function(){

        c(
            input$train_technique / 1000,
            input$train_materials / 1000,
            input$train_fitness / 1000
        )

    }

    get_training_spend <- function(){

        c(
            input$train_data_rows / 10,
            input$train_technique / 1000,
            input$train_materials / 1000,
            input$train_fitness / 1000
        )
    }

    format_money <- function(x){

        paste0(
            "$",
            format(
                x,
                big.mark=",",
                scientific=FALSE
            )
        )
    }

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

    output$train_data_summary <- renderUI({

        req(input$train_data_rows)

        rows <- input$train_data_rows

        div(
            class = "info-box",

            paste0(
                "You will purchase ",
                rows,
                " historical jumps for ",
                format_money(rows * 100),
                "."
            )
        )
    })



    output$train_resource_summary <- renderUI({

        total <-
            input$train_technique +
            input$train_materials +
            input$train_fitness

        div(
            class="info-box",

            paste0(
                "Total resources allocated: ",
                total,
                " / 30"
            )
        )
    })


    output$competition_status <- renderUI({

        tagList(

            p("Training data available: 40 jumps"),

            p("Competition budget remaining: $10,000"),

            p("Previous training decisions retained.")
        )

    })

    output$competition_budget <- renderUI({

        req(input$spend_table_train)

        train_df <- hot_to_r(input$spend_table_train)

        vals <- get_training_spend()
        vals[is.na(vals)] <- 0

        categories <- c(
            "Technique",
            "Materials",
            "Fitness"
        )

        tagList(

            lapply(seq_along(categories), function(i){

                value <- vals[i]

                div(

                    style = "margin-bottom:18px;",

                    div(
                        style = "font-weight:600; margin-bottom:5px;",
                        categories[i]
                    ),

                    div(
                        style = "
                        background:#E9ECEF;
                        height:18px;
                        border-radius:10px;
                        overflow:hidden;
                    ",

                        div(
                            style = paste0(
                                "background:#89C2D9;
                             width:", value * 10, "%;
                             height:100%;"
                            )
                        )
                    ),

                    div(
                        style = "font-size:13px;color:#666;margin-top:3px;",
                        paste0(value, " / 10 used")
                    )
                )
            })
        )
    })


    # =======================================================
    # BUY TRAINING DATA
    # =======================================================

    observeEvent(input$buy_train_data, {

        set.seed(input$seed)

        rv$mu <- 254.5 - 60.5

        if(input$random_weights){

            rv$weight <- as.numeric(
                rdirichlet(1, rep(10,3)) * 9
            )

            rv$sd <- exp(rnorm(1, log(10), 0.25))

        } else {

            rv$weight <- c(4,2,3)
            rv$sd <- 10
        }


        ndata <- input$train_data_rows


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
        <b>",
            ndata,
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

        vals <- c(
            input$train_data_rows / 10,
            input$train_technique / 1000,
            input$train_materials / 1000,
            input$train_fitness / 1000
        )

        msg <- validate_spend(
            vals,
            rep(10,4),
            "Training phase"
        )

        if(!is.null(msg)){

            showNotification(
                msg,
                type = "error"
            )

            return()
        }

        rv$resources_purchased <- TRUE

        output$status_message <- renderUI({

            HTML(paste0(
                "<div class='message-text'>
            🛠️ Training resources purchased:
            <b>",
                sum(vals[2:4]),
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

        vals <- get_training_spend()
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

        vals <- get_training_spend()
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

        vals <- c(
            input$comp_data_rows / 10,
            input$comp_technique / 1000,
            input$comp_materials / 1000,
            input$comp_fitness / 1000
        )

        max_vals <- c(
            10,
            10,
            10,
            10
        )

        msg <- validate_spend(
            vals,
            max_vals,
            "Competition phase"
        )

        if(!is.null(msg)){

            showNotification(
                msg,
                type = "error"
            )

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

        train_vals <- c(
            input$train_technique / 1000,
            input$train_materials / 1000,
            input$train_fitness / 1000
        )

        comp_vals <- c(
            input$comp_technique / 1000,
            input$comp_materials / 1000,
            input$comp_fitness / 1000
        )

        total_spend <- train_vals + comp_vals

        if(any(total_spend > 10)){

            showNotification(
                "Total spending in a category cannot exceed $10,000 across both phases.",
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
            <b>",
                rv$competition_jump,
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
