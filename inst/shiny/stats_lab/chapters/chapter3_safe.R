# Chapter 3: Expected Goals (xG) simulation and modelling

chapter3_ui <- function(id) {
    ns <- shiny::NS(id)

    shiny::fluidPage(

        shiny::titlePanel("Chapter 3: Expected Goals (xG) Simulation"),

        shiny::sidebarLayout(

            shiny::sidebarPanel(

                shiny::h4("Simulate shot data"),

                shiny::numericInput(ns("n_data"),
                                    "Number of shots",
                                    value = 5000,
                                    min = 100),

                shiny::sliderInput(ns("bodypar"),
                                   "Probability of Head",
                                   min = 0, max = 1, value = 0.2, step = 0.01),

                shiny::sliderInput(ns("distpar"),
                                   "Distance rate (exponential)",
                                   min = 0.05, max = 1, value = 0.2, step = 0.01),

                shiny::sliderInput(ns("anglepar"),
                                   "Angle concentration",
                                   min = 0.5, max = 10, value = 3, step = 0.5),

                shiny::hr(),

                shiny::h4("True model parameters"),

                shiny::sliderInput(ns("intercept"),
                                   "Intercept",
                                   min = -2, max = 2, value = 0.25, step = 0.05),

                shiny::sliderInput(ns("dist_coeff"),
                                   "Distance effect",
                                   min = -1, max = 0, value = -0.1, step = 0.05),

                shiny::sliderInput(ns("dist_body_inter"),
                                   "Distance × Head interaction",
                                   min = -1, max = 1, value = -0.25, step = 0.05),

                shiny::sliderInput(ns("body_coeff"),
                                   "Body (Head) effect",
                                   min = -1, max = 1, value = -0.05, step = 0.05),

                shiny::sliderInput(ns("angle_coeff"),
                                   "Angle effect",
                                   min = -2, max = 0, value = -0.8, step = 0.05),

                shiny::actionButton(ns("run_sim"), "Simulate data"),

                shiny::hr(),

                shiny::h4("Prediction tool"),

                shiny::numericInput(ns("x"), "x coordinate", value = 5),
                shiny::numericInput(ns("y"), "y coordinate", value = 10),

                shiny::selectInput(ns("body"),
                                   "Body part",
                                   choices = c("Head", "Foot"),
                                   selected = "Foot")

            ),

            shiny::mainPanel(

                shiny::tabsetPanel(

                    shiny::tabPanel(
                        "Data",

                        shiny::verbatimTextOutput(ns("head_data"))
                    ),

                    shiny::tabPanel(
                        "Model",

                        shiny::tableOutput(ns("model_summary"))
                    ),

                    shiny::tabPanel(
                        "Plot",

                        shiny::plotOutput(ns("xg_plot"), height = 500)
                    ),

                    shiny::tabPanel(
                        "Prediction",

                        shiny::verbatimTextOutput(ns("pred_out")),

                        shiny::verbatimTextOutput(ns("total_xg"))
                    )
                )
            )
        )
    )
}


chapter3_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {

        # ---- simulate data ----
        xG_data <- shiny::eventReactive(input$run_sim, {
            pws::xGsim(
                n_data = input$n_data,
                bodypar = input$bodypar,
                distpar = input$distpar,
                anglepar = input$anglepar,
                intercept = input$intercept,
                dist_coeff = input$dist_coeff,
                dist_body_inter = input$dist_body_inter,
                body_coeff = input$body_coeff,
                angle_coeff = input$angle_coeff
            )
        })

        # ---- fit model ----
        xG_model <- shiny::reactive({
            req(xG_data())
            pws::xGfit(xG_data())
        })

        # ---- data preview ----
        output$head_data <- shiny::renderPrint({
            req(xG_data())
            head(xG_data()$data, 10)
        })

        # ---- model summary ----
        output$model_summary <- shiny::renderTable({
            req(xG_model())
            round(xG_model()$summary, 3)
        })

        # ---- plot ----
        output$xg_plot <- shiny::renderPlot({
            req(xG_data())
            pws::xGplot(xG_data())
        })

        # ---- prediction ----
        pred <- shiny::reactive({
            req(xG_model())
            pws::xGpred(
                xG_model(),
                x = input$x,
                y = input$y,
                body = input$body
            )
        })

        output$pred_out <- shiny::renderPrint({
            req(pred())
            round(pred(), 3)
        })

        # ---- total xG from simulated dataset ----
        output$total_xg <- shiny::renderPrint({
            req(xG_data())
            round(sum(xG_data()$data$prob_true), 3)
        })

    })
}
